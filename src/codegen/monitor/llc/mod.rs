use std::cmp::max;

use handlebars::Handlebars;
use rtlola_frontend::mir::{
    ArithLogOp, Constant, Expression, ExpressionKind, Offset, OutputStream, StreamAccessKind, StreamReference, WindowReference
};
use serde::Serialize;

use crate::hardware_ir::{self, Node};
use crate::{codegen::monitor::register_template, hardware_ir::HardwareIR};

use super::datatypes;

mod streams;

#[derive(Serialize)]
struct Data {
    debug: bool,
    streams: String,
    max_tag: usize,
    has_pipeline_wait: bool,
    pipeline_wait: usize,
    has_sliding_window: bool,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    sliding_windows: Vec<SlidingWindow>,
    tags: Vec<String>,
    enables: Vec<String>,
    output_phase_enables: Vec<String>,
    pacings_type: String,
    slides_type: String,
    all_tags_names: String,
    all_tags_defaults: String,
    cur_tags_levels: Vec<CurTagsLevel>,
    output_level: usize,
    extracted_tags_for_outputs: String,
}

#[derive(Serialize)]
struct Input {
    default_value: String,
}

#[derive(Serialize)]
struct Output {
    idx: usize,
    default_value: String,
    memory_more_than_one: bool,
    deps: Vec<Dependency>,
    level: usize,
    extracted_tags_of_depending: String,
}

#[derive(Serialize)]
struct SlidingWindow {
    idx: usize,
    input_data: String,
    input_data_default: String,
    input_memory_more_than_one: bool,
    tag: String,
    source_tag: String,
    level: usize,
    extracted_tags_of_depending: String,
}

#[derive(Serialize)]
struct CurTagsLevel {
    level: usize,
    delayed_tags: String,
}

#[derive(Serialize, Debug, Clone)]
struct Dependency {
    is_offset_access: bool,
    is_sync_access: bool,
    is_sliding_window_access: bool,
    default_value: String,
    offset: usize,
    source_name: String,
    source_tag: String,
    memory_more_than_one: bool,
    source_node: Node,
    node: Node,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "llc".to_string(),
        "src/codegen/monitor/llc/llc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        debug: ir.debug,
        streams: streams::render(ir, handlebars).unwrap(),
        max_tag: get_max_tag(ir),
        has_pipeline_wait: ir.pipeline_wait > 0,
        pipeline_wait: ir.pipeline_wait,
        has_sliding_window: ir.mir.sliding_windows.len() > 0,
        inputs: get_inputs(ir),
        outputs: get_outputs(ir),
        sliding_windows: get_sliding_windows(ir),
        tags: get_tags(ir),
        enables: get_enables(ir),
        output_phase_enables: get_output_phase_enables(ir),
        pacings_type: get_pacings_type(ir),
        slides_type: get_slides_type(ir),
        all_tags_names: get_all_tags_names(ir),
        all_tags_defaults: get_all_tags_defaults(ir),
        cur_tags_levels: get_cur_tags_levels(ir),
        output_level: ir.evaluation_order.len(),
        extracted_tags_for_outputs: get_extracted_tags_for_outputs(ir),
    };
    match handlebars.render("llc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

/// Example:
/// [
///     "tagIn0 = genTag input0HasData",
///     "tagOut0 = genTag p0",
///     "tagSw0 = genTag p1",
///     "tagOut1 = genTag p1",
/// ]
fn get_tags(ir: &HardwareIR) -> Vec<String> {
    ir.evaluation_order
        .iter()
        .map(|order| {
            order.iter().map(move |nd| match nd {
                Node::InputStream(x) => {
                    format!("tagIn{} = genTag input{}HasData", x.clone(), x.clone())
                }
                Node::OutputStream(x) => format!("tagOut{} = genTag p{}", x.clone(), x.clone()),
                Node::SlidingWindow(x) => format!("tagSw{} = genTag p{}", x.clone(), x.clone()),
            })
        })
        .flatten()
        .collect()
}

/// Example:
/// [
///     "enIn0 = input0HasData",
///     "enOut0 = delay False p0",
///     "enSw0 = delay False (delay False (slide0 .||. p1))",
///     "sld0 = delay False (delay False slide0)",
///     ...
/// ]
fn get_enables(ir: &HardwareIR) -> Vec<String> {
    ir.evaluation_order
        .iter()
        .enumerate()
        .map(|(level, order)| {
            order
                .iter()
                .map(move |nd| {
                    let (has_three, name1, pacing1, name2, pacing2, name3, pacing3) = match nd {
                        Node::InputStream(x) => (
                            false,
                            format!("enIn{}", x.clone()),
                            format!("input{}HasData", x.clone()),
                            String::new(),
                            String::new(),
                            String::new(),
                            String::new(),
                        ),
                        Node::OutputStream(x) => (
                            false,
                            format!("enOut{}", x.clone()),
                            format!("p{}", x.clone()),
                            String::new(),
                            String::new(),
                            String::new(),
                            String::new(),
                        ),
                        Node::SlidingWindow(x) => {
                            let name1 = format!("enSw{}", x.clone());
                            let pacing1 = match ir.mir.sliding_windows[x.clone()].caller {
                                StreamReference::Out(out_idx) => {
                                    format!("slide{} .||. p{}", x.clone(), out_idx.clone())
                                }
                                _ => unreachable!(),
                            };
                            let name2 = format!("sld{}", x.clone());
                            let pacing2 = format!("slide{}", x.clone());
                            let name3 = format!("sw{}DataPacing", x.clone());
                            let pacing3 = match ir.mir.sliding_windows[x.clone()].target {
                                StreamReference::In(x) => format!("input{}HasData", x.clone()),
                                StreamReference::Out(x) => format!("p{}", x.clone()),
                            };
                            (true, name1, pacing1, name2, pacing2, name3, pacing3)
                        }
                    };
                    if has_three {
                        vec![
                            format!(
                                "{} = {}",
                                name1,
                                surround_with_delay(level + 1, "False".to_string(), pacing1)
                            ),
                            format!(
                                "{} = {}",
                                name2,
                                surround_with_delay(level + 1, "False".to_string(), pacing2)
                            ),
                            format!(
                                "{} = {}",
                                name3,
                                surround_with_delay(level + 1, "False".to_string(), pacing3)
                            ),
                        ]
                    } else {
                        vec![format!(
                            "{} = {}",
                            name1,
                            surround_with_delay(level + 1, "False".to_string(), pacing1)
                        )]
                    }
                })
                .flatten()
        })
        .flatten()
        .collect()
}

/// Example:
/// [
///     "output0Aktv = delay False (delay False (delay False (delay False p0)))",
///     ...
/// ]
fn get_output_phase_enables(ir: &HardwareIR) -> Vec<String> {
    let eval_levels = ir.evaluation_order.len();
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| {
            format!(
                "output{}Aktv = {}",
                i.clone(),
                surround_with_delay(
                    eval_levels + 1,
                    "False".to_string(),
                    format!("p{}", i.clone())
                )
            )
        })
        .collect()
}

fn surround_with_delay(times: usize, default_value: String, data: String) -> String {
    if times > 0 {
        let surround_left = format!("delay {} (", default_value).repeat(times);
        let surround_right = ")".repeat(times - 1);
        format!(
            "{}{}{}",
            String::from(&surround_left[..surround_left.len() - 1]),
            data,
            surround_right
        )
    } else {
        data
    }
}

fn get_sliding_windows(ir: &HardwareIR) -> Vec<SlidingWindow> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, sw)| {
            let node = Node::SlidingWindow(i.clone());
            let level = hardware_ir::find_level(&node, &ir.evaluation_order);
            SlidingWindow {
                idx: i.clone(),
                input_data: match sw.target {
                    StreamReference::In(x) => format!("input{}Win", x.clone()),
                    StreamReference::Out(x) => format!("out{}", x.clone()),
                },
                input_data_default: match sw.target {
                    StreamReference::In(x) => datatypes::get_default_for_type(&ir.mir.inputs[x].ty),
                    StreamReference::Out(x) => {
                        datatypes::get_default_for_type(&ir.mir.outputs[x].ty)
                    }
                },
                input_memory_more_than_one: {
                    let node = match sw.target {
                        StreamReference::In(x) => Node::InputStream(x.clone()),
                        StreamReference::Out(x) => Node::OutputStream(x.clone()),
                    };
                    ir.required_memory.get(&node).unwrap().clone() > 1
                },
                tag: {
                    let level = hardware_ir::find_level(&node, &ir.evaluation_order);
                    if level > 0 {
                        format!("{}Level{}TagSw{}", get_node_name(&node), level, i.clone())
                    } else {
                        format!("tagSw{}", i.clone())
                    }
                },
                source_tag: {
                    let source_node = match sw.target {
                        StreamReference::In(x) => Node::InputStream(x.clone()),
                        StreamReference::Out(x) => Node::OutputStream(x.clone()),
                    };
                    get_source_tag(&node, &source_node, ir)
                },
                level: level.clone(),
                extracted_tags_of_depending: get_extracted_tags_for_sliding_window(
                    &node, level, ir,
                ),
            }
        })
        .collect()
}

fn get_inputs(ir: &HardwareIR) -> Vec<Input> {
    ir.mir
        .inputs
        .iter()
        .map(|inpt| Input {
            default_value: datatypes::get_default_for_type(&inpt.ty),
        })
        .collect()
}

fn get_outputs(ir: &HardwareIR) -> Vec<Output> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, out)| {
            let node = Node::OutputStream(i.clone());
            let deps: Vec<Dependency> = {
                let mut deps = get_dependencies_from_expression(
                    Node::OutputStream(i),
                    &out.eval.clauses.first().unwrap().expression,
                    ir,
                );
                order_dependencies_according_to_access_list(&mut deps, out);
                deps
            };
            let level = hardware_ir::find_level(&Node::OutputStream(i), &ir.evaluation_order);
            Output {
                idx: i,
                default_value: datatypes::get_default_for_type(&out.ty),
                memory_more_than_one: ir.required_memory.get(&node).unwrap().clone() > 1,
                deps: deps.clone(),
                level: level.clone(),
                extracted_tags_of_depending: get_extracted_tags_of_dependencies(&node, deps, level, ir),
            }
        })
        .collect()
}

fn get_dependencies_from_expression(
    node: Node,
    expr: &Expression,
    ir: &HardwareIR,
) -> Vec<Dependency> {
    // default values of offsets, hold etc. can only be accessed from the expression
    match &expr.kind {
        ExpressionKind::StreamAccess {
            target: source,
            parameters: _,
            access_kind,
        } => {
            let source_name = match source {
                StreamReference::In(x) => format!("input{}Win", x.clone()),
                StreamReference::Out(x) => format!("out{}", x.clone()),
            };
            let source_node = match source {
                StreamReference::In(x) => Node::InputStream(x.clone()),
                StreamReference::Out(x) => Node::OutputStream(x.clone()),
            };
            let source_tag = get_source_tag(&node, &source_node, ir);
            let memory_more_than_one = match source {
                StreamReference::In(x) => {
                    ir.required_memory
                        .get(&Node::InputStream(x.clone()))
                        .unwrap()
                        .clone()
                        > 1
                }
                StreamReference::Out(x) => {
                    ir.required_memory
                        .get(&Node::OutputStream(x.clone()))
                        .unwrap()
                        .clone()
                        > 1
                }
            };
            let dep = match access_kind {
                StreamAccessKind::Sync => Dependency {
                    is_sync_access: true,
                    is_offset_access: false,
                    is_sliding_window_access: false,
                    offset: 0,
                    node,
                    source_name,
                    source_node,
                    source_tag,
                    memory_more_than_one,
                    default_value: match source {
                        StreamReference::In(x) => {
                            datatypes::get_default_for_type(&ir.mir.inputs[x.clone()].ty)
                        }
                        StreamReference::Out(x) => {
                            datatypes::get_default_for_type(&ir.mir.outputs[x.clone()].ty)
                        }
                    },
                },
                StreamAccessKind::Hold => Dependency {
                    is_sync_access: true,
                    is_offset_access: false,
                    is_sliding_window_access: false,
                    offset: 0,
                    node,
                    source_name,
                    source_node,
                    source_tag,
                    memory_more_than_one,
                    default_value: String::new(),
                },
                StreamAccessKind::Offset(off) => Dependency {
                    is_offset_access: true,
                    is_sync_access: false,
                    is_sliding_window_access: false,
                    node,
                    source_name,
                    source_node,
                    source_tag,
                    memory_more_than_one,
                    default_value: String::new(),
                    offset: match off {
                        Offset::Past(x) => x.clone() as usize,
                        _ => unimplemented!(),
                    },
                },
                StreamAccessKind::SlidingWindow(sw) => {
                    let (default_value, source_name, memory) = match sw {
                        WindowReference::Sliding(x) => {
                            let default_value = format!(
                                "(repeat {})",
                                datatypes::get_default_for_type(
                                    &ir.mir.sliding_windows[x.clone()].ty,
                                )
                            );
                            let source_name = format!("sw{}", x);
                            let memory = ir
                                .required_memory
                                .get(&Node::SlidingWindow(x.clone()))
                                .unwrap()
                                .clone();
                            (default_value, source_name, memory)
                        }
                        _ => unimplemented!(),
                    };
                    let source_node = match sw {
                        WindowReference::Sliding(x) => Node::SlidingWindow(x.clone()),
                        _ => unimplemented!(),
                    };
                    Dependency {
                        is_sliding_window_access: true,
                        is_offset_access: false,
                        is_sync_access: false,
                        offset: 0,
                        default_value,
                        node,
                        source_name,
                        source_node,
                        source_tag,
                        memory_more_than_one: memory > 1,
                    }
                }
                _ => unimplemented!(),
            };
            vec![dep]
        }
        ExpressionKind::Default { expr, default } => {
            let deps = get_dependencies_from_expression(node, &expr, ir);
            let default_value = get_default_value(&default);
            deps.iter()
                .map(|dep| Dependency {
                    is_offset_access: dep.is_offset_access,
                    is_sliding_window_access: dep.is_sliding_window_access,
                    is_sync_access: dep.is_sync_access,
                    offset: dep.offset,
                    node: dep.node.clone(),
                    source_name: dep.source_name.clone(),
                    source_node: dep.source_node.clone(),
                    source_tag: dep.source_tag.clone(),
                    memory_more_than_one: dep.memory_more_than_one,
                    default_value: default_value.clone(),
                })
                .collect()
        }
        ExpressionKind::ArithLog(_, exprs) => exprs
            .iter()
            .map(|expr| get_dependencies_from_expression(node.clone(), expr, ir))
            .reduce(|acc, lst| {
                let mut acc = acc;
                acc.extend(lst);
                acc
            })
            .unwrap(),
        ExpressionKind::LoadConstant(_) => Vec::new(),
        _ => unimplemented!(),
    }
}

fn order_dependencies_according_to_access_list(deps: &mut Vec<Dependency>, out: &OutputStream) {
    deps.sort_by_key(|dep| {
        let accesses: Vec<Node> = out
            .accesses
            .iter()
            .map(|access| {
                let source_node = match access.0 {
                    StreamReference::In(x) => Node::InputStream(x.clone()),
                    StreamReference::Out(x) => Node::OutputStream(x.clone()),
                };
                match access.1.first().unwrap().1 {
                    StreamAccessKind::SlidingWindow(sw) => match sw {
                        WindowReference::Sliding(x) => Node::SlidingWindow(x.clone()),
                        _ => unimplemented!(),
                    },
                    _ => source_node,
                }
            })
            .collect();
        let index = accesses
            .iter()
            .position(|source_node| *source_node == dep.source_node)
            .unwrap();
        index
    });
}

fn get_default_value(expr: &Expression) -> String {
    dbg!(&expr);
    match &expr.kind {
        ExpressionKind::StreamAccess { target, parameters, access_kind } => {
            "0".to_string()
        },
        ExpressionKind::LoadConstant(x) => match x {
            Constant::Int(x) => format!("{}", x),
            _ => unimplemented!(),
        },
        ExpressionKind::ArithLog(op, exprs) => {
            match op {
                ArithLogOp::Add => {
                    "1000".to_string()
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

pub fn get_pacings_type(ir: &HardwareIR) -> String {
    let pacings: Vec<String> = ir.mir.outputs.iter().map(|_| "Bool".to_string()).collect();
    if pacings.len() > 1 {
        format!("({})", pacings.join(", "))
    } else {
        format!("{}", pacings.join(", "))
    }
}

pub fn get_slides_type(ir: &HardwareIR) -> String {
    let pacings: Vec<String> = ir
        .mir
        .sliding_windows
        .iter()
        .map(|_| "Bool".to_string())
        .collect();
    if pacings.len() > 1 {
        format!("({})", pacings.join(", "))
    } else {
        format!("{}", pacings.join(", "))
    }
}

fn get_max_tag(ir: &HardwareIR) -> usize {
    let max_window_size = streams::get_sliding_windows(ir)
        .iter()
        .map(|sw| sw.window_size.clone())
        .max()
        .unwrap_or(0);
    let max_offset = get_max_offset(ir);
    max(max_window_size, max_offset) + 1
}

fn get_max_offset(ir: &HardwareIR) -> usize {
    ir.mir
        .outputs
        .iter()
        .map(|out| {
            out.accesses
                .iter()
                .map(|access| match access.1.first().unwrap().1 {
                    StreamAccessKind::Offset(off) => match off {
                        Offset::Past(x) => x.clone() as usize,
                        _ => unreachable!(),
                    },
                    _ => 0,
                })
                .max()
                .unwrap_or(0)
        })
        .max()
        .unwrap_or(0)
}

fn get_all_tags_names(ir: &HardwareIR) -> String {
    let inputs: Vec<String> = ir
        .mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("tagIn{}", i.clone()))
        .collect();
    let outputs: Vec<String> = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("tagOut{}", i.clone()))
        .collect();
    let slidings: Vec<String> = ir
        .mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| format!("tagSw{}", i.clone()))
        .collect();
    let all_tags: Vec<String> = vec![&inputs[..], &outputs[..], &slidings[..]].concat();
    if all_tags.len() > 1 {
        format!("bundle ({})", all_tags.join(", "))
    } else {
        format!("{}", all_tags.join(", "))
    }
}

pub fn get_all_tags_defaults(ir: &HardwareIR) -> String {
    let inputs: Vec<String> = ir
        .mir
        .inputs
        .iter()
        .map(|_| "invalidTag".to_string())
        .collect();
    let outputs: Vec<String> = ir
        .mir
        .outputs
        .iter()
        .map(|_| "invalidTag".to_string())
        .collect();
    let slidings: Vec<String> = ir
        .mir
        .sliding_windows
        .iter()
        .map(|_| "invalidTag".to_string())
        .collect();
    let all_defaults = vec![&inputs[..], &outputs[..], &slidings[..]].concat();
    if all_defaults.len() > 1 {
        format!("({})", all_defaults.join(", "))
    } else {
        format!("{}", all_defaults.join(", "))
    }
}

fn get_source_tag(node: &Node, source_node: &Node, ir: &HardwareIR) -> String {
    let node_level = hardware_ir::find_level(&node, &ir.evaluation_order);
    let prefix = if node_level > 0 {
        format!("{}Level{}Tag", get_node_name(node), node_level)
    } else {
        "tag".to_string()
    };
    match &source_node {
        Node::InputStream(x) => format!("{}In{}", prefix, x.clone()),
        Node::OutputStream(x) => format!("{}Out{}", prefix, x.clone()),
        _ => unreachable!(),
    }
}

fn get_cur_tags_levels(ir: &HardwareIR) -> Vec<CurTagsLevel> {
    ir.evaluation_order
        .iter()
        .enumerate()
        .map(|(level, _)| CurTagsLevel {
            level: level + 1,
            delayed_tags: surround_with_delay(
                level + 1,
                "tagsDefault".to_string(),
                "curTags".to_string(),
            ),
        })
        .collect()
}

/// Example:
/// (_, _, _, level1TagOut1, _, _) = unbundle curTagsLevel1
fn get_extracted_tags_of_dependencies(
    node: &Node,
    deps: Vec<Dependency>,
    level: usize,
    ir: &HardwareIR,
) -> String {
    let node_name = get_node_name(node);
    let extracted: Vec<String> = get_all_streams(ir)
        .iter()
        .map(|node| {
            let dep = deps.iter().find(|&dep| dep.source_node == *node);
            match dep {
                Some(d) => {
                    if d.memory_more_than_one || true {
                        match d.source_node {
                            Node::InputStream(x) => format!("{}Level{}TagIn{}", node_name, level, x),
                            Node::OutputStream(x) => format!("{}Level{}TagOut{}", node_name, level, x),
                            Node::SlidingWindow(x) => format!("{}Level{}TagSw{}", node_name, level, x),
                        }
                    } else {
                        "_".to_string()
                    }
                }
                None => "_".to_string(),
            }
        })
        .collect();
    if extracted.len() > 1 {
        format!(
            "({}) = unbundle curTagsLevel{}",
            extracted.join(", "),
            level
        )
    } else {
        format!("{} = curTagsLevel{}", extracted.join(", "), level)
    }
}

fn get_extracted_tags_for_sliding_window(node: &Node, level: usize, ir: &HardwareIR) -> String {
    let node_name = get_node_name(node);
    let deps: Vec<Node> = match node {
        Node::SlidingWindow(x) => {
            let dep = Node::from_stream(&ir.mir.sliding_windows[x.clone()].target);
            vec![node.clone(), dep]
        }
        _ => unreachable!(),
    };
    let extracted: Vec<String> = get_all_streams(ir)
        .iter()
        .map(|node| {
            let dep = deps.iter().find(|&dep| *dep == *node);
            match dep {
                Some(d) => match d {
                    Node::InputStream(x) => format!("{}Level{}TagIn{}", node_name, level, x),
                    Node::OutputStream(x) => format!("{}Level{}TagOut{}", node_name, level, x),
                    Node::SlidingWindow(x) => format!("{}Level{}TagSw{}", node_name, level, x),
                },
                None => "_".to_string(),
            }
        })
        .collect();
    format!(
        "({}) = unbundle curTagsLevel{}",
        extracted.join(", "),
        level
    )
}

fn get_all_streams(ir: &HardwareIR) -> Vec<Node> {
    let inputs: Vec<Node> = ir
        .mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, _)| Node::InputStream(i.clone()))
        .collect();
    let outputs: Vec<Node> = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| Node::OutputStream(i.clone()))
        .collect();
    let slidings: Vec<Node> = ir
        .mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| Node::SlidingWindow(i.clone()))
        .collect();
    [&inputs[..], &outputs[..], &slidings[..]].concat()
}

fn get_extracted_tags_for_outputs(ir: &HardwareIR) -> String {
    let level = ir.evaluation_order.len();
    let extracted: Vec<String> = get_all_streams(ir)
        .iter()
        .map(|node| match node {
            Node::OutputStream(x) => {
                format!("level{}TagOut{}", level.clone(), x)
            }
            _ => "_".to_string(),
        })
        .collect();
    if extracted.len() > 1 {
        format!(
            "({}) = unbundle curTagsLevel{}",
            extracted.join(", "),
            level
        )
    } else {
        format!("{} = curTagsLevel{}", extracted.join(", "), level)
    }
}

fn get_node_name(node: &Node) -> String {
    match node {
        Node::OutputStream(x) => format!("out{}", x),
        Node::SlidingWindow(x) => format!("sw{}", x),
        _ => unreachable!(),
    }
}