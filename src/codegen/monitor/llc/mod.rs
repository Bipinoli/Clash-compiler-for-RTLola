use handlebars::Handlebars;
use rtlola_frontend::mir::{
    Constant, Expression, ExpressionKind, Offset, OutputStream, StreamAccessKind, StreamReference,
    WindowReference,
};
use serde::Serialize;

use crate::hardware_ir::Node;
use crate::{codegen::monitor::register_template, hardware_ir::HardwareIR};

use super::datatypes;

mod streams;

#[derive(Serialize)]
struct Data {
    debug: bool,
    streams: String,
    max_tag: usize,
    has_pipeline_wait: bool,
    has_sliding_window: bool,
    inputs: Vec<()>,
    outputs: Vec<Output>,
    sliding_windows: Vec<SlidingWindow>,
    tags: Vec<String>,
    enables: Vec<String>,
    output_phase_enables: Vec<String>,
}

#[derive(Serialize)]
struct Output {
    idx: usize,
    default_value: String,
    memory_more_than_one: bool,
    deps: Vec<Dependency>,
}

#[derive(Serialize)]
struct SlidingWindow {
    idx: usize,
    input_data: String,
    input_data_default: String,
    input_memory_more_than_one: bool,
}

#[derive(Serialize)]
struct Dependency {
    is_offset_access: bool,
    is_sync_access: bool,
    is_sliding_window_access: bool,
    default_value: String,
    offset: usize,
    source: String,
    memory_more_than_one: bool,
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
        max_tag: streams::get_sliding_windows(ir)
            .iter()
            .map(|sw| sw.window_size.clone())
            .max()
            .unwrap_or(5),
        has_pipeline_wait: ir.pipeline_wait > 0,
        has_sliding_window: ir.mir.sliding_windows.len() > 0,
        inputs: ir.mir.inputs.iter().map(|_| ()).collect(),
        outputs: get_outputs(ir),
        sliding_windows: get_sliding_windows(ir),
        tags: get_tags(ir),
        enables: get_enables(ir),
        output_phase_enables: get_output_phase_enables(ir),
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
///   "in0Tag = tag",
///   "out0Tag = delay invalidTag tag",
///   "sw0Tag = delay invalidTag (delay invalidTag tag)"
/// ]
fn get_tags(ir: &HardwareIR) -> Vec<String> {
    ir.evaluation_order
        .iter()
        .enumerate()
        .map(|(i, order)| {
            order.iter().map(move |nd| {
                let tag = surround_with_delay(i, "invalidTag".to_string(), "tag".to_string());
                match nd {
                    Node::InputStream(x) => format!("in{}Tag = {}", x.clone(), tag),
                    Node::OutputStream(x) => format!("out{}Tag = {}", x.clone(), tag),
                    Node::SlidingWindow(x) => format!("sw{}Tag = {}", x.clone(), tag),
                }
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
        .map(|(i, order)| {
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
                                surround_with_delay(i, "False".to_string(), pacing1)
                            ),
                            format!(
                                "{} = {}",
                                name2,
                                surround_with_delay(i, "False".to_string(), pacing2)
                            ),
                            format!(
                                "{} = {}",
                                name3,
                                surround_with_delay(i, "False".to_string(), pacing3)
                            ),
                        ]
                    } else {
                        vec![format!(
                            "{} = {}",
                            name1,
                            surround_with_delay(i, "False".to_string(), pacing1)
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
///     "outputPhaseTag = delay invalidTag (delay invalidTag (delay invalidTag (delay invalidTag tag)))",
///     "output0Aktv = delay False (delay False (delay False (delay False p0)))",
///     ...
/// ]
fn get_output_phase_enables(ir: &HardwareIR) -> Vec<String> {
    let eval_levels = ir.evaluation_order.len();
    let output_phase_tag = vec![format!(
        "outputPhaseTag = {}",
        surround_with_delay(eval_levels, "invalidTag".to_string(), "tag".to_string())
    )];
    let output_aktvs: Vec<String> = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| {
            format!(
                "output{}Aktv = {}",
                i.clone(),
                surround_with_delay(eval_levels, "False".to_string(), format!("p{}", i.clone()))
            )
        })
        .collect();
    vec![&output_phase_tag[..], &output_aktvs[..]].concat()
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
        .map(|(i, sw)| SlidingWindow {
            idx: i.clone(),
            input_data: match sw.target {
                StreamReference::In(x) => format!("input{}Win", x.clone()),
                StreamReference::Out(x) => format!("out{}", x.clone()),
            },
            input_data_default: match sw.target {
                StreamReference::In(x) => datatypes::get_default_for_type(&ir.mir.inputs[x].ty),
                StreamReference::Out(x) => datatypes::get_default_for_type(&ir.mir.outputs[x].ty),
            },
            input_memory_more_than_one: {
                let node = match sw.target {
                    StreamReference::In(x) => Node::InputStream(x.clone()),
                    StreamReference::Out(x) => Node::OutputStream(x.clone()),
                };
                ir.required_memory.get(&node).unwrap().clone() > 1
            },
        })
        .collect()
}

fn get_outputs(ir: &HardwareIR) -> Vec<Output> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, out)| Output {
            idx: i,
            default_value: datatypes::get_default_for_type(&out.ty),
            memory_more_than_one: ir
                .required_memory
                .get(&Node::OutputStream(i.clone()))
                .unwrap()
                .clone()
                > 1,
            deps: {
                let mut deps = get_dependencies_from_expression(
                    &out.eval.clauses.first().unwrap().expression,
                    ir,
                );
                order_dependencies_according_to_access_list(&mut deps, out);
                deps
            },
        })
        .collect()
}

fn get_dependencies_from_expression(expr: &Expression, ir: &HardwareIR) -> Vec<Dependency> {
    // default values of offsets, hold etc. can only be accessed from the expression
    match &expr.kind {
        ExpressionKind::StreamAccess {
            target,
            parameters: _,
            access_kind,
        } => {
            let source = match target {
                StreamReference::In(x) => format!("input{}Win", x),
                StreamReference::Out(x) => format!("out{}", x),
            };
            let node = match target {
                StreamReference::In(x) => Node::InputStream(x.clone()),
                StreamReference::Out(x) => Node::OutputStream(x.clone()),
            };
            let memory_more_than_one = match target {
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
                    source,
                    node,
                    memory_more_than_one,
                    default_value: match target {
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
                    source,
                    node,
                    memory_more_than_one,
                    default_value: String::new(),
                },
                StreamAccessKind::Offset(off) => Dependency {
                    is_offset_access: true,
                    is_sync_access: false,
                    is_sliding_window_access: false,
                    source,
                    node,
                    memory_more_than_one,
                    default_value: String::new(),
                    offset: match off {
                        Offset::Past(x) => x.clone() as usize,
                        _ => unimplemented!(),
                    },
                },
                StreamAccessKind::SlidingWindow(sw) => {
                    let (default_value, source, memory) = match sw {
                        WindowReference::Sliding(x) => {
                            let default_value = format!(
                                "(repeat {})",
                                datatypes::get_default_for_type(
                                    &ir.mir.sliding_windows[x.clone()].ty,
                                )
                            );
                            let source = format!("sw{}", x);
                            let memory = ir
                                .required_memory
                                .get(&Node::SlidingWindow(x.clone()))
                                .unwrap()
                                .clone();
                            (default_value, source, memory)
                        }
                        _ => unimplemented!(),
                    };
                    let node = match sw {
                        WindowReference::Sliding(x) => Node::SlidingWindow(x.clone()),
                        _ => unimplemented!(),
                    };
                    Dependency {
                        is_sliding_window_access: true,
                        is_offset_access: false,
                        is_sync_access: false,
                        offset: 0,
                        default_value,
                        source,
                        node,
                        memory_more_than_one: memory > 1,
                    }
                }
                _ => unimplemented!(),
            };
            vec![dep]
        }
        ExpressionKind::Default { expr, default } => {
            let deps = get_dependencies_from_expression(&expr, ir);
            let default_value = get_default_value(&default);
            deps.iter()
                .map(|dep| Dependency {
                    is_offset_access: dep.is_offset_access,
                    is_sliding_window_access: dep.is_sliding_window_access,
                    is_sync_access: dep.is_sync_access,
                    offset: dep.offset,
                    source: dep.source.clone(),
                    node: dep.node.clone(),
                    memory_more_than_one: dep.memory_more_than_one,
                    default_value: default_value.clone(),
                })
                .collect()
        }
        ExpressionKind::ArithLog(_, exprs) => exprs
            .iter()
            .map(|expr| get_dependencies_from_expression(expr, ir))
            .reduce(|acc, lst| {
                let mut acc = acc;
                acc.extend(lst);
                acc
            })
            .unwrap(),
        _ => unimplemented!(),
    }
}

fn order_dependencies_according_to_access_list(deps: &mut Vec<Dependency>, out: &OutputStream) {
    deps.sort_by_key(|dep| {
        let accesses: Vec<Node> = out
            .accesses
            .iter()
            .map(|access| {
                let node = match access.0 {
                    StreamReference::In(x) => Node::InputStream(x.clone()),
                    StreamReference::Out(x) => Node::OutputStream(x.clone()),
                };
                match access.1.first().unwrap().1 {
                    StreamAccessKind::SlidingWindow(sw) => match sw {
                        WindowReference::Sliding(x) => Node::SlidingWindow(x.clone()),
                        _ => unimplemented!(),
                    },
                    _ => node,
                }
            })
            .collect();
        let index = accesses.iter().position(|node| *node == dep.node).unwrap();
        index
    });
}

fn get_default_value(expr: &Expression) -> String {
    match &expr.kind {
        ExpressionKind::LoadConstant(x) => match x {
            Constant::Int(x) => format!("{}", x),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
