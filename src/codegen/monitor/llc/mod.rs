use std::cmp::max;
use std::collections::HashSet;

use handlebars::Handlebars;
use rtlola_frontend::mir::{
    ArithLogOp, Constant, Expression, ExpressionKind, Offset, StreamAccessKind, StreamReference,
    WindowReference,
};
use serde::Serialize;

use crate::analysis::node::Node;
use crate::{analysis::HardwareIR, codegen::monitor::register_template};

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
    has_input: bool,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    sliding_windows: Vec<SlidingWindow>,
    tags: Vec<String>,
    enables: Vec<String>,
    output_phase_enables: Vec<String>,
    all_tags_names: Vec<String>,
    cur_tags_levels: Vec<CurTagsLevel>,
    output_level: usize,
}

#[derive(Serialize)]
struct Input {
    default_value: String,
}

#[derive(Serialize)]
struct Output {
    idx: usize,
    ty: String,
    default_value: String,
    memory_more_than_one: bool,
    deps: Vec<Dependency>,
    level: usize,
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
    is_hold_access: bool,
    default_value: String,
    offset: usize,
    source_name: String,
    source_tag: String,
    memory_more_than_one: bool,
    source_node: Node,
    target_node: Node,
    has_default_expr: bool,
    default_expr_statements: Vec<String>,
    depending_tags_from_default_expr: Vec<Node>,
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
        has_input: ir.mir.inputs.len() > 0,
        inputs: get_inputs(ir),
        outputs: get_outputs(ir),
        sliding_windows: get_sliding_windows(ir),
        tags: get_tags(ir),
        enables: get_enables(ir),
        output_phase_enables: get_output_phase_enables(ir),
        all_tags_names: get_all_tags_names(ir),
        cur_tags_levels: get_cur_tags_levels(ir),
        output_level: ir.evaluation_order.len(),
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
///     "tIn0 = genTag (getPacing <$> pIn0)",
///     "tOut0 = genTag (getPacing <$> pOut0)",
///     "tSw0 = genTag (getPacing <$> pOut1)",
///     "tOut1 = genTag (getPacing <$> pOut1)",
/// ]
fn get_tags(ir: &HardwareIR) -> Vec<String> {
    ir.evaluation_order
        .iter()
        .map(|order| {
            order.iter().map(move |nd| match nd {
                Node::InputStream(x) => {
                    format!("tIn{} = genTag (getPacing <$> pIn{})", x.clone(), x.clone())
                }
                Node::OutputStream(x) => format!(
                    "tOut{} = genTag (getPacing <$> pOut{})",
                    x.clone(),
                    x.clone()
                ),
                Node::SlidingWindow(x) => {
                    let pacing = datatypes::get_target_from_sliding_window(
                        &ir.mir.sliding_windows[x.clone()],
                    );
                    format!("tSw{} = genTag (getPacing <$> p{})", x.clone(), pacing)
                }
            })
        })
        .flatten()
        .collect()
}

/// Example:
/// [
///     "enIn0 = pIn0",
///     "enOut0 = delayFor d1 nullPacingOut0 pOut0",
///     "enSw0 = delayFor d2 False pOut1",
///     "sld0 = delayFor d2 False slide0",
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
                    let (has_two, name1, pacing1, default1, name2, pacing2, default2) = match nd {
                        Node::InputStream(x) => (
                            false,
                            format!("enIn{}", x.clone()),
                            format!("pIn{}", x.clone()),
                            format!("nullPacingIn{}", x.clone()),
                            String::new(),
                            String::new(),
                            String::new(),
                        ),
                        Node::OutputStream(x) => (
                            false,
                            format!("enOut{}", x.clone()),
                            format!("pOut{}", x.clone()),
                            format!("nullPacingOut{}", x.clone()),
                            String::new(),
                            String::new(),
                            String::new(),
                        ),
                        Node::SlidingWindow(x) => {
                            let name1 = format!("enSw{}", x.clone());
                            let target_name = datatypes::get_target_from_sliding_window(
                                &ir.mir.sliding_windows[x.clone()],
                            );
                            let pacing1 = format!("p{}", &target_name);
                            let default1 = format!("nullPacing{}", &target_name);
                            let name2 = format!("sld{}", x.clone());
                            let pacing2 = format!("slide{}", x.clone());
                            let default2 = format!("False");
                            (true, name1, pacing1, default1, name2, pacing2, default2)
                        }
                    };
                    if has_two {
                        vec![
                            format!(
                                "{} = {}",
                                name1,
                                surround_with_delay(level + 1, default1, pacing1)
                            ),
                            format!(
                                "{} = {}",
                                name2,
                                surround_with_delay(level + 1, default2, pacing2)
                            ),
                        ]
                    } else {
                        vec![format!(
                            "{} = {}",
                            name1,
                            surround_with_delay(level + 1, default1, pacing1)
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
///     "output0Aktv = delayFor d4 False (getPacing <$> pOut0)",
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
                    format!("(getPacing <$> pOut{})", i.clone())
                )
            )
        })
        .collect()
}

fn surround_with_delay(times: usize, default_value: String, data: String) -> String {
    format!("delayFor d{} {} {}", times, default_value, data)
}

fn get_sliding_windows(ir: &HardwareIR) -> Vec<SlidingWindow> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, sw)| {
            let node = Node::SlidingWindow(i.clone());
            let level = ir.find_level(&node);
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
                    let level = ir.find_level(&node);
                    if level > 0 {
                        format!("((.slide{}) <$> curTagsLevel{})", i, level)
                    } else {
                        format!("tSw{}", i.clone())
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
            let deps = get_dependencies_of_output_stream(&node, ir);
            let level = ir.find_level(&Node::OutputStream(i));
            Output {
                idx: i,
                ty: datatypes::get_type(&out.ty),
                default_value: datatypes::get_default_for_type(&out.ty),
                memory_more_than_one: ir.required_memory.get(&node).unwrap().clone() > 1,
                deps: deps.clone(),
                level: level.clone(),
            }
        })
        .collect()
}

fn get_dependencies_of_output_stream(node: &Node, ir: &HardwareIR) -> Vec<Dependency> {
    // Same stream can be a dependency multiple times
    // Eg: output a := b.offset(by: -1).defaults(to: 0) + b.offset(by: -2).defaults(to: 0)
    // To deal with this, on the stream function (file: stream.rs) we get the parameter names from the dependency along with the index
    // Hence, the dependencies which are passed as arguments must also follow the order as in the expression
    match node {
        Node::OutputStream(x) => {
            let out = &ir.mir.outputs[x.clone()];
            get_dependencies_from_expression(
                node,
                &out.eval.clauses.first().unwrap().expression,
                ir,
            )
        }
        _ => unreachable!(),
    }
}

fn get_dependencies_from_expression(
    node: &Node,
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
            let default_expr_statements = Vec::new();
            let depending_tags_from_default_expr = Vec::new();
            let has_default_expr = false;
            let dep = match access_kind {
                StreamAccessKind::Sync => Dependency {
                    is_sync_access: true,
                    is_offset_access: false,
                    is_sliding_window_access: false,
                    is_hold_access: false,
                    offset: 0,
                    target_node: node.clone(),
                    source_name,
                    source_node,
                    source_tag,
                    memory_more_than_one,
                    default_value: format!(
                        "(pure ({}))",
                        match source {
                            StreamReference::In(x) => {
                                datatypes::get_default_for_type(&ir.mir.inputs[x.clone()].ty)
                            }
                            StreamReference::Out(x) => {
                                datatypes::get_default_for_type(&ir.mir.outputs[x.clone()].ty)
                            }
                        }
                    ),
                    has_default_expr,
                    default_expr_statements,
                    depending_tags_from_default_expr,
                },
                StreamAccessKind::Hold => Dependency {
                    is_sync_access: false,
                    is_offset_access: false,
                    is_sliding_window_access: false,
                    is_hold_access: true,
                    offset: 0,
                    target_node: node.clone(),
                    source_name,
                    source_node,
                    source_tag,
                    memory_more_than_one,
                    default_value: String::new(),
                    default_expr_statements,
                    depending_tags_from_default_expr,
                    has_default_expr,
                },
                StreamAccessKind::Offset(off) => Dependency {
                    is_offset_access: true,
                    is_sync_access: false,
                    is_sliding_window_access: false,
                    is_hold_access: false,
                    target_node: node.clone(),
                    source_name,
                    source_node,
                    source_tag,
                    memory_more_than_one,
                    default_value: String::new(),
                    offset: match off {
                        Offset::Past(x) => x.clone() as usize,
                        _ => unimplemented!(),
                    },
                    has_default_expr,
                    default_expr_statements,
                    depending_tags_from_default_expr,
                },
                StreamAccessKind::SlidingWindow(sw) => {
                    let source_node = match sw {
                        WindowReference::Sliding(x) => Node::SlidingWindow(x.clone()),
                        _ => unimplemented!(),
                    };
                    let (default_value, source_name, source_tag, memory) = match sw {
                        WindowReference::Sliding(x) => {
                            let default_value = format!(
                                "(repeat <$> (pure {}))",
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
                            let source_tag = get_source_tag(node, &source_node, ir);
                            (default_value, source_name, source_tag, memory)
                        }
                        _ => unimplemented!(),
                    };

                    Dependency {
                        is_sliding_window_access: true,
                        is_offset_access: false,
                        is_sync_access: false,
                        is_hold_access: false,
                        offset: 0,
                        default_value,
                        target_node: node.clone(),
                        source_name,
                        source_node,
                        source_tag,
                        memory_more_than_one: memory > 1,
                        has_default_expr: false,
                        default_expr_statements: Vec::new(),
                        depending_tags_from_default_expr: Vec::new(),
                    }
                }
                _ => unimplemented!(),
            };
            vec![dep]
        }
        ExpressionKind::Default { expr, default } => {
            let deps = get_dependencies_from_expression(node, &expr, ir);
            let (default_expr_statements, depending_tags_from_default_expr) =
                get_default_expr_statements_and_depending_tags(
                    &default,
                    String::from("<name>"),
                    "".to_string(),
                    ir,
                );
            deps.iter()
                .map(|dep| Dependency {
                    is_offset_access: dep.is_offset_access,
                    is_sliding_window_access: dep.is_sliding_window_access,
                    is_sync_access: dep.is_sync_access,
                    is_hold_access: dep.is_hold_access,
                    offset: dep.offset,
                    target_node: dep.target_node.clone(),
                    source_name: dep.source_name.clone(),
                    source_node: dep.source_node.clone(),
                    source_tag: dep.source_tag.clone(),
                    memory_more_than_one: dep.memory_more_than_one,
                    default_value: format!("<default_expr>"),
                    has_default_expr: true,
                    default_expr_statements: default_expr_statements.clone(),
                    depending_tags_from_default_expr: depending_tags_from_default_expr.clone(),
                })
                .collect()
        }
        ExpressionKind::ArithLog(_, exprs) => exprs
            .iter()
            .map(|expr| get_dependencies_from_expression(node, expr, ir))
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

/// Example: get_default_expr_statements_and_depending_tags(expr, String::from("<name>"), ...)
/// (
///     vec![
///          "<name> = <name>Data0 + <name>Data1".to_string(),
///          "(<name>Data0) = getOffset <$> input1Win <*> ((.input1) <$> curTagsLevel<level>) <*> (pure 1) <*> (pure 10)".to_string(),
///          "(<name>Data1) = getOffsetFromNonVec <$> out0 <*> ((.output0) <$> curTagsLevel<level>) <*> (pure 1) <*> <name>Data1Dflt".to_string(),
///          "(<name>Data1Dflt) = getOffset <$> input0Win <*> ((.input0) <$> curTagsLevel<level>) <*> (pure 1) <*> (pure 20)".to_string(),
///      ],
///     vec![ Node::OutputStream(0)]
/// )
fn get_default_expr_statements_and_depending_tags(
    expr: &Expression,
    name_prefix: String,
    default_name: String,
    ir: &HardwareIR,
) -> (Vec<String>, Vec<Node>) {
    let mut statements: Vec<String> = Vec::new();
    let mut depending_tags: Vec<Node> = Vec::new();
    match &expr.kind {
        ExpressionKind::ArithLog(op, exprs) => match op {
            ArithLogOp::Add => {
                assert_eq!(exprs.len(), 2);
                statements.push(format!(
                    "{} = {}Data0 + {}Data1",
                    name_prefix, name_prefix, name_prefix
                ));
                exprs.iter().enumerate().for_each(|(i, child_expr)| {
                    let (child_statements, child_tags) =
                        get_default_expr_statements_and_depending_tags(
                            child_expr,
                            format!("{}Data{}", name_prefix, i),
                            "".to_string(),
                            ir,
                        );
                    statements.extend(child_statements);
                    depending_tags.extend(child_tags);
                });
            }
            _ => unimplemented!(),
        },
        ExpressionKind::Default { expr, default } => {
            if let ExpressionKind::LoadConstant(constant) = &default.kind {
                let dflt_val = match constant {
                    Constant::Int(x) => format!("(pure ({}))", x),
                    _ => unimplemented!(),
                };
                let (child_statements, child_tags) = get_default_expr_statements_and_depending_tags(
                    expr,
                    name_prefix.clone(),
                    dflt_val,
                    ir,
                );
                statements.extend(child_statements);
                depending_tags.extend(child_tags);
            } else {
                let default_name = format!("{}Dflt", name_prefix.clone());
                let (expr_statements, expr_tags) = get_default_expr_statements_and_depending_tags(
                    expr,
                    name_prefix.clone(),
                    default_name.clone(),
                    ir,
                );
                let (default_statements, default_tags) =
                    get_default_expr_statements_and_depending_tags(
                        &default,
                        default_name.clone(),
                        "".to_string(),
                        ir,
                    );
                statements.extend(expr_statements);
                statements.extend(default_statements);
                depending_tags.extend(expr_tags);
                depending_tags.extend(default_tags);
            };
        }
        ExpressionKind::StreamAccess {
            target,
            parameters: _,
            access_kind,
        } => {
            let target_node = Node::from_stream(target);
            let target_keeps_multiple_values = {
                let memory = ir.required_memory.get(&target_node).unwrap().clone();
                memory > 1
            };
            let (target_name, tag, dflt_val) = match target {
                StreamReference::In(x) => (
                    format!("input{}Win", x.clone()),
                    format!("((.input{}) <$> curTagsLevel<level>)", x.clone()),
                    datatypes::get_default_for_type(&ir.mir.inputs[x.clone()].ty),
                ),
                StreamReference::Out(x) => (
                    format!("out{}", x.clone()),
                    format!("((.output{}) <$> curTagsLevel<level>)", x.clone()),
                    datatypes::get_default_for_type(&ir.mir.outputs[x.clone()].ty),
                ),
            };
            match access_kind {
                StreamAccessKind::Sync => {
                    depending_tags.push(target_node);
                    if target_keeps_multiple_values {
                        statements.push(format!(
                            "{} =  getMatchingTag <$> {} <*> {} <*> (pure ({}))",
                            name_prefix, target_name, tag, dflt_val
                        ));
                    } else {
                        statements.push(format!("(_, {}) = unbundle {}", name_prefix, target_name));
                    };
                }
                StreamAccessKind::Offset(off) => match off {
                    Offset::Past(x) => {
                        let dflt_val = if default_name.is_empty() {
                            format!("(pure ({}))", dflt_val)
                        } else {
                            default_name
                        };
                        depending_tags.push(target_node.clone());

                        if target_keeps_multiple_values {
                            let statement = format!(
                                "{} = getOffset <$> {} <*> {} <*> (pure ({})) <*> {}",
                                name_prefix, target_name, tag, x, dflt_val
                            );
                            statements.push(statement);
                        } else {
                            let statement = format!(
                                "{} = getOffsetFromNonVec <$> {} <*> {} <*> (pure ({})) <*> {}",
                                name_prefix, target_name, tag, x, dflt_val
                            );
                            statements.push(statement);
                        }
                    }
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        }
        ExpressionKind::LoadConstant(constant) => match constant {
            Constant::Int(x) => {
                statements.push(format!("{} = pure ({})", name_prefix, x));
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };
    let depending_tags: Vec<Node> = depending_tags
        .into_iter()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    (statements, depending_tags)
}

fn get_max_tag(ir: &HardwareIR) -> usize {
    let max_window_size = streams::get_sliding_windows(ir)
        .iter()
        .map(|sw| sw.window_size.clone())
        .max()
        .unwrap_or(0);
    let max_offset = get_max_offset(ir);
    let max_memory_window = ir.required_memory.values().max().unwrap_or(&0).clone();
    max(max(max_window_size, max_offset), max_memory_window) + 1
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

fn get_all_tags_names(ir: &HardwareIR) -> Vec<String> {
    let inputs: Vec<String> = ir
        .mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("tIn{}", i.clone()))
        .collect();
    let outputs: Vec<String> = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("tOut{}", i.clone()))
        .collect();
    let slidings: Vec<String> = ir
        .mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| format!("tSw{}", i.clone()))
        .collect();
    vec![&inputs[..], &outputs[..], &slidings[..]].concat()
}

fn get_source_tag(node: &Node, source_node: &Node, ir: &HardwareIR) -> String {
    let node_level = ir.find_level(node);
    if node_level > 0 {
        match &source_node {
            Node::InputStream(x) => format!("((.input{}) <$> curTagsLevel{})", x, node_level),
            Node::OutputStream(x) => format!("((.output{}) <$> curTagsLevel{})", x, node_level),
            Node::SlidingWindow(x) => format!("((.slide{}) <$> curTagsLevel{})", x, node_level),
        }
    } else {
        match &source_node {
            Node::InputStream(x) => format!("tIn{}", x.clone()),
            Node::OutputStream(x) => format!("tOut{}", x.clone()),
            _ => unreachable!(),
        }
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
