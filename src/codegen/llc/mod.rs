use handlebars::Handlebars;
use rtlola_frontend::mir::{
    Constant, Expression, ExpressionKind, Offset, OutputStream, StreamAccessKind, StreamReference,
    WindowReference,
};
use serde::Serialize;

use crate::hardware_ir::Node;
use crate::{codegen::register_template, hardware_ir::HardwareIR};

use super::datatypes;

mod streams;

#[derive(Serialize)]
struct Data {
    streams: String,
    max_tag: usize,
    has_pipeline_wait: bool,
    has_sliding_window: bool,
    outputs: Vec<Output>,
}

#[derive(Serialize)]
struct Output {
    idx: usize,
    deps: Vec<Dependency>,
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
        "src/codegen/llc/llc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        streams: streams::render(ir, handlebars).unwrap(),
        max_tag: streams::get_sliding_windows(ir)
            .iter()
            .map(|sw| sw.window_size.clone())
            .max()
            .unwrap_or(5)
            * 2,
        has_pipeline_wait: ir.pipeline_wait > 0,
        has_sliding_window: ir.mir.sliding_windows.len() > 0,
        outputs: get_outputs(ir),
    };
    match handlebars.render("llc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

fn get_outputs(ir: &HardwareIR) -> Vec<Output> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, out)| Output {
            idx: i,
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
                            let default_value = datatypes::get_default_for_type(
                                &ir.mir.sliding_windows[x.clone()].ty,
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
