use handlebars::Handlebars;
use rtlola_frontend::mir::{Constant, Expression, ExpressionKind, Offset, StreamAccessKind, StreamReference, WindowReference};
use serde::Serialize;

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
        .map(|(i, out)| {
            let deps = out.eval.



            let deps1: Vec<Dependency> = out
                .accesses
                .iter()
                .map(|access| {
                    let source = match access.0 {
                        StreamReference::In(x) => format!("input{}Win", x),
                        StreamReference::Out(x) => format!("out{}", x),
                    };
                    match access.1.first().unwrap().1 {
                        StreamAccessKind::Sync => {
                            let default_value = match access.0 {
                                StreamReference::In(x) => {
                                    datatypes::get_default_for_type(&ir.mir.inputs[x].ty)
                                }
                                StreamReference::Out(x) => {
                                    datatypes::get_default_for_type(&ir.mir.outputs[x].ty)
                                }
                            };
                            Dependency {
                                is_sync_access: true,
                                is_offset_access: false,
                                is_sliding_window_access: false,
                                offset: 0,
                                default_value,
                                source,
                            }
                        }
                        StreamAccessKind::Hold => {

                            Dependency {
                                is_sync_access: true,
                                is_offset_access: false,
                                is_sliding_window_access: false,
                                offset: 0,
                                default_value,
                                source,
                            }
                        }
                        StreamAccessKind::Offset(_) => stream_ref,
                        StreamAccessKind::SlidingWindow(sw) => format!("sw{}", sw.idx()),
                        _ => unimplemented!(),
                    }
                })
                .collect();
            Output { idx: i, deps }
        })
        .collect()
}


fn get_dependencies_from_expression(expr: &Expression, ir: &HardwareIR) -> Vec<Dependency> {
    // default values of offsets, hold etc. can only be accessed from the expression
    match &expr.kind {
        ExpressionKind::StreamAccess { target, parameters: _, access_kind } => {
            let source = match target {
                StreamReference::In(x) => format!("input{}Win", x),
                StreamReference::Out(x) => format!("out{}", x),
            };
            let dep = match access_kind {
                StreamAccessKind::Sync => {
                    Dependency {
                        is_sync_access: true,
                        is_offset_access: false,
                        is_sliding_window_access: false,
                        offset: 0,
                        source,
                        default_value: match target {
                            StreamReference::In(x) => {
                                datatypes::get_default_for_type(&ir.mir.inputs[x.clone()].ty)
                            }
                            StreamReference::Out(x) => {
                                datatypes::get_default_for_type(&ir.mir.outputs[x.clone()].ty)
                            } 
                        },
                     }
                },
                StreamAccessKind::Hold => {
                    Dependency {
                        is_sync_access: true,
                        is_offset_access: false,
                        is_sliding_window_access: false,
                        offset: 0,
                        source,
                        default_value: String::new(), 
                     }
                },
                StreamAccessKind::Offset(off) => {
                   Dependency {
                    is_offset_access: true,
                    is_sync_access: false,
                    is_sliding_window_access: false,
                    source,
                    default_value: String::new(),
                    offset: match off {
                        Offset::Past(x) => x.clone() as usize,
                        _ => unimplemented!(),
                    },
                   } 
                },
                StreamAccessKind::SlidingWindow(sw) => {
                    let (default_value, source) = match sw {
                        WindowReference::Sliding(x) => {
                            let default_value = datatypes::get_default_for_type(&ir.mir.sliding_windows[x.clone()].ty);
                            let source = format!("sw{}", x);
                            (default_value, source)
                        },
                        _ => unimplemented!(),
                    };
                    Dependency {
                        is_sliding_window_access: true,
                        is_offset_access: false,
                        is_sync_access: false,
                        offset: 0,
                        default_value,
                        source,
                    }
                },
                _ => unimplemented!()
            };
            vec![dep]
        },
        ExpressionKind::Default { expr, default } => {
            let deps = get_dependencies_from_expression(&expr, ir);
            let default_value = get_default_value(&default);
            deps.iter().map(|dep| {
                Dependency {
                    is_offset_access: dep.is_offset_access,
                    is_sliding_window_access: dep.is_sliding_window_access,
                    is_sync_access: dep.is_sync_access,
                    offset: dep.offset,
                    source: dep.source.clone(),
                    default_value: default_value.clone()
                }
            }).collect()
        },
        _ => unimplemented!()
    };
    unimplemented!()
}

fn get_default_value(expr: &Expression) -> String {
    match &expr.kind {
       ExpressionKind::LoadConstant(x) => {
        match x {
            Constant::Int(x) => format!("{}", x),
            _ => unimplemented!(),
        }
       },
       _ => unimplemented!(),
    }
}