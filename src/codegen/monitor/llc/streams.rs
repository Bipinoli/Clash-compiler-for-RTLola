use handlebars::Handlebars;
use rtlola_frontend::mir::{
    self as MIR, ArithLogOp, Constant, Expression, ExpressionKind, StreamAccessKind,
    StreamReference, WindowOperation,
};
use serde::Serialize;

use crate::{
    analysis::eval_order::memory,
    analysis::node::Node,
    analysis::HardwareIR,
    codegen::monitor::{datatypes, register_template},
};

#[derive(Serialize)]
struct Data {
    pipeline_wait: usize,
    has_sliding_window: bool,
    input_streams: Vec<InputStream>,
    output_streams: Vec<OutputStream>,
    bucket_functions: Vec<BucketFunction>,
    sliding_windows: Vec<SlidingWindow>,
}

#[derive(Serialize)]
struct InputStream {
    memory: usize,
    data_type: String,
    default_value: String,
}

#[derive(Serialize)]
struct OutputStream {
    is_sliding_window_based: bool,
    output_type: String,
    default_value: String,
    inputs: Vec<String>,
    input_types: Vec<String>,
    expression: String,
    memory: usize,
    sliding_window_inputs: Vec<SlidingWindow>,
    is_accessed_by_offset: bool,
}

#[derive(Serialize)]
struct BucketFunction {
    data_type: String,
    expression: String,
}

#[derive(Serialize)]
pub struct SlidingWindow {
    pub window_idx: usize,
    pub window_size: usize,
    pub data_type: String,
    pub default_value: String,
    pub memory: usize,
    pub pacing: String,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "streams".to_string(),
        "src/codegen/monitor/llc/streams.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        pipeline_wait: ir.pipeline_wait,
        has_sliding_window: ir.mir.sliding_windows.len() > 0,
        input_streams: get_input_streams(ir),
        output_streams: get_output_streams(ir),
        bucket_functions: get_bucket_functions(ir),
        sliding_windows: get_sliding_windows(ir),
    };
    match handlebars.render("streams", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

fn get_input_streams(ir: &HardwareIR) -> Vec<InputStream> {
    ir.mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, inpt)| {
            let data_type = datatypes::get_type(&inpt.ty);
            let default_value = datatypes::get_default_for_type(&inpt.ty);
            let memory = ir
                .required_memory
                .get(&Node::InputStream(i))
                .unwrap()
                .clone();
            InputStream {
                memory,
                data_type,
                default_value,
            }
        })
        .collect()
}

fn get_output_streams(ir: &HardwareIR) -> Vec<OutputStream> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, out)| {
            let is_sliding_window_based = out
                .accesses
                .iter()
                .filter(|access| match access.1.first().unwrap().1 {
                    StreamAccessKind::SlidingWindow(_) => true,
                    _ => false,
                })
                .count()
                > 0;
            let node = Node::OutputStream(i.clone());
            OutputStream {
                is_sliding_window_based,
                inputs: get_inputs(&node, ir),
                input_types: get_input_types(&node, ir),
                output_type: datatypes::get_type(&out.ty),
                default_value: datatypes::get_default_for_type(&out.ty),
                expression: get_expression(
                    "".to_string(),
                    &out.eval.clauses.first().unwrap().expression,
                    ir,
                ),
                memory: ir
                    .required_memory
                    .get(&Node::OutputStream(i))
                    .unwrap()
                    .clone(),
                is_accessed_by_offset: true,
                sliding_window_inputs: get_sliding_window_inputs(out, ir),
            }
        })
        .collect()
}

fn get_bucket_functions(ir: &HardwareIR) -> Vec<BucketFunction> {
    ir.mir
        .sliding_windows
        .iter()
        .map(|sw| {
            let data_type = datatypes::get_type(&sw.ty);
            let expression = match sw.op {
                WindowOperation::Sum => "acc + item".to_string(),
                _ => unimplemented!(),
            };
            BucketFunction {
                data_type,
                expression,
            }
        })
        .collect()
}

pub fn get_sliding_windows(ir: &HardwareIR) -> Vec<SlidingWindow> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, sw)| {
            let default_value = match sw.op {
                WindowOperation::Sum => "0".to_string(),
                _ => unimplemented!(),
            };
            SlidingWindow {
                window_idx: i,
                default_value,
                data_type: datatypes::get_type(&sw.ty),
                window_size: memory::get_sliding_window_size(i, &ir.mir),
                memory: ir
                    .required_memory
                    .get(&Node::SlidingWindow(i))
                    .unwrap()
                    .clone(),
                pacing: datatypes::get_target_from_sliding_window(sw),
            }
        })
        .collect()
}

// Same stream can be a dependency multiple times
// Eg: output a := b.offset(by: -1).defaults(to: 0) + b.offset(by: -2).defaults(to: 0)
// To deal with this, we follow the order of dependencies as in expression appending by a suffix to make them unique
// When calling the function, the arguments are likewise provided in the order as in expression as well (get_dependencies_of_output_stream() in llc/mod.rs)
fn get_expression(suffix: String, expr: &Expression, ir: &HardwareIR) -> String {
    match &expr.kind {
        ExpressionKind::LoadConstant(con) => match con {
            Constant::Int(x) => format!("{}", x),
            _ => unimplemented!(),
        },
        ExpressionKind::StreamAccess {
            target,
            parameters: _,
            access_kind,
        } => match access_kind {
            StreamAccessKind::SlidingWindow(x) => {
                format!("(merge{} <$> sw{})", x.idx(), x.idx())
            }
            _ => match target {
                StreamReference::In(x) => format!("in{}_{}", x, suffix),
                StreamReference::Out(x) => format!("out{}_{}", x, suffix),
            },
        },
        ExpressionKind::Default { expr, default: _ } => get_expression(suffix, &expr, ir),
        ExpressionKind::ArithLog(operator, expressions) => {
            let expressions: Vec<String> = expressions
                .into_iter()
                .enumerate()
                .map(|(i, expr)| get_expression(format!("{}{}", suffix, i), expr, ir))
                .collect();
            let final_expression = match operator {
                ArithLogOp::Add => expressions.join(" + "),
                ArithLogOp::Sub => expressions.join(" - "),
                ArithLogOp::Mul => expressions.join(" * "),
                ArithLogOp::Div => expressions.join(" / "),
                _ => unimplemented!(),
            };
            final_expression
        }
        _ => unimplemented!(),
    }
}

// Same stream can be a dependency multiple times
// Eg: output a := b.offset(by: -1).defaults(to: 0) + b.offset(by: -2).defaults(to: 0)
// To deal with this, we follow the order of dependencies as in expression appending by a suffix to make them unique
// When calling the function, the arguments are likewise provided in the order as in expression as well (get_dependencies_of_output_stream() in llc/mod.rs)
fn get_inputs_from_expression(suffix: String, expr: &Expression, ir: &HardwareIR) -> Vec<String> {
    let mut inputs: Vec<String> = Vec::new();
    match &expr.kind {
        ExpressionKind::LoadConstant(_) => (),
        ExpressionKind::StreamAccess {
            target,
            parameters: _,
            access_kind,
        } => match access_kind {
            StreamAccessKind::SlidingWindow(x) => inputs.push(format!("sw{}", x.idx())),
            _ => match target {
                StreamReference::In(x) => inputs.push(format!("in{}_{}", x, suffix)),
                StreamReference::Out(x) => inputs.push(format!("out{}_{}", x, suffix)),
            },
        },
        ExpressionKind::Default { expr, default: _ } => {
            inputs.extend(get_inputs_from_expression(suffix, &expr, ir))
        }
        ExpressionKind::ArithLog(_, expressions) => {
            expressions.into_iter().enumerate().for_each(|(i, expr)| {
                inputs.extend(get_inputs_from_expression(
                    format!("{}{}", suffix, i),
                    expr,
                    ir,
                ));
            });
        }
        _ => unimplemented!(),
    };
    inputs
}

fn get_inputs(output_node: &Node, ir: &HardwareIR) -> Vec<String> {
    let expr = match output_node {
        Node::OutputStream(x) => {
            &ir.mir.outputs[x.clone()]
                .eval
                .clauses
                .first()
                .unwrap()
                .expression
        }
        _ => unreachable!(),
    };
    get_inputs_from_expression("".to_string(), expr, ir)
}

fn get_input_types(output_node: &Node, ir: &HardwareIR) -> Vec<String> {
    super::get_dependencies_of_output_stream(output_node, ir)
        .iter()
        .map(|dep| match dep.source_node {
            Node::InputStream(x) => datatypes::get_type(&ir.mir.inputs[x].ty),
            Node::OutputStream(x) => datatypes::get_type(&ir.mir.outputs[x].ty),
            Node::SlidingWindow(x) => {
                let stream_data_type = datatypes::get_type(&ir.mir.outputs[x].ty);
                let window_size = memory::get_sliding_window_size(x.clone(), &ir.mir);
                format!("(Vec {} {})", window_size, stream_data_type)
            }
        })
        .collect()
}

fn get_sliding_window_inputs(out: &MIR::OutputStream, ir: &HardwareIR) -> Vec<SlidingWindow> {
    let mut windows: Vec<SlidingWindow> = Vec::new();
    out.accesses.iter().for_each(|access| {
        for (_, access_kind) in &access.1 {
            match access_kind {
                StreamAccessKind::SlidingWindow(sw) => {
                    windows.push(SlidingWindow {
                        window_idx: sw.idx(),
                        window_size: memory::get_sliding_window_size(sw.idx(), &ir.mir),
                        data_type: datatypes::get_type(&ir.mir.sliding_windows[sw.idx()].ty),
                        default_value: datatypes::get_default_for_type(
                            &ir.mir.sliding_windows[sw.idx()].ty,
                        ),
                        memory: ir
                            .required_memory
                            .get(&Node::SlidingWindow(sw.idx()))
                            .unwrap()
                            .clone(),
                        pacing: datatypes::get_target_from_sliding_window(
                            &ir.mir.sliding_windows[sw.idx()],
                        ),
                    });
                }
                _ => (),
            }
        }
    });
    windows
}
