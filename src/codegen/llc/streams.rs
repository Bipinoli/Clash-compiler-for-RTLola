use handlebars::Handlebars;
use rtlola_frontend::mir::{
    ArithLogOp, Constant, Expression, ExpressionKind, MemorizationBound, StreamAccessKind,
    StreamReference, WindowOperation,
};
use serde::Serialize;

use crate::{
    codegen::{datatypes, register_template},
    hardware_ir::HardwareIR,
};

#[derive(Serialize)]
struct Data {
    has_pipeline_wait: bool,
    pipeline_wait: usize,
    has_sliding_window: bool,
    streams: Vec<Stream>,
    bucket_functions: Vec<BucketFunction>,
    sliding_windows: Vec<SlidingWindow>,
}

#[derive(Serialize)]
struct Stream {
    is_sliding_window_based: bool,
    input_types: String,
    output_type: String,
    inputs: String,
    default_value: String,
    expression: String,
    sliding_window: String,
    window_size: String,
}

#[derive(Serialize)]
struct BucketFunction {
    data_type: String,
    expression: String,
}

#[derive(Serialize)]
struct SlidingWindow {
    window_size: String,
    data_type: String,
    default_value: String,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "streams".to_string(),
        "src/codegen/llc/streams.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        has_pipeline_wait: ir.pipeline_wait > 0,
        pipeline_wait: ir.pipeline_wait,
        has_sliding_window: ir.mir.sliding_windows.len() > 0,
        streams: get_streams(ir),
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

fn get_streams(ir: &HardwareIR) -> Vec<Stream> {
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
            let output_type = datatypes::get_type(&out.ty);
            let input_types = out
                .accesses
                .iter()
                .map(|access| match access.0 {
                    StreamReference::In(x) => {
                        format!("Signal dom {}", datatypes::get_type(&ir.mir.inputs[x].ty))
                    }
                    StreamReference::Out(x) => {
                        format!("Signal dom {}", datatypes::get_type(&ir.mir.outputs[x].ty))
                    }
                })
                .collect::<Vec<_>>()
                .join(" -> ");
            let inputs = out
                .accesses
                .iter()
                .enumerate()
                .map(|(i, _)| format!("d{}", i))
                .collect::<Vec<_>>()
                .join(" ");
            let default_value = datatypes::get_default_for_type(&out.ty);
            let expression = if !is_sliding_window_based {
                get_expression(0, &out.eval.clauses.first().unwrap().expression, ir).0
            } else {
                String::new()
            };
            let (sliding_window, window_size) = if is_sliding_window_based {
                let window_idx = match out.accesses.first().unwrap().1.first().unwrap().1 {
                    StreamAccessKind::SlidingWindow(win) => win.idx(),
                    _ => unreachable!(),
                };
                (
                    format!("{}", window_idx),
                    get_sliding_window_size(window_idx, ir),
                )
            } else {
                (String::new(), String::new())
            };
            Stream {
                is_sliding_window_based,
                input_types,
                output_type,
                inputs,
                default_value,
                expression,
                window_size,
                sliding_window,
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

fn get_sliding_windows(ir: &HardwareIR) -> Vec<SlidingWindow> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, sw)| {
            let data_type = datatypes::get_type(&sw.ty);
            let window_size = get_sliding_window_size(i, ir);
            let default_value = match sw.op {
                WindowOperation::Sum => "0".to_string(),
                _ => unimplemented!(),
            };
            SlidingWindow {
                data_type,
                window_size,
                default_value,
            }
        })
        .collect()
}

fn get_expression(tag: usize, expr: &Expression, ir: &HardwareIR) -> (String, usize) {
    match &expr.kind {
        ExpressionKind::LoadConstant(con) => match con {
            Constant::Int(x) => (format!("{}", x), tag),
            _ => unimplemented!(),
        },
        ExpressionKind::StreamAccess {
            target: _,
            parameters: _,
            access_kind: _,
        } => (format!("d{}", tag), tag + 1),
        ExpressionKind::Default { expr, default: _ } => get_expression(tag, &expr, ir),
        ExpressionKind::ArithLog(operator, expressions) => {
            let mut tag = tag;
            let expressions: Vec<String> = expressions
                .into_iter()
                .enumerate()
                .map(|(i, expr)| {
                    let (expr, next_tag) = get_expression(tag, expr, ir);
                    tag = next_tag;
                    expr
                })
                .collect();
            let final_expression = match operator {
                ArithLogOp::Add => expressions.join(" + "),
                ArithLogOp::Sub => expressions.join(" - "),
                ArithLogOp::Mul => expressions.join(" * "),
                ArithLogOp::Div => expressions.join(" / "),
                _ => unimplemented!(),
            };
            (final_expression, tag)
        }
        _ => unimplemented!(),
    }
}

fn get_sliding_window_size(window_idx: usize, ir: &HardwareIR) -> String {
    // In RTLola sliding-window aggregation, the span of window is inlclusive in the right but exclusive to the left
    // For example:
    //      output b @1kHz := x.aggregate(over: 0.003s, using: sum)
    // Here, in the output b, data at the exact time 0.001s won't be included in the aggregation at time 0.003s
    // To deal with this we need one more bucket such that this extreme data can be put into it
    // Also we must not mix this extreme data into the bucket aggregation
    // Also note, when we have both new data and time to slide the window, first we must update the window and then slide it
    match ir.mir.sliding_windows[window_idx].num_buckets {
        MemorizationBound::Bounded(x) => {
            format!("{}", x + 1)
        }
        _ => unimplemented!(),
    }
}
