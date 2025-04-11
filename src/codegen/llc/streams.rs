use handlebars::Handlebars;
use rtlola_frontend::mir::{
    self as MIR, ArithLogOp, Constant, Expression, ExpressionKind, MemorizationBound,
    StreamAccessKind, StreamReference, WindowOperation,
};
use serde::Serialize;

use crate::{
    codegen::{datatypes, register_template},
    hardware_ir::HardwareIR,
    hardware_ir::Node,
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
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "streams".to_string(),
        "src/codegen/llc/streams.hbs".to_string(),
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
            OutputStream {
                is_sliding_window_based,
                inputs: get_inputs(out),
                input_types: get_input_types(out, ir),
                output_type: datatypes::get_type(&out.ty),
                default_value: datatypes::get_default_for_type(&out.ty),
                expression: get_expression(&out.eval.clauses.first().unwrap().expression, ir),
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
                window_size: get_sliding_window_size(i, ir),
                memory: ir.required_memory.get(&Node::SlidingWindow(i)).unwrap().clone()
            }
        })
        .collect()
}

fn get_expression(expr: &Expression, ir: &HardwareIR) -> String {
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
                StreamReference::In(x) => format!("in{}", x),
                StreamReference::Out(x) => format!("out{}", x),
            },
        },
        ExpressionKind::Default {
            expr,
            default: _,
        } => {
            get_expression(&expr, ir)
        }
        ExpressionKind::ArithLog(operator, expressions) => {
            let expressions: Vec<String> = expressions
                .into_iter()
                .map(|expr| get_expression(expr, ir))
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

fn get_inputs(out: &MIR::OutputStream) -> Vec<String> {
    out.accesses
        .iter()
        .map(|access| {
            let stream_ref = match access.0 {
                StreamReference::In(x) => format!("in{}", x),
                StreamReference::Out(x) => format!("out{}", x),
            };
            match access.1.first().unwrap().1 {
                StreamAccessKind::Sync => stream_ref,
                StreamAccessKind::Hold => stream_ref,
                StreamAccessKind::Offset(_) => stream_ref,
                StreamAccessKind::SlidingWindow(sw) => format!("sw{}", sw.idx()),
                _ => unimplemented!(),
            }
        })
        .collect::<Vec<_>>()
}

fn get_input_types(out: &MIR::OutputStream, ir: &HardwareIR) -> Vec<String> {
    out.accesses
        .iter()
        .map(|access| {
            let stream_data_type = match access.0 {
                StreamReference::In(x) => datatypes::get_type(&ir.mir.inputs[x].ty),
                StreamReference::Out(x) => datatypes::get_type(&ir.mir.outputs[x].ty),
            };
            match access.1.first().unwrap().1 {
                StreamAccessKind::Sync => stream_data_type,
                StreamAccessKind::Hold => stream_data_type,
                StreamAccessKind::Offset(_) => stream_data_type,
                StreamAccessKind::SlidingWindow(sw) => {
                    let window_size = get_sliding_window_size(sw.idx(), ir);
                    format!("(Vec {} {})", window_size, stream_data_type)
                }
                _ => unimplemented!(),
            }
        })
        .collect()
}

fn get_sliding_window_inputs(out: &MIR::OutputStream, ir: &HardwareIR) -> Vec<SlidingWindow> {
    out.accesses
        .iter()
        .filter(|access| match access.1.first().unwrap().1 {
            StreamAccessKind::SlidingWindow(_) => true,
            _ => false,
        })
        .map(|access| match access.1.first().unwrap().1 {
            StreamAccessKind::SlidingWindow(sw) => SlidingWindow {
                window_idx: sw.idx(),
                window_size: get_sliding_window_size(sw.idx(), ir),
                data_type: datatypes::get_type(&ir.mir.sliding_windows[sw.idx()].ty),
                default_value: datatypes::get_default_for_type(
                    &ir.mir.sliding_windows[sw.idx()].ty,
                ),
                memory: ir.required_memory.get(&Node::SlidingWindow(sw.idx())).unwrap().clone()
            },
            _ => unreachable!(),
        })
        .collect()
}

fn get_sliding_window_size(window_idx: usize, ir: &HardwareIR) -> usize {
    // In RTLola sliding-window aggregation, the span of window is inlclusive in the right but exclusive to the left
    // For example:
    //      output b @1kHz := x.aggregate(over: 0.003s, using: sum)
    // Here, in the output b, data at the exact time 0.001s won't be included in the aggregation at time 0.003s
    // To deal with this we need one more bucket such that this extreme data can be put into it
    // Also we must not mix this extreme data into the bucket aggregation
    // Also note, when we have both new data and time to slide the window, first we must update the window and then slide it
    match ir.mir.sliding_windows[window_idx].num_buckets {
        MemorizationBound::Bounded(x) => (x + 1) as usize,
        _ => unimplemented!(),
    }
}
