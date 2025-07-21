use csv::StringRecord;
use handlebars::Handlebars;
use rtlola_frontend::mir::Type;
use serde::Serialize;

use crate::analysis::HardwareIR;

#[derive(Serialize)]
struct Data {
    debug: bool,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    trace_data: Vec<TraceData>,
    sliding_windows: Vec<()>,
}

#[derive(Serialize)]
struct Input {
    datatype: String,
}

#[derive(Serialize)]
struct Output {
    datatype: String,
}

#[derive(Serialize)]
struct TraceData {
    time: String,
    inputs: Vec<TraceDataInput>,
}

#[derive(Serialize)]
struct TraceDataInput {
    new_input: usize,
    input_value: String,
}

pub fn generate_verilog_testbench(
    trace_data: Vec<StringRecord>,
    hard_ir: HardwareIR,
) -> Option<String> {
    let mut reg = Handlebars::new();

    register_template(
        "testbench".to_string(),
        "src/codegen/testbench/verilog_testbench.hbs".to_string(),
        &mut reg,
    );

    let data = Data {
        debug: hard_ir.debug,
        inputs: get_inputs(&hard_ir),
        outputs: get_outputs(&hard_ir),
        trace_data: get_trace_data(trace_data),
        sliding_windows: get_sliding_windows(&hard_ir),
    };
    match reg.render("testbench", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

fn register_template(name: String, path: String, handlebars: &mut Handlebars) {
    match handlebars.register_template_file(name.as_str(), path.as_str()) {
        Ok(_) => {}
        Err(e) => {
            println!("Template registration error: {}", e);
            return;
        }
    }
}

fn get_inputs(ir: &HardwareIR) -> Vec<Input> {
    ir.mir
        .inputs
        .iter()
        .map(|inpt| Input {
            datatype: {
                match inpt.ty {
                    Type::Int(_) => "signed [63:0]".to_string(),
                    Type::UInt(_) => "unsigned [63:0]".to_string(),
                    Type::Bool => "".to_string(),
                    _ => unimplemented!(),
                }
            },
        })
        .collect()
}

fn get_outputs(ir: &HardwareIR) -> Vec<Output> {
    ir.mir
        .outputs
        .iter()
        .map(|outpt| Output {
            datatype: {
                match outpt.ty {
                    Type::Int(_) => "signed [63:0]".to_string(),
                    Type::UInt(_) => "unsigned [63:0]".to_string(),
                    Type::Bool => "".to_string(),
                    _ => unimplemented!(),
                }
            },
        })
        .collect()
}

fn get_trace_data(trace_data: Vec<StringRecord>) -> Vec<TraceData> {
    // Note:
    // Input is synchronised with the rising edge of the clock
    // Clock period is 1us. The clock rises in every even microsecond
    let mut last_input_time: f64 = 0.0;
    trace_data
        .iter()
        .enumerate()
        .map(|(i, d)| TraceData {
            time: {
                if i == 0 {
                    let seconds: f64 = d
                        .get(0)
                        .unwrap()
                        .to_string()
                        .parse::<f64>()
                        .expect("Failed to parse time from trace data");
                    // Initial reset signal for #2
                    let microseconds = seconds * 1e6 - 2 as f64;
                    last_input_time = next_immediate_clock_rise_time(seconds * 1e6);
                    format!("{}", microseconds)
                } else {
                    let cur_row_time = d
                        .get(0)
                        .unwrap()
                        .to_string()
                        .parse::<f64>()
                        .expect("Failed to parse time from trace data");
                    // #2 from one cycle delay caused by putting input to zero in the impulse input
                    let microseconds = cur_row_time * 1e6 - last_input_time - 2 as f64;
                    let microseconds = microseconds.max(0.0);
                    last_input_time =
                        next_immediate_clock_rise_time(last_input_time + microseconds + 2.0);
                    format!("{}", microseconds)
                }
            },
            inputs: d
                .iter()
                .enumerate()
                .filter(|(i, _)| *i > 0)
                .map(|(_, item)| TraceDataInput {
                    new_input: if item != "#" { 1 } else { 0 },
                    input_value: if item != "#" {
                        item.to_string()
                    } else {
                        "0".to_string()
                    },
                })
                .collect(),
        })
        .collect()
}

fn next_immediate_clock_rise_time(microseconds: f64) -> f64 {
    // Note:
    // Input is synchronised with the rising edge of the clock
    // Clock period is 1us. The clock rises in every even microsecond
    let mut micros = microseconds.ceil();
    if micros % 2.0 == 1.0 {
        micros += 1.0;
    }
    micros
}

fn get_sliding_windows(ir: &HardwareIR) -> Vec<()> {
    ir.mir.sliding_windows.iter().map(|_| ()).collect()
}
