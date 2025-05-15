use std::collections::HashSet;

use handlebars::Handlebars;
use rtlola_frontend as RF;
use serde::Serialize;

use crate::{codegen::monitor::register_template, hardware_ir::HardwareIR};

#[derive(Serialize)]
struct Data {
    inputs: Vec<Stream>,
    outputs: Vec<Stream>,
    sliding_windows: Vec<Stream>,
    all_streams: Vec<Stream>,
    all_unique_types: Vec<String>,
    has_sliding_window: bool,
}

#[derive(Serialize, Clone)]
pub struct Stream {
    stream_type: String,
    index: usize,
    ty: String,
    default: String,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "datatypes".to_string(),
        "src/codegen/monitor/datatypes.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        inputs: get_inputs(ir),
        outputs: get_outputs(ir),
        sliding_windows: get_sliding_windows(ir),
        all_streams: get_all_streams(ir),
        all_unique_types: get_all_unique_types(ir),
        has_sliding_window: get_sliding_windows(ir).len() > 0,
    };
    match handlebars.render("datatypes", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

pub fn get_inputs(ir: &HardwareIR) -> Vec<Stream> {
    ir.mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, inpt)| Stream {
            stream_type: "input".to_string(),
            index: i,
            ty: get_type(&inpt.ty),
            default: get_default_for_type(&inpt.ty),
        })
        .collect()
}

pub fn get_outputs(ir: &HardwareIR) -> Vec<Stream> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, outpt)| Stream {
            stream_type: "output".to_string(),
            index: i,
            ty: get_type(&outpt.ty),
            default: get_default_for_type(&outpt.ty),
        })
        .collect()
}

fn get_sliding_windows(ir: &HardwareIR) -> Vec<Stream> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| Stream {
            stream_type: "slide".to_string(),
            index: i,
            ty: "Bool".to_string(),
            default: "False".to_string(),
        })
        .collect()
}

fn get_all_streams(ir: &HardwareIR) -> Vec<Stream> {
    vec![
        &get_inputs(ir)[..],
        &get_outputs(ir)[..],
        &get_sliding_windows(ir)[..],
    ]
    .concat()
}

fn get_all_unique_types(ir: &HardwareIR) -> Vec<String> {
    vec![&get_inputs(ir)[..], &get_outputs(ir)[..]]
        .concat()
        .into_iter()
        .map(|s| s.ty)
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}

pub fn get_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Int(_) => "Int".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}

pub fn get_default_for_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Int(_) => "0".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}
