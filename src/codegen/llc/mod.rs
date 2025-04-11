use handlebars::Handlebars;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

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
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "llc".to_string(),
        "src/codegen/llc/llc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        streams: streams::render(ir, handlebars).unwrap(),
        max_tag: streams::get_sliding_windows(ir).iter().map(|sw| sw.window_size.clone()).max().unwrap_or(5) * 2,
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
    ir.mir.outputs.iter().enumerate().map(|(i, out)| {
        Output {
            idx: i,
        }
    }).collect()
}
