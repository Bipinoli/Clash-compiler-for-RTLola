use handlebars::Handlebars;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

mod streams;

#[derive(Serialize)]
struct Data {
    streams: String,
    max_tag: usize,
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
    };
    match handlebars.render("llc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}
