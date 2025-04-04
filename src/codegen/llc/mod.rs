use handlebars::Handlebars;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

mod streams;

#[derive(Serialize)]
struct Data {
    streams: String,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "llc".to_string(),
        "src/codegen/llc/llc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        streams: streams::render(ir, handlebars).unwrap(),
    };
    match handlebars.render("llc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}
