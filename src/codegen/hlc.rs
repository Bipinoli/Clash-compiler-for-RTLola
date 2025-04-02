use handlebars::Handlebars;
use rtlola_frontend as RF;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

#[derive(Serialize)]
struct Data {}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "hlc".to_string(),
        "src/codegen/hlc.hbs".to_string(),
        handlebars,
    );
    let data = Data {};
    match handlebars.render("hlc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}
