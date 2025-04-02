use handlebars::Handlebars;
use rtlola_frontend as RF;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

#[derive(Serialize)]
struct Data {
    has_periodic_pacing: bool,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "hlc".to_string(),
        "src/codegen/hlc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        has_periodic_pacing: true,
    };
    match handlebars.render("hlc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

fn get_pacing(ir: &HardwareIR) -> String {
    unimplemented!()
}
