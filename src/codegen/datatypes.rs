use handlebars::Handlebars;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

#[derive(Serialize)]
struct Data {
    input_types: Vec<String>,
    output_types: Vec<String>,
    input_defaults: Vec<String>,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "datatypes".to_string(),
        "src/codegen/datatypes.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        input_types: vec!["Int".to_string(), "Int".to_string()],
        output_types: vec!["Int".to_string(), "Int".to_string()],
        input_defaults: vec!["0".to_string(), "0".to_string()],
    };
    match handlebars.render("datatypes", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}
