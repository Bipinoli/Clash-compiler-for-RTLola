use handlebars::Handlebars;
use rtlola_frontend as RF;
use serde::Serialize;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

#[derive(Serialize)]
struct Data {
    input_types: Vec<String>,
    output_types: Vec<String>,
    input_defaults: Vec<String>,
    has_sliding_window: bool,
    slides: String,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "datatypes".to_string(),
        "src/codegen/datatypes.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        input_types: get_input_types(ir),
        output_types: get_output_types(ir),
        input_defaults: get_input_defaults(ir),
        has_sliding_window: get_slides(ir).len() > 0,
        slides: get_slides(ir),
    };
    match handlebars.render("datatypes", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

fn get_input_types(ir: &HardwareIR) -> Vec<String> {
    ir.mir
        .inputs
        .iter()
        .map(|inpt| get_type(&inpt.ty))
        .collect()
}

fn get_input_defaults(ir: &HardwareIR) -> Vec<String> {
    ir.mir
        .inputs
        .iter()
        .map(|inpt| get_default_for_type(&inpt.ty))
        .collect()
}

fn get_output_types(ir: &HardwareIR) -> Vec<String> {
    ir.mir
        .outputs
        .iter()
        .map(|outpt| get_type(&outpt.ty))
        .collect()
}

fn get_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Int(_) => "Int".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}

fn get_default_for_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Int(_) => "0".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}

fn get_slides(ir: &HardwareIR) -> String {
    let slides = ir.mir
        .sliding_windows
        .iter()
        .map(|_| "Bool".to_string())
        .collect::<Vec<String>>()
        .join(", ");
    if ir.mir.sliding_windows.len() > 1 {
        format!("({})", slides)
    } else {
        slides
    }
}
