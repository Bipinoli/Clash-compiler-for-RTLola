use handlebars::Handlebars;
use serde::Serialize;

use crate::hardware_ir::HardwareIR;

#[derive(Serialize)]
struct Data {}

pub fn generate_verilog_testbench(hard_ir: HardwareIR) {
    let mut reg = Handlebars::new();

    register_template(
        "testbench".to_string(),
        "src/codegen/testbench/verilog_testbench.hbs".to_string(),
        &mut reg,
    );

    let data = Data {};
    match reg.render("testbench", &data) {
        Ok(result) => {
            println!("{}", result);
        }
        Err(e) => {
            println!("Rendering error: {}", e);
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
