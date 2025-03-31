use handlebars::Handlebars;
use crate::hardware_ir::HardwareIR;
use serde::Serialize;
use std::error::Error;
use std::fs;

pub fn generate_clash(hard_ir: HardwareIR) -> Result<(), Box<dyn Error>> {
    let reg = Handlebars::new();
    // let template = fs::read_to_string("src/templates/final.hbs")
    //     .expect("Couldn't read the templates/final.hbs");

    // println!("{}", reg.render_template(template.as_str(), &generated)?);
    Ok(())
}