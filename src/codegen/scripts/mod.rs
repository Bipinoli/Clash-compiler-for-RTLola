use handlebars::Handlebars;
use serde::Serialize;

#[derive(Serialize)]
struct Data {
    generated_clash_filename: String,
}

pub fn generate_verilog_gen_script(generated_clash_filename: String) -> (String, String) {
    let mut reg = Handlebars::new();

    register_template(
        "verilog.exp".to_string(),
        "src/codegen/scripts/except_script.hbs".to_string(),
        &mut reg,
    );

    register_template(
        "gen_verilog".to_string(),
        "src/codegen/scripts/gen_verilog.hbs".to_string(),
        &mut reg,
    );

    let data = Data {
        generated_clash_filename,
    };

    let verilog_exp = match reg.render("verilog.exp", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    };
    let gen_verilog_sh = match reg.render("gen_verilog", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    };
    (verilog_exp.unwrap(), gen_verilog_sh.unwrap())
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
