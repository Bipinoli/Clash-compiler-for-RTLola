use crate::hardware_ir::HardwareIR;
use handlebars::{
    Context, Handlebars, Helper, Output, RenderContext, RenderError, RenderErrorReason,
};
use serde::Serialize;

mod datatypes;
mod hlc;

#[derive(Serialize)]
struct Data {
    spec_name: String,
    queue_size: usize,
    data_types: String,
    hlc: String,
}

pub fn generate_clash(hard_ir: HardwareIR) {
    let mut reg = Handlebars::new();
    register_helpers(&mut reg);
    register_template(
        "monitor".to_string(),
        "src/codegen/monitor.hbs".to_string(),
        &mut reg,
    );

    let dtypes = datatypes::render(&hard_ir, &mut reg);
    let hlc = hlc::render(&hard_ir, &mut reg);

    let all_parts_reddy = vec![dtypes.clone(), hlc.clone()]
        .iter()
        .all(|d| d.is_some());
    if all_parts_reddy {
        let data = Data {
            spec_name: "Monitor_name".to_string(),
            data_types: dtypes.unwrap(),
            queue_size: hard_ir.pipeline_wait + 2,
            hlc: hlc.unwrap(),
        };
        match reg.render("monitor", &data) {
            Ok(result) => {
                println!("{}", result);
            }
            Err(e) => {
                println!("Rendering error: {}", e);
            }
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

fn register_helpers(handlebars: &mut Handlebars) {
    handlebars.register_helper("first", Box::new(first));
    handlebars.register_helper("build_tuple", Box::new(build_tuple));
}

fn first(
    h: &Helper,
    _: &Handlebars,
    _: &Context,
    _: &mut RenderContext,
    out: &mut dyn Output,
) -> Result<(), RenderError> {
    let param = h
        .param(0)
        .ok_or_else(|| RenderError::from(RenderErrorReason::ParamNotFoundForIndex("first", 0)))?
        .value();
    let arr = param.as_array().ok_or_else(|| {
        RenderError::from(RenderErrorReason::ParamTypeMismatchForName(
            "first",
            param.to_string(),
            "array".to_string(),
        ))
    })?;
    if let Some(first) = arr.first() {
        if let Some(s) = first.as_str() {
            out.write(s)?;
        } else {
            return Err(RenderError::from(
                RenderErrorReason::ParamTypeMismatchForName(
                    "first",
                    first.to_string(),
                    "string".to_string(),
                ),
            ));
        }
    } else {
        return Err(RenderError::from(RenderErrorReason::Other(
            "Array is empty".to_string(),
        )));
    }
    Ok(())
}

fn build_tuple(
    h: &Helper,
    _: &Handlebars,
    _: &Context,
    _: &mut RenderContext,
    out: &mut dyn Output,
) -> Result<(), RenderError> {
    let param = h.param(0).map(|v| v.value()).ok_or_else(|| {
        RenderError::from(RenderErrorReason::ParamNotFoundForIndex("build_tuple", 0))
    })?;
    let str_val = param.as_str().ok_or_else(|| {
        RenderError::from(RenderErrorReason::ParamTypeMismatchForName(
            "build_tuple",
            param.to_string(),
            "string".to_string(),
        ))
    })?;
    let commas = str_val.matches(',').count();
    let tuple_val = if commas > 0 {
        format!("({})", str_val)
    } else {
        str_val.to_string()
    };
    out.write(&tuple_val)?;
    Ok(())
}
