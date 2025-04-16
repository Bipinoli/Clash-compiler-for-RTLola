use crate::hardware_ir::{
    prettify_eval_order, prettify_required_memory, visualize_pipeline, HardwareIR, Node,
};
use handlebars::{
    Context, Handlebars, Helper, Output, RenderContext, RenderError, RenderErrorReason,
};
use serde::Serialize;

mod datatypes;
mod hlc;
mod llc;

#[derive(Serialize)]
struct Data {
    spec_name: String,
    spec: String,
    eval_order: String,
    required_memory: String,
    pipeline_visualization: String,
    stream_names: Vec<String>,
    queue_size: usize,
    data_types: String,
    hlc: String,
    llc: String,
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
    let llc = llc::render(&hard_ir, &mut reg);

    let all_parts_reddy = vec![dtypes.clone(), hlc.clone(), llc.clone()]
        .iter()
        .all(|d| d.is_some());
    if all_parts_reddy {
        let data = Data {
            spec_name: to_pascal_case(&hard_ir.spec_name),
            spec: get_spec(&hard_ir),
            eval_order: get_eval_order(&hard_ir),
            required_memory: get_required_memory(&hard_ir),
            pipeline_visualization: get_pipeline_visualization(&hard_ir),
            stream_names: get_stream_names(&hard_ir),
            data_types: dtypes.unwrap(),
            queue_size: hard_ir.pipeline_wait + 2,
            hlc: hlc.unwrap(),
            llc: llc.unwrap(),
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

fn get_spec(ir: &HardwareIR) -> String {
    ir.spec
        .split("\n")
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn to_pascal_case(s: &str) -> String {
    s.split(|c: char| c == ' ' || c == '_' || c == '-')
        .filter(|w| !w.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first_char) => {
                    let mut capitalized = String::new();
                    capitalized.push_str(&first_char.to_uppercase().to_string());
                    capitalized.push_str(&chars.as_str().to_lowercase());
                    capitalized
                }
                None => String::new(),
            }
        })
        .collect()
}

fn get_eval_order(ir: &HardwareIR) -> String {
    prettify_eval_order(&ir.evaluation_order, &ir.mir)
        .iter()
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_required_memory(ir: &HardwareIR) -> String {
    prettify_required_memory(&ir.evaluation_order, &ir.mir)
        .iter()
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_pipeline_visualization(ir: &HardwareIR) -> String {
    visualize_pipeline(&ir.evaluation_order, ir.pipeline_wait, 10, &ir.mir)
        .iter()
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_stream_names(ir: &HardwareIR) -> Vec<String> {
    let inputs: Vec<String> = ir
        .mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("input{} = {}", i, Node::InputStream(i).prettify(&ir.mir)))
        .collect();
    let outputs: Vec<String> = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("output{} = {}", i, Node::OutputStream(i).prettify(&ir.mir)))
        .collect();
    let sliding_windows: Vec<String> = ir
        .mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| format!("sw{} = {}", i, Node::SlidingWindow(i).prettify(&ir.mir)))
        .collect();
    [&inputs[..], &outputs[..], &sliding_windows[..]].concat()
}
