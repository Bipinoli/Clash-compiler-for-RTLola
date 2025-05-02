use crate::hardware_ir::{
    prettify_eval_order, prettify_required_memory, visualize_pipeline, HardwareIR, Node,
};
use handlebars::{
    Context, Handlebars, Helper, HelperResult, Output, RenderContext, RenderError, RenderErrorReason, Renderable
};
use llc::{get_pacings_type, get_slides_type};
use serde::Serialize;

mod datatypes;
mod hlc;
mod llc;

#[derive(Serialize)]
struct Data {
    debug: bool,
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
    has_sliding_window: bool,
    pacings_type: String,
    slides_type: String,
}

pub fn generate_clash(hard_ir: HardwareIR) -> Option<String> {
    let mut reg = Handlebars::new();
    register_helpers(&mut reg);
    register_template(
        "monitor".to_string(),
        "src/codegen/monitor/monitor.hbs".to_string(),
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
            debug: hard_ir.debug,
            spec_name: hard_ir.spec_name.clone(),
            spec: get_spec(&hard_ir),
            eval_order: get_eval_order(&hard_ir),
            required_memory: get_required_memory(&hard_ir),
            pipeline_visualization: get_pipeline_visualization(&hard_ir),
            stream_names: get_stream_names(&hard_ir),
            data_types: dtypes.unwrap(),
            queue_size: hard_ir.pipeline_wait + 2,
            hlc: hlc.unwrap(),
            llc: llc.unwrap(),
            has_sliding_window: hard_ir.mir.sliding_windows.len() > 0,
            pacings_type: get_pacings_type(&hard_ir),
            slides_type: get_slides_type(&hard_ir),
        };
        match reg.render("monitor", &data) {
            Ok(result) => Some(result),
            Err(e) => {
                println!("Rendering error: {}", e);
                None
            }
        }
    } else {
        None
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
    handlebars.register_helper("replace", Box::new(ReplaceHelper));
}

struct ReplaceHelper;
impl handlebars::HelperDef for ReplaceHelper {
    fn call<'reg: 'rc, 'rc>(
        &self,
        h: &Helper<'rc>,
        r: &'reg Handlebars<'reg>,
        ctx: &'rc Context,
        rc: &mut RenderContext<'reg, 'rc>,
        out: &mut dyn Output,
    ) -> HelperResult {
        replace_helper(h, r, ctx, rc, out)
    }
}

fn replace_helper<'reg: 'rc, 'rc>(
    h: &Helper<'rc>,
    r: &'reg Handlebars<'reg>,
    ctx: &'rc Context,
    rc: &mut RenderContext<'reg, 'rc>,
    out: &mut dyn Output,
) -> Result<(), RenderError> {
    let targets = h.param(0)
        .ok_or(RenderError::from(RenderErrorReason::ParamNotFoundForIndex("target list", 0)))?
        .value()
        .as_array()
        .ok_or(RenderError::from(RenderErrorReason::InvalidParamType("must be an array")))?;

    let replacements = h.param(1)
        .ok_or(RenderError::from(RenderErrorReason::ParamNotFoundForIndex("replacement list", 1)))?
        .value()
        .as_array()
        .ok_or(RenderError::from(RenderErrorReason::InvalidParamType("must be an array")))?;
    
    if targets.len() != replacements.len() {
        return Err(RenderError::from(RenderErrorReason::InvalidParamType("Size of target and replacement lists don't match")));
    }

    let template = h.template().ok_or_else(|| {
        RenderError::from(RenderErrorReason::TemplateNotFound("Block content for replace helper not found".to_string()))
    })?;
    
    let block_content = template.renders(r, ctx, rc)?;
    
    // Perform replacements
    let mut result = block_content;
    for (target, replacement) in targets.iter().zip(replacements.iter()) {
        let tgt_str = target.as_str().ok_or(RenderError::from(RenderErrorReason::InvalidParamType("Target must be a string")))?;
        let repl_str = replacement.as_str().ok_or(RenderError::from(RenderErrorReason::InvalidParamType("Replacement must be a string")))?;
        result = result.replace(tgt_str, repl_str);
    }

    out.write(&result)?;
    Ok(())
}

fn get_spec(ir: &HardwareIR) -> String {
    ir.spec
        .split("\n")
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
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
