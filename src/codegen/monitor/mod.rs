use crate::hardware_ir::{prettify_required_memory, HardwareIR, Node};
use handlebars::Handlebars;
use serde::Serialize;

mod datatypes;
mod handlebars_helpers;
pub mod hlc;
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
    has_input: bool,
}

pub fn generate_clash(hard_ir: HardwareIR) -> Option<String> {
    let mut reg = Handlebars::new();
    handlebars_helpers::register_helpers(&mut reg);

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
            has_input: hard_ir.mir.inputs.len() > 0,
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

fn get_spec(ir: &HardwareIR) -> String {
    ir.spec
        .split("\n")
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_eval_order(ir: &HardwareIR) -> String {
    ir.prettify_eval_order()
        .iter()
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_required_memory(ir: &HardwareIR) -> String {
    prettify_required_memory(&ir)
        .iter()
        .map(|s| format!("-- {}", s))
        .collect::<Vec<_>>()
        .join("\n")
}

fn get_pipeline_visualization(ir: &HardwareIR) -> String {
    ir.visualize_pipeline(10)
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
