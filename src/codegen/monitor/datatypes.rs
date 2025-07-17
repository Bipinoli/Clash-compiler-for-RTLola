use std::collections::HashSet;

use handlebars::Handlebars;
use rtlola_frontend::{
    self as RF,
    mir::{ActivationCondition, OutputStream, PacingType, SlidingWindow, StreamReference},
};
use serde::Serialize;
use uom::si::rational64::Frequency;

use crate::{analysis::HardwareIR, codegen::monitor::register_template};

#[derive(Serialize)]
struct Data {
    debug: bool,
    inputs: Vec<Stream>,
    outputs: Vec<Stream>,
    sliding_windows: Vec<Stream>,
    all_streams: Vec<Stream>,
    all_unique_types: Vec<String>,
    has_sliding_window: bool,
}

#[derive(Serialize, Clone)]
pub struct Stream {
    stream_type: String,
    index: usize,
    ty: String,
    default: String,
    get_pacing_stmt: String,
    pacing_deps: Vec<String>,
    is_periodic: bool,
}

#[derive(Serialize, Clone)]
pub enum Pacing {
    Input,
    Stream(String),
    And(Vec<Pacing>),
    Or(Vec<Pacing>),
    Periodic(Frequency),
}
impl Pacing {
    fn get_function_stmt(&self, stream_name: String) -> String {
        match &self {
            Pacing::Input => format!("getPacing (Pacing{} x) = x", stream_name),
            Pacing::Periodic(_) => format!("getPacing (Pacing{} x) = x", stream_name),
            Pacing::Stream(_) => format!("getPacing (Pacing{} x) = getPacing x", stream_name),
            Pacing::And(v) => {
                let params: Vec<String> = v
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("x{}", i))
                    .collect();
                let clauses: Vec<String> = v
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("getPacing x{}", i))
                    .collect();
                format!(
                    "getPacing (Pacing{} {}) = {}",
                    stream_name,
                    params.join(" "),
                    clauses.join(" && ")
                )
            },
            Pacing::Or(v) => {
                let params: Vec<String> = v
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("x{}", i))
                    .collect();
                let clauses: Vec<String> = v
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("getPacing x{}", i))
                    .collect();
                format!(
                    "getPacing (Pacing{} {}) = {}",
                    stream_name,
                    params.join(" "),
                    clauses.join(" || ")
                )
            }
        }
    }
    fn get_pacing_deps(&self) -> Vec<String> {
        match &self {
            Pacing::Input => vec![format!("Bool")],
            Pacing::Periodic(_) => vec![format!("Bool")],
            Pacing::Stream(x) => vec![format!("{}", x)],
            Pacing::And(v) => v.iter().map(|it| it.get_pacing_deps()).flatten().collect(),
            Pacing::Or(v) => v.iter().map(|it| it.get_pacing_deps()).flatten().collect(),
        }
    }
    pub fn is_periodic(&self) -> bool {
        match &self {
            Pacing::Periodic(_) => true,
            _ => false,
        }
    }
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "datatypes".to_string(),
        "src/codegen/monitor/datatypes.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        debug: ir.debug,
        inputs: get_inputs(ir),
        outputs: get_outputs(ir),
        sliding_windows: get_sliding_windows(ir),
        all_streams: get_all_streams(ir),
        all_unique_types: get_all_unique_types(ir),
        has_sliding_window: get_sliding_windows(ir).len() > 0,
    };
    match handlebars.render("datatypes", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

pub fn get_inputs(ir: &HardwareIR) -> Vec<Stream> {
    ir.mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, inpt)| Stream {
            stream_type: "input".to_string(),
            index: i,
            ty: get_type(&inpt.ty),
            default: get_default_for_type(&inpt.ty),
            pacing_deps: Pacing::Input.get_pacing_deps(),
            get_pacing_stmt: Pacing::Input.get_function_stmt(format!("In{}", i)),
            is_periodic: false,
        })
        .collect()
}

pub fn get_outputs(ir: &HardwareIR) -> Vec<Stream> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, outpt)| Stream {
            stream_type: "output".to_string(),
            index: i,
            ty: get_type(&outpt.ty),
            default: get_default_for_type(&outpt.ty),
            pacing_deps: get_pacing_from_output(&outpt).get_pacing_deps(),
            get_pacing_stmt: get_pacing_from_output(&outpt).get_function_stmt(format!("Out{}", i)),
            is_periodic: get_pacing_from_output(&outpt).is_periodic(),
        })
        .collect()
}

fn get_sliding_windows(ir: &HardwareIR) -> Vec<Stream> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, sw)| {
            let target_name = get_target_from_sliding_window(sw);
            let pacing = Pacing::Stream(target_name.clone());
            Stream {
                stream_type: "slide".to_string(),
                index: i,
                ty: "Bool".to_string(),
                default: "False".to_string(),
                pacing_deps: pacing.get_pacing_deps(),
                get_pacing_stmt: pacing.get_function_stmt(target_name),
                is_periodic: pacing.is_periodic(),
            }
        })
        .collect()
}

pub fn get_pacing_from_output(outpt: &OutputStream) -> Pacing {
    match &outpt.eval.eval_pacing {
        PacingType::GlobalPeriodic(freq) => Pacing::Periodic(freq.clone()),
        PacingType::Event(ac) => get_pacing_from_activation_cond(ac),
        _ => unimplemented!(),
    }
}

pub fn get_target_from_sliding_window(sw: &SlidingWindow) -> String {
    let target_name = match sw.target {
        StreamReference::In(x) => format!("In{}", x),
        StreamReference::Out(x) => format!("Out{}", x),
    };
    target_name
}

fn get_pacing_from_activation_cond(ac: &ActivationCondition) -> Pacing {
    match ac {
        ActivationCondition::Conjunction(v) => {
            let pacings: Vec<Pacing> = v
                .into_iter()
                .map(|a| get_pacing_from_activation_cond(a))
                .collect();
            Pacing::And(pacings)
        }
        ActivationCondition::Disjunction(v) => {
            let pacings: Vec<Pacing> = v
                .into_iter()
                .map(|a| get_pacing_from_activation_cond(a))
                .collect();
            Pacing::Or(pacings)
        }
        ActivationCondition::Stream(s) => Pacing::Stream(match s {
            StreamReference::In(x) => format!("In{}", x),
            StreamReference::Out(x) => format!("Out{}", x),
        }),
        _ => unimplemented!(),
    }
}

fn get_all_streams(ir: &HardwareIR) -> Vec<Stream> {
    vec![
        &get_inputs(ir)[..],
        &get_outputs(ir)[..],
        &get_sliding_windows(ir)[..],
    ]
    .concat()
}

fn get_all_unique_types(ir: &HardwareIR) -> Vec<String> {
    vec![&get_inputs(ir)[..], &get_outputs(ir)[..]]
        .concat()
        .into_iter()
        .map(|s| s.ty)
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}

pub fn get_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Int(_) => "Int".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}

pub fn get_default_for_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Int(_) => "0".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}
