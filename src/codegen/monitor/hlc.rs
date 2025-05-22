use std::collections::HashSet;

use handlebars::Handlebars;
use rtlola_frontend::mir::PacingType;
use serde::Serialize;
use uom::num_rational::Ratio;
use uom::si::rational64::Frequency;

use crate::{
    codegen::monitor::datatypes, codegen::monitor::register_template, hardware_ir::HardwareIR,
};

#[derive(Serialize)]
struct Data {
    has_periodic_pacing: bool,
    has_input: bool,
    has_sliding_window: bool,
    inputs: Vec<datatypes::Stream>,
    output_pacings: Vec<OutputPacing>,
    periods: Vec<String>,
    slides: Vec<String>,
}

#[derive(Serialize)]
pub struct OutputPacing {
    pub deps: Vec<String>,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "hlc".to_string(),
        "src/codegen/monitor/hlc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        has_input: ir.mir.inputs.len() > 0,
        has_periodic_pacing: get_has_periodic_pacing(ir),
        has_sliding_window: ir.mir.sliding_windows.len() > 0,
        inputs: datatypes::get_inputs(ir),
        output_pacings: get_output_pacings(ir),
        slides: get_slides(ir, &get_all_periods(ir)),
        periods: get_all_periods(ir),
    };
    match handlebars.render("hlc", &data) {
        Ok(result) => Some(result),
        Err(e) => {
            println!("Rendering error: {}", e);
            None
        }
    }
}

fn get_has_periodic_pacing(ir: &HardwareIR) -> bool {
    ir.mir.outputs.iter().any(|out| match out.eval.eval_pacing {
        PacingType::Event(_) => false,
        PacingType::GlobalPeriodic(_) => true,
        _ => unimplemented!(),
    })
}

pub fn get_output_pacings(ir: &HardwareIR) -> Vec<OutputPacing> {
    let all_periods = &get_all_periods(ir);
    ir.mir
        .outputs
        .iter()
        .map(|outpt| {
            let pacing = datatypes::get_pacing_from_output(outpt);
            OutputPacing {
                deps: get_pacing_deps_names(pacing, all_periods),
            }
        })
        .collect()
}

fn get_pacing_deps_names(pacing: datatypes::Pacing, all_periods: &Vec<String>) -> Vec<String> {
    match pacing {
        datatypes::Pacing::Periodic(freq) => {
            let period = get_period_in_ns(&freq);
            let indx = all_periods.iter().position(|p| *p == period).unwrap();
            vec![format!("timer{}Over", indx)]
        }
        datatypes::Pacing::Stream(name) => vec![format!("p{}", name)],
        datatypes::Pacing::And(v) => v
            .iter()
            .map(|p| get_pacing_deps_names(p.clone(), all_periods))
            .flatten()
            .collect(),
        datatypes::Pacing::Input => unreachable!(),
    }
}

fn get_period_in_ns(freq: &Frequency) -> String {
    let period_in_seconds = Ratio::new(freq.value.denom().clone(), freq.value.numer().clone());
    let nanos = Ratio::new(1_000_000_000, 1);
    let period_in_nanoseconds = period_in_seconds * nanos;
    format!("{}", period_in_nanoseconds.round())
}

fn get_all_periods(ir: &HardwareIR) -> Vec<String> {
    let output_periods: HashSet<String> = ir
        .mir
        .outputs
        .iter()
        .filter(|&out| match out.eval.eval_pacing {
            PacingType::GlobalPeriodic(_) => true,
            _ => false,
        })
        .map(|out| match out.eval.eval_pacing {
            PacingType::GlobalPeriodic(freq) => get_period_in_ns(&freq),
            _ => unreachable!(),
        })
        .collect();
    let sliding_periods: HashSet<String> = ir
        .mir
        .sliding_windows
        .iter()
        .map(|win| format!("{}", win.bucket_size.as_nanos()))
        .collect();
    let mut all_periods: Vec<String> = output_periods
        .union(&sliding_periods)
        .cloned()
        .collect::<HashSet<_>>()
        .iter()
        .cloned()
        .collect();
    all_periods.sort_by_key(|p| {
        let num: u32 = p.parse().expect("couldn't parse");
        num
    });
    all_periods
}

fn get_slides(ir: &HardwareIR, all_pacings: &Vec<String>) -> Vec<String> {
    ir.mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, sw)| {
            let period = format!("{}", sw.bucket_size.as_nanos());
            let indx = all_pacings.iter().position(|p| *p == period).unwrap();
            format!("s{} = timer{}Over", i, indx)
        })
        .collect()
}
