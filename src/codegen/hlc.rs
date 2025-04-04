use std::collections::HashSet;

use handlebars::Handlebars;
use rtlola_frontend::mir::{ActivationCondition, OutputStream, PacingType, StreamReference};
use serde::Serialize;
use uom::num_rational::Ratio;
use uom::si::rational64::Frequency;

use crate::{codegen::register_template, hardware_ir::HardwareIR};

#[derive(Serialize)]
struct Data {
    has_periodic_pacing: bool,
    has_sliding_window: bool,
    new_event_condition: String,
    bundled_slides: String,
    bundled_pacings: String,
    unbundled_inputs: Vec<String>,
    pacings: Vec<String>,
    periods: Vec<String>,
    slides: Vec<String>,
}

pub fn render(ir: &HardwareIR, handlebars: &mut Handlebars) -> Option<String> {
    register_template(
        "hlc".to_string(),
        "src/codegen/hlc.hbs".to_string(),
        handlebars,
    );
    let data = Data {
        has_periodic_pacing: get_has_periodic_pacing(ir),
        has_sliding_window: get_has_sliding_window(ir),
        new_event_condition: get_new_event_condition(ir),
        bundled_slides: get_bundled_slides(ir),
        bundled_pacings: get_bundled_pacings(ir),
        unbundled_inputs: get_unbundled_inputs(ir),
        pacings: get_pacings(ir, &get_all_periods(ir)),
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

fn get_has_sliding_window(ir: &HardwareIR) -> bool {
    ir.mir.sliding_windows.len() > 0
}

fn get_has_periodic_pacing(ir: &HardwareIR) -> bool {
    ir.mir.outputs.iter().any(|out| match out.eval.eval_pacing {
        PacingType::Event(_) => false,
        PacingType::GlobalPeriodic(_) => true,
        _ => unimplemented!(),
    })
}

fn get_new_event_condition(ir: &HardwareIR) -> String {
    let slides = ir
        .mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| format!("slide{}", i))
        .collect::<Vec<_>>()
        .join(" .||. ");
    let pacings = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("pacing{}", i))
        .collect::<Vec<_>>()
        .join(" .||. ");
    if slides.len() > 0 {
        format!("{} .||. {}", slides, pacings)
    } else {
        pacings
    }
}

fn get_bundled_slides(ir: &HardwareIR) -> String {
    let slides = ir
        .mir
        .sliding_windows
        .iter()
        .enumerate()
        .map(|(i, _)| format!("slide{}", i))
        .collect::<Vec<_>>()
        .join(", ");
    if ir.mir.sliding_windows.len() > 1 {
        format!("bundle ({})", slides)
    } else {
        slides
    }
}

fn get_bundled_pacings(ir: &HardwareIR) -> String {
    let pacings = ir
        .mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("pacing{}", i))
        .collect::<Vec<_>>()
        .join(", ");
    if ir.mir.outputs.len() > 1 {
        format!("bundle ({})", pacings)
    } else {
        pacings
    }
}

fn get_unbundled_inputs(ir: &HardwareIR) -> Vec<String> {
    if ir.mir.inputs.len() == 1 {
        return vec!["(_, hasInput0) = unbundle inputs".to_string()];
    }
    let inputs = ir
        .mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("input{}", i))
        .collect::<Vec<_>>()
        .join(", ");
    let mut retval = vec![format!("({}) = unbundle inputs", inputs)];
    let unbundled: Vec<String> = ir
        .mir
        .inputs
        .iter()
        .enumerate()
        .map(|(i, _)| format!("(_, hasInput{}) = unbundle input{}", i, i))
        .collect();
    retval.extend(unbundled);
    retval
}

fn get_pacings(ir: &HardwareIR, all_periods: &Vec<String>) -> Vec<String> {
    ir.mir
        .outputs
        .iter()
        .enumerate()
        .map(|(i, output)| {
            format!(
                "pacing{} = {}",
                i,
                get_pacing_from_output(output, all_periods)
            )
        })
        .collect()
}

fn get_pacing_from_output(output: &OutputStream, all_periods: &Vec<String>) -> String {
    match &output.eval.eval_pacing {
        PacingType::Event(cond) => get_pacing_from_activation_cond(&cond),
        PacingType::GlobalPeriodic(freq) => {
            let period = get_period_in_ns(freq);
            let indx = all_periods.iter().position(|p| *p == period).unwrap();
            format!("timer{}Over", indx)
        }
        _ => unimplemented!(),
    }
}

fn get_pacing_from_activation_cond(cond: &ActivationCondition) -> String {
    match cond {
        ActivationCondition::Stream(st) => match st {
            StreamReference::In(x) => format!("hasInput{}", x),
            _ => unreachable!(),
        },
        ActivationCondition::Conjunction(conj) => conj
            .iter()
            .map(|cond| get_pacing_from_activation_cond(cond))
            .collect::<Vec<_>>()
            .join(" .&&. "),
        _ => unimplemented!(),
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
        .map(|win| format!("{}", win.duration.as_nanos()))
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
            let period = format!("{}", sw.duration.as_nanos());
            let indx = all_pacings.iter().position(|p| *p == period).unwrap();
            format!("slide{} = timer{}Over", i, indx)
        })
        .collect()
}
