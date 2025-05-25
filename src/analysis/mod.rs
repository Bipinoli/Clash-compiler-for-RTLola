use crate::analysis::node::Node;
use rtlola_frontend::mir::RtLolaMir;
use serde::Serialize;
use std::{collections::HashMap, usize};

mod eval_order;
pub mod node;

use eval_order::{memory, pipeline, utils};

/// Intermediate representation for code generation obtained by analysing the dependency graph of the RTLola spec   
///
/// Evaluation is done in a pipeline fashion in the order gived by "evaluation_order"   
/// It is sometime necessary to wait for data propagation before the next pipeline cycle can be started as indicated by "pipeline_wait"   
/// Nodes need to remember the value until consumed which is indicated by the "required_memory"   
#[derive(PartialEq, Clone, Debug, Serialize)]
pub struct HardwareIR {
    pub mir: RtLolaMir,
    pub evaluation_order: Vec<Vec<Node>>,
    pub pipeline_wait: usize,
    pub required_memory: HashMap<Node, usize>,
    pub spec: String,
    pub spec_name: String,
    pub debug: bool,
}

impl HardwareIR {
    pub fn new(mir: RtLolaMir, spec: String, spec_name: String, debug: bool) -> Self {
        let eval_order = eval_order::find_eval_order(&mir);
        let pipeline_wait = pipeline::calculate_necessary_pipeline_wait(&eval_order, &mir);
        let required_memory = memory::calculate_required_memory(&eval_order, &mir);
        HardwareIR {
            evaluation_order: eval_order,
            mir,
            pipeline_wait,
            required_memory,
            spec,
            spec_name,
            debug,
        }
    }

    pub fn find_level(&self, node: &Node) -> usize {
        utils::find_level(node, &self.evaluation_order)
    }

    pub fn prettify_eval_order(&self) -> Vec<String> {
        utils::prettify_eval_order(&self.evaluation_order, &self.mir)
    }

    pub fn prettify_required_memory(&self) -> Vec<String> {
        memory::prettify_required_memory(&self)
    }

    pub fn visualize_pipeline(&self, time_steps: usize) -> Vec<String> {
        pipeline::visualize_pipeline(
            &self.evaluation_order,
            self.pipeline_wait,
            time_steps,
            &self.mir,
        )
    }

    pub fn display_analysis(&self) {
        println!("\n------- The evaluation pipeline --------");
        println!("{}\n", self.prettify_eval_order().join("\n"));
        println!("{}\n", memory::prettify_required_memory(&self).join("\n"));
        println!("{}\n", self.visualize_pipeline(10).join("\n"));
    }
}
