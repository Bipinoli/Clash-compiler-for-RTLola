use clap::{command, Parser};
use hardware_ir::prettify_eval_order;
use rtlola_frontend::{self, RtLolaMir};
use serde_json;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

// mod transpile;
mod hardware_ir;
mod utils;

/// Compile RTLola specs to Clash
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the specification
    spec: PathBuf,
}

fn save_mir_as_json(mir: RtLolaMir, filename: String) {
    let serialized = serde_json::to_string_pretty(&mir).unwrap();
    let mut file =
        File::create(format!("mir/{}", filename)).expect("couldn't create a file to save MIR");
    let _ = file.write_all(serialized.as_bytes());
}

// fn to_clash(mir: RtLolaMir) {
//     transpile::transpile(&mir);
// }

fn main() {
    let args = Args::parse();
    let spec_path = args.spec;
    let mir_filename = spec_path
        .clone()
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
        + ".json";
    let config = rtlola_frontend::ParserConfig::from_path(spec_path).unwrap_or_else(|e| {
        eprintln!("{}", e);
        std::process::exit(1)
    });
    let handler = rtlola_frontend::Handler::from(&config);
    match rtlola_frontend::parse(&config) {
        Ok(mir) => {
            save_mir_as_json(mir.clone(), mir_filename);
            let roots = hardware_ir::extract_roots(&mir);
            println!(
                "Roots: {}",
                roots
                    .iter()
                    .map(|nd| nd.prettify(&mir))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let order = hardware_ir::find_eval_order(&mir);
            println!("{}", prettify_eval_order(&order, &mir));
            let order = hardware_ir::refine_eval_order(&mir, order);
            println!("{}", prettify_eval_order(&order, &mir));
            // to_haskell(mir.clone());
            // let node_tree = hardware_ir::node_tree(mir.clone());
            // dbg!(&node_tree);
        }
        Err(e) => {
            handler.emit_error(&e);
            std::process::exit(1)
        }
    }
}
