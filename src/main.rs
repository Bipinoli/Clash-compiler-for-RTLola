use clap::{command, Parser};
use rtlola_frontend::{self, RtLolaMir};
use serde_json;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
mod codegen;
mod hardware_ir;

/// Compile RTLola specs to Clash
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to RTLola (*.lola) specification
    #[arg(short, long)]
    spec: PathBuf,

    /// Output path
    #[arg(short, long)]
    output: PathBuf,

    /// Should generate verilog testbench
    #[arg(short, long, default_value_t = true)]
    testbench: bool,

    /// Should generate verilog code & scripts
    #[arg(short, long, default_value_t = true)]
    verilog: bool,

    /// Should output RTLolaMIR into a json file
    #[arg(short, long, default_value_t = false)]
    mir: bool,

    /// Generate clash code & testbench with debug information
    #[arg(short, long, default_value_t = true)]
    debug: bool,
}

fn main() {
    let args = Args::parse();
    let spec_path = args.spec;

    let config = rtlola_frontend::ParserConfig::from_path(spec_path.clone()).unwrap_or_else(|e| {
        eprintln!("{}", e);
        std::process::exit(1)
    });
    let handler = rtlola_frontend::Handler::from(&config);
    match rtlola_frontend::parse(&config) {
        Ok(mir) => {
            let spec = read_file(spec_path.clone());
            if args.mir {
                save_mir_as_json(
                    mir.clone(),
                    extract_file_stem(&spec_path) + ".json",
                    args.output.clone(),
                );
            };
            // hardware_ir::display_analysis(&mir);
            let hard_ir = hardware_ir::HardwareIR::new(
                mir,
                spec,
                to_pascal_case(&extract_file_stem(&spec_path)),
                args.debug,
            );
            match codegen::monitor::generate_clash(hard_ir) {
                Some(generated) => {
                    write_to_file(
                        args.output.join(extract_file_stem(&spec_path) + ".hs"),
                        generated,
                    );
                    // if args.testbench {
                    //     todo!("generate testbench");
                    // };
                    if args.verilog {
                        let (verilog_exp, gen_verilog_sh) =
                            codegen::scripts::generate_verilog_gen_script(
                                extract_file_stem(&spec_path) + ".hs",
                            );
                        write_to_file(args.output.join("verilog.exp"), verilog_exp);
                        write_to_file(args.output.join("gen_verilog.sh"), gen_verilog_sh);
                    };
                    println!(
                        "Compilation successful! Ouput at {}",
                        args.output.to_str().unwrap()
                    );
                }
                None => {
                    println!("Couldn't generate clash code");
                }
            };
        }
        Err(e) => {
            handler.emit_error(&e);
            std::process::exit(1)
        }
    }
}

fn read_file(path: PathBuf) -> String {
    let mut file = File::open(&path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    drop(file);
    content
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

fn save_mir_as_json(mir: RtLolaMir, filename: String, output_path: PathBuf) {
    let serialized = serde_json::to_string_pretty(&mir).unwrap();
    let mut file =
        File::create(output_path.join(filename)).expect("couldn't create a file to save MIR");
    let _ = file.write_all(serialized.as_bytes());
}

fn write_to_file(path: PathBuf, content: String) {
    let mut file = File::create(path).expect("couldn't create a file to write");
    let _ = file.write_all(content.as_bytes());
}

fn extract_file_stem(path: &PathBuf) -> String {
    path.clone()
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
}
