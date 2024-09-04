use handlebars::Handlebars;
use rtlola_frontend as RF;
use serde::Serialize;
use std::error::Error;
use std::fs;

use crate::main;

#[derive(Serialize, Clone, PartialEq)]
struct Input {
    name: String,
    typ: String,
    is_secondary: bool,
}

#[derive(Serialize, Clone)]
struct Output {
    name: String,
    typ: String,
    inputs: Vec<Input>,
    clause_cond: String,
    clause_expr: String,
    eval_pacing: PacingRef,
    clause_pacing: PacingRef,
}

#[derive(Serialize, Clone)]
struct Pacing {
    inputs: Vec<Input>,
    expr: String,
}

#[derive(Serialize, Clone)]
struct PacingRef {
    pref: usize,
    inputs: Vec<Input>,
}

#[derive(Serialize, Clone)]
struct Generated {
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    pacings: Vec<Pacing>,
}

pub fn transpile(mir: &RF::RtLolaMir) -> Result<(), Box<dyn Error>> {
    let (outputs, pacings) = gen_outputs_and_pacings(mir);
    let generated = Generated {
        inputs: gen_inputs(mir),
        outputs,
        pacings,
    };
    let reg = Handlebars::new();

    let template = fs::read_to_string("src/templates/final.hbs")
        .expect("Couldn't read the templates/final.hbs");

    println!("{}", reg.render_template(template.as_str(), &generated)?);
    Ok(())
}

fn gen_inputs(mir: &RF::RtLolaMir) -> Vec<Input> {
    let mut inputs = vec![Input {
        name: "time".to_string(),
        typ: "Float".to_string(),
        is_secondary: false,
    }];
    inputs.extend(mir.inputs.iter().map(|x| gen_input_from_inputstream(x)));
    inputs
}

fn gen_outputs_and_pacings(mir: &RF::RtLolaMir) -> (Vec<Output>, Vec<Pacing>) {
    let mut pacings: Vec<Pacing> = Vec::new();
    let outputs = mir
        .outputs
        .iter()
        .map(|x| {
            let inputs = x
                .accesses
                .iter()
                .map(|acc| {
                    let (sr, _) = acc;
                    match sr {
                        RF::mir::StreamReference::In(a) => {
                            gen_input_from_inputstream(&mir.inputs[*a])
                        }
                        RF::mir::StreamReference::Out(a) => {
                            gen_input_from_outputstream(&mir.outputs[*a])
                        }
                    }
                })
                .collect();

            assert!(x.eval.clauses.len() == 1);
            let clause_cond = match &x.eval.clauses.first().unwrap().condition {
                Some(e) => gen_expression(e, mir),
                None => "True".to_string(),
            };
            let clause_expr = gen_expression(&x.eval.clauses.first().unwrap().expression, mir);

            let eval_pacing = gen_pacing(&x.eval.eval_pacing, mir);
            let eval_pacing = PacingRef {
                inputs: eval_pacing.inputs.clone(),
                pref: collect_pacings(&eval_pacing, &mut pacings),
            };
            let clause_pacing = gen_pacing(&x.eval.clauses.first().unwrap().pacing, mir);
            let clause_pacing = PacingRef {
                inputs: clause_pacing.inputs.clone(),
                pref: collect_pacings(&clause_pacing, &mut pacings),
            };
            Output {
                name: x.name.clone(),
                typ: gen_type(&x.ty),
                inputs,
                clause_cond,
                clause_expr,
                eval_pacing,
                clause_pacing,
            }
        })
        .collect();
    (outputs, pacings)
}

fn collect_pacings(pacing: &Pacing, pacings: &mut Vec<Pacing>) -> usize {
    let indx = pacings
        .iter()
        .position(|p| p.expr == pacing.expr && p.inputs == pacing.inputs);
    match indx {
        Some(i) => i,
        None => {
            pacings.push(pacing.clone());
            pacings.len() - 1
        }
    }
}

fn gen_expression(expr: &RF::mir::Expression, mir: &RF::RtLolaMir) -> String {
    match &expr.kind {
        RF::mir::ExpressionKind::LoadConstant(c) => format!("{}", c),
        RF::mir::ExpressionKind::ArithLog(op, exprs) => {
            let params = exprs
                .iter()
                .map(|x| gen_expression(x, mir))
                .fold(String::new(), |acc, x| format!("{} ({})", acc, x));
            match op {
                RF::mir::ArithLogOp::Add => format!("(+){}", params),
                RF::mir::ArithLogOp::Sub => format!("(-){}", params),
                RF::mir::ArithLogOp::Mul => format!("(*){}", params),
                RF::mir::ArithLogOp::Div => format!("(/){}", params),
                RF::mir::ArithLogOp::Lt => format!("(<){}", params),
                RF::mir::ArithLogOp::Gt => format!("(>){}", params),
                RF::mir::ArithLogOp::Le => format!("(<=){}", params),
                RF::mir::ArithLogOp::Ge => format!("(>=){}", params),
                RF::mir::ArithLogOp::Ne => format!("(/=){}", params),
                RF::mir::ArithLogOp::Eq => format!("(==){}", params),
                RF::mir::ArithLogOp::Not => format!("(not){}", params),
                RF::mir::ArithLogOp::And => format!("(&&){}", params),
                RF::mir::ArithLogOp::Or => format!("(||){}", params),
                RF::mir::ArithLogOp::Pow => format!("(**){}", params),
                _ => unimplemented!("unknown arithmetic and logic operation {}", op),
            }
        }
        RF::mir::ExpressionKind::StreamAccess {
            target,
            parameters,
            access_kind,
        } => match target {
            RF::mir::StreamReference::In(a) => mir.inputs[*a].name.clone(),
            RF::mir::StreamReference::Out(a) => mir.outputs[*a].name.clone(),
        },
        RF::mir::ExpressionKind::Function(name, exprs) => {
            let params = exprs
                .iter()
                .map(|x| gen_expression(x, mir))
                .fold(String::new(), |acc, x| format!("{} ({})", acc, x));
            match name.as_ref() {
                "sqrt" => format!("sqrt {}", params),
                "sin" => format!("sin {}", params),
                "cos" => format!("cos {}", params),
                "tan" => format!("tan {}", params),
                "arcsin" => format!("asin {}", params),
                "arccos" => format!("acos {}", params),
                "arctan" => format!("atan {}", params),
                "abs" => format!("abs {}", params),
                "min" => format!("min {}", params),
                "max" => format!("max {}", params),
                "round" => format!("round {}", params),
                _ => unimplemented!("unknown function name {}", name),
            }
        }
        RF::mir::ExpressionKind::Default { expr, default } => {
            let main_expr = gen_expression(expr, mir);
            let default_expr = gen_expression(default, mir);
            format!("(exprOrElse ({}) ({}))", main_expr, default_expr)
        }
        _ => unimplemented!("unknown expression type {:?}", expr),
    }
}

fn gen_type(typ: &RF::mir::Type) -> String {
    match typ {
        RF::mir::Type::Bool => "Bool".to_string(),
        RF::mir::Type::Int(_) => "Integer".to_string(),
        RF::mir::Type::Float(_) => "Float".to_string(),
        RF::mir::Type::String => "String".to_string(),
        _ => unreachable!("unknown type {}", typ),
    }
}

fn gen_input_from_inputstream(input: &RF::mir::InputStream) -> Input {
    Input {
        name: input.name.clone(),
        typ: gen_type(&input.ty),
        is_secondary: false,
    }
}

fn gen_input_from_outputstream(input: &RF::mir::OutputStream) -> Input {
    Input {
        name: input.name.clone(),
        typ: gen_type(&input.ty),
        is_secondary: true,
    }
}

fn gen_pacing_from_activation_cond(
    ac: &RF::mir::ActivationCondition,
    mir: &RF::mir::RtLolaMir,
) -> Pacing {
    match ac {
        RF::mir::ActivationCondition::True => Pacing {
            inputs: Vec::new(),
            expr: "True".to_string(),
        },
        RF::mir::ActivationCondition::Stream(sr) => match sr {
            RF::mir::StreamReference::In(a) => {
                let inpt_name = mir.inputs[*a].name.clone();
                Pacing {
                    inputs: Vec::from([gen_input_from_inputstream(&mir.inputs[*a])]),
                    expr: format!("(eventIn {})", inpt_name),
                }
            }
            RF::mir::StreamReference::Out(a) => {
                let inpt_name = mir.outputs[*a].name.clone();
                Pacing {
                    inputs: Vec::from([gen_input_from_outputstream(&mir.outputs[*a])]),
                    expr: format!("(eventIn {})", inpt_name),
                }
            }
        },
        RF::mir::ActivationCondition::Conjunction(conds) => {
            let pacings = conds
                .iter()
                .map(|x| gen_pacing_from_activation_cond(x, mir));
            let inputs = pacings.clone().flat_map(|x| x.inputs).collect();
            let expr = pacings
                .map(|x| x.expr)
                .collect::<Vec<String>>()
                .join(" && ");
            Pacing { inputs, expr }
        }
        RF::mir::ActivationCondition::Disjunction(conds) => {
            let pacings = conds
                .iter()
                .map(|x| gen_pacing_from_activation_cond(x, mir));
            let inputs = pacings.clone().flat_map(|x| x.inputs).collect();
            let expr = pacings
                .map(|x| x.expr)
                .collect::<Vec<String>>()
                .join(" || ");
            Pacing { inputs, expr }
        }
    }
}

fn gen_pacing(pacing: &RF::mir::PacingType, mir: &RF::mir::RtLolaMir) -> Pacing {
    match pacing {
        RF::mir::PacingType::Constant => Pacing {
            inputs: Vec::new(),
            expr: "True".to_string(),
        },
        RF::mir::PacingType::Event(ac) => gen_pacing_from_activation_cond(ac, mir),
        _ => unimplemented!("unknown pacing type {:?}", pacing),
    }
}
