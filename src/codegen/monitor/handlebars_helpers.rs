use handlebars::{
    Context, Handlebars, Helper, HelperDef, HelperResult, Output, RenderContext, RenderError,
    RenderErrorReason, Renderable, ScopedJson,
};

pub fn register_helpers(handlebars: &mut Handlebars) {
    handlebars.register_helper("replace", Box::new(ReplaceHelper));
    handlebars.register_helper("eval", Box::new(EvaluateHelper));
    handlebars.register_helper("array", Box::new(ArrayHelper));
}

/// Example:
/// {{#replace ["_name_", "_surname_", "_age_"] ["Bipin", "Oli", "28"]}}
/// My name is _name_ _surname_. I am _age_ years old.
/// {{/replace}}
struct ReplaceHelper;
impl HelperDef for ReplaceHelper {
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
    let targets = h
        .param(0)
        .ok_or(RenderError::from(RenderErrorReason::ParamNotFoundForIndex(
            "target list",
            0,
        )))?
        .value()
        .as_array()
        .ok_or(RenderError::from(RenderErrorReason::InvalidParamType(
            "array",
        )))?;

    let replacements = h
        .param(1)
        .ok_or(RenderError::from(RenderErrorReason::ParamNotFoundForIndex(
            "replacement list",
            1,
        )))?
        .value()
        .as_array()
        .ok_or(RenderError::from(RenderErrorReason::InvalidParamType(
            "array",
        )))?;

    if targets.len() != replacements.len() {
        return Err(RenderError::from(RenderErrorReason::Other(
            "Size of target and replacement lists don't match".to_string(),
        )));
    }

    let template = h.template().ok_or_else(|| {
        RenderError::from(RenderErrorReason::TemplateNotFound(
            "Block content for replace helper not found".to_string(),
        ))
    })?;

    let block_content = template.renders(r, ctx, rc)?;

    // Perform replacements
    let mut result = block_content;
    for (target, replacement) in targets.iter().zip(replacements.iter()) {
        let tgt_str =
            target
                .as_str()
                .ok_or(RenderError::from(RenderErrorReason::InvalidParamType(
                    "Target must be a string",
                )))?;
        let repl_str =
            replacement
                .as_str()
                .ok_or(RenderError::from(RenderErrorReason::InvalidParamType(
                    "Replacement must be a string",
                )))?;
        result = result.replace(tgt_str, repl_str);
    }

    out.write(&result)?;
    Ok(())
}

/// Evaluates dynamic template before passing as an agrugment to another helper
///
/// Example:
/// {{#replace
///     (array "_default_expr_")
///     (array (eval "out{{../idx}}Data{{@index}}Dflt"))
/// }}
/// output{{../idx}}Data{{@index}} = _default_expr_
/// {{/replace}}
struct EvaluateHelper;
impl HelperDef for EvaluateHelper {
    fn call_inner<'reg: 'rc, 'rc>(
        &self,
        h: &Helper<'rc>,
        _reg: &'reg Handlebars<'reg>,
        ctx: &'rc Context,
        rc: &mut RenderContext<'reg, 'rc>,
    ) -> Result<ScopedJson<'rc>, RenderError> {
        let template_str = h
            .param(0)
            .and_then(|v| v.value().as_str())
            .ok_or_else(|| RenderError::from(RenderErrorReason::InvalidParamType("string")))?;

        let re = regex::Regex::new(r"\{\{\s*(.*?)\s*\}\}").unwrap();
        let variables: Vec<String> = re
            .captures_iter(template_str)
            .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
            .collect();

        let mut result = String::from(template_str);
        variables.iter().for_each(|var| {
            let var_str = format!("{{{{{}}}}}", var);
            if let Some(block_ctx) = rc.block() {
                let val = if var.starts_with("@") {
                    // local variable
                    match block_ctx.get_local_var(&var[1..]) {
                        Some(val) => {
                            if val.is_number() {
                                format!("{}", val.as_number().unwrap())
                            } else {
                                format!("{}", val.as_str().unwrap())
                            }
                        }
                        None => String::new(),
                    }
                } else {
                    // context variable
                    match rc.evaluate(ctx, &var) {
                        Ok(scoped_json) => match scoped_json {
                            ScopedJson::Context(val, _) => {
                                if val.is_number() {
                                    format!("{}", val.as_number().unwrap())
                                } else {
                                    format!("{}", val.as_str().unwrap())
                                }
                            }
                            _ => String::new(),
                        },
                        _ => String::new(),
                    }
                };
                result = result.replace(&var_str, &val);
            }
        });

        Ok(ScopedJson::Derived(serde_json::Value::String(result)))
    }
}

struct ArrayHelper;
impl HelperDef for ArrayHelper {
    fn call_inner<'reg: 'rc, 'rc>(
        &self,
        h: &Helper<'rc>,
        _reg: &'reg Handlebars<'reg>,
        _ctx: &'rc Context,
        _rc: &mut RenderContext<'reg, 'rc>,
    ) -> Result<ScopedJson<'rc>, RenderError> {
        // Collect all parameters into an array
        let mut array = Vec::new();
        for param in h.params() {
            array.push(param.value().clone());
        }

        Ok(ScopedJson::Derived(serde_json::Value::Array(array)))
    }
}
