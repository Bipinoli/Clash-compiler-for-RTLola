use handlebars::{
    Context, Handlebars, Helper, HelperResult, Output, RenderContext, RenderError,
    RenderErrorReason, Renderable, ScopedJson, HelperDef
};

pub fn register_helpers(handlebars: &mut Handlebars) {
    handlebars.register_helper("replace", Box::new(ReplaceHelper));
    handlebars.register_helper("eval", Box::new(EvaluateHelper));
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
            "must be an array",
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
            "must be an array",
        )))?;

    if targets.len() != replacements.len() {
        return Err(RenderError::from(RenderErrorReason::InvalidParamType(
            "Size of target and replacement lists don't match",
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

struct EvaluateHelper;
impl HelperDef for EvaluateHelper {
    fn call_inner<'reg: 'rc, 'rc>(
        &self,
        h: &Helper<'rc>,
        reg: &'reg Handlebars<'reg>,
        ctx: &'rc Context,
        rc: &mut RenderContext<'reg, 'rc>,
    ) -> Result<ScopedJson<'rc>, RenderError> {
        let Some(template_param) = h.param(0) else {
            return Err(RenderErrorReason::MissingVariable("template string".into()).into());
        };
        let template_str = template_param
            .value()
            .as_str()
            .ok_or(RenderErrorReason::InvalidParamType("Expected a string"))?;

        // Compile and render the dynamic string as a template
        let compiled = reg.compile_template(template_str)?;
        let mut output = StringOutput::new();
        compiled.render(ctx, rc, &mut output)?;

        Ok(ScopedJson::Derived(Value::String(output.into_string()?)))
    }
}