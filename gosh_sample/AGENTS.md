# Gosh Style Guidelines — Working Draft

**Status:** Recommended practices for discussion and gradual refinement. These are not finalized requirements. `mek01-default_run.sh` is the current reference example, not a template that every workflow must copy exactly.

The main goals are correct model execution, scientific validity, and reproducibility. Readability supports those goals, but formatting consistency alone is not a reason to change a working script. Follow the repository-wide rules in the root [`AGENTS.md`](../AGENTS.md); this draft records observations specific to `gosh_sample/`.

## User settings

- Keep experiment settings together near the beginning of the script, before path setup and execution logic. Mark where user settings end. This lets users find the controls without reading through the processing steps.
- Group related settings and use names that describe their role, such as `MAPDIR`, `NEXTXY`, `RIVWTH`, and `RUNOFF_DIR`. Clear names make it easier to see which dataset or model input a setting controls.
- Add concise inline comments to editable settings. Explain units, conventions, or available choices rather than merely restating the current value. For Boolean options, describe `TRUE` and `FALSE` where useful. For frequencies and special numeric values, give verified examples and meanings.
- Keep map input selection visible. When users can choose between products, show the alternatives beside each other and identify the intended default. For example, `NEXTXY` selects river connectivity and `RIVWTH` selects channel width. Avoid hiding a scientific data choice inside execution logic.
- Keep map, runoff, and output paths easy to distinguish. State any working-directory requirement and explain whether paths are relative to the repository, script, or model run directory.

These practices were adopted in `mek01-default_run.sh` because the original settings mixed choices and explanations with implementation details. Grouping the settings and making map alternatives explicit helps users inspect or change an experiment with less risk of selecting the wrong file.

## Namelist and experiment record

- Add short Fortran `!` comments to generated Namelist assignments so the saved `input_cmf.nam` explains the run without requiring the shell script to be open.
- Describe what an option controls, including units or Boolean meanings where known. Keep comments independent of the current setting when the user may change that setting.
- Preserve all existing Namelist variables, values, and Shell expansions unless a task explicitly authorizes a behavior change. Check explanations against `src_main/`; do not guess about unfamiliar settings.

The generated Namelist is part of the experiment record. Inline comments make archived runs easier to understand and compare while keeping the actual model configuration unchanged.

## Execution, logging, and errors

- State how and from where to run the script. Select a run directory deliberately so the executable copy, Namelist, logs, restart files, and outputs have predictable locations.
- Choose rerun behavior according to the script's purpose. A bounded verification or sample script may automatically replace its own known generated outputs to make repeated checks convenient. A general experiment script should normally protect prior results or use a new run directory. In either case, avoid deleting the whole run directory; limit cleanup to files the script owns and make the affected names or patterns clear.
- Keep the major stages visible: configure, prepare the run directory, write the Namelist, execute the model, and report results. Use concise progress messages when they help a user identify where execution stopped.
- Report useful paths and, where relevant, start time, end time, and elapsed model wall-clock time. Time only the model process when reporting model runtime; keep setup time separate.
- Stop on failures and provide a useful error summary, including the current stage and original exit status when practical. Do not turn shell orchestration into a large validation framework; prefer substantive input checks and detailed diagnostics in the program that reads the data.

These practices were added to make failures and completed runs easier to diagnose and reproduce. Keep logging proportional to the workflow and avoid messages that obscure model output.

## Flexibility and future revision

HPC systems, coupled execution, schedulers, and local development can require different launch commands, path conventions, logging, or error handling. Adapt the script to its actual execution environment rather than forcing a single layout. Preserve scientific behavior and required workflow semantics over stylistic uniformity.

As more `gosh_sample/` examples are completed, record useful lessons here. Review the recommendations against multiple working scripts before promoting them to a stable style guide; do not retrofit existing scripts solely to make them look alike.
