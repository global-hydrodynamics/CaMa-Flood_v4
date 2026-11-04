# Gosh Style Guidelines — Working Draft

**Status:** Recommended practices for discussion and gradual refinement. These are not finalized requirements. `mek01-default_run.sh` is a reference for settings and Namelist explanations; each workflow should also document its own execution and reproducibility design.

The main goals are correct model execution, scientific validity, and reproducibility. Readability supports those goals, but formatting consistency alone is not a reason to change a working script. Follow the repository-wide rules in the root [`AGENTS.md`](../AGENTS.md); this draft records observations specific to `gosh_sample/`.

## Applicability

- Apply the readability principles below by default to new v5 scripts and scripts that are substantially revised under `gosh_sample/`.
- Do not impose them retroactively on historical `gosh/` scripts or untouched legacy examples.
- Keep execution/scientific behavior separate from comment and layout improvements. Preserve settings, command order, filenames, and interfaces unless a task explicitly approves a behavior change.
- Do not use “minimal diff” as a reason to leave important experiment settings or generated Namelists unexplained. Improve comments where they help users understand and safely change a run, without turning this draft into a rigid formatter specification.

## User settings

- After any minimal script-location/repository-root discovery needed to form paths, keep experiment settings together near the beginning and before internal paths or execution logic. Mark where user settings end. This lets users find controls without reading through processing steps.
- Group related settings and use names that describe their role, such as `MAPDIR`, `NEXTXY`, `RIVWTH`, and `RUNOFF_DIR`. Clear names make it easier to see which dataset or model input a setting controls.
- Add concise inline comments to user-editable settings. Explain purpose, units, Boolean meanings, allowed or representative values, special values, and dependencies where useful. Do not merely translate the variable name or explain only what the current value happens to do.
- Group related settings and leave a blank line between different groups. Align comments where that improves scanning, but do not add spacing mechanically.
- Keep scientific data-product choices visible. Put alternatives next to each other as commented assignments, label the default, and ensure the selected path is the one written to the Namelist. This lets a researcher compare choices without editing execution logic.
- Keep map, runoff, and output paths easy to distinguish. State any working-directory requirement and explain whether paths are relative to the repository, script, or model run directory.

These practices were adopted in `mek01-default_run.sh` because the original settings mixed choices and explanations with implementation details. Grouping the settings and making map alternatives explicit helps users inspect or change an experiment with less risk of selecting the wrong file.

## Namelist and experiment record

- Add short Fortran `!` comments to generated Namelist assignments so the saved `input_cmf.nam` explains the run without requiring the shell script to be open.
- Describe what an option controls, including units or Boolean meanings where known. Keep comments independent of the current setting when the user may change that setting.
- Preserve all existing Namelist variables, values, and Shell expansions unless a task explicitly authorizes a behavior change. Check explanations against `src_main/`; do not guess about unfamiliar settings.
- For a new or substantially revised v5 script, add a short Fortran `!` comment to every generated Namelist assignment unless its meaning cannot be established. Explain units, file roles, Boolean behavior, timing, and special values where relevant; comments should remain useful if users change the value.
- Keep the generated Namelist's sections, assignments, and comments readable and reasonably aligned. The year-specific Namelist saved by a multi-year script should contain the same explanations as the active Namelist.

The generated Namelist is part of the experiment record. Inline comments make archived runs easier to understand and compare while keeping the actual model configuration unchanged.

## Execution, logging, and errors

- State how and from where to run the script. Select a run directory deliberately so the executable copy, Namelist, logs, restart files, and outputs have predictable locations.
- Choose rerun behavior according to the script's purpose. A bounded verification or sample script may automatically replace its own known generated outputs to make repeated checks convenient. A general experiment script should normally protect prior results or use a new run directory. In either case, avoid deleting the whole run directory; limit cleanup to files the script owns and make the affected names or patterns clear.
- Keep the major stages visible: configure, prepare the run directory, write the Namelist, execute the model, and report results. Use concise progress messages when they help a user identify where execution stopped.
- Separate user settings from internal paths and processing with the established three-line separator. Use accurate `[STEP n/N]`, `[SUCCESS]`, and `[ERROR]` messages for the major stages; in annual loops, identify the year being configured and run.
- Report useful paths and, where relevant, start time, end time, and elapsed model wall-clock time. Time only the model process when reporting model runtime; keep setup time separate.
- Run years in order and stop if a model invocation fails. Report its status when practical, but do not duplicate detailed input or data validation performed by the Fortran program.

These practices were added to make failures and completed runs easier to diagnose and reproduce. Keep logging proportional to the workflow and avoid messages that obscure model output.

## Simple sample execution

Gosh scripts are human-readable experiment configurations, not validation frameworks. Prioritize clear experimental settings, required paths and annual Namelists, then direct sequential model execution. Avoid checks, helper functions, input hashing, completion markers, resume logic, persistent status tracking, and recovery machinery unless they serve a concrete scientific or execution requirement.

For small, reproducible sample tests, follow the legacy `gosh/` approach: remove the previous run's known generated outputs once before a fresh simulation, using explicit filename patterns restricted to that sample's run directory. Do not delete the run directory or input data. Keep restart continuation when it is part of the test, and stop if a model invocation fails.

Do not duplicate input, NetCDF structure, output, or numerical validation already performed by the Fortran model. This recommendation is for short repeatable samples; long production runs and HPC workflows may need restart management, result protection, or recovery. Choose complexity according to the experiment.

## AI completion review

When creating or substantially revising a v5 script, Codex should review the result against this file before handoff. Confirm that settings and scientific choices are understandable, units/options/special values are explained where verified, generated Namelists are self-describing, workflow stages are clear, and execution behavior is unchanged unless explicitly approved. This review supplements, and does not replace, maintainer review.

## Flexibility and future revision

HPC systems, coupled execution, schedulers, and local development can require different launch commands, path conventions, logging, or error handling. Adapt the script to its actual execution environment rather than forcing a single layout. Preserve scientific behavior and required workflow semantics over stylistic uniformity.

As more `gosh_sample/` examples are completed, record useful lessons here. Review the recommendations against multiple working scripts before promoting them to a stable style guide; do not retrofit existing scripts solely to make them look alike.
