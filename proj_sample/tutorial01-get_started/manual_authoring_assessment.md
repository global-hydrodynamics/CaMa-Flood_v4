# Tutorial01 Manual-Authoring Experiment: Assessment

This note records what the first AI-assisted manual-authoring run demonstrated. It complements the original, preserved [`manual_guidelines.md`](manual_guidelines.md); it does not replace or silently revise that specification.

## What the agent could document directly

- Repository paths, scripts, settings, compiler flags, and input/output contracts were read from tracked files (`s01-setup_fortran.sh`, `s02-python_setup.sh`, `src_main/Makefile`, `mek01-default_run.sh`, map metadata, the input-dimension file, and the variable registry).
- The setup, clean build, 40-day simulation, output listing, generated CTL, model log, and reset were executed or inspected in a disposable clone of `feature/gosh-refactor`.
- Output file sizes, record count, and finite-value checks were measured from the actual generated Binary files. The tutorial distinguishes these operational checks from scientific validation.
- The reset script was run after the simulation. It removed the tutorial output and build executable while preserving `config/Mkinclude`; its checksum was identical before and after.

## What required interpretation

- The manual explains that the exclusive end date and daily output averaging produce 40 records labeled from January 2 through February 10. This interpretation was cross-checked against the generated CTL time axis and model log, rather than inferred from the settings alone.
- The roles of river map, runoff grid, input matrix, Restart, and output variables required joining information from scripts, metadata, and source documentation.
- The Python setup failed to install optional requirements because the package index could not be reached. The actual Tutorial01 run still succeeded with Python 3.15.2 because its CTL utilities use the standard library. This scope difference needs to be explicit in future setup instructions.

## Difficulties and ambiguities

- The Fortran program writes progress to `log_CaMa.txt`; `run.stdout.log` was empty. A manual author must distinguish wrapper stdout, captured stderr, and the model's own log instead of treating one console transcript as complete evidence.
- Setup output varies by machine and prompts for confirmation. The test environment had a pre-existing staging `Mkinclude`; it was preserved rather than overwritten. A fresh user's command path must still be explained separately.
- The current repository is a development branch, not an assembled v5 download package. A successful branch checkout must not be described as release-package validation.
- Large compile and runtime output required selective excerpts. The manual states where output was abbreviated and keeps the source log files available during authoring, rather than copying full logs into the tutorial.

## Suggested guideline improvements

Keep the original guide unchanged for comparison. For the next revision, consider adding:

1. An evidence ledger mapping each displayed command/output excerpt and each factual claim to its source file, captured log, or measured artifact.
2. A required distinction among shell stdout/stderr, application log files, and tool-generated metadata; document which stream is authoritative for each step.
3. A dependency-scope table separating packages required to run this tutorial from optional repository-wide Python tools, including what to do when package installation is offline.
4. A required statement of repository revision and distribution status, so branch validation is not conflated with release-package validation.
5. A consistent fresh-user versus developer-reset walkthrough, and a recorded before/after check for files the reset must preserve.
6. A short reproducibility record for environment versions, commands, data identity, and output checks. Avoid building an automatic documentation framework until more tutorials reveal repeated needs.

## Repeating the process for Tutorial02 or Tutorial03

Each tutorial needs its own approved experiment specification, source and data-contract review, scripts and exact supported inputs, a disposable clean checkout, actual build and run evidence, output-specific verification, and maintainer review of scientific interpretation. Tutorial02 additionally needs its multi-year Restart and NetCDF workflow, comparisons, and figures to be regenerated or verified from the chosen revision. Tutorial03 needs its own learning objective and experiment design before implementation; Tutorial01's settings should not be copied as scientific defaults without review.

## Knowledge-gap review

The milestone review found no new scientific-assumption question that blocks Tutorial01. Remaining process limitations are documented above: Python dependency installation was network-limited, and a release-package workflow is not available. These do not invalidate the tested branch-based Tutorial01 run. The task does not claim scientific validation or portability beyond the tested macOS environment.
