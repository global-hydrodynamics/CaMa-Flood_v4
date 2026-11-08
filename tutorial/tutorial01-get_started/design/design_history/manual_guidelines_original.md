# Manual Authoring Guidelines: tutorial01-get_started

**CaMa-Flood v5 — Instructions for Codex**

Status: Revised draft  
Source: `experiment_design.md`  
Target: `tutorial01-get_started.md`

## 1. Objective

Codex must create a beginner-friendly Markdown tutorial by inspecting the experiment design, actual repository files, and execution results.

The tutorial must document the real workflow from environment setup to successful simulation.

Do not produce a generic or hypothetical installation manual.

The goal is to create documentation grounded in actual commands, actual terminal output, and actual generated files.

## 2. Source of Truth

Use the following sources, in priority order:

1. Experiment design: `experiment_design.md`.
2. Manual authoring guidelines: this document.
3. Actual repository scripts, Makefiles, configuration files, and metadata.
4. Terminal output from tested commands.
5. Generated files and verification results.

If the experiment design and the actual implementation are inconsistent, investigate and resolve the discrepancy before finalizing the manual.

Do not invent information to fill gaps.

## 3. Required Workflow for Codex

### Phase A — Inspect

Read the experiment design and these guidelines.

Inspect:

- Repository directory structure.
- Root `s01` and `s02` setup procedures.
- `config/Mkinclude` handling.
- `src_main/Makefile`.
- Existing Mekong sample script.
- Tutorial run/reset scripts.
- Map metadata and runoff registry.
- Output variable registry.

Identify the exact commands needed for the tutorial.

### Phase B — Prepare a clean state

Ensure the required Fortran configuration is available.

Run:

```bash
bash proj_sample/tutorial01-get_started/tutorial01-reset.sh
```

The reset script must preserve `config/Mkinclude`.

Before executing the reset script, confirm that the current working copy is suitable for cleanup and that removing its tutorial outputs is acceptable.

### Phase C — Capture the initial state

Capture actual terminal output showing:

- Repository root contents.
- `src_main/` contents before compilation.
- Absence of the main executable.
- Absence of the tutorial `out/` directory.
- Presence of `config/Mkinclude`.
- Presence of sample map and runoff datasets.

Record the commands along with their standard output and standard error.

### Phase D — Compile

Follow the actual compilation procedure from the repository.

Capture:

- Commands used.
- Representative compilation messages.
- Command exit status.
- Executable filename and attributes after compilation.

If compilation fails, investigate and resolve the problem before documenting the workflow as successful.

### Phase E — Run Tutorial01

Execute:

```bash
bash proj_sample/tutorial01-get_started/tutorial01-get_started.sh
```

Capture:

- Run command.
- Representative simulation messages.
- Completion status.
- Output directory listing.
- Expected output file verification.

Use the actual simulation period, settings, and output variable names.

### Phase F — Write the manual

Create:

```text
proj_sample/tutorial01-get_started/tutorial01-get_started.md
```

Use the required chapter structure:

1. Introduction
2. Download CaMa-Flood
3. Explore the package
4. Set up Fortran and Python
5. Compile the model
6. Inspect the sample input data
7. Understand the simulation settings
8. Run Tutorial01
9. Verify output files
10. Summary and next steps

Each major step must explain:

**Purpose → Command → Expected terminal output → Explanation → Checkpoint**

Use fenced code blocks for terminal commands and terminal output.

Separate commands from their results.

### Phase G — Verify the completed manual

Check that:

- Every command is correct.
- All displayed paths match the current repository.
- File listings were captured at the correct execution stage.
- Displayed compiler and simulation output comes from actual execution.
- `config/Mkinclude` was preserved.
- Output filenames and units are correct.
- The manual is understandable to a first-time user.
- Markdown formatting is valid.

Revise the manual if any check fails.

## 4. Rules for Terminal Output

Use real terminal output whenever possible.

Include short examples of:

- `pwd` and `ls` after obtaining the package.
- `ls` before compilation.
- The missing-executable message before compilation.
- Representative compiler output.
- `ls` after compilation.
- The missing-output-directory message before simulation.
- Representative model execution output.
- `ls` of generated results after simulation.

Long output may be shortened, but omissions must be clearly indicated.

Do not present fabricated terminal output as an observed result.

Environment-specific paths, timestamps, file sizes, and compiler versions may vary.

Explain which parts of the example output users should compare with their own results.

## 5. Initial State and Reset

The manual must distinguish between two situations.

**New user**

Starts from a freshly downloaded package and follows setup, compilation, and simulation instructions.

A new user should not be required to execute the reset script.

**Developer or documentation author**

May have compiled and executed the tutorial previously.

Uses `tutorial01-reset.sh` to restore the clean build/output state before capturing manual examples.

The reset script is a development and testing utility, not a mandatory installation step.

It must never remove `config/Mkinclude`.

## 6. Manual Writing Principles

Use clear and concise English.

Explain why each step is necessary, not just which command to type.

Prefer actual repository filenames, paths, and configuration values.

Explain basic concepts briefly:

- Source code.
- Compiler.
- Executable.
- River map.
- Runoff forcing.
- Simulation period.
- Output variable.

Avoid advanced scientific explanations or visualization.

Do not require users to download or prepare map or runoff datasets.

Do not overwhelm readers with full compilation logs or hundreds of filenames.

Use short, meaningful examples and explain the key lines.

## 7. Automation Policy

Codex is responsible for the entire manual-authoring process.

The human developer should not need to manually transcribe terminal outputs, file listings, or simulation settings.

Codex may create small helper scripts to capture and format information when useful.

However:

- Keep the implementation simple.
- Preserve raw logs or other evidence supporting the manual.
- Do not introduce a large documentation-generation framework.
- Do not duplicate simulation settings.
- Do not alter numerical algorithms merely to simplify documentation.
- Keep generated files outside version control unless they are intended documentation artifacts.

Automated extraction should be preferred for technical information.

Explanatory prose may be written by Codex using the experiment design and inspected implementation.

## 8. Completion Report

After generating the manual, Codex should report:

- Files created or modified.
- Commands executed.
- Compilation result.
- Simulation result.
- Manual verification result.
- Any steps that could not be tested.
- Any differences between the experiment design and the actual implementation.

The manual must clearly distinguish verified results from unverified guidance.

The work is complete when Tutorial01 can be followed from a fresh package and the documented outputs are consistent with a tested run.