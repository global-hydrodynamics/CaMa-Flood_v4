# Manual Design — Tutorial01 Get Started

**Audience:** a new CaMa-Flood user who can use a terminal and follow copyable commands, but is not expected to know Fortran build systems or the repository's map/runoff conventions.

The technical run contract and success criteria are in [`tutorial_design.md`](tutorial_design.md). Shared writing and evidence conventions are in [`../manual_authoring_guidelines.md`](../manual_authoring_guidelines.md).

## Learning goals

Teach the reader how to obtain the development repository, identify the main v5 components, prepare the build, compile and run a sample, and recognize the files produced. Explain why each operation is performed. Keep the manual focused on first-run operation, not advanced scientific analysis or visualization.

## Content outline

Use these ten sections in order:

1. **Introduction** — state what the reader will build and run, and distinguish operational success from scientific validation.
2. **Download CaMa-Flood** — show the tested repository checkout route and distinguish the development branch from an assembled release package.
3. **Explore the package** — locate source, map, runoff, setup scripts, and tutorial files.
4. **Set up Fortran and Python** — explain the purpose of `s01`, activation of `config/Mkinclude`, and the optional broader-workflow Python setup.
5. **Compile the model** — show the build command, executable, and a concise real output excerpt.
6. **Inspect the sample input data** — explain map metadata, runoff grid, input matrix, and daily forcing filenames.
7. **Understand the simulation settings** — summarize the 40-day run, non-MPI/bifurcation/adaptive settings, output variables, and wrapper overrides.
8. **Run Tutorial01** — show the wrapper command and real completion/status output.
9. **Verify output files** — explain expected products, 40 daily records, CTL time axis, model log, and Restart files.
10. **Summary and next steps** — recap the workflow and describe the reset script as developer/documentation tooling, not a new-user requirement.

## Required explanation pattern

For each operational section, include the step's purpose, command, representative observed output, an explanation, and a checkpoint. Include real before/after terminal output or file listings for the clean build and simulation stages. Do not reproduce the full build or runtime log. State when output is abbreviated and point to the model log when it contains the authoritative program messages.

## Teaching emphasis

- Define source code, compiler, executable, river map, runoff forcing, input matrix, simulation period, model output, and Restart briefly at first use.
- Explain that the runoff grid is coarser than the river map and the input matrix provides their mapping.
- Explain the exclusive end time and why daily mean output records are labeled at interval end.
- Explain that the reset is only for developers/documentation authors and preserves `config/Mkinclude`.
- Avoid advanced scheme comparisons, scientific claims, or visual analysis; reserve those for separately designed tutorials.

## Completion criteria

The manual is complete when a first-time reader can follow the commands from repository root, identify the expected success signals and files, distinguish the model log from wrapper output, and understand which checks do not establish scientific validity. All shown outputs must correspond to captured execution or be clearly labeled as illustrative expectations rather than observed facts.
