# Manual Design — Tutorial01 Get Started

**Audience:** first-time CaMa-Flood users who can follow terminal commands but are not expected to know Fortran build systems, river maps, runoff forcing, or the repository's data layout.

The experiment contract and acceptance criteria are in [`tutorial_design.md`](tutorial_design.md). General evidence, writing, and verification practices are in [`../manual_authoring_guidelines.md`](../manual_authoring_guidelines.md).

## Learning objectives

After reading the manual, a learner should be able to:

- Obtain the v5 development repository and distinguish it from a separately assembled distribution package.
- Locate model source, build configuration, the Mekong river map, runoff forcing, and runoff-to-map input matrix.
- Prepare the Fortran configuration and understand the role of the Python setup script.
- Compile and run Tutorial01 from the repository root.
- Identify the main output files, the model log, and Restart products, and recognize the normal completion signals.
- Explain why a successful run and file checks do not establish scientific validation.

## Prior knowledge for later tutorials

Later introductory tutorials may assume the learner can use a terminal, activate `config/Mkinclude`, build `src_main/MAIN_cmf`, distinguish map data from runoff forcing and its input matrix, run a sample script, and locate model outputs and logs. They should briefly reference these skills rather than reteach the full first-run workflow.

## Instructional sequence

Organize the manual in this order:

1. **Introduction** — explain the first build/run goal and its limits.
2. **Download CaMa-Flood** — show the tested GitHub development-checkout route and explain the package-validation distinction.
3. **Explore the repository** — locate source, map, runoff, setup scripts, and tutorial files.
4. **Prepare Fortran and Python** — explain `s01`, activation of `config/Mkinclude`, and the role and dependency scope of `s02`.
5. **Compile the model** — show the absent executable before compilation and its presence after a successful build.
6. **Inspect the sample inputs** — explain the map, runoff grid, input matrix, and daily forcing filenames.
7. **Understand the experiment** — summarize the 40-day dates, model options, output variables, and tutorial wrapper overrides.
8. **Run Tutorial01** — show the command and representative successful completion output.
9. **Inspect the results** — show the tutorial `out/` directory before and after the run; explain the output records, CTLs, model log, and Restart.
10. **Summary and next steps** — recap what the learner can now do and describe reset as developer/documentation tooling.

## Explanations to emphasize

- Define source code, compiler, executable, river map, runoff forcing, input matrix, simulation period, model output, and Restart when first introduced.
- Explain that runoff is supplied on a coarser grid and the input matrix maps it to the river-map grid.
- Explain the exclusive end time and why daily mean output timestamps mark completed intervals.
- Explain the difference between wrapper output and `log_CaMa.txt`, which contains model progress.
- Explain that reset is for developers or documentation authors, is not part of normal first-time setup, and preserves `config/Mkinclude`.
- Avoid advanced scheme comparisons, scientific conclusions, and visualization; those belong in separately designed tutorials.

The manual should remain a beginner-facing guide, not a copy of the experiment specification or a general authoring checklist. Include the specified before/after build and simulation states, selecting excerpts that help a beginner recognize each transition.
