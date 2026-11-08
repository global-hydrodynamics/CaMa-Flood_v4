# Manual Design — Tutorial01: Get Started

**Audience:** first-time CaMa-Flood users who can follow terminal commands but may not know Fortran build systems, river maps, runoff forcing, or the repository's data layout.

The experiment contract is [`../tutorial_design.md`](../tutorial_design.md). Shared evidence and writing instructions are in [`../../agent_manual_authoring.md`](../../agent_manual_authoring.md).

## Learning outcomes

After reading the manual, learners should be able to:

- Understand the basic CaMa-Flood repository layout, confirm its root with `pwd`, and use `${cmf_root}` as the absolute root path for later sections.
- Explain the relationship between Fortran source, the compiled executable, and a GOSH experiment script.
- Prepare the required Fortran configuration without creating the existing `config/` directory, and understand the role of the Python setup procedure.
- Compile or update the model from `src_main/` with `make all`, whether an executable is absent or already present.
- Identify the selected Mekong river map and understand the `MAPDIR`, `NEXTXY`, and `RIVWTH` choices.
- Identify the runoff dataset, its filenames, interval, and units.
- Explain why a runoff-to-map input matrix is needed and locate its dimension descriptor and matrix file.
- Read the main settings in a standalone GOSH script and connect them to repository files.
- Run the model and locate its outputs, logs, CTLs, and Restart files.
- Distinguish successful execution and file checks from scientific validation.

Later tutorials may assume these basic repository and execution concepts; they should explain their own new technical content.

## Instructional sequence

1. Introduce the build-and-run goal and state that it is not scientific validation.
2. Obtain the tested source route, inspect the repository root with `pwd` and `ls -l`, and set `cmf_root="$(pwd)"` in the same terminal session.
3. Explain the main root directories without listing every file.
4. Prepare Fortran and Python environments from the repository root and activate `config/Mkinclude`.
5. Build or update `MAIN_cmf` from `${cmf_root}/src_main/` with `make all`; do not require that it was absent beforehand.
6. Inspect the Tutorial01 GOSH script and identify where its experiment settings are defined.
7. Inspect the river-map directory, then connect its files to `MAPDIR`, `NEXTXY`, and `RIVWTH`.
8. Inspect the runoff directory, then connect its daily filenames to `ROFFTAG`, `RUNOFF_DIR`, `CROFFREQ`, `CROFPRE`, and `CROFSUF`; introduce frequency and units afterward.
9. Inspect the input-dimension and input-matrix files, then explain `INPDIM`, `INPMAT`, and `LINTERP`.
10. Run the standalone tutorial script and inspect outputs, CTLs, logs, and Restart files.

## Teaching emphasis

- Immediately after the learner confirms the repository root with `pwd`, define `cmf_root="$(pwd)"` once and explain that it remains available only in the same terminal shell session. Use `${cmf_root}` consistently thereafter.
- At the start of every major command-oriented section, first run `cd "${cmf_root}"`, then enter the section's relevant subdirectory using a relative path. Include `pwd` before directory listings. Keep each section independently navigable and do not rely on the previous section's final working directory.
- Begin each data topic by returning to the repository root, entering the data directory relatively, and listing representative real files. Explain what those files contain before showing the GOSH settings that select them. Keep listings short.

- Define source code, compiler, executable, GOSH, river map, runoff forcing, input matrix, simulation interval, model output, and Restart when first introduced.
- Explain that the runoff grid is coarser than the model map and that the input matrix links runoff values to map cells.
- Explain the end time as exclusive and describe the daily output timestamps using captured CTL/model evidence.
- The GOSH script is standalone so learners can see the experiment settings. Explain that it is an educational copy of the maintained `gosh_sample` workflow with isolated output and one thread.
- Distinguish shell output from `log_CaMa.txt` and generated metadata.
- Do not include scientific visualization, observational comparison, or claims of physical accuracy.
- Mention the reset script only as optional developer tooling; ordinary learners do not need to run it.
- Do not assume `MAIN_cmf` or `out/` is absent before the learner starts. Explain that `make all` builds required targets and that the sample script creates `out/` and replaces its known Tutorial01-generated files on rerun.
