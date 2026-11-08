# Tutorial01 Development Materials

This directory contains materials for maintaining and documenting Tutorial01; learners do not need them for the normal workflow.

- [`manual_design.md`](manual_design.md): audience, learning outcomes, and manual sequence.
- [`manual_generation_log.md`](manual_generation_log.md): developer-only record of tested revision, environment, command evidence, and limitations behind the learner manual.
- [`tutorial01-reset.sh`](tutorial01-reset.sh): optional developer reset for a disposable clean build/run demonstration. It requires confirmation unless `--yes` is supplied, cleans `src_main/` build products, and removes only the selected `out/PROJECT/` directory (default `tutorial01-get_started`), preserving sibling projects, the output parent, `config/Mkinclude`, and bundled inputs.
- [`manual_authoring_assessment.md`](manual_authoring_assessment.md): lessons from the first AI-assisted manual-authoring experiment.
- [`design_history/`](design_history/README.md): original specifications preserved for comparison.
