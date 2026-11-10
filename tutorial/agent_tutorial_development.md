# Agent Instructions: Developing Executable Tutorials

These evolving instructions implement the framework principles in the [tutorial README](README.md); the human-facing process is in [How to develop tutorials](how_to_develop_tutorials.md). Tutorial-specific experiment and learning requirements belong in that tutorial's specifications. Agents investigate and fill minor implementation gaps from evidence, while scientific decisions remain with the human domain expert. For repository authorization, branch coordination, research records, and commit rules, follow the root [`AGENTS.md`](../AGENTS.md).

## Inspect and plan

- Read the tutorial's `tutorial_design.md` and inspect relevant scripts, model interfaces, bundled data, metadata, and existing tools before editing.
- Treat a tutorial specification as an executable contract: include inputs, configuration, working directories, outputs, and concrete completion checks. Keep tutorial-specific paths, experiment names, and dates in that tutorial's specification.
- Make each learner activity explicit: name what the learner inspects or changes, the starting file, the essential artifact absent at the beginning, the artifact they create, the transferable skill, and observable evidence of completion. Summarize a tutorial's activities in its single row in [`tutorial_requirements.csv`](tutorial_requirements.csv); use separate cells for R1–R8 compliance, Tutorial Design, Manual Design, package, and remaining-issue checks. Distinguish documented intent from verified implementation.
- Treat the design specification as the intended user workflow and acceptance contract. Source and data show current behavior; they do not authorize scientific changes.
- Report conflicts, missing inputs, and unresolved scientific decisions instead of guessing.
- Reuse maintained tools and workflows where they fit. Avoid duplicate configurations, scientific algorithms, and general-purpose frameworks without a concrete need.

## Implement the tutorial

- In beginner-tutorial designs and manuals, use `make all` from `src_main/` as the standard model build/update command. Do not assume the executable is absent, and do not change the Makefile's default target to make the tutorial command work.
- For multiple experiments, keep generated files in project-specific subdirectories such as `out/${PROJ}/`. A rerun may replace that project's documented products but must preserve sibling projects and their outputs.
- Preserve scientific settings, model behavior, file formats, and established interfaces unless an approved design explicitly requires a change.
- Make prerequisites, inputs, working directories, settings, expected outputs, and success criteria clear and reproducible.
- Keep runtime outputs and build products in tutorial-specific ignored locations. Track only intentional deliverables called for by the design, such as reviewed reference figures.
- Scripts should be understandable to their intended learners. Do not turn a small tutorial into a validation framework or duplicate checks already performed by the model.
- Normally reuse an existing maintained GOSH configuration rather than maintaining a second experiment configuration. **Educational exception:** when learning to read and edit GOSH is a central objective, a tutorial may include a standalone, self-contained GOSH script with its settings visible. Keep it scientifically equivalent to the maintained sample, explain the reason for the copy, and avoid copying scientific algorithms or adding an execution framework.
- Add a reset procedure only when needed for reproducible initial state or evidence collection. Limit cleanup to explicit generated paths and preserve user configuration and inputs.
- Keep complete runnable references, developer reset scripts, and developer evidence in each tutorial's `design/` directory. Keep it out of learner packages while retaining it in the GitHub development repository. Put approved expected-result images in `sample_figure/`; learner-generated figures belong in `figure/`, which starts without generated figures in the distributed package. Never include completed learner-generated maps, alternative GOSH solutions, or model outputs that remove the required learning activity.

## Execute and verify

- Run the documented workflow when the required environment and data are available. Use a disposable checkout when validation requires destructive cleanup or could overwrite valuable results.
- Verify project isolation by placing an unrelated sentinel output beside the selected project, exercising the run/reset in a disposable checkout, and confirming that the sibling output and shared inputs/configuration survive.
- Check exit status and tutorial-specific contracts such as output files, dimensions, records, metadata, and paths in proportion to risk.
- Distinguish operational success from scientific validation. Do not invent tolerances or claim scientific correctness from a successful run.
- Record commands, environment, evidence, limitations, and failed or unavailable checks. Never report an unexecuted check as passed.
- Keep useful program logs and generated metadata available while authoring the manual; identify which logs are authoritative.

## Evolving guidance

These are working defaults, not finalized standards. Improve them when repeated experience across tutorials shows a reusable need. Keep one-off experiment requirements in the tutorial specification and avoid overfitting shared rules to Tutorial01.

Before distribution, inspect the actual package process. If no tutorial assembler exists, do not treat the v4 `doc/make_package.sh` as one; prepare and inspect a temporary learner snapshot and report that the official packaging check remains partial. Follow the three review gates in [`how_to_develop_tutorials.md`](how_to_develop_tutorials.md).

Review the tutorial registry when a tutorial is proposed, either design specification is completed or revised, implementation or learner-package contents change, and before a tutorial is marked ready to distribute. Use only `PASS`, `PARTIAL`, `FAIL`, or `NOT_REVIEWED`; do not report a planned package or learner run as verified.
