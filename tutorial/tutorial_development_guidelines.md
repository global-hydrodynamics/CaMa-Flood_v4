# Tutorial Development Guidelines

These shared guidelines apply to all executable tutorials under `tutorial/`. Tutorial-specific requirements belong in that tutorial's `tutorial_design.md`.

These are evolving defaults rather than fixed standards. Refine a shared rule when practical experience shows that it applies to multiple tutorials; keep requirements that are unique to one experiment in its design specification.

## Repository inspection and reuse

- Inspect the repository, current branch and worktree, authoritative status, relevant source, metadata, data, scripts, and existing documentation before editing.
- Reuse existing model scripts, analysis programs, and utilities when they satisfy the design. Avoid duplicated scientific algorithms, configurations, or general-purpose frameworks without a concrete need.
- Follow repository paths and naming conventions. Keep runtime outputs and build products isolated in tutorial-specific ignored directories. Track intentional, reviewed tutorial deliverables such as reference figures only when the design calls for them; do not confuse these with incidental run output.
- Preserve the approved persistent branch and scope. Follow the root [`AGENTS.md`](../AGENTS.md) for authorization, coordination, research records, and commit/push rules.

## Specification and scientific correctness

- Treat `tutorial_design.md` as the tutorial-specific statement of intended user actions and success criteria. Treat source and data as evidence of current behavior, not automatic authorization to change the intended science.
- Compare design against source interfaces, namelists, data contracts, and current workflows. Report conflicts, unsupported settings, and unresolved scientific questions instead of silently guessing.
- Do not change scientific model behavior, numerical settings, file formats, or compatibility contracts without the required maintainer approval.
- Keep experiment-specific requirements in the tutorial specification; do not copy shared process rules into every tutorial.

## Reproducibility and configuration

- State prerequisites, dependencies, input data, working-directory requirements, and configuration steps.
- Provide executable commands or scripts and define objective expected outputs and success criteria.
- Reuse the same experiment configuration when possible instead of maintaining duplicate settings in wrappers and manuals.
- Support repeatable initial conditions. Add a reset procedure when it is necessary for a clean run or reproducible evidence collection.
- Keep each tutorial's generated output separate and ignored by Git. Do not commit model outputs, temporary build products, or local machine configuration unless an approved design requires them.

## Execution and verification

- Execute the documented workflow when the required environment and inputs are available.
- Check exit status, expected files, dimensions/record counts, and relevant metadata or numerical contracts according to the risk of the change.
- Record successful and failed steps and identify which checks were not run and why. Never claim an unexecuted command or test passed.
- Distinguish operational execution from scientific validation. Do not invent tolerances or infer scientific correctness from a successful run.
- If an external dependency or environment limitation blocks part of the workflow, complete independent checks and report the concrete limitation.

## Safety and compatibility

- Preserve user configuration, source data, and unrelated outputs.
- Limit cleanup to explicitly authorized tutorial-generated paths. Do not use broad cleanup or overwrite unrelated experiments.
- Preserve backward compatibility and existing workflows unless a separately approved change requires otherwise.
- Keep scripts understandable to their intended users; do not build validation frameworks or a universal logging layer for simple tutorials.

## Evidence interface for manuals

- Keep useful execution logs, generated metadata, and verification evidence available while authoring the manual.
- Prefer reading facts from actual scripts, source, data metadata, and outputs over manually maintained duplicate descriptions.
- Identify which stream or file contains the application's authoritative log; distinguish it from wrapper stdout and stderr.
- Capture representative commands and real output, but do not require a universal logging or documentation-generation framework at this stage.
