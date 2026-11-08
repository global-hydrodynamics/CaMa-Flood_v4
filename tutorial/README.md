# Specification-Driven Tutorial and Manual Development

This directory is the canonical home for CaMa-Flood v5 tutorials, their specifications, executable workflows, and learner-facing manuals. The framework lets domain experts describe the intended learning and experiment in text, while AI coding agents implement, execute, verify, and document the work. Human experts retain final scientific and educational approval.

## Four document types

| Document | Scope | Defines |
| --- | --- | --- |
| `tutorial_design.md` | One tutorial | **What users execute and accomplish:** objectives, prerequisites, inputs, workflow, outputs, and objective success criteria. It does not prescribe manual structure or style. |
| `tutorial_development_guidelines.md` | Shared | **How agents develop and verify executable tutorials:** repository inspection, reuse, scientific correctness, paths, configuration, reproducibility, execution, safety, and evidence. |
| `manual_design.md` | One tutorial | **What its manual teaches and how content is organized:** audience, prior knowledge, learning objectives, explanations, examples, figures, and educational completion criteria. It does not duplicate technical experiment settings. |
| `manual_authoring_guidelines.md` | Shared | **How agents investigate, write, and verify manuals:** evidence, commands and output, explanations, figures, Markdown, uncertainty, consistency, and maintenance. |

Tutorial specifications are analogous to source code; shared guidelines are analogous to build rules and compilation settings; AI agents perform implementation, execution, documentation, and verification; tutorials and manuals are resulting artifacts. This is a conceptual analogy only: agents are not deterministic compilers, so execution checks and human review remain necessary.

## Iterative workflow

1. A domain expert discusses the intended learning experience with a conversational AI.
2. Together they prepare or refine the tutorial and manual design specifications.
3. A coding agent inspects the repository, source, data, and existing tools, then reports conflicts and unresolved decisions.
4. The expert confirms scientific and educational choices before implementation where needed.
5. The coding agent implements the tutorial using the shared development guidelines, executes it, and verifies the results.
6. The agent writes or updates the manual using actual files and execution evidence.
7. The expert reviews the tutorial and manual, then revises the relevant specifications through discussion.
8. The agent updates only the affected implementation or manual and repeats relevant checks.

The main iteration should improve specifications and regenerate artifacts. Fix an implementation defect in implementation code; not every discrepancy requires a specification change. Do not repeatedly edit generated manuals while leaving their intended teaching design stale.

## Learning progression

Tutorial01 introduces core skills with detailed explanations. Later tutorials may assume concepts already taught, shorten repeated material, and link to earlier tutorials. Explain newly introduced scientific and technical concepts in depth. Keep tutorials independently executable when practical, even when their manuals build on prior learning.

## Responsibilities

- **Domain experts** make scientific, experiment, and educational decisions.
- **Conversational AI** helps refine specifications and surface questions.
- **Coding agents** implement, execute, collect evidence, and document within the approved scope.
- **GitHub** provides version control, collaboration, review, and repository distribution.

Human experts give final scientific and educational approval.

## Precedence and change management

Shared guidelines define default conventions. Each tutorial's design specifications define its tutorial-specific requirements and may refine shared defaults, but must not contradict mandatory safety or correctness requirements. If requirements genuinely conflict, stop the affected work and report the conflict rather than silently choosing one.

Repository files and verified execution results are evidence of actual behavior; they do not automatically override the intended scientific requirements. When implementation differs from design, investigate the discrepancy and correct the appropriate source. Review specification and artifact changes together with Git diffs, and keep unrelated changes out of regeneration work.

## Current tutorials

### Tutorial01 — Get started

[`tutorial01-get_started/`](tutorial01-get_started/) contains the Tutorial Design, Manual Design, runnable wrapper, reset utility, tested manual, and first manual-authoring assessment. Its original experiment-design and authoring-guide texts are retained under `design_history/` for comparison. Generated files go to its ignored `out/` directory.

### Tutorial02 — Mekong NetCDF experiments

[`tutorial02-mekong_netcdf_4exp/tutorial_design.md`](tutorial02-mekong_netcdf_4exp/tutorial_design.md) records the approved four-experiment design and execution evidence. The scripts, visualizations, and reference figures are present. A manual design and beginner-facing manual have not yet been authored; do not create placeholders or infer their educational scope.

Other material under `proj_sample/` is not moved merely because it is sample or tutorial-like. In particular, `proj_sample/test-mek01/` remains separate historical assignment material.
