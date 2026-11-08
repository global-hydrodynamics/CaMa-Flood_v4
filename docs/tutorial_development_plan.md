# CaMa-Flood v5 Tutorial Development Plan

## Status and scope

This document tracks tutorial development status and next stages. The framework's canonical organization and shared rules are in [`tutorial/README.md`](../tutorial/README.md). Tutorial 01 has been implemented and operationally checked; Tutorial 02 has a researcher-approved design, four implemented experiment scripts, completed 2000–2003 simulations, analysis scripts, and reference figures on `feature/gosh-refactor`.

The framework is intended to become the primary beginner-oriented entry point for v5 experiments when its materials are implemented and validated. Existing `gosh_sample/` workflows remain available as implementation references; this plan does not remove or reorganize them.

## Framework documents

The framework separates tutorial-specific intent from shared agent procedures:

- [`tutorial/README.md`](../tutorial/README.md) explains the specification-driven workflow, roles, learning progression, and precedence.
- [`tutorial/tutorial_development_guidelines.md`](../tutorial/tutorial_development_guidelines.md) governs implementation and verification of executable tutorials.
- [`tutorial/manual_authoring_guidelines.md`](../tutorial/manual_authoring_guidelines.md) governs evidence-based manual authoring.
- Each tutorial uses `tutorial_design.md` for executable scope and `manual_design.md` for audience and teaching content when that manual has been designed.

Tutorial specifications are iterative design records. Source inspection and execution provide evidence about actual behavior, but do not independently approve scientific or educational intent.

## Planned workflow

1. A domain expert discusses the intended tutorial with a conversational AI.
2. They prepare or refine the tutorial and manual design specifications.
3. A coding agent inspects source, data, metadata, and existing tools, then reports conflicts and unresolved decisions.
4. The domain expert confirms scientific and educational choices; the specifications are revised and approved.
5. The coding agent implements the tutorial according to the shared development guidelines.
6. The agent executes and verifies the workflow, records evidence, and reports failed or unavailable checks.
7. The agent authors the manual using actual repository state and execution evidence.
8. The expert reviews the artifacts and revises the relevant specification; the agent updates affected artifacts and repeats focused checks.

Execution results may expose technical or scientific issues and return the work to interactive review and specification revision. The specification is an evolving design record, not a one-pass translation into settings. An AI must not finalize a scientific design solely because it has found a technically valid configuration.

Experiment design, implementation and validation, and tutorial writing are separate stages. Findings from source inspection may revise the specification before implementation. A tutorial may describe only operations and results that were actually checked.

## Manual evidence

Manuals should reflect actual scripts, source interfaces, data, metadata, and execution. Use copyable commands and representative captured output; state when long output is abbreviated, distinguish application logs from shell streams, and never claim an unexecuted operation passed. Shared rules are in [`tutorial/manual_authoring_guidelines.md`](../tutorial/manual_authoring_guidelines.md).

## Tutorial 02: four-experiment specification

Tutorial 02 specifies a 2000–2003 Mekong simulation with annual Restart handoffs, NetCDF runoff/output/Restart, and four configurations: Default, No Bifurcation, Kinematic Wave, and No Floodplain. Four GRDC locations and one MRC-referenced Can Tho model location are used only for simulation-output sampling. The four-year execution, daily hydrograph comparisons, and annual-maximum spatial figures have been generated and checked; no observed discharge comparison is made. The current specification, evidence, and remaining scientific questions are in [`tutorial/tutorial02-mekong_netcdf_4exp/tutorial_design.md`](../tutorial/tutorial02-mekong_netcdf_4exp/tutorial_design.md). A Tutorial 02 manual design and beginner-facing manual remain uncreated and require a separate educational-design stage.

## Repository and beginner-package policy

The GitHub repository is for development and should retain the complete validated experiment implementation. Tutorial 02 has its `tutorial_design.md`, four experiment scripts, visualization tools, and verified reference figures. Its manual design and beginner-facing manual are future work. Generated simulation outputs, Restart files, and run logs are not to be tracked.

The downloadable beginner package has a different teaching purpose. Its planned Tutorial 02 contents include the tutorial documentation, visualization tools, verified reference figures, and only `mek_default.sh`. Users will practice creating the No Bifurcation, Kinematic Wave, and No Floodplain scripts by copying and editing the Default configuration. Those three scripts remain in GitHub and are excluded during package assembly, rather than deleted from the repository. Packaging behavior has not been implemented by this plan.

## Tutorial 01 implementation and manual-authoring experiment

Tutorial 01 is implemented under [`tutorial/tutorial01-get_started/`](../tutorial/tutorial01-get_started/). Its tutorial and manual design, runnable wrapper, reset utility, tested manual, and authoring assessment are present. The original design and writing instructions are retained in `design_history/` for comparison. The 40-day Mekong workflow was executed in a disposable checkout on macOS Apple Silicon; the reset script was also run after the simulation and preserved `config/Mkinclude`. This is operational validation of the development branch, not validation of a separately assembled release package or scientific correctness.

The first manual-authoring experiment and its assessment are recorded in the Tutorial 01 directory. The run succeeded with system Python because its CTL scripts use the standard library; installation of the full optional Python requirements was blocked by package-index DNS access in the test environment. Tutorial 02's manual design, manual, and package integration remain separate follow-up work.

## Long-term goals

- **Human learning:** help students and researchers design, run, debug, and interpret simulations.
- **AI-assisted development:** provide agents with structured scientific context and explicit questions rather than relying only on unstructured source and README files.
- **Reproducibility:** connect scientific objectives, configuration, commands, outputs, and verification evidence.
- **Maintainability:** make tutorial updates traceable when source, data formats, or workflows change, with revalidation on regeneration.
- **Reusable knowledge:** accumulate reviewed experiment specifications, validated workflows, and scientific explanations for future work.

These are goals of the proposed framework, not demonstrated outcomes.

## Open design questions and next steps

- Review the executed Tutorial 02 station interpretation, spin-up implications, and the observed Prek Kdam/Can Tho behavior before writing the beginner-facing README.
- Define a maintainable way to capture representative terminal evidence and regenerate figures while keeping generated outputs out of Git.
- Determine how the beginner distribution assembly will select Tutorial 02 files and verify that the three variant scripts are omitted from the package.

The Tutorial 02 design was revised after interactive researcher review of source and dataset findings, then implemented and executed in four yearly experiments. The specification distinguishes approved settings, verified source/data contracts, actual runtime observations, and unresolved interpretation questions. Results are operationally checked but are not observation-validated. The next stage is maintainer review and beginner-facing tutorial authoring.
