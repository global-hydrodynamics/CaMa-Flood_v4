# Study protocol — draft for maintainer review

## Purpose and status

This protocol supports a long-term, cross-project case study of how the publicly available CaMa-Flood model can evolve from v4 into a reproducible, extensible, and community-maintainable modeling platform. The study includes AI-assisted development, but is not limited to documenting AI-assisted refactoring. It is a research plan, not a report of demonstrated outcomes.

Modernization is intended to make the existing model more general, portable, maintainable, usable, and extensible. Its direct purpose is not to improve scientific accuracy. Unless separately approved as a scientific development task, modernization preserves the established scientific methods, physical parameterizations, and model functionality. Code restructuring, data-format work, and interface organization are in scope when they support general use and maintainability.

Three infrastructure areas are in scope:

- **Pre-processing generalization:** make map and other model-input preparation reusable across datasets, regions, resolutions, and environments; clarify formats, variables, metadata, conversion, and validation.
- **Input/output generalization:** clarify and standardize input and output formats, variables, units, dimensions, and metadata; make compatible additions to inputs, state variables, and diagnostics easier.
- **Post-processing generalization:** make output conversion, extraction, analysis, and visualization reusable across experiments and directory layouts.

Documentation, code organization, tests, CI, development guidance, and AI-assisted development support are cross-cutting infrastructure. These categories describe intended work, not evidence that a capability has been implemented.

The maintainer-approved conceptual foundation and primary evaluation dimensions for v5 are:

1. **Knowledge accessibility:** scientific and technical knowledge can be found and understood through documentation, specifications, and code.
2. **Reproducibility:** users and developers can reproduce calculations and verify the consequences of changes.
3. **Extensibility:** new functionality can be implemented without comprehensive knowledge of, or unnecessary changes to, the entire model.
4. **Governance:** contribution, scientific review, approval, and integration procedures are transparent and clearly defined.

These dimensions refine the study's questions: how knowledge becomes accessible and reusable; whether calculations and changes can be reproduced; whether scoped extensions can be developed safely; and how human, scientific, and AI-assisted contributions are governed. Human–AI collaboration remains a cross-cutting question about task performance, missing context, human interventions, and effective controls.

The research hypothesis is that generalizing the surrounding infrastructure, without requiring changes to the scientific method, may make subsequent scientific extensions easier and more efficient. This is a hypothesis, not an expected result. Scientific accuracy improvement is not an outcome of modernization. The study may assess extension correctness and preservation of existing behavior as success conditions, while recording numerical differences and their causes where relevant.

## Unit of observation and evidence

The primary unit is a bounded development task. For modernization work, record which infrastructure area it addresses, the constraint or dependency it is intended to remove, and what concrete operation or extension the change enables. Also distinguish infrastructure generalization from scientific-function changes, routine bug fixes, and external contributions when the evidence permits. Significant events within a task are indexed in `events.csv`; linked records document important discussions, decisions, knowledge gaps, interventions, and validation runs. Evidence sources include GitHub metadata, commits/diffs, reviewed discussion summaries, task instructions available to the participants, reproducible commands/results, and environment/dataset identifiers.

Classify evidence as:

- **Contemporaneous:** recorded with the work or in its original public artifacts.
- **Retrospective:** reconstructed later from evidence that still exists; label the reconstruction date and do not present it as an original contemporaneous note.
- **Prospective:** recorded under this protocol as the work occurs.

Keep source facts, maintainer-confirmed explanations, AI interpretations, and unresolved assumptions distinct. Git commits establish code changes, not the reasons for them. A test run establishes observed execution, not scientific validity.

## Proposed outcomes and evaluation

Evaluate change from a documented v4 baseline toward evolving v5 states through concrete, repeatable development and use tasks, not by counting README files, lines of code, commits, or tests alone. For each task, record the starting revision and environment, task prompt and scope, allowed assistance, observable completion criteria, evidence artifacts, failures, interventions, and limitations. Where possible, use comparable tasks and input data on the v4 and v5 states; explain when model capability or setup differences make a direct comparison invalid.

The proposed central comparison asks whether a fixed scientific-extension request can be implemented and verified more easily and efficiently on the modernized state than on the pre-modernization state. Select tasks from plausible future scientific needs; do not pre-implement a chosen evaluation feature solely to improve its later result. A task may exercise preprocessing, I/O, or postprocessing generalization directly, or require a scientific change in the calculation core. Classify these separately so results can show where infrastructure generalization helps and where core-model knowledge remains necessary. Do not claim that a comparison establishes causality unless the design controls relevant differences such as model/agent version, environment, input data, and task exposure.

A proposed evaluation matrix is:

| Dimension | Example task evidence (to be specified before formal comparison) |
| --- | --- |
| Knowledge accessibility | A user or agent answers a defined question about a scientific variable, workflow, or module interface using cited repository sources; record correctness, unresolved gaps, and maintainer intervention. |
| Reproducibility | A developer follows a clean-environment recipe, runs a named sample calculation, and compares outputs using recorded inputs, environment, and pre-agreed checks or tolerances. |
| Extensibility | A developer implements a bounded feature or scheme change; record affected modules/interfaces, required context, compatibility checks, and whether unrelated core changes were necessary. |
| Governance | A contributor follows a scoped change from proposal through scientific review, approval, validation, and integration; inspect whether authority, rationale, and outcome are traceable in repository records. |

These are examples for protocol development, not finalized tasks, acceptance criteria, or results. Counts and timing may supplement task evidence, but must not stand in for correctness, user understanding, scientific validity, or maintainability. Numerical comparisons must state source revisions, compiler/library/precision settings, datasets, comparison method, and agreed variable-specific tolerances. Bitwise identity and numerical agreement are different outcomes.

Before a formal extension comparison, capture at minimum:

- the exact task specification, starting commit/tag, allowed files and assistance, and observable success criteria;
- software and data revisions, checksums where practical, operating system, compiler/libraries, and relevant configuration;
- AI tool/model and version when available, plus run settings that could affect results;
- elapsed time, token use, and cost only when actually measured, with their source and measurement basis;
- implementation outcome, tests and numerical/compatibility checks, failures, bottlenecks, and resulting artifacts;
- human questions, decisions, manual edits, and other interventions needed to complete or validate the task.

Where repeated runs are used, record the run identity and preserve failures as well as successes. These requirements are a collection checklist, not a finalized experimental design or scoring rubric. The number of tasks, repetitions, participant population, and scoring method remain open.

Before formal comparison, approve a protocol defining participant population (if any), tasks, starting states, allowed assistance, success criteria, effort measures, data handling, and analysis. Prospective evaluation suggestions are not results. Do not infer that AI, documentation, or a particular code change caused improvement from descriptive observations alone.

## Data quality and bias controls

- Include material failures, rejected proposals, and unresolved questions; do not select only successful tasks.
- When scientific methods or physical parameterizations change, distinguish that work from infrastructure modernization and record the approved rationale and affected behavior. For modernization validation, check numerical consistency and backward compatibility as appropriate; do not require bitwise identity by default. If results differ, record the cause and scientific meaning, and do not conceal differences by regenerating a reference baseline.
- External PRs, bug fixes, and scientific extensions may proceed alongside modernization. Record their change type and relationship to modernization when known; the study does not require normal development to pause.
- Do not implement a selected scientific evaluation task in advance solely to favor a later comparison. General interfaces and extension mechanisms remain legitimate modernization work.
- Do not infer missing times, model versions, token usage, effort, or costs. Empty values mean unavailable or not collected; basis fields distinguish measured and estimated metrics.
- Keep the original artifact as the evidence source and use short summaries that point to it. Avoid duplicating Issue/PR content.
- Have the maintainer review AI-generated summaries before treating them as research data. Do not log inaccessible internal reasoning.
- Do not silently rewrite historical records. Corrections are new Git changes and remain traceable.

## Privacy and research ethics

The public repository may contain public metadata and reviewed summaries only. Full chat transcripts, credentials, private filesystem paths, unpublished sensitive research, restricted datasets, and personal participant data must not be committed. Reference restricted evidence by an approved archive ID without copying its contents. If a source is unavailable, say so.

If future evaluation recruits or analyzes human participants or external contributors, determine consent, data minimization, institutional research-ethics review, retention, and publication rules before collection. This framework itself does not grant consent or ethics approval.

## Historical baseline and scope

Systematic logging is continuous across development projects and branches. Earlier work may be reconstructed later only from surviving evidence and must be labeled retrospective. The Git `v4.30` tag is a candidate source-code baseline; its target commit is recorded in [`baseline/v4_baseline.md`](baseline/v4_baseline.md). The tag alone does not prove identity with a separately distributed package.

The framework records the maintainer-approved v5 principles and modernization scope, but concrete comparative tasks, rubrics, exact v4 package identity, and participant procedures still require specification. It does not freeze a complete runtime environment, conduct human-participant evaluation, or claim a validated scientific comparison. Historical development details that cannot be recovered from available evidence remain unknown rather than inferred.

## Decisions still open

- Which exact downloadable v4.30 package and checksum should be paired with the source tag as the distribution baseline?
- Which numerical regression experiments, input datasets, compilers, and tolerances are scientifically acceptable for v5 comparisons?
- What consent and institutional ethics process applies before participant-level onboarding or AI-assistance studies?
- What stable, access-controlled archive can hold private transcripts, and what citation/retention policy should be used?
