# How to Develop CaMa-Flood Tutorials

This guide is for researchers, students, and contributors developing tutorials with conversational AI and coding agents. The framework's principles are summarized in the [tutorial README](README.md). Human contributors define and review scientific and educational intent; agents help investigate, implement, execute, gather evidence, and prepare manuals.

## Documentation architecture

| Document | Scope | Main responsibility |
| --- | --- | --- |
| [`tutorial/README.md`](README.md) | Shared | Framework principles, overview, and tutorial index. |
| This guide | Shared | Human-facing development, review, and improvement workflow. |
| [`agent_tutorial_development.md`](agent_tutorial_development.md) | Shared | AI instructions for implementing, executing, and verifying tutorials. |
| [`agent_manual_authoring.md`](agent_manual_authoring.md) | Shared | AI instructions for researching and generating learner manuals. |
| `<tutorial>/tutorial_design.md` | Tutorial-specific | Experiment, scientific and technical requirements, inputs, expected outputs, acceptance checks, and learning outcomes. |
| `<tutorial>/design/manual_design.md` | Tutorial-specific | Audience, prior knowledge, educational sequence, useful examples, and section Learning Checks. |
| `<tutorial>/manual_*.md` | Tutorial-specific | Learner-facing instructions for performing the experiment and checking the intended learning outcomes. |
| `<tutorial>/design/manual_generation_log.md` | Tutorial-specific, when applicable | Developer-only evidence about the tested revision, environment, commands, results, and limitations. |
| `tutorial_requirements.csv` | Shared | Evidence-based registry of required learner activities and design, manual, and package checks. |

Keep experiment-specific scientific and educational decisions in each tutorial's specifications. Shared guides describe reusable procedures and quality expectations, not the details of an individual experiment. Generated manuals explain the workflow to learners; they are not independent scientific authorities.

## What each specification decides

### Tutorial Design Specification

The Tutorial Design Specification answers:

- What experiment is performed, and what scientific intent or comparison does it represent?
- What data, configuration, methods, and execution steps are required?
- What outputs and validation or acceptance checks are expected?
- What operational skills and knowledge should learners acquire?

It is the experiment and execution contract. Source code and existing data provide evidence about current behavior, but they do not authorize an unapproved change in scientific meaning.

### Manual Design Specification

The Manual Design Specification answers:

- Who are the learners, and what prior knowledge can be assumed?
- What concepts and skills should be explained, and in what order?
- Which commands, real output excerpts, and examples best support those goals?
- What should each section's Learning Check assess?

Its learning depth should match the specific tutorial. Tutorial01 intentionally checks basic navigation, settings lookup, build, and execution; Tutorial02 has more advanced comparison objectives. Do not apply one tutorial's educational depth automatically to another.

### Shared development guides

The shared AI guides explain how agents inspect, implement, execute, verify, document, and review tutorial work in a reusable way. They should not duplicate an experiment specification or prescribe a single tutorial's scientific settings as general rules.

### Generated manual

The manual turns the two specifications and verified repository behavior into a sequence learners can follow. It explains how to perform the experiment, what evidence to look for, and how the activities connect to the stated learning outcomes. It must not invent commands, terminal output, results, or scientific interpretations.

## Specification-driven development workflow

Use this sequence as a practical guide. In real projects it is iterative: source inspection or a test may reveal a missing requirement, and a specification or implementation may need revision before proceeding.

1. Discuss the intended learner experience and scientific objective with a conversational AI. Identify what is decided and what still needs researcher judgment.
2. Prepare the Tutorial Design Specification with the experiment, inputs, settings, workflow, outputs, and acceptance criteria.
3. Prepare the Manual Design Specification with the audience, learning outcomes, teaching sequence, examples, and Learning Checks.
4. Ask a coding agent to read the specifications and shared agent instructions, then inspect relevant source code, scripts, data, metadata, and existing documentation.
5. Resolve gaps from evidence where possible. Ask the researcher about unresolved scientific or important educational decisions before making dependent changes.
6. Implement or update the executable tutorial while preserving the approved scientific setup and existing interfaces.
7. Execute and verify the workflow where the environment and data permit. Record the actual revision, environment, commands, results, and limitations.
8. Generate or update the learner manual from the specifications and verified evidence.
9. Review scientific fidelity, instructional quality, Learning Checks, links, and generated artifacts. Distinguish operational execution evidence from scientific validation.
10. Feed stable lessons into the appropriate specification or shared guide, then regenerate and recheck affected outputs.

### Learner activities and distribution checks

Every tutorial must make the learner do meaningful work that develops a reusable CaMa-Flood skill. For each required activity, specify the starting material, the essential artifact intentionally absent at the start, what the learner creates or changes, the transferable skill, and an observable completion criterion. Track one activity per row in [`tutorial_requirements.csv`](tutorial_requirements.csv); use `PASS`, `PARTIAL`, `FAIL`, or `NOT_REVIEWED` separately for Tutorial Design, Manual Design, and learner-package checks. Notes must say whether a requirement is explicit, only implied, or missing and point to evidence.

Keep complete working solutions and developer reset/evidence materials under the tutorial's `design/` directory. `design/` stays in GitHub for maintainers but is excluded from the learner package. Put approved reference images in `sample_figure/`; reserve `figure/` for files learners generate and keep it free of pre-generated results in the package. Do not package completed modified datasets, alternative experiment scripts, or generated model outputs that would bypass the intended activity.

Use three review gates:

- **After Tutorial Design:** Is a meaningful learner action explicit, does it produce an essential artifact absent from the initial package, and does it teach a transferable skill?
- **After Manual Design:** Does the design teach how to perform that action, identify the starting file and edits, and explain how to check the result?
- **Before Distribution:** Does the actual package omit completed solutions while retaining enough starting materials and instructions? Can a learner complete the task and approximately reproduce the documented examples?

Inspect the real package assembler before claiming package compliance. The legacy `doc/make_package.sh` builds a v4 distribution and is not a tutorial packager. If no suitable assembler exists, do not repurpose it or create a general package framework for one task; prepare a temporary allowlisted learner snapshot, inspect its contents, and report package validation as partial because the official distribution path remains unimplemented.

For beginner tutorials that build the model, use `cd "${cmf_root}"`, `cd src_main`, and `make all`. This explicit target builds or updates `MAIN_cmf`; do not change Makefile behavior solely to simplify a tutorial command.

## Specification completeness and AI autonomy

Specifications should communicate scientific intent, essential requirements, and learning objectives clearly, but need not settle every implementation or teaching detail. Before asking a person, agents should investigate reliable evidence in code, data, scripts, metadata, and existing documentation.

Agents may use informed judgment to fill minor implementation and instructional gaps, such as explaining a command, ordering steps, choosing a representative verified output excerpt, or drafting Learning Checks that follow the stated objectives. They must not invent scientific facts, change experimental meaning, or make unresolved scientific decisions without human review. Important educational decisions also require human review.

When the work reveals a missing stable requirement, update the relevant specification. Keep one-off implementation details and environment-specific evidence in developer records rather than copying them into shared policy or a scientific design without reason.

## Feedback, verification, and continuous improvement

- A correction specific to one experiment or audience belongs in that tutorial's specifications.
- A reusable lesson that applies across tutorials belongs in the shared development or manual-authoring guidance.
- Environment-specific commands, output excerpts, test results, and limitations belong in tutorial developer records when needed.
- An unresolved scientific or important educational question should be flagged for researcher review, not silently settled by an agent.

Generated manuals are maintained outputs of the framework. If code, data, settings, or specifications change, review and regenerate affected manual sections. Feedback from learners and experts, plus execution evidence, should improve the specifications and shared process over time.

## Examples

### Tutorial01 — beginner first run

[Tutorial01's design](tutorial01-get_started/tutorial_design.md) defines a short Mekong experiment. Its standalone [GOSH script](tutorial01-get_started/tutorial01-get_started.sh) makes settings visible because locating and reading them is part of the learning goal. The [Manual Design Specification](tutorial01-get_started/design/manual_design.md) describes the introductory teaching sequence, and the [generated manual](tutorial01-get_started/manual_tutorial01-get_started.md) guides learners through building, running, and locating outputs. The developer `design/` materials preserve evidence and history. Other tutorials should reuse maintained workflows unless reading the configuration is itself an objective.

### Tutorial02 — comparative experiment

[Tutorial02's design](tutorial02-mekong_netcdf_4exp/tutorial_design.md) defines a more advanced multi-year, four-configuration comparison using NetCDF, annual Restart handoffs, hydrographs, and spatial summaries. Its scientific and educational scope is specific to that tutorial; it does not make the framework depend on those settings. Its learner-facing manual is a separate design and authoring task.

## Review before sharing

Confirm that the specifications match approved scientific and educational intent, the executable workflow uses the documented inputs and paths, checks actually ran, and the manual reflects verified evidence. Ask a domain expert to review scientific meaning. Operational success shows that a workflow executed; it does not establish model accuracy. Follow the repository's [`AGENTS.md`](../AGENTS.md) for authorization, branch coordination, research records, and contribution rules.
