# CaMa-Flood v5 Tutorials

Each tutorial is a reproducible model experiment paired with an educational manual. Its specifications define the scientific workflow and the intended learning; the manual guides learners through the executable materials and helps them check their progress. Operational success is not, by itself, scientific validation.

This is the primary learning environment for general CaMa-Flood users. The repository's [`gosh_test/`](../gosh_test/) directory is for developers who need to run model examples and perform operational checks; tutorials should not depend on outputs created there.

## Core Principles

### 1. Executable Learning Materials

A tutorial is more than a sample script. It combines a meaningful, repeatable model workflow with opportunities to inspect inputs, settings, and results. Learners progress toward explicit objectives and use short Learning Checks to confirm what they can do. The manual teaches the experiment; it is not merely technical documentation of the scripts. Educational depth varies by tutorial: Tutorial01 emphasizes navigation and execution, while later tutorials can teach experiment comparison and scientific interpretation.

### 2. Specification-Driven Development

Tutorial-specific specifications define **what to execute and what learners should learn**. Shared guides define **how people and AI agents develop, verify, review, and maintain tutorials**. Manuals are generated from those specifications and checked against actual repository behavior and execution evidence.

### 3. Design principle: Minimize modifications outside the tutorial directory

Keep tutorial-specific generated files inside the tutorial directory by
default. This includes modified input datasets, model outputs, analysis
products, and figures. Avoid unnecessary changes to shared map datasets or
files owned by other tutorials. Local ownership makes it easier to reproduce
the workflow, understand which tutorial created a file, and clean up generated
products.

This is a recommended default, not an absolute restriction. An external change
may be justified by technical, scientific, or educational needs—for example,
a tutorial that teaches a shared-map levee scheme may need to add levee
parameters to a map dataset. The tutorial design should then identify the
affected files and directories, explain why the external change is needed,
describe how tutorial-generated files are distinguished, explain how existing
shared data and other tutorials are protected, and document how to reproduce
and, where appropriate, reverse the change.

### 4. AI-Assisted Generation and Continuous Improvement

AI agents can inspect code and data, implement and run workflows, collect evidence, and draft manuals. The goal is reproducible, maintainable, and improvable scientific learning material, not just reduced writing effort. Agents may resolve minor implementation and instructional gaps from evidence. Researchers retain authority over scientific decisions, and important educational decisions need human review. Generated material must be checked against code, data, and runs; successful execution does not prove scientific validity. Learner feedback, expert review, and execution findings should improve the specifications and shared guides.

### 5. Learner-centered activities

Every tutorial must state observable learning objectives and require at least
one meaningful learner operation. Running a fully prepared workflow alone is
not sufficient. The required operation must create or modify an essential
tutorial artifact, teach a skill reusable in independent CaMa-Flood work, and
have a clear completion check. The design specifications identify starting
materials, intentionally absent artifacts, learner-created results, and how
those results are used. The shared
[`tutorial_requirements.csv`](tutorial_requirements.csv) records this audit,
one tutorial per row.

### 6. Developer references and learner packages

Keep complete runnable reference solutions and developer records in each
tutorial's `design/` directory. Keep figures used as examples under
`sample_figure/`; reserve `figure/` for learner-generated figures and leave it
without generated images in a learner package. A learner package excludes
`design/`, completed solutions, build products, modified inputs, model outputs,
and other completed learner artifacts. It includes starting materials,
required shared inputs, manuals, approved reference figures, and an empty
generated-output location where appropriate. A GitHub checkout is a developer
repository, not automatically a prepared learner package. There is currently
no tutorial package assembler; the v4-oriented `doc/make_package.sh` is not a
tutorial packager. Until a suitable tutorial package workflow exists, stage
and inspect the intended learner files explicitly rather than treating a
repository ZIP as the package.

## Available Tutorials

| Tutorial | Learning objective | Manual |
| --- | --- | --- |
| [Tutorial01 — Get Started](tutorial01-get_started/tutorial_design.md) | Navigate the v5 repository, locate GOSH inputs, build and run a short Mekong example, and find its outputs. | [Beginner manual](tutorial01-get_started/manual_tutorial01-get_started.md) |
| [Tutorial02 — Mekong NetCDF four-experiment comparison](tutorial02-mekong_netcdf_4exp/tutorial_design.md) | Run annual NetCDF experiments and compare model configurations through hydrograph and spatial output analyses. | Manual not yet prepared. |

## Contributor learning resource

The numbered tutorials teach people how to use CaMa-Flood. [Guide01 —
Developing Tutorials with Chat AI and Agent AI](guide01_how_to_develop_tutorial/README.md)
documents the contributor workflow and keeps its numbered development
history. The actual river-depth sensitivity tutorial is
[Guide01 Workflow Example](guide01_workflow_example/tutorial_design.md), at
the same directory level as Tutorial01 and Tutorial02. It demonstrates the
development process but is not, by itself, a separately maintained or fully
validated Tutorial03 package.

## Documentation Structure and Contributing

| Documents | Responsibility |
| --- | --- |
| This `README.md` | Framework principles, overview, and tutorial index. |
| [`how_to_develop_tutorials.md`](how_to_develop_tutorials.md) | Human-facing development and review workflow. |
| [`agent_tutorial_development.md`](agent_tutorial_development.md), [`agent_manual_authoring.md`](agent_manual_authoring.md) | Reusable AI instructions for executable tutorials and learner manuals. |
| `<tutorial>/tutorial_design.md`, `<tutorial>/design/manual_design.md` | Tutorial-specific experiment and educational design, including required learner actions and completion criteria. |
| `<tutorial>/manual_*.md` | Learner-facing guide to performing the experiment and checking its learning goals. |
| [`tutorial_requirements.csv`](tutorial_requirements.csv) | One-row-per-tutorial summary and design review of learner objectives, required operations, provided materials, intentionally absent artifacts, learner results, and compliance checks. It is a review aid, not a replacement for the individual specifications. |

The registry columns group as follows:

| Column group | Meaning |
| --- | --- |
| `tutorial_id`, `tutorial_directory`, `tutorial_title`, `development_status` | Identify the tutorial and summarize its current maturity. |
| `learning_objective`, `required_learner_action`, `provided_starting_material`, `intentionally_missing_artifact`, `learner_generated_artifact`, `transferable_skill`, `expected_result` | Describe the intended educational design: what learners do, what is present or absent, what they create, and what they should be able to reuse and observe. |
| `R1_learning_objective` through `R8_learner_reproduction` | Assess each shared educational requirement using evidence from the current files and workflow. |
| `tutorial_design_check`, `manual_design_check`, `package_check`, `remaining_issues` | Summarize whether each specification explains the required work and whether the available learner materials support it; list gaps explicitly. |

Compliance values are `PASS`, `PARTIAL`, `FAIL`, or `NOT_REVIEWED`. Use `NOT_REVIEWED` when the relevant evidence has not been inspected; do not mark a package check `PASS` because a package is only planned. The educational-design fields record intended outcomes, while compliance cells record the evidence-supported current state.

Review the registry when a tutorial is proposed, when Tutorial Design or Manual Design is completed or revised, when implementation or learner-package contents change, and before a tutorial is marked ready to distribute. The registry is a lightweight design review tool, not a replacement for each tutorial's specification and not an automated updater target.
| [`tutorial_requirements.csv`](tutorial_requirements.csv) | One-row-per-tutorial audit of educational design and specification/package requirements. |
| `<tutorial>/design/` | Developer-only reference solutions, reset tools, evidence, and design history; excluded from learner packages. |
| `<tutorial>/sample_figure/`, `<tutorial>/figure/` | Packaged reference figures and learner-generated figures, respectively. |

Start with [How to develop tutorials](how_to_develop_tutorials.md) to contribute. Tutorial01 is a beginner example; Tutorial02 demonstrates a more advanced scientific comparison. Keep their specific scientific and educational decisions in their own specifications.

## Tutorial02 visualization setup

The annual maximum discharge example uses the optional CMF Shade-Rainbow
colormap. From the CaMa-Flood repository root, install the local package into
the Python environment used to run the visualization scripts, then launch the
Tutorial02 visualization bundle:

```bash
python -m pip install -e ./visualization/cmf_colormap
bash tutorial/tutorial02-mekong_netcdf_4exp/v00-visualize_all.sh
```

The model output and scientific calculations are unchanged; only the selected
figure's color map is provided by the shared package. See the [CMF Colormaps
guide](../visualization/cmf_colormap/README.md) for installation details and
palette limitations.
