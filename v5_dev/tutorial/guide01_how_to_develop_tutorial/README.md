# Guide01 — Developing CaMa-Flood Tutorials with AI Assistance

Guide01 teaches researchers and students **how to develop scientific
tutorials and learner manuals with AI assistance**. It is not simply another
tutorial about running CaMa-Flood. Its worked example applies the development
process to a river-channel-depth sensitivity experiment, but the main subject
is how a human researcher, Chat AI, and Agent AI (Codex) collaborate to
design, implement, review, and explain a tutorial.

AI can help clarify requirements, inspect a repository, write code, and
prepare documentation. A successful run does not establish that the scientific
interpretation is correct or that the lesson is effective. Human review and
approval remain essential.

## Who does what?

| Participant | Responsibility |
| --- | --- |
| **Human researcher or developer** | Defines the scientific and educational objectives, makes scientific decisions, evaluates results and interpretations, and approves the tutorial. |
| **Chat AI** | Helps discuss objectives and alternatives, identifies ambiguities, reviews designs and initial results, and drafts focused instructions for implementation or revision. |
| **Agent AI (Codex)** | Checks the actual repository, compares proposals with code and data, implements approved work, runs appropriate checks, and updates the technical documentation. |

The process is collaborative and iterative. Chat AI does not replace
repository inspection; Agent AI does not decide scientific meaning on the
researcher's behalf. The researcher should review both the decisions and the
evidence before treating a tutorial as ready for learners.

> **Before you begin:** [Set up your development environment](00_SETUP.md) to
> prepare a local checkout, Git branch, Agent AI access, compiler, Python
> environment, and the sample-workflow checks.

## Two parts of Guide01

### This guide and its authoring resources

The Guide01 README introduces the method and points to its development
records. The shared
[tutorial-development workflow](../how_to_develop_tutorials.md),
[Agent AI implementation guidance](../agent_tutorial_development.md), and
[manual-authoring guidance](../agent_manual_authoring.md) provide reusable
conventions for planning, checking, and documenting tutorials.

### The example tutorial: `tutorial/guide01_workflow_example/`

The actual Mekong river-channel-depth sensitivity tutorial is a sibling of
Tutorial01 and Tutorial02, not a subdirectory of this development guide:

- [`../guide01_workflow_example/tutorial_design.md`](../guide01_workflow_example/tutorial_design.md)
  defines the scientific experiment, learner actions, and technical
  requirements.
- [`../guide01_workflow_example/design/manual_design.md`](../guide01_workflow_example/design/manual_design.md)
  specifies the intended learner operations and completion evidence.
- [`../guide01_workflow_example/manual_workflow_example.md`](../guide01_workflow_example/manual_workflow_example.md)
  is the learner-facing experimental walkthrough.
- `tutorial/guide01_workflow_example/` owns the executable scripts, design
  references, data/analysis inputs, and `sample_figure/`. Its `design/`
  directory contains completed developer references and is excluded from a
  learner package.

The numbered development records remain directly under this Guide01 directory.
They document how the example was developed; they are not relocated or
rewritten to imply that the current sibling layout existed earlier.

The numbered files directly under this Guide01 directory are historical
snapshots. Their descriptions of what had or had not been implemented are
true for the stage recorded, not necessarily for the current repository. The
latest scripts, learner manual, and outputs describe the current example;
numbered records explain how it developed. For example,
[Step 23](23report_agents_tutorial_results.md) predates the learner manual and
should be read as an implementation report from that point in time.

At a glance, the current layout is:

- Guide01 root: this overview and all numbered human–AI development records.
- `../guide01_workflow_example/`: the actual example tutorial, including its
  Tutorial Design, Manual Design, learner manual, starter scripts, and
  `sample_figure/`.
- `../guide01_workflow_example/design/`: developer-only complete references,
  reset script, and evidence; excluded from learner packages. Generated
  `out/`, map-modification, analysis, and `figure/` products are absent from a
  clean learner start.

## Development phases and record navigation

The records use separate number ranges so readers can follow the development
stages without confusing the first experiment design with later reviews.

| Phase | Human researcher | Chat AI | Agent AI (Codex) | Output and navigation |
| --- | --- | --- | --- | --- |
| **Steps 01–06: Tutorial Design** | Introduces the tutorial idea, clarifies scientific choices, and keeps unresolved decisions open for review. | Discusses learning and scientific goals, drafts the initial concept, and raises questions. | Inspects the repository and sample inputs, then checks and refines the technical design. | Repository-informed Tutorial Design. Records: [01 initial request](01chat_ask_rough_tutorial_design.md), [02 discussion](02chat_discuss_tutorial_design.md), [03 first draft](03draft_first_tutorial_design.md), [04 review prompt](04prompt_brushup_tutorial_design.md), [05 repository review](05codex_discuss_tutorial_design.md), [06 refined design](06draft_agend_generated_tutorial_design.md). |
| **Steps 11–16: Manual Design** | Reviews what learners need to understand and decides how tutorial-generated files should be owned. | Discusses teaching questions, reviews the repository-informed design from a learner's perspective, and helps surface concerns. | Produces and refines a repository-informed Manual Design and updates it after the human–Chat AI review. | A Manual Design that separates learner instruction from scientific and implementation specifications. Records: [11 discussion](11chat_discuss_manual_design.md), [12 Chat AI draft](12draft_chat_generated_manual_design.md), [13 Codex prompt](13prompt_brushup_manual_design.md), [14 repository-informed draft](14draft_agent_generated_manual_design.md), [15 review](15chat_review_agent_design.md), [16 update prompt](16prompt_update_design_based-on-chat.md). |
| **Steps 21–25: Implementation** | Sets implementation priorities, reviews outputs, and asks for the initial learner manual. | Helps turn the design into an implementation plan and reviews initial results. | Implements and checks map preparation, simulations, analysis, downscaling, figures, and the learner manual. | A runnable workflow and its execution evidence. Records: [21 plan discussion](21chat_plan_implementation.md), [22 implementation prompt](22prompt_implement_tutorial.md) and [execution record](22implementation_record.md), [23 implementation report](23report_agents_tutorial_results.md), [24 result review](24chat_review_initial_results.md), [25 manual prompt](25prompt_write_initial_manual.md). |
| **Steps 31 onward: Review and Refinement** | Evaluates scientific and educational quality, identifies needed changes, and decides which proposals to adopt. | Helps analyze feedback, explore improvements, and prepare focused implementation requests. | The intended role is to implement approved revisions and check their effects; the current records mainly document review and proposed changes. | An iterative review record with improvements and unresolved questions. Records: [31 review tasks](31tasks_review_and_refine_workflow.md), [32 manual-improvement discussion](32chat_propose_manual_improvements.md), [33 learning-check discussion](33chat_learn_check_improvements.md). |

The ranges identify phases, not a claim that every requested refinement has
already been implemented. Step 31 onward documents an ongoing review process;
check the current learner manual and implementation before assuming a
particular proposal was adopted.

## How to use Guide01

You can follow the example in sequence without reading every historical
record in full:

1. Read this README to understand the roles and phases.
2. Start with the initial researcher request and Chat AI discussion, then
   compare them with the repository-informed Tutorial Design.
3. Follow the Manual Design phase and notice how the team separates the
   learner's needs from scientific and implementation specifications.
4. Compare implementation prompts with Codex's repository checks, scripts,
   test results, and execution records.
5. Read the [learner manual](../guide01_workflow_example/manual_workflow_example.md)
   and inspect its six reference figures. The learner-generated analysis
   products are absent until the workflow is performed.
6. Follow the review records to see how human feedback is translated into
   specific changes and how unresolved items remain visible.
7. Apply the same approach to another CaMa-Flood topic, making the scientific
   decisions and educational objectives explicit before implementation.

Open the linked records when you want to study a particular decision or prompt.
The overview and current working documents are intended to make navigation
possible without treating the entire history as required reading.

## Iterative development is part of the lesson

A first implementation that runs is a starting point, not necessarily a
finished scientific or educational tutorial. Review can reveal problems in
experimental settings, selected locations, analysis methods, figure design,
explanations, or learning checks. The appropriate response may require
revisiting the Tutorial Design or Manual Design, changing an implementation,
and checking the resulting evidence again.

The later records show that process in progress: discuss a concern, decide
what evidence or change is needed, give Codex a bounded task, and review the
result. Record both what improved and what remains unresolved. This makes
difficulties and responsible uses of AI visible to future developers and
students continuing the work.

## How the example relates to the other tutorials

[Tutorial01](../tutorial01-get_started/manual_tutorial01-get_started.md)
and [Tutorial02](../tutorial02-mekong_netcdf_4exp/README.md) teach
particular CaMa-Flood operations and scientific comparisons. Guide01 teaches
how such tutorials and manuals can be developed. Its river-depth experiment
is the worked example for the development process; it is not, by that fact
alone, an independently maintained or fully validated Tutorial03 package.
The early design records use the name Tutorial03 while exploring the idea;
later repository-informed documents place the worked example under
`workflow_example/`. Those records represent different stages and should not
be read as evidence that a separate Tutorial03 package exists.

For the broader tutorial framework and its documentation roles, see the
[tutorial collection README](../README.md).
