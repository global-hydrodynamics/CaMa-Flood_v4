# How to Develop CaMa-Flood Tutorials

This guide is for researchers and developers who want to create a tutorial with help from a conversational AI and an AI coding agent. You primarily work with concise text specifications: describe what people should learn, what experiment they should run, and what results count as a successful workflow. You do not need to write a long technical prompt or manually collect every terminal log.

Human domain experts still make the final scientific and educational decisions. AI agents can inspect files, implement a specified workflow, run it, and gather evidence, but their output is not deterministic or automatically correct. Review the specifications, scripts, and manual before sharing them.

## Four documents

| Document | Purpose |
| --- | --- |
| Tutorial Design Specification (`tutorial_design.md`) | Defines what users execute: objective, prerequisites, inputs, configuration, workflow, outputs, and acceptance checks. |
| Manual Design Specification (`design/manual_design.md`) | Defines who the manual is for, what readers should learn, and the order and emphasis of explanations. |
| Agent Tutorial Development Instructions (`agent_tutorial_development.md`) | Shared, evolving instructions for agents implementing, executing, and verifying tutorials. |
| Agent Manual Authoring Instructions (`agent_manual_authoring.md`) | Shared, evolving instructions for agents investigating, writing, and checking manuals. |

The two specifications are analogous to source code, shared agent instructions to build rules, and an AI coding agent to the process that produces and checks the executable tutorial and manual. The analogy has limits: an AI agent is not a deterministic compiler, so tests and human review remain necessary.

## A practical development process

1. Discuss the intended learning experience with ChatGPT or another conversational AI.
2. Prepare `tutorial_design.md` with the experiment, inputs, and objective success criteria.
3. Prepare `design/manual_design.md` with the audience, learning outcomes, and instructional sequence.
4. Ask an AI coding agent to read both specifications and the shared agent instructions.
5. Have the agent inspect the actual repository, identify unresolved choices, implement the tutorial, and run it when possible.
6. Have the agent write the manual from current repository information and captured execution evidence.
7. Review the executable workflow and manual. Confirm scientific settings and educational explanations as a domain expert.
8. Refine specifications through discussion when evidence exposes a missing or unclear requirement.
9. Ask the agent to update affected artifacts and rerun focused checks.
10. Review the final changes and contribute them through the project's GitHub workflow.

For beginner tutorials that build the model, use `cd "${cmf_root}"`, `cd src_main`, and `make all` as the standard command sequence. The explicit target builds or updates `MAIN_cmf`; do not assume it is absent or change Makefile behavior solely for a tutorial.

The specifications should stay concise enough for a domain expert to edit through ordinary conversation. Put experiment-specific requirements in the tutorial's own design, not the shared instructions.

## Tutorial01 worked example

[Tutorial01's design](tutorial01-get_started/tutorial_design.md) specifies a first v5 build and 40-day Mekong run. Its [manual design](tutorial01-get_started/design/manual_design.md) describes the beginner learning sequence. The [standalone GOSH script](tutorial01-get_started/tutorial01-get_started.sh) exposes the experiment settings so readers can connect them to map, runoff, and input-matrix files. Its [manual](tutorial01-get_started/manual_tutorial01-get_started.md) shows the build, run, and observed outputs. Developer materials and original drafts are collected under [`tutorial01-get_started/design/`](tutorial01-get_started/design/README.md).

The standalone script is an educational choice: understanding GOSH configuration is a core learning goal. It retains the sample experiment settings while isolating output in Tutorial01. Other tutorials should reuse maintained workflows unless learning the configuration itself justifies a visible standalone example.

## Review before sharing

Confirm that the design matches the approved scientific intent, the script uses the documented inputs and paths, the checks were actually run, the manual reflects observed evidence, and limitations are explicit. A successful run shows that the workflow executed; it does not establish scientific validity. Follow the repository's [`AGENTS.md`](../AGENTS.md) for branch, data, research-record, and contribution rules.
