# CaMa-Flood v5 Tutorials

Each tutorial is a reproducible model experiment paired with an educational manual. Its specifications define the scientific workflow and the intended learning; the manual guides learners through the executable materials and helps them check their progress. Operational success is not, by itself, scientific validation.

## Core Principles

### 1. Executable Learning Materials

A tutorial is more than a sample script. It combines a meaningful, repeatable model workflow with opportunities to inspect inputs, settings, and results. Learners progress toward explicit objectives and use short Learning Checks to confirm what they can do. The manual teaches the experiment; it is not merely technical documentation of the scripts. Educational depth varies by tutorial: Tutorial01 emphasizes navigation and execution, while later tutorials can teach experiment comparison and scientific interpretation.

### 2. Specification-Driven Development

Tutorial-specific specifications define **what to execute and what learners should learn**. Shared guides define **how people and AI agents develop, verify, review, and maintain tutorials**. Manuals are generated from those specifications and checked against actual repository behavior and execution evidence.

### 3. AI-Assisted Generation and Continuous Improvement

AI agents can inspect code and data, implement and run workflows, collect evidence, and draft manuals. The goal is reproducible, maintainable, and improvable scientific learning material, not just reduced writing effort. Agents may resolve minor implementation and instructional gaps from evidence. Researchers retain authority over scientific decisions, and important educational decisions need human review. Generated material must be checked against code, data, and runs; successful execution does not prove scientific validity. Learner feedback, expert review, and execution findings should improve the specifications and shared guides.

## Available Tutorials

| Tutorial | Learning objective | Manual |
| --- | --- | --- |
| [Tutorial01 — Get Started](tutorial01-get_started/tutorial_design.md) | Navigate the v5 repository, locate GOSH inputs, build and run a short Mekong example, and find its outputs. | [Beginner manual](tutorial01-get_started/manual_tutorial01-get_started.md) |
| [Tutorial02 — Mekong NetCDF four-experiment comparison](tutorial02-mekong_netcdf_4exp/tutorial_design.md) | Run annual NetCDF experiments and compare model configurations through hydrograph and spatial output analyses. | Manual not yet prepared. |

## Documentation Structure and Contributing

| Documents | Responsibility |
| --- | --- |
| This `README.md` | Framework principles, overview, and tutorial index. |
| [`how_to_develop_tutorials.md`](how_to_develop_tutorials.md) | Human-facing development and review workflow. |
| [`agent_tutorial_development.md`](agent_tutorial_development.md), [`agent_manual_authoring.md`](agent_manual_authoring.md) | Reusable AI instructions for executable tutorials and learner manuals. |
| `<tutorial>/tutorial_design.md`, `<tutorial>/design/manual_design.md` | Tutorial-specific experiment and educational design. |
| `<tutorial>/manual_*.md` | Learner-facing guide to performing the experiment and checking its learning goals. |
| `<tutorial>/design/` | Developer evidence, generation history, and other tutorial-specific records when applicable. |

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
