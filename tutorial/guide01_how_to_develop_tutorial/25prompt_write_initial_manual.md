# Step 25 — Write the Initial Learner Manual for the Guide01 Workflow Example

## 1. Objective

Repository: `global-hydrodynamics/CaMa-Flood_v4`

Target directory:

`tutorial/guide01_how_to_develop_tutorial/workflow_example/`

**Task:** Write the first complete learner-facing tutorial manual at:

`workflow_example/README.md`

The initial implementation has now produced two simulation experiments, analysis products, and six figures.

Your task is to turn these verified implementation results into a clear, reproducible, educational manual for beginning CaMa-Flood users.

**This is a documentation task, not a new implementation or visualization task.**

The initial manual should accurately document the current working implementation. Further scientific analysis, visualization improvements, and editorial polishing will be performed in a later development phase.

## 2. Materials to inspect

Before writing, read and inspect:

### Design documents

- `workflow_example/tutorial_design.md`
- The current repository-reviewed Manual Design under `workflow_example/design/`

Use the Tutorial Design for scientific and technical requirements and the Manual Design for educational structure.

### Implementation records

- `22implementation_record.md`
- `23report_agents_tutorial_results.md`, if present

Review the actual implementation rather than relying on report summaries alone.

### Implemented scripts

Inspect the existing scripts, including:

- `s01-prepare_map.py`
- `run-experiment.sh`
- `a01-analyze_outputs.py`
- `a02-prepare_downscale_input.py`
- `s04-downscale_event.sh`
- `v01-plot_results.py`

Also inspect any other scripts, tests, or configuration files used by the workflow.

### Actual results

Inspect the existing:

- `map_modify/` products
- `out/control/` and `out/deep_channel/`
- `analysis/` products
- `figure/` images
- Execution logs and validation records

Review the current Tutorial02 README and repository Python environment instructions where useful.

Do not invent commands, paths, expected values, or software requirements.

## 3. Educational objective

The manual should teach learners how to perform and interpret a controlled sensitivity experiment in which the mapped river-channel depth is increased by 3.0 m.

Organize the manual around:

**Question → Concept → Run → Inspect → Interpret → Checkpoint**

Use this sequence naturally in the practical chapters. Not every chapter needs six repetitive subsection headings.

The user should understand both:

1. How to reproduce the workflow.
2. Why each operation and analysis is scientifically meaningful.

Keep the explanation accessible to students and new CaMa-Flood users while preserving scientific accuracy.

## 4. Recommended manual structure

Follow the nine-chapter structure of the Manual Design, adapting it to the actual implementation.

### Chapter 1 — Introduction

Explain the sensitivity experiment, Control and Deep Channel cases, learning objectives, and expected scientific questions.

Distinguish sensitivity analysis from observational validation.

Invite learners to predict possible changes before inspecting the results.

### Chapter 2 — Preparation

Document:

- Required CaMa-Flood executable and Python environment.
- Shared map, runoff, and input-matrix dependencies.
- Starting working directory.
- Existing inputs versus generated products.
- Output directory structure.
- Important environment checks.

Reuse existing documentation by linking to it rather than duplicating long setup instructions.

### Chapter 3 — Modify the River-Depth Map

Provide the actual tested map-preparation command.

Explain:

- What `rivhgt` represents.
- The +3.0 m parameter change.
- Why the modified map is stored in `map_modify/`.
- Why the original shared map is kept unchanged.
- How the modified file is validated.
- How Deep Channel uses `CRIVHGT` to select the tutorial-local file.

Include Figure 1 and discuss its result.

### Chapter 4 — Run the Two Simulations

Explain the actual tested runner and how to select Control versus Deep Channel.

Use the real `run-experiment.sh` interface and any verified wrapper scripts. Do not assume that earlier proposed script names exist.

Document the model period, forcing, physics, output variables, and expected files.

Clarify the daily output timestamp convention:

- The run covers calendar year 2000.
- The output contains 366 daily intervals.
- NetCDF timestamps represent interval ends, from 2000-01-02 to 2001-01-01.
- Each record represents the preceding daily interval.

Explain the one-year cold-start limitation.

### Chapter 5 — Understand the Four Output Variables

Introduce:

- `outflw`: interval-mean discharge.
- `rivdph`: channel-water-depth snapshot.
- `sfcelv`: water-surface-elevation snapshot.
- `fldfrc`: flooded-fraction snapshot.

Provide verified ways to inspect the files and their dimensions, units, and time coordinates.

Include Figure 2 and explain what its four panels show.

### Chapter 6 — Compare River Dynamics

Explain the relationship:

`sfcelv = elevtn - rivhgt + rivdph`

For corresponding cells in this experiment:

`Delta(sfcelv) = Delta(rivdph) - 3.0 m`

Explain why an increase in channel-water depth does not necessarily increase absolute water-surface elevation.

Include Figure 3.

Discuss the observed Control/Deep Channel differences at the plotted locations without overinterpreting the physical mechanisms.

Clearly identify the selected cells as provisional.

### Chapter 7 — Analyze Flooded Fraction and Area

Explain:

`cell_flooded_area = fldfrc × ctmare`

and how regional flooded area is calculated in km².

Include Figure 4.

Describe the actual provisional regional selection used by the implementation, including its coordinate bounds and boundary-cell rule.

Make clear that the example's rectangular, cell-center-based region is not an authoritative delineation of the entire Mekong Delta.

Discuss the observed regional flooded-area difference as a model sensitivity result.

### Chapter 8 — Spatial Sensitivity and One-Minute Downscaling

Include Figures 5 and 6.

For Figure 5:

- Explain the signed difference convention: Deep Channel minus Control.
- Note that discharge and water-depth differences vary spatially.
- Distinguish changes in river depth from changes in water-surface elevation and flooded fraction.
- Explain any percentile-based color-scale clipping used in the actual plot.

For Figure 6:

Explain the actual implemented downscaling workflow.

Importantly, the implementation uses a Python adapter to read a selected `rivdph` time slice from NetCDF, combine it with the corresponding static `rivhgt`, derive floodplain depth, and prepare Binary input for the existing Fortran downscaler.

The existing Fortran downscaling algorithm was not rewritten for direct NetCDF input.

Document this implemented approach rather than describing the earlier proposed direct NetCDF reader as completed.

Explain:

`flddph = max(rivdph - rivhgt, 0)`

subject to the selected model physics and proper handling of missing cells.

Document the common Control-selected event date, the actual provisional crop, and the role of high-resolution terrain data.

Explain that the output is one-minute postprocessing, not a separate one-minute hydrodynamic simulation.

Do not claim that the two downscaled results are identical or nearly identical without supporting numerical diagnostics.

### Chapter 9 — Discussion and Exercises

Summarize what can be learned from the experiment.

Emphasize:

- Channel-depth and water-surface-elevation changes are not equivalent.
- Model responses vary spatially.
- Flooded fraction and flooded area answer different questions.
- Downscaling provides spatial detail but is not independent validation.
- One-year cold-start experiments have limitations.

Include a few short learning questions or exercises based on the results.

## 5. Figure integration

Use all six existing figures:

1. `figure/f01-rivhgt_map_change.png`
2. `figure/f02-model_fields.png`
3. `figure/f03-river_timeseries.png`
4. `figure/f04-flood_fraction_and_cell_area.png`
5. `figure/f05-event_sensitivity.png`
6. `figure/f06-one_minute_inundation.png`

Embed them in the relevant chapters using relative Markdown paths.

Each figure should have a concise explanatory caption.

Captions should identify the scientific variable, units, experiment or difference convention, relevant date or period, and any important limitations.

Do not simply say "the figure shows the result." Explain what the learner should notice and why it matters.

Do not modify the figures or plotting scripts in this task.

## 6. Scientific interpretation guidance

The researcher and Chat AI reviewed the initial figures and identified several teaching points.

Treat these as preliminary interpretation guidance and verify them against available numerical outputs before making quantitative claims.

### River dynamics

Figure 3 indicates that channel-water depth and water-surface elevation respond differently to channel deepening.

In some locations, discharge and channel depth increase while water-surface elevation decreases.

Explain this using the elevation relationship, but do not claim that a particular hydrodynamic mechanism has been isolated without additional analysis.

### Regional flooding

Figure 4 shows reduced flooded area in the provisional lower-Mekong comparison region during much of the flood season.

Explain the result as a response of this particular model configuration and region, not a universal prediction about channel engineering.

### Spatial differences

Figure 5 shows heterogeneous responses, including positive and negative discharge differences.

Avoid statements such as "deeper channels always increase discharge" or "deeper channels always reduce flooding."

### Downscaling

Figure 6 shows similar large-scale inundation patterns for both experiments on the same selected event date.

Do not conclude that the high-resolution results are numerically identical, or quantify their similarity, without supporting analysis.

### Scientific limitations

Explicitly mention:

- No observational validation was performed.
- The simulations cover only 2000 and use cold-start initialization.
- Selected locations and domains are provisional.
- Model sensitivity is not necessarily an estimate of a real intervention's effects.
- The downscaled product depends on high-resolution terrain and postprocessing assumptions.

## 7. Reproducibility requirements

For every practical step, provide:

- Exact tested command.
- Directory from which it should be executed.
- Required inputs.
- Generated outputs.
- A simple way to check successful completion.
- Known limitations or verified troubleshooting advice.

Identify the actual scripts and generated file paths from the repository.

Do not substitute filenames from the earlier design if the implementation uses different names.

Distinguish generated files from files distributed with Git.

Where relevant, provide the verified environment requirements and dependencies.

The manual must be usable by a new user without undocumented manual intervention.

## 8. Verification during manual writing

This is a documentation-focused task.

Inspect existing outputs and logs before making claims.

You may run lightweight checks or help commands where necessary to verify the documented syntax and file paths.

Avoid unnecessarily rerunning the year-long simulations or downscaling.

Do not modify model physics, scientific analysis, or plotting scripts.

If a documented command cannot be verified, identify the uncertainty instead of inventing a successful test.

Check that relative Markdown links to scripts, design files, and figures are valid.

## 9. Preserve the development example

This manual is part of Guide01, which demonstrates AI-assisted scientific tutorial development.

The goal is to preserve an initial working implementation before extensive human refinement.

Therefore:

- Do not redesign existing figures.
- Do not add new scientific analyses.
- Do not change the selected experimental conditions.
- Do not silently replace provisional research choices.
- Do not update historical Chat AI or Agent AI records.
- Do not modify unrelated repository files.

Create or update only the learner-facing manual and any minimal documentation directly necessary for its correctness.

## 10. Deliverables

Produce:

`workflow_example/README.md`

The README should be a complete, readable, beginner-friendly initial tutorial manual in English.

At the end of the task, provide a short report covering:

1. The sections written.
2. The scripts, logs, data products, and figures consulted.
3. The commands and file paths verified.
4. Scientific results explained in the manual.
5. Unresolved or provisional research decisions.
6. Any documentation or reproducibility issues requiring attention.

Do not commit or push automatically.

**Success criterion:** A new user can understand the scientific purpose, follow the documented workflow, inspect the existing six figures, and recognize the limitations of the results without relying on undocumented assumptions.

The next development phase will separately review and improve the scientific analysis, figures, tutorial design, and manual.



### Tutorial documentation and authoring guidelines

Before writing the manual, inspect the existing documentation under `tutorial/`, particularly:

- `tutorial/README.md` — common tutorial organization, conventions, and design principles.
- `tutorial/guide01_how_to_develop_tutorial/README.md` — purpose and workflow of Guide01.
- The tutorial design, implementation, and manual-writing guidelines provided under `guide01_how_to_develop_tutorial/`.
- The README files and relevant examples from Tutorial01 and Tutorial02.

Follow the existing repository conventions for tutorial structure, Markdown formatting, script documentation, figures, execution instructions, and reproducibility.

Use the Guide01 authoring guidelines when deciding how to organize and explain the learner-facing manual.

If these guidelines differ from the Manual Design, identify the discrepancy and follow the repository-wide tutorial conventions unless doing so would conflict with an approved scientific requirement. Report any unresolved conflicts.

Avoid duplicating general instructions already documented elsewhere. Link to the relevant documentation when appropriate.
