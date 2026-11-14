# Task: Update the Manual Design Based on the Human–Chat AI Review

## 1. Background

This task is part of Guide01, which demonstrates how researchers can develop CaMa-Flood tutorials using Chat AI and Agent AI (Codex).

After Codex produced the repository-informed Tutorial Design and Manual Design, the researcher reviewed them with Chat AI.

This discussion identified an important issue: the initial workflow proposed generating modified map files outside the tutorial directory.

The researcher and Chat AI discussed alternative file-management approaches and agreed to keep the modified map within the tutorial directory whenever practical.

The discussion is recorded in:

`15chat_review_agent_design.md`

**Read this document before making changes.**

The purpose of this task is to incorporate the agreed decisions into the current Manual Design.

## 2. Files to inspect

Repository: `global-hydrodynamics/CaMa-Flood_v4`

Working branch: `feature/gosh-refactor`

Inspect the following:

- `tutorial/guide01_how_to_develop_tutorial/15chat_review_agent_design.md`
- `tutorial/guide01_how_to_develop_tutorial/workflow_example/tutorial_design.md`
- The current repository-reviewed Manual Design under `workflow_example/design/`
- `tutorial/README.md`
- Relevant Tutorial02 scripts and documentation
- The current CaMa-Flood implementation of `CRIVHGT` and NetCDF map reading

Use the actual repository state rather than assuming that previous documentation updates were completed.

## 3. Agreed design decisions

The following decisions have been approved by the researcher.

### 3.1 Keep modified inputs within the tutorial

The Guide01 workflow example will generate:

`workflow_example/map_modify/rivhgt_deep.nc`

The baseline input remains:

`map/mekong_15min/netcdf/rivhgt.nc`

The original shared map must not be modified.

### 3.2 Read the modified map directly

The Control experiment uses the original `rivhgt.nc`.

The Deep Channel experiment should reference the tutorial-local `rivhgt_deep.nc` directly through `CRIVHGT`.

Verify the actual path-resolution behavior against the repository.

Do not introduce symlinks or copies into the shared map directory unless a technical limitation is discovered and explicitly reported.

### 3.3 Separate source scripts from generated files

Follow the existing Tutorial02 conventions:

- Preparation, simulation, analysis, and visualization scripts live directly under `workflow_example/`.
- `map_modify/` contains the generated modified map.
- `out/` contains model outputs.
- `analysis/` contains derived datasets.
- `figure/` contains generated figures.

The exact script names remain subject to implementation.

### 3.4 Treat tutorial-local files as a design principle, not a universal restriction

The common principle should be:

**Prefer to keep tutorial-generated files inside the tutorial directory to minimize unintended effects on shared repository resources.**

Exceptions are allowed when justified by the tutorial's scientific or educational purpose.

For example, a future tutorial introducing a levee scheme may require adding levee-parameter maps to the shared map structure.

The relevant file ownership and external changes must be explicitly described in each tutorial's design.

## 4. Update the Manual Design

Locate the current repository-reviewed Manual Design.

Update it to reflect the agreed directory organization and file-management approach.

Pay particular attention to the sections on:

- Environment preparation.
- Input file identification.
- Map-parameter modification.
- Control and Deep Channel simulation configuration.
- File ownership.
- Reproducibility and cleanup.
- Validation of the generated map.

The planned learner manual should explicitly distinguish:

| File or directory | Role | Expected operation |
|---|---|---|
| `map/mekong_15min/netcdf/rivhgt.nc` | Shared baseline map | Read only |
| `workflow_example/map_modify/rivhgt_deep.nc` | Tutorial-specific modified input | Generate and read |
| `workflow_example/out/` | Simulation results | Generate |
| `workflow_example/analysis/` | Derived analysis results | Generate |
| `workflow_example/figure/` | Figures | Generate |

Explain that users should be able to rerun or remove tutorial-generated products without affecting shared CaMa-Flood map data.

Do not claim this behavior has been tested until it has actually been verified.

## 5. Preserve the scientific and educational design

The Manual Design should continue to use:

**Question → Concept → Run → Inspect → Interpret → Checkpoint**

In particular, the map-modification chapter should teach both:

1. How changing `rivhgt` affects model geometry.
2. Why the modified map should be stored separately from the original input.

Preserve the approved scientific experiment:

- Uniform `rivhgt + 3.0 m` on valid cells.
- Same model physics and forcing in both experiments.
- Calendar year 2000 simulation.
- Daily NetCDF output.
- Comparison of `outflw`, `rivdph`, `sfcelv`, and `fldfrc`.
- Derived flooded-area diagnostics.
- One-minute flood-inundation downscaling.

Do not alter the experiment or reinterpret its scientific objectives.

## 6. Check consistency with the Tutorial Design

Review:

`workflow_example/tutorial_design.md`

Confirm that the Tutorial Design and Manual Design agree about:

- Map modification script location.
- Modified NetCDF output path.
- Direct `CRIVHGT` reference.
- Preservation of the shared baseline map.
- Generated file ownership.
- No unnecessary changes outside `workflow_example/`.

If the Tutorial Design still contains outdated paths or conflicting instructions, update only the relevant sections to restore consistency.

Do not modify unrelated scientific specifications.

## 7. Preserve historical development records

Do not modify the historical documents under `workflow_example/design/`, including:

- Original Chat AI instructions.
- Earlier Chat AI drafts.
- Earlier Codex prompts.
- Previous review reports.
- `15chat_review_agent_design.md`.

The purpose of these records is to show how the tutorial design evolved.

Update only the current working Manual Design and, if necessary, the current working Tutorial Design.

## 8. Scope restrictions

Do not:

- Implement new scripts.
- Generate modified map files.
- Execute simulations or analysis.
- Create figures.
- Modify CaMa-Flood model physics.
- Write the final learner-facing README.
- Reorganize unrelated tutorials.
- Commit or push automatically.

This is a design-documentation update only.

## 9. Deliverables

Provide:

1. The updated current Manual Design.
2. Any minimal consistency updates to `workflow_example/tutorial_design.md`.
3. A short summary identifying:
   - Which files were changed.
   - How the new `map_modify/` structure is documented.
   - Whether direct `CRIVHGT` references are supported.
   - Whether any conflicting paths remain.
   - Any unresolved technical concerns.

Leave the changes available for researcher review.
