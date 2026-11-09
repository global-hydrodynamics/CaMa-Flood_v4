# Step 21 — Discussing the Implementation Plan with Chat AI

**Document type:** `chat` — summary of a researcher–Chat AI discussion, not a verbatim transcript  
**Guide:** `tutorial/guide01_how_to_develop_tutorial/`  
**Example:** `workflow_example/`  
**Inputs reviewed:** `tutorial_design.md` and `manual_draft.md`  
**Next step:** `22prompt_implement_tutorial.md` (instructions to Codex)

## Purpose of this step

After preparing and reviewing the scientific Tutorial Design and the learner-oriented Manual Design, the researcher asked Chat AI to formulate an implementation prompt:

> Review the two attached design documents and prepare instructions for generating the required map-preparation, simulation, analysis, and visualization scripts.

This step illustrates **discussing the implementation scope with Chat AI before assigning coding work to Agent AI (Codex)**. The two designs have distinct roles: the Tutorial Design defines the scientific experiment and technical constraints, while the Manual Design establishes what learners should run, inspect, and understand.

## Implementation plan derived from the designs

1. **Map preparation:** Read the shared Mekong `rivhgt.nc` without modifying it. Generate `workflow_example/map_modify/rivhgt_deep.nc` by adding 3.0 m to valid cells only, preserving dimensions, coordinates, metadata, and missing-value mask. Confirm the source remains unchanged.
2. **Simulation:** Create matched Control and Deep Channel run scripts based on Tutorial02's `mek_default` configuration. Simulate calendar year 2000 (including leap day) with identical physics, forcing, and initialization procedure. Deep Channel selects the tutorial-local map through `CRIVHGT`; output daily NetCDF for `outflw`, `rivdph`, `sfcelv`, and `fldfrc`.
3. **Analysis:** Read saved outputs, verify their coordinates, masks, and actual time semantics, and calculate matched experiment differences and diagnostics. Distinguish daily-mean discharge from snapshot fields; derive flooded area as `fldfrc × ctmare`. Identify an event date from the Control `sfcelv` peak near Phnom Penh.
4. **One-minute downscaling:** Extend the existing Binary-input downscaler to accept the selected NetCDF `rivdph` time slice and tutorial-specific `rivhgt`, deriving `flddph = max(rivdph - rivhgt, 0)` with correct missing-data handling. Preserve the existing algorithm and Binary compatibility, and test the reader before the regional calculation.
5. **Visualization:** Produce the six planned figures: river-depth modification; four output fields; river time series; flooded fraction versus flooded area; spatial sensitivity; and paired one-minute inundation maps. Scripts should use saved analysis products and not silently rerun simulations.

Preparation, simulation, analysis, and plotting scripts should be kept **directly under `workflow_example/`**, following Tutorial02 naming conventions. Generated products belong to `map_modify/`, `out/`, `analysis/`, and `figure/` within the same example.

## Points highlighted during the discussion

- **Keep the tutorial self-contained where practical.** Shared map and forcing data are read-only. Do not create modified map files or symlinks under `map/mekong_15min/`.
- **Implement and test incrementally.** Each stage has explicit structural and scientific checks; do not change approved physics or settings just to pass tests.
- **Preserve model semantics.** An increase in channel water depth does not necessarily mean higher absolute water-surface elevation; sensitivity signs are results to measure, not predetermined success criteria.
- **Respect unresolved research choices.** The Phnom Penh reference cell, high-resolution crop, regional area mask, and representative locations/dates were not fixed in the designs. Codex may propose reproducible, configurable provisional choices, but must identify them as pending researcher approval.
- **Separate implementation from manual writing.** The final learner-facing `README.md` should be drafted later, using tested commands, actual outputs, and inspected figures.
- **Record evidence.** Codex should report created files, commands executed, successful checks, failures, uncertainties, and any shared-code changes required for NetCDF downscaling.

## Outcome and next step

Chat AI prepared an English Codex implementation prompt covering repository inspection, scope, execution order, scientific constraints, validation, six figures, downscaling compatibility, and reporting. Save that instruction as:

`22prompt_implement_tutorial.md`

Then have Codex implement and test the workflow. The researcher should subsequently review the generated scripts, data, figures, and validation report with Chat AI before writing or polishing the final user manual.

**Teaching point:** The researcher does not simply pass design files to an agent with a broad “write the code” request. Chat AI helps turn scientific and educational designs into a bounded, testable implementation assignment; Agent AI implements it; the researcher retains responsibility for scientific review.
