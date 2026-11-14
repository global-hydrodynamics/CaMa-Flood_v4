# Step 22 — Implement the Guide01 Workflow Example

## 1. Task overview

Repository: `global-hydrodynamics/CaMa-Flood_v4`  
Development branch: `feature/gosh-refactor`

Target directory:

`tutorial/guide01_how_to_develop_tutorial/workflow_example/`

Read the two attached design documents:

- `tutorial_design.md` — scientific experiment and technical specifications.
- `manual_draft.md` — educational objectives and learner-facing workflow.

If the corresponding working documents already exist in the repository, compare them with the attachments and identify any substantive differences before implementation. Use the supplied designs as the task baseline; report conflicts rather than silently replacing approved decisions.

**Your task is to implement, execute, test, and validate the initial workflow example described in these documents.**

Create the necessary:

1. Map-preparation scripts.
2. Simulation scripts.
3. Analysis scripts.
4. Downscaling workflow and required NetCDF input support.
5. Visualization scripts.

The goal is to produce a scientifically consistent, reproducible **first working implementation**, not a thoroughly polished final tutorial.

Do not write the full learner manual yet. That will be a separate development step based on the verified scripts, outputs, and figures.

## 2. Development principles

### Follow both design documents

Treat `tutorial_design.md` as authoritative for scientific experiments, input data, model settings, calculations, and validation requirements.

Use `manual_draft.md` to determine the educational purpose of each script, analysis product, and figure.

Every major workflow stage should help learners answer a scientific question.

### Reuse existing code and conventions

Before implementing new functionality, inspect the relevant current repository files, especially:

- `tutorial/tutorial02-mekong_netcdf_4exp/`
- `src_main/`
- `variable_table/`
- `visualization/`
- `etc/downscale_flddph/`
- `map/mekong_15min/`
- Root Python environment setup and existing build instructions.

Follow established naming, execution, NetCDF, output, and plotting conventions.

Avoid unnecessary frameworks, dependencies, or large refactoring.

### Keep tutorial-generated files local

The shared CaMa-Flood map, input data, and model code are treated as shared resources.

Generate the modified `rivhgt` map inside:

`workflow_example/map_modify/rivhgt_deep.nc`

Do not create modified map files or symlinks under `map/mekong_15min/`.

Store simulation outputs, analysis products, and figures inside the workflow example.

Any necessary changes to shared source code, particularly the downscaler, must be minimal, documented, and backward compatible.

### Separate workflow stages

Keep simulation, analysis, and visualization independent.

A plotting script must not silently launch model simulations or regenerate expensive intermediate products.

Each stage should be repeatable from documented inputs.

## 3. Proposed directory organization

Follow Tutorial02 conventions and place executable workflow scripts directly under `workflow_example/`.

A possible structure is:

```text
workflow_example/
├── tutorial_design.md
├── manual_design.md                   # If present
├── README.md                          # Final manual: not yet required
├── design/                             # Historical development records
│
├── s01-prepare_map.sh
├── s02-run_control.sh
├── s03-run_deep_channel.sh
├── s04-downscale_event.sh              # Name/order may be adjusted
│
├── a01-analyze_outputs.py
├── a02-analyze_inundation.py
│
├── v01-plot_map_parameters.py
├── v02-plot_model_fields.py
├── v03-plot_river_timeseries.py
├── v04-plot_flooded_area.py
├── v05-plot_sensitivity.py
├── v06-plot_downscaling.py
│
├── map_modify/
│   └── rivhgt_deep.nc
├── out/
│   ├── control/
│   └── deep_channel/
├── analysis/
└── figure/
```

This is a suggested organization, not a requirement to create exactly these scripts.

Adjust names, numbers, and grouping based on Tutorial02 conventions and technical dependencies.

Do not create redundant scripts simply to match the example tree.

Source scripts should be kept in Git. Generated products should follow the repository's existing Git tracking and ignore conventions.

## 4. Stage A — Map preparation

Create a reproducible procedure that:

1. Reads the shared `map/mekong_15min/netcdf/rivhgt.nc`.
2. Identifies valid river cells using the source missing-data mask.
3. Adds exactly 3.0 m to valid `rivhgt` values.
4. Preserves missing values, dimensions, coordinates, dtype, variable name, and relevant attributes.
5. Writes the modified NetCDF file to `workflow_example/map_modify/rivhgt_deep.nc`.

Do not use `nextx > 0` as a generic valid-network test, because valid endpoint codes may be negative.

Validate:

- The shared source file is unchanged.
- The output grid and mask match the original.
- The valid-cell difference is +3.0 m within Float32 precision.
- Missing cells remain missing.
- The NetCDF file can be read by the CaMa-Flood map reader.

Provide a repeatable validation check, not just a successful file-write message.

## 5. Stage B — Two controlled simulations

Implement Control and Deep Channel experiments for calendar year 2000.

Use the verified Tutorial02 `mek_default` configuration as the reference.

Preserve the approved model physics, numerical settings, runoff forcing, input matrix, initialization procedure, and output frequency.

The only intended scientific input difference is `rivhgt`.

### Control

Read:

`map/mekong_15min/netcdf/rivhgt.nc`

### Deep Channel

Read:

`workflow_example/map_modify/rivhgt_deep.nc`

Use `CRIVHGT` to reference the tutorial-local NetCDF directly. Construct its absolute path from the script location so execution does not depend on the user's working directory.

### Simulation settings

- Start: `2000-01-01 00:00`, inclusive.
- End: `2001-01-01 00:00`, exclusive.
- Include leap day.
- No previous-year Restart.
- Daily NetCDF output.

Required outputs:

- `outflw`
- `rivdph`
- `sfcelv`
- `fldfrc`

Preserve the correct temporal semantics:

- `outflw`: interval mean.
- `rivdph`, `sfcelv`, `fldfrc`: snapshots.

Do not add redundant daily `flddph` output.

### Simulation validation

Check:

1. Both experiments complete successfully.
2. Their configurations differ only as scientifically intended, allowing different output paths, labels, and log locations.
3. The model reads the intended original or modified `rivhgt` file.
4. Expected NetCDF files exist in their respective output directories.
5. Both experiments have identical coordinates, time axes, and compatible masks.
6. The calendar includes leap day and covers the intended intervals.
7. The actual output timestamp convention is documented.

Do not treat any particular increase or decrease in discharge, water level, or inundation as an execution success condition.

Record warnings and cold-start limitations.

## 6. Stage C — Analysis

Implement reusable analysis scripts that read the saved outputs from both experiments.

Do not rerun the simulations during analysis.

### 6.1 Basic output analysis

Inspect and summarize:

- Coordinates and dimensions.
- Time records.
- Units and missing values.
- Valid-cell masks.
- The four output variables.

Calculate appropriately matched Control and Deep Channel comparisons.

Use the signed difference convention:

`Deep Channel - Control`

Preserve negative discharge where physically or numerically represented; do not silently clip it.

### 6.2 River dynamics

Generate time series for representative cells using:

- `outflw`
- `rivdph`
- `sfcelv`

The design has not yet approved all reference locations. Inspect the available network and propose reproducible candidate cells.

For the Phnom Penh reference, propose a named river cell or a documented nearest-valid-cell selection rule using a clearly identified reference coordinate.

Record the selected coordinates, indices, and criteria.

Do not present provisional locations as researcher-approved.

### 6.3 Flooded area

Use:

`flooded_area = fldfrc × ctmare`

where `ctmare` is the unit-catchment area in m².

For regional total flooded area, sum over an explicitly defined catchment mask and convert m² to km².

The regional mask and treatment of boundary-crossing catchments remain unresolved.

Inspect the available map products and propose a scientifically meaningful, reproducible mask.

Keep this selection configurable and clearly marked as provisional until approved.

Use `fldfrc` for cellwise fraction comparisons and physical flooded area for area-weighted quantities.

### 6.4 Event selection

Select the common downscaling event from the maximum 2000 daily-snapshot `sfcelv` at the selected Control reference cell near Phnom Penh.

Use the same event date for both simulations.

Record:

- Grid cell and coordinates.
- Actual NetCDF timestamp.
- Calendar interval represented.
- Peak water-surface elevation.
- Tie-breaking rule.

Account explicitly for the verified interval-end timestamp convention.

Store these selections and diagnostic results in reusable analysis products such as CSV or NetCDF, as appropriate.

Avoid unnecessarily large derived files.

## 7. Stage D — One-minute downscaling

Inspect and adapt the existing implementation under:

`etc/downscale_flddph/`

The current implementation uses Binary model output as input. The approved workflow requires reading the selected daily `rivdph` field from NetCDF and combining it with the corresponding static `rivhgt`.

For the selected model physics, derive:

`flddph = max(rivdph - rivhgt, 0)`

Apply missing-data masks correctly before arithmetic.

### Implementation requirements

1. Preserve the existing tributary-aware numerical downscaling algorithm.
2. Add direct NetCDF model-input support.
3. Resolve the requested event from the NetCDF time coordinate.
4. Maintain the existing Binary input path.
5. Reuse the existing high-resolution binary map products.
6. Preserve the existing high-resolution output convention unless a change is explicitly justified.
7. Use the same selected event, geographical extent, map data, and visualization scale for both experiments.

Do not redesign the numerical algorithm merely to support NetCDF.

If the existing implementation requires significant restructuring, prefer a small independent input adapter or minimally invasive reader extension.

### Validation

Before running the Mekong example:

- Test the new input reader using small synthetic fixtures.
- Verify coordinates, dimensions, time selection, units, and missing-value handling.
- Compare Binary and NetCDF paths for equivalent inputs.
- Confirm the original Binary workflow still operates.
- Verify the derived coarse-grid `flddph`.

Then test the real one-minute downscaling for both experiments.

Inspect:

- Spatial extent and required upstream/buffer coverage.
- Grid dimensions and orientation.
- Valid and unassigned pixels.
- Missing values.
- Depth range.
- Assignment to appropriate coarse catchments.

The downscaling crop remains subject to researcher approval. Propose a valid reproducible crop near Phnom Penh, verify high-resolution data availability, and mark it provisional.

Do not interpret this postprocessing result as an independent one-minute hydrodynamic simulation.

## 8. Stage E — Visualization

Implement visualization scripts that generate the six figures specified in the Manual Design.

Use the repository's existing visualization utilities, including `cmf_colormap` where appropriate.

Avoid duplicating existing plotting functionality.

Each visualization script must read existing map, model, or analysis products without launching simulations.

### Figure 1 — River-depth map modification

Show:

- Original `rivhgt`.
- Modified `rivhgt`.
- Difference.

Preserve the valid-data mask and show the +3.0 m perturbation clearly.

### Figure 2 — Four model output fields

Show representative spatial fields of:

- `outflw`
- `rivdph`
- `sfcelv`
- `fldfrc`

Use appropriate units, scales, colorbars, and documented timestamps.

Distinguish mean discharge from snapshot variables.

### Figure 3 — River time series

Compare Control and Deep Channel at matched locations for the selected variables.

Label selected locations and their cell coordinates.

Emphasize the physical distinction between channel water depth and absolute water-surface elevation.

### Figure 4 — Flooded fraction and area

Show the distinction between:

- Unit-catchment flooded fraction.
- Physical flooded-area contribution.
- Regional total flooded area through time.

Document the region and boundary-cell rule.

### Figure 5 — Spatial sensitivity

Show selected differences using:

`Deep Channel - Control`

Use appropriately centered diverging color scales for signed differences.

Select meaningful dates or statistics, report the selection rule, and preserve the actual direction of model responses.

### Figure 6 — One-minute inundation

Compare downscaled inundation depth for both experiments on the same Control-selected event date.

Use the same extent, color scale, and treatment of missing values.

Do not treat `prmwat` as a high-resolution mask. If included, present it only as scientifically appropriate contextual information.

### Figure quality

For every figure:

- Use readable labels.
- Use variable-appropriate units and color scales.
- Document the geographical domain and time period.
- Avoid misleading visual differences caused solely by inconsistent scales.
- Preserve the distinction between observation, simulation, sensitivity, and validation.
- Inspect the generated image visually, not just whether the plotting command exited successfully.

The purpose is clear and scientifically interpretable figures. Extensive aesthetic tuning is not required at this stage.

## 9. Handling unresolved scientific choices

The two design documents identify several open choices:

- Phnom Penh reference cell.
- Downscaling crop.
- Regional flooded-area mask and boundary treatment.
- Representative time-series locations.
- Figure 2 and Figure 5 dates or statistics.

Do not silently treat arbitrary choices as approved scientific specifications.

For each unresolved item:

1. Inspect the available repository data.
2. Propose one reasonable, reproducible default.
3. Explain its scientific and technical basis.
4. Expose the selection in a clear configuration or documented parameter.
5. Mark it as provisional pending researcher review.

Proceed with independent implementation and tests where possible.

If a provisional choice is sufficient to exercise the workflow, use it for clearly labeled preliminary outputs rather than unnecessarily stopping all development.

If an essential choice cannot be made without risking scientifically invalid results, report it as blocked and continue with unaffected stages.

Do not conceal these uncertainties in final figures or reports.

## 10. Execution strategy

Work incrementally:

1. Inspect and plan.
2. Implement map preparation; run its validation.
3. Implement both simulation scripts; run and validate the simulations.
4. Implement analysis; save and inspect derived products.
5. Extend and validate NetCDF downscaling.
6. Implement visualization; generate and inspect all six figures.
7. Perform an end-to-end reproducibility and consistency check.

Run the appropriate tests after each stage.

Fix implementation errors encountered during testing.

Do not change researcher-approved scientific configurations merely to make tests pass.

If the full simulation or downscaling cannot run because of missing dependencies, unavailable data, resource limits, or environment problems, report the precise blocker.

Do not claim that a workflow has been validated unless it was actually executed and checked.

## 11. Tests and acceptance criteria

The initial implementation is acceptable when:

- The original shared map remains unchanged.
- The modified map contains the intended +3.0 m change only at valid cells.
- Both experiments have matched settings and valid daily NetCDF outputs.
- The analysis correctly handles units, time semantics, masks, and signed differences.
- Flooded-area calculations use the correct area weighting.
- The downscaling adapter handles NetCDF correctly and preserves legacy Binary compatibility.
- The six planned figures can be regenerated from stored products.
- Scripts can be rerun without undocumented manual steps.
- All proposed scientific choices are explicitly identified.
- No generated files or symlinks are unintentionally left in shared map directories.

Where a test cannot be completed, identify it clearly as unverified.

Do not substitute plausibility checks for scientific or numerical equivalence tests.

## 12. File ownership and Git tracking

Keep source scripts and necessary small configuration files under version control.

Treat large model outputs and derived datasets as generated artifacts, following repository conventions.

Review `.gitignore` before changing tracking rules.

Avoid adding unnecessary copies of large shared input data.

The `workflow_example/` directory should own its generated map variant, output files, analysis products, and figures.

Do not delete or overwrite unrelated files.

Do not reset or discard existing user changes.

## 13. Deliverables

At completion, report:

### A. Files created or modified

List new scripts, configuration files, shared-source modifications, and generated products.

Explain why each shared-source modification was necessary.

### B. Execution and validation

Report:

- Commands actually executed.
- Map preparation validation.
- Model run completion and output checks.
- Analysis checks.
- Downscaling compatibility and correctness checks.
- Figure generation and visual inspection.
- Tests that could not be completed.

### C. Scientific findings

Briefly summarize observed sensitivity in:

- Discharge.
- Channel water depth.
- Water-surface elevation.
- Flooded fraction and area.
- Downscaled inundation.

Include enough quantitative information to evaluate the results, but do not present a detailed scientific interpretation as established fact without validation.

### D. Open issues

List unresolved scientific selections, implementation concerns, limitations, and decisions requiring researcher approval.

### E. Reproducibility information

Record sufficient detail for the later manual-writing stage:

- Actual script names.
- Run directories and commands.
- Input file paths.
- Generated output paths.
- Required software and dependencies.
- Environment and repository revision.
- Verified output dimensions and timestamp interpretation.
- Known failures and tested remedies.

## 14. Scope limitations

Do not:

- Write the final learner-facing tutorial manual yet.
- Rewrite the historical Guide01 Chat AI and Codex development records.
- Make unrelated changes to the CaMa-Flood main model.
- Change scientific experiment specifications without approval.
- Add symlinks or modified input files to shared map directories.
- Perform unrelated repository restructuring.
- Commit or push automatically.

This task is to deliver a **working, tested initial implementation** that can subsequently be reviewed by the researcher and Chat AI, then documented and polished.

The final user manual will be prepared in a separate step using verified implementation results.
