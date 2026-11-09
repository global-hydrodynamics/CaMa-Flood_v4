# Tutorial 03: River Channel Depth Modification and Flood Inundation Analysis

**Directory:** `tutorial/tutorial03-rivhgt_modification/`  
**Status:** Post-discussion design draft (v0.3) — researcher decisions incorporated; implementation details remain open  
**Target:** CaMa-Flood v5 / Mekong 15-minute example

> **Teaching-example context:** This document is the **design draft after the conversation summarized in [`02chat_discuss_tutorial_design.md`](02chat_discuss_tutorial_design.md)**, not the AI's initial, pre-discussion v0.1 draft. It incorporates all scientific decisions made in that discussion, including the +3 m channel-depth perturbation and the Phnom Penh peak-day selection. It is not an implemented or validated tutorial. Unresolved details remain explicitly marked.

## 1. Purpose

Tutorial03 demonstrates how to modify a river-map parameter and evaluate its effects on simulated river discharge, channel water depth, water-surface elevation, and flood inundation. Unlike Tutorial02, which compares different model physics, Tutorial03 conducts a controlled **parameter-sensitivity experiment using the same physical configuration**.

The tutorial is organized into four independent stages:

1. **Map preparation:** Create and validate a modified river-depth parameter in the Mekong map directory.
2. **Simulation:** Run Control and Deep Channel experiments for 2000.
3. **Analysis:** Calculate derived diagnostics, area time series, and downscaling products.
4. **Visualization:** Plot model variables and differences from prepared analysis products.

The separation is intentional: users should be able to repeat figure generation without rerunning the model or recomputing expensive analyses.

## 2. Learning objectives

By the end, users should be able to:

1. Find and inspect the river-depth parameter in `map/mekong_15min/`.
2. Distinguish river-channel depth, channel-top elevation, and water-surface elevation.
3. Create a separate modified NetCDF map parameter without overwriting the original.
4. Configure GOSH to use a user-created parameter dataset.
5. Run two simulations with identical forcing and physics except for channel depth.
6. Interpret `outflw`, `rivdph`, `sfcelv`, and `fldfrc` and their temporal meanings.
7. Explain why `fldfrc` is generally preferable to `fldare` for cellwise inundation maps.
8. Calculate flooded-area time series using `fldfrc × ctmare`.
9. Form and test hypotheses about channel-depth sensitivity.
10. Explain how `prmwat` contributes to interpreting permanent water and inundation.
11. Apply existing CaMa-Flood downscaling at 1-minute resolution for a selected flood day.
12. Separate simulation, analysis, and visualization into reproducible stages.

## 3. Experiment design

### 3.1 Domain and model configuration

- **Map:** `map/mekong_15min/`.
- **Runoff:** Same runoff dataset as Tutorial02.
- **Physics:** The `mek_default` physical configuration from Tutorial02, including Local Inertial routing, floodplain representation, and bifurcation routing.
- **Simulation period:** `2000-01-01 00:00` to `2001-01-01 00:00` (exclusive; includes February 29).
- **Output cadence and format:** Daily NetCDF, as in Tutorial02.

Both experiments must share the same numerical settings, initial-state treatment, runoff, physical options, and dates. Tutorial02 itself must not be modified.

### 3.2 Two experiments

| Experiment | River-channel depth | Other settings |
|---|---|---|
| Control | Original `rivhgt` | Tutorial02 `mek_default` |
| Deep Channel | `rivhgt + 3.0 m` | Identical to Control |

Apply the additive change to **all valid river-channel cells**:

`rivhgt_deep(i,j) = rivhgt_control(i,j) + 3.0 m`

Preserve the original NetCDF file, invalid-cell masks, coordinates, dimensions, units, and relevant metadata. Do not change other map parameters. The precise generated filename is **TBD**.

### 3.3 Scientific hypotheses

Deeper channels may increase conveyance and alter channel–floodplain storage exchange. Examine whether:

- discharge peaks increase;
- `rivdph` increases;
- `sfcelv` decreases;
- floodplain inundation decreases.

These are **hypotheses**, not prescribed outcomes. Increasing channel depth changes the channel bottom; a greater river depth does not necessarily imply a higher absolute water-surface elevation. A single cold-start year may not represent equilibrium sensitivity.

## 4. Part 1 — Map preparation

**Working location:** `map/mekong_15min/` (outside the tutorial's simulation directory).

### 4.1 Inspect parameters

Introduce `rivhgt` (channel depth), `elevtn` (channel-top elevation), `rivwth` (channel width), `rivlen` (channel length), and `fldhgt` (floodplain elevation profile). Explain the geometry and check the model's variable definitions rather than inferring them from filenames alone.

### 4.2 Generate the modified map product

Create a reproducible map-preparation script that:

1. Reads the original `rivhgt` NetCDF parameter.
2. Identifies valid river cells and excludes invalid or missing values.
3. Adds **exactly 3.0 m** to those cells.
4. Retains coordinate grids, dimensions, metadata, and units.
5. Writes a **new** NetCDF parameter file without changing the original.
6. Verifies that every modified valid cell differs by +3.0 m and invalid cells are preserved.

Do not copy the entire Mekong map unnecessarily. The exact script and product names are **TBD**.

### 4.3 Validate parameter changes

Record statistics for original and modified depth, the number of changed/invalid cells, and the distribution of differences. Save analysis data for the final parameter-comparison figure.

## 5. Part 2 — Model simulations

**Working location:** `tutorial/tutorial03-rivhgt_modification/`.

### 5.1 GOSH experiments

Prepare Control and Deep Channel GOSH configurations using Tutorial02 `mek_default` as the baseline. The intended scientific difference is solely which `rivhgt` file is loaded. Verify the actual map path and runtime setting for each experiment.

### 5.2 Period and initialization

Run **2000 only**, including leap day, with identical initial-state and restart settings. No multiyear restart sequence is planned.

### 5.3 Required outputs

Enable daily NetCDF output for both experiments:

| Variable | Meaning | Units | Temporal interpretation |
|---|---|---|---|
| `outflw` | River-plus-floodplain discharge | m³/s | Daily mean |
| `rivdph` | Channel water depth | m | Daily snapshot |
| `sfcelv` | Water-surface elevation | m | Daily snapshot |
| `fldfrc` | Flooded fraction of a unit catchment | 1 | Daily snapshot |

Explain that the daily discharge mean and the state-variable snapshots do not have the same temporal meaning. Both runs must use matching output timestamps.

### 5.4 Simulation validation

Check successful completion; actual parameter-file selection; identical physical/numerical settings apart from depth; complete daily time axes; coordinate and unit consistency; missing data; and plausible output ranges.

## 6. Part 3 — Scientific analysis

Analyze simulation outputs **before** generating final figures. Save derived data (NetCDF and/or CSV as appropriate) that can be reused by visualization scripts.

### 6.1 Variable-specific sensitivity

Prepare comparative diagnostics for:

- `outflw`: annual maximum daily-mean discharge and selected hydrographs;
- `rivdph`: temporal variations and spatial differences;
- `sfcelv`: temporal variations and spatial differences;
- `fldfrc`: spatial and temporal inundation responses.

For corresponding values, compute `Deep Channel − Control`. Exact choices of other temporal aggregates and locations are **TBD**.

### 6.2 Flooded area versus flooded fraction

Use the identity:

`fldare = fldfrc × ctmare`

where `fldare` and `ctmare` have units of m² and `fldfrc` is dimensionless. A cell with greater area may have greater `fldare` even if its inundated *fraction* is small. Therefore, use `fldfrc` for comparable cellwise spatial maps and `fldfrc × ctmare` for total regional flooded area. Demonstrate this distinction using actual outputs, rather than only describing it in text.

### 6.3 Regional flooded-area time series

For a selected **lower-Mekong / delta** region, calculate at each daily output time:

`A_flood(t) = Σ_i [fldfrc(i,t) × ctmare(i)]`

Compare total area (km²), absolute difference (km²), and relative difference where meaningful. The exact region and treatment of boundary catchments are **TBD**.

### 6.4 Permanent water

Introduce the existing `prmwat` data and explain permanent water versus temporary inundation. Use it in an inundation visualization after verifying its representation and relation to model/downscaling outputs. Whether it is a background-only layer or enters quantitative temporary-inundation calculations remains **TBD**. Avoid unverified subtraction or double-counting.

### 6.5 One-minute inundation downscaling

**Resolution:** 1 minute. **Event date:** the 2000 date of **maximum daily `sfcelv` at a representative model location near Phnom Penh in the Control experiment**.

Event selection:

1. Identify and document the representative Phnom Penh model cell (**exact cell / selection rule TBD**).
2. Extract its Control daily `sfcelv` time series for 2000.
3. Determine the date of maximum `sfcelv`; record the date, cell, and peak value in a reproducible analysis product.
4. Use this **same date** for both Control and Deep Channel. Do not choose a separate Deep Channel peak date.

Downscaling:

1. Obtain the required model state for *all relevant coarse cells* on the selected date, not just the Phnom Penh reference cell.
2. Run the **existing** CaMa-Flood 1-minute inundation downscaling workflow for each experiment.
3. Generate and validate high-resolution inundation products.
4. Compare the two experiments over a selected area around Phnom Penh (**map extent TBD**).
5. Incorporate `prmwat` where scientifically appropriate after verifying its conventions.

Do not reimplement the downscaling algorithm. The peak at a fixed Phnom Penh cell need not coincide with the day of maximum flooded area over the wider delta. The actual downscaling tool, input variables, and output structure must be checked in the repository.

## 7. Part 4 — Visualization

Final visualization scripts should primarily read previously generated analysis products. Use the repository's CMF Colormaps package where appropriate, with scientifically meaningful normalization and units.

### Figure 1 — Channel-depth parameter modification

Map original `rivhgt`, modified `rivhgt`, and their +3 m difference; demonstrate which cells were changed.

### Figure 2 — Meaning of model output variables

Show examples of spatial maps for `outflw`, `rivdph`, `sfcelv`, and `fldfrc`, with variable-specific color scales and physical interpretation.

### Figure 3 — River-channel sensitivity time series

Compare Control and Deep Channel discharge, river depth, and water-surface elevation at representative locations (**locations TBD**).

### Figure 4 — Flooded fraction and flooded area

Illustrate the spatial difference between `fldfrc` and `fldare`, and show regional total-flooded-area time series using `fldfrc × ctmare`.

### Figure 5 — Flood-inundation sensitivity

Compare Control and Deep Channel flooded-fraction distributions and their differences using consistent dates/statistics and color scales (**exact temporal summary TBD**).

### Figure 6 — One-minute downscaling and permanent water

Compare 1-minute inundation around Phnom Penh for **both experiments on the Control peak-`sfcelv` day**. Include permanent-water context where scientifically defensible. The exact figure extent remains **TBD**.

Panel arrangements may be refined after examining the actual outputs; the scientific purpose of each figure should remain explicit.

## 8. Workflow and file organization

| Stage | Location | Output |
|---|---|---|
| Map preparation | `map/mekong_15min/` | Separate modified `rivhgt` NetCDF and validation data |
| Simulation | `tutorial/tutorial03-rivhgt_modification/` | Daily NetCDF for two experiments |
| Analysis | Tutorial03 analysis stage | Derived NetCDF/CSV, flooded-area time series, downscaling products |
| Visualization | Tutorial03 visualization stage | Figures and explanations |

Follow the existing CaMa-Flood v5 script and naming conventions once inspected. Keep generated large model and derived files out of Git unless explicitly approved as tutorial samples.

## 9. Interpretation and reproducibility

- Preserve the original map and distinguish generated parameters from baseline input.
- Keep the experiments identical except for `rivhgt`.
- Validate complete and aligned output time axes.
- Handle missing values and physical units consistently in spatial and regional calculations.
- Make analysis independently repeatable from model outputs.
- Make visualization repeatable from derived products.
- Present +3 m effects as **tested model responses**, not guaranteed results.
- Recognize possible cold-start effects in the one-year 2000 simulation.
- This tutorial does not perform observational calibration, parameter optimization, or model-physics development.

## 10. Remaining decisions before implementation

The **researcher has already decided** the +3 m depth change, Tutorial02 Default physics, daily NetCDF output, 1-minute downscaling resolution, and Control Phnom Penh peak-day event selection. Do not reopen those choices without evidence of a conflict.

The following are still open:

1. Exact generated `rivhgt` filename and script conventions.
2. The Phnom Penh reference cell or reproducible selection rule.
3. The display extent around Phnom Penh for downscaling.
4. The lower-Mekong/delta region for area aggregation and treatment of edge catchments.
5. Dates and temporal statistics for the other sensitivity maps and time series.
6. Whether `prmwat` is only a visualization layer or also part of quantitative temporary-inundation analysis; verify its model definition.
7. Exact existing downscaling script, required high-resolution data, and output products.
8. Verify the terminology mismatch in the original request (`rivpht`) versus current map variable (`rivhgt`); the draft uses `rivhgt` consistently.

**Implementation status:** This is a post-discussion **design draft**, not evidence that any scripts, simulations, analyses, or figures have been completed.
