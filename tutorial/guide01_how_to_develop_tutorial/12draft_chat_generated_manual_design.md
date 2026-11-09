# Guide01 Workflow Example — Tutorial Manual Design (Draft)

**Working title:** River-Channel Depth Sensitivity and Flood-Inundation Analysis with CaMa-Flood  
**Status:** Draft manual specification; not an executable user manual  
**Primary implementation specification:** `workflow_example/tutorial_design.md` (repository-informed Tutorial Design)  
**Intended manual:** `tutorial/guide01_how_to_develop_tutorial/workflow_example/README.md`  
**Audience:** New CaMa-Flood users and students who have completed the introductory tutorials; the accompanying Guide01 material also serves tutorial developers.

> **Editorial principle:** The Tutorial Design defines *what the developer must implement*. The tutorial manual explains *what a learner should do, observe, and understand*. Do not convert the design document into a user manual by simply removing technical details.

## 1. Educational concept

### Central question

**How does increasing mapped river-channel depth affect simulated discharge, water depth, water-surface elevation, and flood inundation?**

A deeper channel does not necessarily produce higher absolute water-surface elevation or less inundation everywhere. The tutorial uses two otherwise matched simulations to investigate the actual model response rather than to demonstrate a predetermined outcome.

### Learning outcomes

After completing the tutorial, learners should be able to:

1. Identify `rivhgt` as a map parameter and modify it without overwriting the original input.
2. Explain the distinction between channel-bed elevation, channel water depth (`rivdph`), and absolute water-surface elevation (`sfcelv`).
3. Run Control and Deep Channel experiments with identical forcing, model options, and initialization procedure, except for the channel-depth map.
4. Read daily NetCDF outputs and distinguish interval-mean discharge (`outflw`) from daily snapshots (`rivdph`, `sfcelv`, `fldfrc`).
5. Compare time series and spatial fields without assuming the sign of the response.
6. Distinguish fractional flood inundation (`fldfrc`) from flooded area (`fldfrc × ctmare`) and aggregate flooded area over a documented region.
7. Explain what the 1-minute inundation downscaling adds to the 15-minute simulation—and what it does *not* validate.
8. Identify important limitations, particularly a one-year cold start, the selected event date, map resolution, and lack of observational validation.

### Pedagogical format

Use **Question → Concept → Run → Inspect → Interpret → Checkpoint** in each practical chapter. Keep commands copyable and their expected products explicit. Ask the learner to predict a response before showing a comparison figure, but never require the prediction to be true.

## 2. Scope and prerequisites

- **Example location:** Mekong 15-minute (`map/mekong_15min/`), calendar year **2000**, including **29 February**.
- **Experiments:** Control: original `rivhgt`; Deep Channel: valid `rivhgt + 3.0 m` while preserving the original file and missing mask.
- **Physics:** Match Tutorial02 `mek_default` settings; same runoff, input matrix, execution period, and initial-state procedure in both experiments.
- **Outputs:** Daily NetCDF `outflw`, `rivdph`, `sfcelv`, and `fldfrc`.
- **Analysis:** One reference location near Phnom Penh, selected regional mask, summary maps, time series, flooded-area aggregate, and 1-minute downscaling on the **same Control-selected peak-`sfcelv` day**.
- **Prerequisite knowledge:** Basic shell use, elementary map grids/NetCDF, and the ability to run a prior CaMa-Flood tutorial. Link to Tutorial02 rather than repeating its environment setup.
- **Execution requirements:** Describe the required compiler, NetCDF libraries, Python environment, input files, and approximate resources only after verified during implementation. Do not invent runtimes, memory requirements, command-line options, or file sizes.

The manual is for a **working first implementation**, not a polished research article. It must still be correct, reproducible, and clear about unfinished verification.

## 3. Proposed user-facing README structure

The final `workflow_example/README.md` should be a single navigable tutorial with a short table of contents, numbered chapters, and relative links to scripts/figures. Prefer one document for the initial version; split out appendices only when it becomes unwieldy.

### 1. Introduction — What does a deeper river channel change?

**Question:** If river-channel depth increases by 3 m, what should happen to water depth, discharge, water-surface elevation, and inundation?

- Explain the two-experiment comparison and why a single parameter is changed.
- Define *sensitivity experiment*, not *validation* or a realistic engineering intervention.
- Show a compact schematic of `elevtn`, `rivhgt`, channel bed, `rivdph`, and `sfcelv`; create or adapt it if helpful, without implying the model dynamics are determined by geometry alone.
- Briefly state the six anticipated figures and what questions each answers.
- **Learner checkpoint:** Write down two plausible, potentially competing predictions before running the experiment.

### 2. Preparation — Confirm that the required inputs are available

**Question:** Can we reproduce both runs from shared, known input datasets?

- Link to the repository's existing setup instructions and Tutorial02 environment.
- Give exact, tested commands for setting the repository root and checking relevant map, runoff, input-matrix, and executable availability.
- Describe the input/output directory layout and which files are created vs. pre-existing.
- Explain generated-file and Git-ignore conventions. Never imply that all generated data are committed.
- **Learner checkpoint:** Confirm that the original `rivhgt.nc` is present and will not be overwritten.

### 3. Modify the mapped river depth

**Question:** How can we change the parameter without accidentally changing the map's geometry or missing-data mask?

- Explain `rivhgt` (m) and the relation `channel-bed elevation = elevtn − rivhgt`.
- Run the verified map-preparation script **in the map area**, or invoke it through the documented entry point; do not silently relocate map preparation into a tutorial-specific copy.
- Identify original and new NetCDF paths and the model's `CRIVHGT` map setting.
- Check dimensions, coordinates, dtype, variable name, units, valid/missing-cell masks, and the +3.0 m valid-cell difference.
- **Figure 1:** Original depth, modified depth, and difference. The +3 m change should not appear over missing cells.
- **Learner checkpoint:** Why is `nextx > 0` not a valid general-purpose network mask? (Valid endpoint codes may be negative.)

### 4. Run the two simulations

**Question:** How can we isolate the influence of river depth?

- Explain the matched Control and Deep Channel experiments, using Tutorial02 Default as the *configuration* reference.
- Present the **actual tested** run commands and a small comparison table of the sole intended scientific input difference (`CRIVHGT`).
- Identify output directories, expected four daily NetCDF variable names, and completion-log checks.
- Explain the 2000-01-01 inclusive to 2001-01-01 exclusive simulation interval and daily output semantics. Check timestamps rather than presuming record indices.
- Explain cold-start limitations and distinguish identical **initialization procedure** from guaranteed identical physical initial states after a changed geometry.
- **Learner checkpoint:** List all conditions that must be identical for a controlled sensitivity comparison.

### 5. Inspect four model output variables

**Question:** What do the four output variables actually measure?

- Describe `outflw` (m³/s; daily interval mean), `rivdph` (m; snapshot), `sfcelv` (m; snapshot), and `fldfrc` (unitless; snapshot).
- Include a small variable/reference table linking back to `variable_table/output_variables.csv` where useful.
- Show tested analysis commands and clearly name the produced analysis data products.
- Verify matched grids, timestamps, units, and missing masks before subtracting experiments.
- **Figure 2:** Representative field(s) of the four variables; distinct, physically appropriate color scales and units.
- **Learner checkpoint:** Explain why the date of maximum daily `sfcelv` is not a maximum of daily-mean water level.

### 6. Compare river dynamics

**Question:** Did channel deepening increase water depth, and did absolute water-surface elevation behave the same way?

- Show selected time series of Control and Deep Channel for `outflw`, `rivdph`, and `sfcelv` (and optionally `fldfrc` where useful).
- Introduce the code-derived diagnostic relationship `sfcelv = elevtn − rivhgt + rivdph`.
- Explain that adding +3 m to `rivhgt` lowers channel-bed elevation by 3 m, while `rivdph` is measured from the bed.
- Use measured results to discuss the response; do not label an unexpected sign as a software failure without investigation.
- **Figure 3:** Matched experiment time series with clearly documented point coordinates.
- **Learner checkpoint:** Could `rivdph` rise while `sfcelv` falls? Explain geometrically.

### 7. From inundated fraction to inundated area

**Question:** How is a cell's flooded fraction different from the total flooded area of a region?

- Define `fldfrc` as a fraction of unit-catchment area, `ctmare` in m², and `fldare = fldfrc × ctmare` in m².
- Explain why `fldfrc` is the primary map field for cellwise fraction comparison, while `fldare` is the correct contribution when aggregating areas.
- Specify the reproducible region/catchment mask, including treatment of catchments intersecting the region boundary; **this choice is currently pending**.
- Give the equation `A_region(t) [km²] = Σ_i fldfrc(i,t) × ctmare(i) / 10^6` over the approved mask.
- **Figure 4:** Side-by-side fraction and cell flooded area, plus the region's aggregate flooded-area time series.
- **Learner checkpoint:** Why can two cells with the same `fldfrc` contribute different flooded areas?

### 8. Examine spatial sensitivity and downscale one event

**Questions:** Where are the strongest differences? What extra spatial detail does 1-minute terrain provide?

- **Figure 5:** Deep Channel minus Control maps for selected comparable diagnostics, with differences and date/statistics defined consistently. Choose these diagnostics during scientific review.
- Explain the fixed event-selection rule: select the day of maximum **Control daily-snapshot `sfcelv`** at the approved Phnom Penh-area coarse cell; apply precisely that day to both experiments. Record cell ID/location, timestamp, value, and tie rule in analysis metadata.
- Introduce the downscaler as **postprocessing** of 15-minute hydrodynamic states using 1-minute terrain/network data, not a new 1-minute hydrodynamic simulation.
- Explain the derived coarse floodplain depth `flddph = max(rivdph − rivhgt, 0)` under the selected model physics, its relationship to the new NetCDF input adapter, and the retention of the original Binary input path. Put detailed implementation information in a note, not the main learner workflow.
- Confirm crop coordinates and map coverage, including the required buffer, before providing an executable command; **the crop is currently pending**.
- **Figure 6:** Control and Deep Channel 1-minute inundation depth for the shared event day; identical comparison scales, extent, and meaningful masks. A difference panel is optional if it improves interpretation.
- If `prmwat` is explained, identify it correctly as an optional coarse *per-catchment permanent-water area* (m²). It is **not** the 1-minute pixel mask or an input to the existing downscaler. Do not subtract it from flooded area without an approved method.
- **Learner checkpoint:** What does finer spatial detail reveal, and what uncertainty remains unresolved without independent observations?

### 9. Discussion, limitations, and optional exercises

**Question:** What conclusions does the comparison support—and which conclusions would be overstatements?

- Summarize measured responses of discharge, water depth, surface elevation, flooded fraction, flooded area, and 1-minute flood depth once runs have been completed.
- Explain cold-start effects, one-year selection, fixed event date, regional mask dependence, downscaling assumptions, and absence of observational validation.
- Optional exercises: modify the +3 m perturbation to a different amount; compare a second river cell; inspect a second event date; or test sensitivity to the aggregation region. Require users to preserve the baseline and label changes clearly.
- Provide a brief troubleshooting section for missing map data, environment issues, NetCDF dimension/time mismatches, and missing high-resolution tiles, using only verified fixes.

## 4. Figure and output contract

| Figure | Reader-facing question | Minimum inputs | Required visual behavior |
|---|---|---|---|
| Fig. 1 — Parameter change | What was modified? | Baseline and modified `rivhgt` | Same scales for before/after; missing cells preserved; +3 m difference |
| Fig. 2 — Four variables | What do the outputs mean? | Daily `outflw`, `rivdph`, `sfcelv`, `fldfrc` | Units/time semantics explicit; suitable per-variable scales |
| Fig. 3 — Time series | How does river state respond? | Matched Control/Deep daily outputs | Same locations/times; clear experiment colors; avoid conflating water depth and elevation |
| Fig. 4 — Fraction and area | How is regional flood area calculated? | `fldfrc`, `ctmare`, approved mask | Cellwise fraction vs. physical cell area; regional km² series |
| Fig. 5 — Sensitivity | Where are effects largest? | Matched fields and selected summary | Deep − Control sign convention; diverging colors for signed differences |
| Fig. 6 — 1-minute event | What changes after downscaling? | Same-day coarse state, high-resolution map | Identical extent/scales for two experiments; explain postprocessing nature |

Figures should be generated from saved analysis/downscaling products wherever possible; the visualization scripts should not secretly rerun the simulation. Include informative captions with dates, region, units, definitions, and limitations. Do not invent figures or describe plot results before they have been generated.

**Output organization:** Follow the Tutorial02 convention: runnable `sNN-*`, `aNN-*`, and `vNN-*` scripts in `workflow_example/`, generated model files in `out/`, derived products in `analysis/`, and reference images in `figure/`. The map parameter preparation belongs to `map/mekong_15min/`. Exact script filenames are still provisional and must reflect the implemented code.

## 5. Manual writing and verification rules

1. **Write from executed commands.** Every shell command must be tested from the documented starting directory; do not paste pseudocode into the final manual as if executable.
2. **Separate confirmed facts from expected science.** The physical-response direction is a hypothesis until computed.
3. **State input and output files.** Each chapter identifies the inputs used, generated files, and how to recognize a successful step.
4. **Explain the why.** Include scientific context, but do not bury a beginner under implementation internals such as Fortran module names unless needed for a linked advanced note.
5. **Preserve time semantics.** `outflw` daily mean is not the same kind of daily value as the three snapshots.
6. **Verify comparisons.** Check equal timestamps, coordinates, masks, and normalization before plotting differences.
7. **Protect existing users.** Keep the legacy Binary downscaling option available and test it separately; NetCDF support does not imply a new NetCDF downscaled output format.
8. **Include a reproducibility summary.** Record CaMa-Flood revision, relevant environment/version details, source data identifiers, experiment options, selected reference point and region, and tested execution steps.
9. **Avoid overstating validation.** Running without an error and passing structural checks are not observational validation.
10. **Keep the initial manual concise.** Use links to pre-existing Tutorial02/setup docs and to the developer design instead of copying them wholesale.

## 6. Implementation-dependent decisions to resolve before manual finalization

| Decision | Current status | Effect on manual |
|---|---|---|
| Phnom Penh reference grid cell/coordinate | **Researcher approval required** | Defines Control peak date and Fig. 3 / Fig. 6 references |
| 1-minute downscaling geographic crop | **Researcher approval + coverage check required** | Determines command arguments, buffered map availability, Fig. 6 extent |
| Regional flooded-area catchment mask and boundary rule | **Researcher approval required** | Determines Fig. 4 and reported km² |
| Other representative daily map dates and time-series points | **Researcher selection/approval required** | Determines Fig. 2, Fig. 3 and Fig. 5 |
| NetCDF downscaler adapter and Binary regression | **Not implemented or tested in the supplied design** | Determines runnable workflow and verified results for Fig. 6 |
| Final script names, actual outputs, resource requirements, execution results | **Not yet established** | Must be filled from successful runs; never inferred |
| `prmwat` presentation | **Optional context, not quantitative adjustment** | Add an explanatory note only if it improves learning |

Until these are resolved, this document is a **manual design draft**, not a ready-to-publish learner README.

## 7. Proposed authoring sequence and acceptance criteria

**Sequence**

1. Review this manual design together with the repository-informed Tutorial Design.
2. Resolve the researcher-controlled location, region, date, and figure specifications.
3. Implement and verify map modification and the two matched simulations.
4. Implement and verify analysis, saved derived products, and Figs. 1–5.
5. Implement and regression-test the NetCDF downscaler reader; create and verify Fig. 6.
6. Write the first `workflow_example/README.md` using **actual commands, outputs, and figures**.
7. Ask a new user (e.g., a student) to follow the manual without informal guidance; collect reproducibility and comprehension issues.
8. Preserve the first working version as the Guide01 example before optional polishing of the full Tutorial03.

**Acceptance criteria for the first working manual**

- All listed commands and referenced files exist and have been verified in a clean, documented workflow.
- Every chapter has a clear question, action, expected artifact, and interpretation/checkpoint.
- Both experiments have matched configurations, apart from the intended `rivhgt` input; source data remain intact.
- Figures are reproducible from documented products and have appropriate units and temporal semantics.
- The region, reference cell, selected day, and downscaling crop are explicitly documented.
- The coarse-to-fine downscaling step is identified as postprocessing and has passed its input and compatibility checks.
- Failures, unverified claims, and scientific limitations are not hidden.

## 8. Guide01 educational note: how this document should be used

This draft can become an intermediate record in `workflow_example/design/` showing **Chat AI-assisted manual planning after Codex has refined the scientific implementation design**. The resulting learner-facing `README.md` is a separate artifact, written only after scripts and figures exist. This contrast is itself part of the Guide01 lesson: design documents, prompts, repository review reports, runnable tutorial packages, and user manuals serve different purposes and should not be conflated.
