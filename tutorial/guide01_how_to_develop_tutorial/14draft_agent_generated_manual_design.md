# Workflow Example — Learner Manual Design

**Status:** Repository-informed design for a future learner manual. The
workflow example has not been implemented or run.

**Scientific authority:** [`../tutorial_design.md`](workflow_example/tutorial_design.md)
defines the approved experiment and technical specification. This document
defines how to teach that workflow. It must not override the scientific design
or present proposed files and steps as already available.

**Planned learner manual:** `../README.md` (write only after the workflow has
been implemented and verified).

## Purpose and audience

The manual will guide new CaMa-Flood users and students through a small,
controlled sensitivity experiment on the Mekong 15-minute map. It is also a
worked example for Guide01 contributors learning how to turn an approved
scientific design into a tested tutorial and a usable manual.

The example changes the valid-cell values of the `rivhgt` map parameter by
`+3.0 m` for the Deep Channel case. Control and Deep Channel use otherwise
matched model settings, forcing, simulation period, and initialization
procedure. This is a model sensitivity experiment, not an observational
validation or a prediction of the effect of a real engineering intervention.
The expected direction of change is a hypothesis; the manual must report the
measured response without making a predicted sign a success criterion.

## Learning objectives

After completing the tutorial, a learner should be able to:

1. Make and validate a separate map-parameter variant without overwriting the
   baseline `rivhgt` file.
2. Explain how `rivhgt`, channel-bed elevation, channel water depth
   (`rivdph`), and water-surface elevation (`sfcelv`) differ.
3. Set up a controlled pair of simulations and identify their intended
   scientific difference.
4. Read daily model outputs while distinguishing interval-mean discharge
   (`outflw`) from snapshot variables (`rivdph`, `sfcelv`, and `fldfrc`).
5. Compare matched fields and time series, and distinguish flooded fraction
   from physical flooded area.
6. Describe what the planned 1-minute postprocessing adds and what it does not
   establish.
7. State the limitations of a one-year cold-start sensitivity experiment and
   the absence of observational validation.

## Relationship to the scientific design

Use the current [`tutorial_design.md`](workflow_example/tutorial_design.md) for exact
experiment settings, map and runoff metadata, output definitions, and the
planned analysis. Do not duplicate its implementation-level specifications
in full. Link to it for technical details and keep this manual focused on the
learner's purpose, actions, observations, and interpretation.

Repository inspection confirms that the design is not yet an executable
workflow: no Tutorial03 preparation, run, analysis, or visualization scripts
or results are established here. Script names, commands, paths, resource
requirements, expected numeric results, and troubleshooting fixes must be
filled in only after implementation and execution. The existing Tutorial02
README and scripts are examples of repository conventions, not proof that the
new workflow has run.

## Educational approach

Organize practical chapters around:

**Question → Concept → Run → Inspect → Interpret → Checkpoint**

Begin each chapter with a scientific question. Explain only the concepts
needed for that step, then provide tested commands and name their inputs and
outputs. Ask learners to predict or explain what they see, while making clear
that a prediction is not a pass/fail test. Use a short checkpoint to help them
assess understanding before moving on.

Keep the first manual to the core workflow and six figures described below.
Link to Tutorial02 for general environment setup and to the scientific design
for detailed specifications. Avoid turning the manual into a complete
reference for Fortran internals, all map tools, or every available output.

## Proposed manual chapters

### 1. What might a deeper river channel change?

**Question:** If the mapped river depth increases by 3 m, how might discharge,
channel water depth, water-surface elevation, and inundation respond?

Introduce the Control and Deep Channel comparison and distinguish a
sensitivity experiment from validation. Invite learners to write down one or
more plausible responses before viewing results; do not imply that any one
response is guaranteed.

**Checkpoint:** Why is this comparison not, by itself, evidence of what would
happen after a real river-engineering project?

### 2. Prepare the environment and confirm the inputs

**Question:** Are the software environment and source datasets ready for two
reproducible runs?

Link to the repository-root Python setup and the Tutorial02 README where
applicable. Once implemented, list tested setup and preflight commands,
starting directory, required executables, map files, runoff, and the input
matrix. Identify which inputs are repository samples and which are generated
locally. State how outputs are stored and which generated files are excluded
from Git, based on the actual implementation.

**Inspect:** Confirm the baseline `map/mekong_15min/netcdf/rivhgt.nc` exists and
will remain unchanged; confirm that the required map, forcing, and input-matrix
files can be opened by the documented workflow.

**Checkpoint:** Which files are source inputs, and which will this tutorial
generate?

### 3. Create and check the Deep Channel map parameter

**Question:** How can one map parameter be changed without altering the
baseline map or its valid-cell coverage?

Explain that `rivhgt` is channel depth in metres and that the model derives
channel-bed elevation as `elevtn - rivhgt`. Use the verified map-preparation
entry point after it exists; do not imply that a proposed script is already
available. The modified file must be separate from the original.

The implementation checklist should verify dimensions, coordinates, variable
name, units, data type, fill values, and valid-cell mask. Add 3.0 m only at
valid cells and confirm that missing cells remain missing. Do not use
`nextx > 0` as a generic valid-network mask: the map contains valid endpoint
codes such as `-9` and `-10`.

**Figure 1 — The map change:** Show baseline `rivhgt`, modified `rivhgt`, and
their difference with consistent scales where appropriate. The difference
should show the intended valid-cell perturbation, not values over missing
cells. Generate and describe this figure only after the preparation step has
been run.

**Checkpoint:** What does the mask check tell you that a successful file write
alone would not?

### 4. Run the matched simulations

**Question:** How do we isolate the model response to the intended map
parameter change?

Use the Tutorial02 `mek_default` configuration as the configuration reference.
Both cases use the same map geometry, runoff, input matrix, period
(2000-01-01 inclusive to 2001-01-01 exclusive), and configured initialization
procedure; the intended scientific input difference is the `CRIVHGT` map.
The specification uses a one-year start without a prior-year Restart, so
cold-start effects remain a limitation. Identical initialization procedures
do not guarantee identical physical states after changing channel geometry.

After implementation, provide the actual run commands, exact configuration
files and their paths, the one intended difference, expected output names,
logs to inspect, and a practical completion check. Do not copy Tutorial02's
multi-year Restart-connected state into this one-year example.

The model outputs are intended to be daily NetCDF for `outflw`, `rivdph`,
`sfcelv`, and `fldfrc`. The model's daily records are interval-end stamped in
the inspected Tutorial02 workflow: the first daily interval is labeled at the
following midnight. Confirm the actual Tutorial03 time coordinate and explain
which calendar day each record represents before finalizing commands or
captions. In particular, `sfcelv` is a snapshot, not a daily-mean water level.

**Checkpoint:** Apart from the river-depth map, which run conditions must be
matched for this comparison to be interpretable?

### 5. Read the four daily outputs

**Question:** What does each output measure, and what does its time record
represent?

Introduce the outputs using the runtime variable registry:

| Variable | Learner-facing meaning | Temporal meaning |
|---|---|---|
| `outflw` | River plus floodplain discharge (m³/s) | Mean over the output interval |
| `rivdph` | River-channel water depth (m) | Snapshot |
| `sfcelv` | Water-surface elevation (m) | Snapshot |
| `fldfrc` | Flooded fraction of a unit catchment (dimensionless) | Snapshot |

Once results exist, show tested commands to inspect variables, dimensions,
coordinates, units, fill values, and timestamps. Before computing differences,
check that both experiments have aligned grids, masks, and time coordinates.

**Figure 2 — Four model fields:** Show representative fields of the four
variables with units and suitable per-variable scales. State each panel's
date and temporal meaning. Do not call a `sfcelv` snapshot a daily mean or
label it a daily-mean maximum.

**Checkpoint:** Why can a daily mean discharge and a daily snapshot water
level not be interpreted as the same kind of daily value?

### 6. Compare river response through time

**Question:** Does channel water depth respond in the same way as absolute
water-surface elevation?

Explain the code-derived relationship
`sfcelv = elevtn - rivhgt + rivdph`. A deeper mapped channel lowers the
calculated bed elevation, so changes in `rivdph` alone do not determine the
sign of changes in `sfcelv`. Use only reference locations selected and
documented during implementation; identify them by reproducible coordinates
and map cells.

**Figure 3 — Matched time series:** Compare Control and Deep Channel at the
same selected locations and times. Label units, timestamps, and whether each
series is an interval mean or snapshot. Discuss actual results without
assuming a preferred sign.

**Checkpoint:** Could `rivdph` increase while `sfcelv` decreases? Explain
using the bed-elevation relationship.

### 7. From flooded fraction to regional flooded area

**Question:** Why does a flooded fraction not directly tell us the flooded
area of a region?

Explain that `fldfrc` is a dimensionless fraction and `ctmare` is
unit-catchment area in m². Cell contribution is `fldfrc × ctmare`; a regional
total is the sum over an explicitly defined set of valid catchments, divided
by 1,000,000 to report km². A cellwise fraction map and a physically
area-weighted regional total answer different questions.

The regional mask and boundary rule are unresolved. Do not show a regional
total or present a mask as canonical until the maintainer approves and the
implementation documents how catchments crossing the region boundary are
handled.

**Figure 4 — Fraction and area:** Once the mask is approved, show the cellwise
fraction comparison, cell flooded-area contribution where useful, and the
regional time series. Clearly document the mask, boundary rule, units, and
time semantics.

**Checkpoint:** If two cells have the same `fldfrc`, why can their flooded
area contributions differ?

### 8. Locate sensitivity and downscale a shared event

**Questions:** Where are the experiment differences largest? What extra
spatial detail does the 1-minute postprocessing provide?

For the common event, select the day of maximum Control daily-snapshot
`sfcelv` at the approved Phnom Penh-area coarse cell. Use precisely that date
for both experiments. Record the cell, coordinates, timestamp, value, and
tie rule. The reference cell and 1-minute crop (including required map
coverage/buffer) still need maintainer approval.

The existing downscaler uses high-resolution binary map inputs and daily
binary floodplain-depth input. A NetCDF input adapter for this example is
planned, not implemented or validated. The approved design avoids writing
redundant `flddph` model output: for the selected physics, derive
`flddph = max(rivdph - rivhgt, 0)` from `rivdph` and static `rivhgt`, respecting
their masks. Preserve the existing Binary workflow. Do not provide a runnable
NetCDF downscaling command until that adapter and its compatibility checks
exist. The downscaled product is postprocessing on finer terrain, not a
1-minute hydrodynamic simulation.

`prmwat` is optional coarse per-catchment permanent-water area in m². It is
not a high-resolution water-pixel mask and is not an input to the existing
downscaler. Treat it as separate context or omit it; do not subtract it from
flood fraction or downscaled depth without an approved method.

**Figure 5 — Spatial sensitivity:** Show selected, matched Deep Channel minus
Control diagnostics only after the quantities, date/statistic, masks, and
sign convention are defined. Use signed-difference scales and explain the
selected summary; the exact fields and summary dates remain to be decided.

**Figure 6 — One-minute event:** Once the NetCDF adapter is implemented and
verified, compare Control and Deep Channel on the shared event day using the
same extent, scales, masks, and high-resolution map. Explain what finer
spatial detail adds and that no independent observations are used to validate
it.

**Checkpoint:** What does the finer grid reveal about spatial pattern, and
what can it not establish about hydrologic accuracy?

### 9. Discuss findings and limitations

**Question:** Which conclusions follow from this comparison, and which would
go beyond its evidence?

After execution, summarize measured responses in discharge, channel depth,
water-surface elevation, flooded fraction/area, and downscaled depth. Discuss
the one-year cold start, selected event and locations, regional-mask choice,
downscaling assumptions, and absence of observational validation. Optional
exercises may vary the perturbation or analysis region, but must preserve the
baseline and clearly label each departure from the approved comparison.

## Figure plan

| Figure | Question answered | Required inputs and checks | Status |
|---|---|---|---|
| 1. River-depth map change | What was modified? | Baseline/variant `rivhgt`; same grid and mask; valid-cell `+3.0 m` | Planned |
| 2. Four model fields | What do the outputs represent? | Daily four-variable outputs; units and timestamp semantics | Planned |
| 3. River time series | How do the matched runs respond through time? | Same cells and times; means and snapshots distinguished | Planned |
| 4. Fraction and flooded area | How is flooded area derived and aggregated? | `fldfrc`, `ctmare`, approved regional mask and boundary rule | Planned; mask open |
| 5. Spatial sensitivity | Where and how do selected diagnostics differ? | Matched data, named statistic/date, masks, signed convention | Planned; selection open |
| 6. 1-minute event | What spatial detail does postprocessing add? | Shared Control-selected day, verified NetCDF adapter, map coverage | Planned; adapter open |

Every eventual caption should state the date or period, location/domain,
units, variable/time meaning, comparison convention, and material limitation.
Figures must be generated from saved workflow products; a plotting command
must not silently launch model runs or downscaling.

## Requirements for the eventual manual

For each practical step, document:

- the directory from which the tested command is run;
- required inputs, their source, and whether they are tracked or locally
  generated;
- exact command and configuration values as implemented;
- output paths and file roles, including temporary/intermediate products;
- a structural or scientific check that indicates whether the step succeeded;
- how to recover from observed errors, using only fixes verified during the
  workflow.

The final tutorial should include a compact reproducibility record: repository
revision, relevant software/environment versions, map and runoff identifiers,
experiment settings, chosen cells/region/event, and the verified run and
analysis steps. Structural success is not scientific validation. Preserve
source datasets and baseline parameters; do not imply that generated outputs
are distributed unless Git/package tracking confirms this.

## Open decisions and post-implementation evidence

| Item | What is needed | Why it matters |
|---|---|---|
| Phnom Penh reference cell | Researcher-approved cell/coordinates and tie rule | Event selection and time-series interpretation |
| Downscaling crop | Approved extent and high-resolution coverage/buffer check | Reproducible command and valid output domain |
| Flooded-area region | Approved catchment mask and boundary treatment | Makes Fig. 4 aggregation reproducible |
| Other dates and sites | Researcher-selected representative fields/time-series points | Fixes Fig. 2, 3, and 5 examples |
| Fig. 5 diagnostics | Variables/statistics and date or summary window | Ensures differences answer a defined question |
| NetCDF downscaler adapter | Implementation, input checks, preservation of Binary path, focused validation | Required before Fig. 6 is a runnable step |
| Actual workflow record | Commands, configs, output names, observed dimensions/times/values, failures, runtime/environment | Prevents invented instructions and results |

The scientific design already approves the `+3.0 m` perturbation, Tutorial02
Default configuration, daily output, one-year 2000 interval, shared event rule
based on the Control peak surface elevation near Phnom Penh, and one-minute
postprocessing. Keep those decisions intact. Resolve the open location, crop,
mask, and representative-analysis choices before the dependent implementation
or final manual claims are fixed.

## Finalization and learner validation

Write the learner-facing `workflow_example/README.md` only after each cited
command has been run from its documented starting directory and the resulting
files have been inspected. Then:

1. Check all relative links, script names, file paths, parameters, and output
   names against the repository.
2. Verify that the Control and Deep Channel inputs differ only as intended,
   and that all comparisons use aligned grids, masks, units, and times.
3. Confirm leap-day coverage and the actual daily time-coordinate convention.
4. Reproduce each figure from saved products and inspect its scientific
   labels, units, scales, and caption.
5. Have a new user follow the manual without undocumented help; record where
   they get stuck and correct the instructions based on observed issues.
6. Keep the design, implementation records, and learner manual distinct.

The Guide01 example should preserve the Chat AI draft as history and use this
reviewed design as the bridge to implementation. The manual itself should be
written from verified execution evidence, not from anticipated outcomes.
