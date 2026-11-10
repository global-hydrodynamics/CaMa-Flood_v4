# Codex 05 — Repository Review and Tutorial Design Discussion

**Example project:** Guide01 `workflow_example` — river-channel-depth sensitivity
and flood-inundation analysis  
**Purpose:** Record how the researcher and Codex refined the post-discussion
Chat AI design against the current CaMa-Flood repository before implementation.  
**Status:** Design document prepared; tutorial implementation and execution have
not started.

This is a concise development record, not a verbatim transcript. It records the
requests, evidence checks, and clarifications that shaped
[`workflow_example/tutorial_design.md`](../guide01_workflow_example/tutorial_design.md).
The earlier Chat AI draft and Codex prompt remain historical records and were
not rewritten.

## 1. Handoff from Chat AI to Codex

The researcher asked Codex to read
[`03draft_first_tutorial_design.md`](03draft_first_tutorial_design.md) and follow
[`04prompt_brushup_tutorial_design.md`](04prompt_brushup_tutorial_design.md).
The draft already contained researcher-approved scientific decisions:

- use the Mekong 15-minute map;
- compare Control with a uniform +3.0 m `rivhgt` perturbation on valid river
  cells;
- use the Tutorial02 `mek_default` configuration and change no other intended
  scientific input;
- simulate calendar year 2000, including leap day, with daily NetCDF output for
  `outflw`, `rivdph`, `sfcelv`, and `fldfrc`;
- keep map preparation, simulation, analysis, and visualization as separate
  stages; and
- use the Control experiment's 2000 peak daily `sfcelv` near Phnom Penh to
  select one date for 1-minute downscaling in both experiments.

The researcher clarified the output location to be
`tutorial/guide01_how_to_develop_tutorial/workflow_example/tutorial_design.md`.
The design must not be placed under
`tutorial/tutorial03-rivhgt_modification/`. The original Chat AI draft was to
remain unchanged. Codex was asked to refine technical details from repository
evidence only, leave unresolved scientific choices open, and not implement or
execute the tutorial.

## 2. Repository questions Codex investigated

Codex checked the current map files, registries, model source, Tutorial02
configuration, runoff sample, and existing flood-depth downscaling workflow.
Important findings recorded in the refined design include:

- `map/mekong_15min/params.txt` describes an 80 × 120, 0.25° grid covering
  90–110°E and 5–35°N. The inspected `netcdf/rivhgt.nc` stores Float32
  `rivhgt(lat,lon)` in metres with `-9999` missing values. Its missing mask
  matches `nextxy`; endpoint values mean `nextx > 0` is not a valid test for
  identifying all river cells.
- `src_main/cmf_ctrl_maps_mod.F90` calculates channel-bed elevation from
  `elevtn - rivhgt`; `D2SFCELV` combines channel-bed elevation and
  `rivdph`. Therefore, river depth and absolute water-surface elevation must
  not be interpreted as the same quantity.
- The current Tutorial02 `mek_default.sh` supplies the reference routing,
  floodplain, bifurcation, timestep, runoff, input-matrix, and daily NetCDF
  output settings. The annual sample runoff is daily `mm day-1`; the script
  uses `CROFFREQ="YEAR"`, `IFRQ_INP=24`, and `DROFUNIT=86400000`.
- `output_variables.csv` identifies `outflw` as a daily interval mean and
  `rivdph`, `sfcelv`, and `fldfrc` as snapshots. Output semantics must be
  described per variable, rather than labeling every output a daily mean.
- The model calculates flooded area as `D2GRAREA × D2FLDFRC`; this supports
  cellwise `fldfrc` comparisons and area-weighted regional totals, while the
  region mask and edge-catchment policy still require a decision.
- The existing `prmwat` product is aggregated permanent-water area per
  unit-catchment in m², not a 1-minute pixel mask. The existing downscaler uses
  high-resolution `rivwth` classifications instead; it does not consume
  `prmwat`.
- `etc/downscale_flddph/` contains the existing tributary-aware method. Its
  current Fortran executable reads a daily coarse Binary `flddph` record and
  high-resolution Binary map layers, then writes one Float32 high-resolution
  depth map per selected date. Its wrapper does not read NetCDF time metadata.

These are implementation observations from the checked worktree and files;
they do not establish scientific validation of the maps, forcing, or model
responses.

## 3. Downscaling clarification from the researcher

After Codex identified that the existing downscaler expects Binary `flddph`,
the researcher specified that the 1-minute downscaler should be made compatible
with NetCDF, following the existing Binary implementation. The researcher also
clarified that `flddph` can be derived from `rivdph - rivhgt`, so it should not
be added to the model output list; avoiding that redundant daily field reduces
output size.

Codex then checked `src_main/cmf_calc_fldstg_mod.F90`. Under the selected
Tutorial02 Default configuration (`LFPLAIN=.TRUE.`), the staging code defines
channel maximum storage from channel length, width, and `rivhgt`; once storage
exceeds that capacity, it represents the additional channel-equivalent depth
as `flddph`. Below bankfull, `flddph` is zero. The refined design therefore
specifies the nonnegative form:

```text
flddph = max(rivdph - rivhgt, 0)
```

The proposed NetCDF input path reads the requested daily `rivdph` time slice
and static map `rivhgt`, checks their grid and missing masks before arithmetic,
and passes the derived field to the existing tributary-aware calculation. It
keeps the established high-resolution Binary map inputs and per-day Float32
depth output, and preserves the existing Binary input path for compatibility.
This is a future implementation requirement, not a change made during this
design phase. It must be checked with small fixtures and against the staging
relation before regional use.

## 4. Choices that remain with the researcher

The repository cannot determine these scientific or presentation choices, so
they remain open in the design:

1. The reference coordinate or model cell that defines “near Phnom Penh” and
   therefore determines the Control peak-`sfcelv` event date.
2. The geographic crop for the 1-minute comparison, including coverage of the
   existing downscaler's five-model-cell buffer.
3. The lower-Mekong/delta catchment mask and treatment of catchments that cross
   its boundary.
4. Other point locations and dates/statistics for the sensitivity figures.
5. Whether `prmwat` should be used beyond separate contextual visualization.
   No subtraction from inundation is specified because its aggregated area
   representation is not a pixel mask.

The +3 m perturbation, Default physics, daily output cadence, 1-minute
resolution, and Control-based event-selection rule are already approved and
were not reopened. Expected physical responses remain hypotheses rather than
acceptance criteria.

## 5. Outcome and next step

Codex created
[`workflow_example/tutorial_design.md`](../guide01_workflow_example/tutorial_design.md)
as the repository-informed specification. The document separates verified
repository behavior, proposed workflow details, and open researcher decisions.
No simulation, downscaling run, analysis, or figure generation was performed;
no model source or existing tutorial file was changed. The next phase is to
review the open choices, then request implementation in small, testable stages.
