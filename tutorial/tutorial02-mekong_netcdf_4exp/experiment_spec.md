# Tutorial 02: Mekong Multi-Year NetCDF Simulation and Four-Experiment Comparison

**Status:** Researcher-approved four-experiment design, source-reviewed for implementation planning. No Tutorial 02 run or scientific result is claimed. Timestep suitability, station selection, spin-up interpretation, and full execution remain to be validated; see Appendix A.

## 1. Purpose and learning outcomes

This tutorial uses the CaMa-Flood v5 Mekong sample to demonstrate a four-year simulation with annual Restart handoffs and a controlled comparison of river routing, bifurcation flow, and floodplain representation. Users begin with the Default experiment, then copy its script and change a small number of clearly identified settings to create three variants.

By the end, users should be able to:

1. Find the map, runoff, input-matrix, experiment, and output directories used by a v5 run.
2. Read the experiment settings in a GOSH script and explain the main physical switches.
3. Run annual NetCDF runoff, model-output, and Restart workflows.
4. Carry model state across annual runs and retain each year’s results.
5. Create independent experiment variants by copying and editing a configuration.
6. Extract discharge at selected Mekong gauge locations and compare model simulations.
7. Compare spatial patterns of annual maximum daily-mean discharge.
8. Examine how initialization and annual runoff variability relate to differences between the spin-up year and later years.

The tutorial uses GRDC station metadata only to locate model grid cells. It does not use or compare observed discharge records.

## 2. Experiment design

The four configurations and their control settings are:

| Experiment | Routing | Floodplain storage representation | `PROJ` | `LKINE` | `LFPLAIN` | `LPTHOUT` | `LADPSTP` |
|---|---|---|---|---|---|---|---|
| Default | Local Inertial | Enabled | `mek_default` | `.FALSE.` | `.TRUE.` | `.TRUE.` | `.TRUE.` |
| No Bifurcation | Local Inertial | Enabled | `mek_nobif` | `.FALSE.` | `.TRUE.` | `.FALSE.` | `.TRUE.` |
| Kinematic Wave | Kinematic Wave | Enabled | `mek_kine` | `.TRUE.` | `.TRUE.` | `.FALSE.` | `.FALSE.` |
| No Floodplain | Kinematic Wave | Disabled | `mek_nofld` | `.TRUE.` | `.FALSE.` | `.FALSE.` | `.FALSE.` |

All four experiments use the same map, runoff, input matrix, simulation dates, output settings, `DT=600` seconds, and other physical parameters. The researcher-approved scheme-specific options are:

- `DT=600` seconds in every experiment.
- `LLEAPYR=.TRUE.` to keep the model calendar aligned with the Gregorian CF runoff time axis, including February 29, 2000.
- `LADPSTP=.TRUE.` for the two Local Inertial cases and `.FALSE.` for the two Kinematic Wave cases. The source warns that adaptive stepping is recommended only with Local Inertial routing; Kinematic Wave should use a suitable fixed step.
- `LPTHOUT=.TRUE.` for Default and `.FALSE.` for the other three cases. Source validation permits bifurcation with Local Inertial routing and stops if it is combined with Kinematic Wave.
- Keep `LFLDOUT=.TRUE.` across all four runs. `LFPLAIN` controls whether floodplain capacity is represented during map initialization; `LFLDOUT` is a separate high-water channel-flow switch. No Floodplain changes `LFPLAIN` only.
- Keep `LSLPMIX=.FALSE.`, reservoir operation off, and all other optional schemes at the same values in every run.

The source accepts `DT=600`: `CMF_CONFIG_CHECK` requires a timestep of at least 60 seconds and an integer multiple of 60. It warns, rather than stopping, if adaptive time stepping is enabled with Kinematic Wave. Actual stability and suitability of the approved timestep/options still require execution evidence.

The Kinematic Wave routing code computes channel discharge from Manning velocity using the local water depth and a slope bounded below by `PMINSLP`; available channel storage limits discharge during a timestep. It can also route floodplain flow when `LFLDOUT` is enabled. The No Floodplain experiment turns off the floodplain storage representation through `LFPLAIN=.FALSE.` while retaining the Kinematic Wave routing configuration.

Interpret the comparisons as follows:

- **Default vs. No Bifurcation:** both use Local Inertial routing, floodplain representation, adaptive time stepping, and `DT=600`; only bifurcation flow changes. This comparison examines bifurcation routing.
- **No Bifurcation vs. Kinematic Wave:** both have floodplain representation and bifurcation flow off, but routing formulation and adaptive-time-step setting both change. This is not a perfectly isolated routing-equation comparison.
- **Kinematic Wave vs. No Floodplain:** both use Kinematic Wave, no bifurcation, and fixed time stepping; only `LFPLAIN` changes. This examines floodplain storage representation within that configuration.
- **Default vs. Kinematic Wave** and **Default vs. No Floodplain** change multiple settings and must not be interpreted as isolating one physical mechanism.

Distinguish changes to physical formulations (`LKINE`, `LPTHOUT`, `LFPLAIN`) from the numerical time-stepping choice (`LADPSTP`).

Do not predict the sign or magnitude of discharge differences. Interpret them only after execution and inspection of the results.

## 3. Domain and input data

### 3.1 River map

Use the existing map at `map/mekong_15min/` without copying its products into the tutorial directory. `params.txt` describes an 80 × 120 grid at 0.25 degrees, with edge bounds 90–110°E and 5–35°N.

Use the individual NetCDF map products under `map/mekong_15min/netcdf/` with `LMAPCDF=.FALSE.` (individual-file mode):

| Namelist setting | File | Role |
|---|---|---|
| `CNEXTXY` | `nextxy_noedge.nc` | River-network downstream indices |
| `CGRAREA` | `ctmare.nc` | Unit-catchment area |
| `CELEVTN` | `elevtn.nc` | Channel-top elevation |
| `CNXTDST` | `nxtdst.nc` | Distance to downstream catchment |
| `CRIVLEN` | `rivlen.nc` | River length within the catchment |
| `CFLDHGT` | `fldhgt.nc` | Layered floodplain elevation profile |
| `CRIVWTH` | `rivwth_merit.nc` | MERIT-derived river width |
| `CRIVHGT` | `rivhgt.nc` | River-channel depth |
| `CRIVMAN` | `rivman.nc` | River Manning roughness |

`params.txt`, the runoff-grid descriptor `inpdim_test-mek_1deg.txt`, and the map-specific `inpmat_test-mek_1deg.bin` remain text/Binary support inputs. Reuse the existing 20 × 30 runoff-to-map input matrix; do not regenerate or duplicate it for the NetCDF runoff representation. `bifprm.nc` is required for Default (`LPTHOUT=.TRUE.`) and not required for the other three experiments.

`LMAPCDF=.TRUE.` selects the separate legacy consolidated NetCDF map interface and must not be used for these individual files.

### 3.2 Runoff

Use the annual NetCDF dataset `inp/test-mek_1deg_nc/`, derived without rescaling from the tracked daily Binary sample `inp/test-mek_1deg/`. The NetCDF runoff field is `runoff(time, latitude, longitude)`, float32, daily `mm day-1`, on a 20 × 30 grid covering the same 90–110°E, 5–35°N extent. The files have CF daily time bounds and a `proleptic_gregorian` calendar. `DROFUNIT=86400000` converts the stored daily depth units to the model’s `m/s` forcing rate; `CROFFREQ="YEAR"`, `CVNTIME="time"`, and `CVNROF="runoff"` select calendar-year files and records. Use the Binary input matrix above.

Annual inputs are `runoff2000.nc` through `runoff2003.nc`. The current branch tracks NetCDF files for 2000–2005, so all four tutorial years are available in a clean checkout. The existing converter supports generating later years from the tracked daily Binary source; the 2006–2009 files are not needed for this tutorial.

### 3.3 Annual simulation and Restart sequence

Run one calendar year per model invocation, with an exclusive January 1 endpoint:

| Segment | Start | Exclusive end | Runoff records |
|---|---|---|---:|
| Spin-up | 2000-01-01 00:00 | 2001-01-01 00:00 | 366 |
| Analysis 1 | 2001-01-01 00:00 | 2002-01-01 00:00 | 365 |
| Analysis 2 | 2002-01-01 00:00 | 2003-01-01 00:00 | 365 |
| Analysis 3 | 2003-01-01 00:00 | 2004-01-01 00:00 | 365 |

For the 2000 segment, set `LRESTART=.FALSE.`. The current initialization first zeros prognostic storage arrays, then initializes river-channel storage along the network so water surface does not fall below the downstream boundary elevation. Therefore, describe this as a cold start without a Restart, not as a guaranteed all-zero storage state.

For 2001–2003, set `LRESTART=.TRUE.` and read the preceding segment’s final Restart. Use `LRESTCDF=.TRUE.` and `IFRQ_RST=0` to write a NetCDF Restart at the end of each segment. With `CVNREST="restart"` and the output directory as `CRESTDIR`, the expected handoff names are `restart2001010100.nc`, `restart2002010100.nc`, and `restart2003010100.nc`. The Restart reader restores river and floodplain storage and the previous-step hydraulic state required by a Normal Restart; enabled scheme states are additionally included when their options are on.

The final endpoint is 2004-01-01 00:00 exclusive. A four-year uninterrupted timeline is the design target, but annual boundary continuity must be checked from actual run logs, Restart read/write evidence, and output timestamps. No four-year run has been executed for this specification.

## 4. Output and comparison methods

### 4.1 Discharge output

Write only `outflw`, the river-plus-floodplain discharge mean in `m3/s`, directly through the model’s NetCDF output path. Use `LOUTCDF=.TRUE.`, `LOUTVEC=.FALSE.`, and `IFRQ_OUT=24`. The registry marks `outflw` as a time mean; `CMF_OUTPUT_WRITE` selects `D2OUTFLW_oAVG` and the NetCDF writer records `cell_methods="time: mean"`. The output variable is float32 in NetCDF-4 with the configured compression level. Annual maximum maps therefore represent the maximum daily mean discharge in each year, not an instantaneous or sub-daily peak.

Set a year-specific `COUTTAG` for each annual run. The current filename rule is `o_` + variable name + tag + `.nc`, so a run with `COUTTAG="2000"` writes `o_outflw2000.nc`. Keep all files for each experiment in its independent directory:

```text
tutorial/tutorial02-mekong_netcdf_4exp/out/
├── mek_default/
├── mek_nobif/
├── mek_kine/
└── mek_nofld/
```

Each experiment directory holds its annual discharge files, dated Restarts, year-specific Namelists, and logs. The repository-root `out/` directory is not used. Generated run artifacts remain untracked.

The model labels daily averages at the end of their averaging interval. Since `CMF_TIME_NEXT` advances to the end time before diagnostics are written, the first daily record is time-stamped 2000-01-02 00:00 and represents the interval beginning 2000-01-01; the final record in a segment is stamped at the exclusive January 1 endpoint. A visualization should label or shift these interval-ending timestamps consistently rather than treating them as daily midpoint values.

Each annual output is expected to contain 366 records for 2000 and 365 for each of 2001–2003. The intended total is 1,461 records per experiment, or 16 annual NetCDF discharge files across four experiments. These are expected contracts, not execution-verified results.

### 4.2 GRDC model locations

Use GRDC gauge records as model-location references only. Select approximately five allocated Mekong River stations spanning upstream, middle, and downstream reaches and a range of mapped upstream areas. `GRDC_alloc.txt` provides the allocated one-based map indices (`ix1`, `iy1`) and `area_CaMa`; `map/data/GRDC_allocated.csv` provides station name, river name, country, and source location. `Type` in the allocated map table is the number of substitute upstream catchments, not a mainstem/tributary classification.

The following are source-backed candidate locations on the Mekong River, not a final scientifically reviewed site selection:

| GRDC ID | Station | `GRDC_alloc.txt` cell `(ix1,iy1)` | `area_CaMa` (km2) | Role to review |
|---|---|---:|---:|---|
| `2469050` | Luang Prabang (Viengkham) | (49, 61) | 272,205.22 | Upper Mekong |
| `2469072` | Near Vientiane | (51, 69) | 304,814.28 | Upper-middle reach |
| `2469102` | Thakhek | (59, 71) | 373,389.03 | Middle reach |
| `2469261` | Ban Chan Noi | (64, 83) | 552,168.25 | Lower reach above Cambodia |
| `2569002` | Phnom Penh (Chroui Changvar) | (62, 93) | 663,000.25 | Downstream reach |

The table demonstrates that usable allocated records exist; it does not establish that these five locations best represent floodplain influence or are all independent hydrologic sites. Final location selection should be reviewed before tutorial scripts and figures are produced. No observed discharge is included.

### 4.3 Spin-up visualization and interpretation

For the local-inertial reference only, plot daily discharge for 2000–2003 at the selected locations on a common January–December seasonal axis. The educational hypothesis is that the no-Restart start may produce an initial adjustment that differs from later years. The source confirms a cold start followed by downstream-boundary channel initialization, but whether a visible transient occurs, how long it persists, and whether one year is sufficient cannot be established without simulation.

The 2000 input and model calendar include February 29. To compare equivalent month/day positions on a common 365-day axis, the visualization should document a leap-day policy; the proposed policy is to omit February 29 from the seasonal overlay while retaining all 366 model records in the underlying output. Keep interannual runoff differences separate from any inferred initialization effect. Do not claim the spin-up hypothesis was demonstrated until the four annual outputs have been inspected.

### 4.4 Four-experiment hydrograph comparison

Compare daily `outflw` over 2001–2003 at the reviewed gauge locations. Include all four experiments, using separate panels if overlaying four series obscures the intended comparisons. Report the variables and dates actually used, and inspect missing/non-finite values before plotting. The comparison shows differences among configured simulations; it does not establish which is more accurate because observed discharge is not used.

### 4.5 Spatial comparison of annual maximum discharge

For each experiment, derive an annual-maximum map from its daily `outflw` NetCDF for 2001, 2002, and 2003. At each river grid cell, calculate the maximum of the daily mean discharge records belonging to that calendar year; retain `outflw` units of `m3/s`. This statistic compares spatial expression of the configurations and complements the selected-location hydrographs. It is not an instantaneous flood peak.

The NetCDF time coordinate labels each daily mean at the end of its averaging interval; the current writer does not define a time-bounds variable. Annual files end at the exclusive January 1 boundary. Assign records to the year of the represented daily interval (or use the known annual file/record range), not naively to the timestamp's calendar date; the final interval-ending January 1 record belongs to the preceding calendar year.

The primary visualization may use one panel per experiment for each year, or a 3 × 4 layout with years as rows and experiments as columns. Optional signed difference maps may show Default − No Bifurcation, No Bifurcation − Kinematic Wave, and Kinematic Wave − No Floodplain. These are secondary analyses; label pair order and units clearly and interpret them using the comparison limitations in Section 2. Mask missing/non-river cells consistently with the source outputs.

## 5. Tutorial structure and deliverables

The future beginner-facing README should guide users to inspect the tutorial and input paths, review and run the Default configuration, check that annual output and Restart files were produced, copy the Default script to create the three variants, and edit `PROJ`, `LKINE`, `LFPLAIN`, `LPTHOUT`, and `LADPSTP` as specified in Section 2. It should then guide them through the hydrograph and annual-maximum map comparisons. Commands, sample terminal output, expected files, and troubleshooting guidance must be based on the implemented scripts and actual validation runs.

Planned GitHub development contents are:

- `experiment_spec.md`, `README.md`,
- `mek_default.sh`, `mek_nobif.sh`, `mek_kine.sh`, and `mek_nofld.sh`,
- visualization scripts and verified reference figures.

The generated model output, Restart files, and logs are excluded from Git. Scripts, figures, and tutorial README are not part of this specification task; see Appendix B for package policy.

## Appendix A. Technical verification and open questions

### A.1 Verification table

| Setting or requirement | Verified behavior | Evidence | Status |
|---|---|---|---|
| `LKINE` routing | `CMF_PHYSICS_ADVANCE` selects `CMF_CALC_OUTFLW_KINE`; the routine applies a Manning kinematic relation to river depth and slope, with a minimum-slope bound and available-storage discharge cap. | `src_main/cmf_ctrl_physics_mod.F90` (`CMF_PHYSICS_ADVANCE`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| `LFPLAIN` | `CMF_TOPO_INIT` sets river storage capacity to a very large value when false, instead of the channel cross-section capacity used when true. The Kinematic routine separately controls floodplain discharge with `LFLDOUT`. | `src_main/cmf_ctrl_maps_mod.F90` (`CMF_TOPO_INIT`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| Four experiment option compatibility | `LKINE=.TRUE.` with `LPTHOUT=.TRUE.` stops configuration. Adaptive timestep with Kinematic Wave warns but does not stop. `DT=600` passes the configured minimum and minute-multiple checks. | `src_main/cmf_ctrl_nmlist_mod.F90` (`CMF_CONFIG_CHECK`) | Code-verified; researcher-approved configuration, runtime stability pending |
| Cold-start state | `CMF_PROG_INIT` zeros prognostic arrays, then `STORAGE_SEA_SURFACE` fills river storage to avoid an initial water surface below downstream boundary level. | `src_main/cmf_ctrl_vars_mod.F90` (`CMF_PROG_INIT`, `STORAGE_SEA_SURFACE`) | Code-verified |
| Annual Restart continuity | A Normal Restart restores river/floodplain storage and previous-step flow/depth fields; optional scheme states are conditional. End-only Restart is selected by `IFRQ_RST=0`. | `src_main/cmf_ctrl_restart_mod.F90` (`CMF_RESTART_INIT`, `CMF_RESTART_WRITE`, `RESTART_UTIL_OPEN_WRITE`); `src_main/cmf_drv_advance_mod.F90` | Code-verified; four-year continuity pending execution |
| Runoff NetCDF | Grouping `YEAR` resolves `runoffYYYY.nc`; the reader matches CF time metadata, verifies calendar/interval/coverage and requires the first time record to start at the year boundary. The variable is read as `(NXIN,NYIN,time)` through the Fortran NetCDF interface. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_RESOLVE_FILE`, `FORCING_UTIL_GET_CDF`); `inp/test-mek_1deg_nc/README.md`; `runoff_datasets.csv` | Code-verified; 2000–2005 files tracked, including all Tutorial 02 years |
| Runoff units and matrix | Runoff is converted by dividing by `DROFUNIT`; `CINPMAT` supplies the spatial weights. The annual NetCDF values are documented as daily `mm day-1`; use `86400000` and the existing Binary matrix. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_ROFF_INTERP`); `inp/test-mek_1deg_nc/README.md`; `map/mekong_15min/inpdim_test-mek_1deg.txt` | Code- and data-verified |
| Individual NetCDF map inputs | `LMAPCDF=.FALSE.` selects per-file extension-based readers. The nine map files listed in Section 3.1 are present in the map's NetCDF directory; the dimension descriptor and input matrix are separate support files. | `src_main/cmf_ctrl_maps_mod.F90` (`CMF_MAPS_NMLIST`, `CMF_RIVMAP_INIT`, `CMF_TOPO_INIT`, `MAP_UTIL_READ_*`); `map/mekong_15min/netcdf/`; `map/mekong_15min/params.txt` | Code- and data-verified; Tutorial 02 use pending execution |
| Daily mean `outflw` | Registry metadata selects a mean; `CMF_OUTPUT_WRITE` writes `D2OUTFLW_oAVG`. The NetCDF writer uses float32, `time: mean`, and file pattern `o_` + name + tag + `.nc`. Output time is calculated at `KMINNEXT`, the end of the step/averaging interval. | `variable_table/output_variables.csv`; `src_main/cmf_ctrl_output_mod.F90` (`CMF_OUTPUT_INIT`, `CMF_OUTPUT_WRITE`, `OUTPUT_UTIL_CREATE_OUTCDF`, `OUTPUT_UTIL_WRITE_MAP`); `src_main/cmf_drv_advance_mod.F90`; `src_main/cmf_ctrl_time_mod.F90` | Code-verified; annual count/time labels pending execution |
| Annual maximum map statistic | Maximum is taken across daily mean `outflw` records per calendar year. Annual output files contain one calendar year's intervals; interval-ending Jan 1 records belong to the prior year's final day. | Sections 4.1 and 4.5; `src_main/cmf_ctrl_output_mod.F90`; `src_main/cmf_ctrl_time_mod.F90` | Analysis design specified; implementation and output-based validation pending |
| GRDC station allocations | Valid Mekong river records and allocated cell indices are present; `Type` counts substitute catchments, not river-order categories. Five candidate station names and cells are listed in Section 4.2. | `map/mekong_15min/GRDC_alloc.txt`; `map/data/GRDC_allocated.csv`; `map/mekong_15min/script_params/README.md` | Data-verified; final selection pending maintainer review |
| Spin-up effect | The source determines initialization behavior but cannot establish a transient's presence, duration, or interpretive strength. | `src_main/cmf_ctrl_vars_mod.F90`; Section 4.3 | Pending simulation and scientific review |

### A.2 Decisions and unresolved prerequisites

1. **Fixed timestep adequacy:** The researcher specified `DT=600`, adaptive stepping for Local Inertial, and fixed stepping for Kinematic Wave. The source accepts these combinations, but stability and scientific suitability across the full map require execution validation.
2. **Gauge set:** Review whether the five candidate sites provide appropriate upstream-to-downstream coverage and useful floodplain contrasts before extracting figures.
3. **Spin-up interpretation:** Determine from actual outputs whether initialization effects are visible and whether a one-year spin-up is sufficient; separate these effects from interannual runoff variation.
4. **Annual-maximum maps:** Confirm the implementation assigns interval-ending records to their represented calendar year, masks non-river/missing cells correctly, and preserves the stated `m3/s` units.
5. **Full execution verification:** Build the NetCDF-enabled model, run all 16 annual experiment segments, validate Restart handoffs and daily time axes, inspect 1,461 records per experiment, and generate hydrograph and annual-maximum comparisons. No run or figure is validated yet.

## Appendix B. Development, distribution, and Benchmark integration

### B.1 GitHub and beginner package

The intended GitHub tutorial retains all four experiment scripts, the specification and README, visualization tooling, and reference figures only after they are generated from verified runs. Generated output, Restart files, and logs remain untracked.

The beginner package includes only `mek_default.sh` among the experiment scripts, plus tutorial documentation, visualization tools, and verified reference figures. It excludes `mek_nobif.sh`, `mek_kine.sh`, and `mek_nofld.sh` so learners create the variants by copying and editing Default settings. This is a teaching choice, not a packaging limitation. No package assembly change is made by this specification.

### B.2 Optional Benchmark v2 use

The 2001–2003 discharge series may be considered as a small Benchmark v2 dataset after Benchmark v2's scientific and data contracts have been reviewed. No extraction, tracking, or Benchmark integration is authorized here; the tutorial remains complete without that downstream use.
