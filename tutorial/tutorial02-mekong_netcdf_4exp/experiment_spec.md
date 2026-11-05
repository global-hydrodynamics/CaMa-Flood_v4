# Tutorial 02: Mekong Multi-Year NetCDF Simulation and Four-Experiment Comparison

**Status:** Researcher-approved four-experiment design, implemented and run for 2000–2003, with output-based checks and reference figures. This is a model-behavior comparison, not an observational validation. Scientific interpretation of the selected 15-minute cells and spin-up adequacy remains for maintainer review; see Appendix A.

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
9. Examine a Mun River tributary, Tonle Sap flow-reversal potential, and delta bifurcation sensitivity at selected model cells.

The tutorial uses GRDC and MRC station metadata only to locate model grid cells. It does not use or compare observed discharge records.

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

The final endpoint is 2004-01-01 00:00 exclusive. All four experiments completed the four annual segments. Each 2001–2003 log records reading the preceding year's NetCDF Restart, and the corresponding year-end Restart was written. Output interval timestamps map to an uninterrupted 2000-01-01 through 2004-01-01 daily sequence with no gaps or duplicates. See Appendix A.3 for the run evidence.

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

Each annual output contains 366 records for 2000 and 365 for each of 2001–2003: 1,461 records per experiment and 16 annual NetCDF discharge files total. All files have shape `(time, latitude, longitude) = (days, 120, 80)`, `outflw` units `m3/s`, and finite unmasked values. The first record is stamped January 2 and represents January 1; the last is stamped the exclusive January 1 boundary and represents December 31.

### 4.2 Selected model locations

These five researcher-selected sites are simulation-output sampling locations, not observation-validation sites. GRDC allocations supply four station locations and their one-based model indices. Can Tho has no selected GRDC allocation here; its geographic reference is the Mekong River Commission (MRC) Water Quality Monitoring station H039803. That report labels the site as Bassac River; this tutorial refers to the corresponding Hau/Bassac delta reach. No observed discharge series is used.

| Location | River / objective | Location source | Model cell `(ix,iy)` | Model-cell center (lon, lat) | Mapped drainage area (km²) |
|---|---|---|---:|---:|---:|
| Vientiane | Mekong mainstream; upper-reach discharge and flood-wave propagation | GRDC 2469072; allocated coordinates 102.613°E, 17.929°N | (51, 69) | 102.625°E, 17.875°N | 304,814.28 |
| Ubon Ratchathani | Mun River tributary; tributary response | GRDC 2969200; allocated coordinates 104.863°E, 15.221°N; allocation metadata identifies the Mun | (60, 80) | 104.875°E, 15.125°N | 106,544.10 |
| Stung Treng | Mekong mainstream in Cambodia; large basin after major tributary contributions | GRDC 2569005; allocated coordinates 105.946°E, 13.537°N | (64, 86) | 105.875°E, 13.625°N | 637,963.19 |
| Prek Kdam | Tonle Sap River; investigate modeled reverse-flow potential | GRDC 2569020; allocated coordinates 104.804°E, 11.821°N; allocation metadata identifies Tonle Sap | (60, 93) | 104.875°E, 11.875°N | 87,707.56 |
| Can Tho | Hau/Bassac delta reach; inspect bifurcation sensitivity | MRC WQ station H039803, 105.79775°E, 10.0580°N | (64, 100) | 105.875°E, 10.125°N | 773,786.94 |

Cell centers and downstream indices were read from the Mekong 15-minute map. At all five cells the individual NetCDF `nextx`/`nexty` values match `nextxy_noedge.bin`; each selected cell has a valid downstream connection. For the four GRDC sites, the table reports the allocation file's `area_CaMa`; for Can Tho, it reports the CaMa-Flood analysis tool's drainage area at the selected cell. These are upstream drainage areas, not the local `ctmare` unit-catchment areas. At Ubon, the chosen allocated cell is identified as the Mun tributary and drains to the next cell (61,79), distinct from the Mekong mainstream location. Prek Kdam drains first to (60,94), where mapped upstream area increases from about 87,708 to 751,186 km²; following `nextxy` downstream reaches the selected Can Tho cell (64,100) after seven links. This confirms a modeled connection through the Tonle Sap/delta network, not a detailed validation of river geometry.

The 0.25-degree grid is coarse relative to station locations. The allocated/model-cell center differs from the station coordinate by a fraction of a cell, and the map's catchment outlet location can differ further from the station point. Can Tho's geographic coordinate and selected model-cell center are deliberately listed separately. The model grid represents this as a delta reach rather than a point gauge or tidal station.

For Can Tho, `bifprm.csv` contains bifurcation paths 337 and 338 beginning at selected cell (64,100), with downstream destinations (65,100) and (65,101). This establishes that the selected cell is directly associated with configured branches, but only the Default/No Bifurcation output comparison can establish the simulated effect there. It does not establish agreement with real distributary flow.

The local-inertial routing computes signed flow in the direction of the map's downstream connection, so a negative `outflw` can represent reverse flow. The Kinematic Wave implementation constrains slope to a positive minimum before calculating downstream flow; it is therefore not expected to represent reversal in the same way. Whether negative flow occurs at Prek Kdam in these runs remains an empirical question. These sites represent the researcher's intended reach contrasts, but their suitability for broader floodplain inference and the ability of this coarse map to resolve local river geometry remain limitations for scientific review. Geographic source coordinates are not discharge observations.

The Can Tho coordinate and Bassac River label are from the [MRC 2018 Lower Mekong Water Quality Monitoring Report](https://www.mrcmekong.org/publications/2018-lower-mekong-water-quality-monitoring-report/). The [MRC PMFM overview](https://pmfm.mrcmekong.org/?showtemplate=1) describes the Tonle Sap River's seasonal reverse-flow connection to the Mekong. These sources establish site context only; neither supplies observed discharge to this tutorial.

### 4.3 Spin-up visualization and interpretation

For the local-inertial reference only, the generated figure plots daily discharge for 2000–2003 at the selected locations on a common January–December seasonal axis. At Vientiane and the other sites, the seasonal curves vary substantially between years; the 2000 curve also differs during parts of the rising/falling seasons. These runs do not isolate initial-condition adjustment from interannual runoff differences, so they do not establish how long a transient lasts or whether one year is sufficient.

The 2000 input and model calendar include February 29. The seasonal overlay omits February 29 and maps the other dates to a common non-leap calendar; the source NetCDF retains all 366 daily records. Interannual runoff differs, so this visualization is descriptive and does not demonstrate spin-up sufficiency.

### 4.4 Four-experiment hydrograph comparison

The generated comparison uses daily `outflw` for 2001–2003 at all five selected model cells, with separate panels by site and year. The unmasked values are finite and the files share the same 120 × 80 coordinates. At Prek Kdam, negative daily means occur in Default (302 of 1,095 days) and No Bifurcation (312 days), while no negative values occur in Kinematic Wave or No Floodplain. This is a simulated result at the selected Tonle Sap cell, not validation against observed reverse-flow discharge.

At Can Tho cell (64,100), Default and No Bifurcation differ on all 1,095 daily records in 2001–2003. By year, their maximum absolute differences are 21,504, 20,243, and 12,792 m³/s; mean absolute differences are 6,720, 5,710, and 3,083 m³/s. The cell begins two configured bifurcation paths, so the output difference is consistent with local bifurcation sensitivity, though these runs do not prove the real-world branch partition is represented accurately. Ubon's selected Mun tributary series remains distinct from the mainstream series; Vientiane and Stung Treng show seasonal mainstream hydrographs with large wet-season peaks. These are comparisons among configured simulations only, not accuracy evaluations.

### 4.5 Spatial comparison of annual maximum discharge

For each experiment, derive an annual-maximum map from its daily `outflw` NetCDF for 2001, 2002, and 2003. At each river grid cell, calculate the maximum of the daily mean discharge records belonging to that calendar year; retain `outflw` units of `m3/s`. This statistic compares spatial expression of the configurations and complements the selected-location hydrographs. It is not an instantaneous flood peak.

The NetCDF time coordinate labels each daily mean at the end of its averaging interval; the current writer does not define a time-bounds variable. Annual files end at the exclusive January 1 boundary. Assign records to the year of the represented daily interval (or use the known annual file/record range), not naively to the timestamp's calendar date; the final interval-ending January 1 record belongs to the preceding calendar year.

The generated figure uses a 3 × 4 layout with years as rows and experiments as columns, one shared logarithmic scale from 1 to the largest observed annual maximum (94,305 m³/s), and a common river-cell mask. The largest annual maximum occurs in No Floodplain in 2002. This configuration also has higher spatial 99th-percentile maxima than the other experiments in all three comparison years. The maps show broad shared river-network patterns and spatial differences concentrated along some reaches; they do not identify which configuration is more accurate. Optional signed difference maps remain secondary and were not generated.

## 5. Tutorial structure and deliverables

The future beginner-facing README should guide users to inspect the tutorial and input paths, review and run the Default configuration, check annual output and Restart files, copy Default to create the three variants, and edit `PROJ`, `LKINE`, `LFPLAIN`, `LPTHOUT`, and `LADPSTP` as specified in Section 2. It should then guide them through the hydrograph and annual-maximum map comparisons. The current scripts and figure workflow have been executed; learner-facing commands, sample output, and troubleshooting text still need to be written separately.

The implemented GitHub development contents are:

- `experiment_spec.md`, `README.md`,
- `mek_default.sh`, `mek_nobif.sh`, `mek_kine.sh`, and `mek_nofld.sh`,
- visualization scripts and verified reference figures.

Generated model output, Restart files, and logs are excluded from Git. The four scripts and figures are included in this implementation phase. The beginner-facing tutorial README remains a separate later task; see Appendix B for package policy.

## Appendix A. Technical verification and open questions

### A.1 Verification table

| Setting or requirement | Verified behavior | Evidence | Status |
|---|---|---|---|
| `LKINE` routing | `CMF_PHYSICS_ADVANCE` selects `CMF_CALC_OUTFLW_KINE`; the routine applies a Manning kinematic relation to river depth and slope, with a minimum-slope bound and available-storage discharge cap. | `src_main/cmf_ctrl_physics_mod.F90` (`CMF_PHYSICS_ADVANCE`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| `LFPLAIN` | `CMF_TOPO_INIT` sets river storage capacity to a very large value when false, instead of the channel cross-section capacity used when true. The Kinematic routine separately controls floodplain discharge with `LFLDOUT`. | `src_main/cmf_ctrl_maps_mod.F90` (`CMF_TOPO_INIT`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| Four experiment option compatibility | `LKINE=.TRUE.` with `LPTHOUT=.TRUE.` stops configuration. Adaptive timestep with Kinematic Wave warns but does not stop. `DT=600` passes the configured minimum and minute-multiple checks; the approved combinations completed the sample runs. | `src_main/cmf_ctrl_nmlist_mod.F90` (`CMF_CONFIG_CHECK`); 16 run logs | Code- and runtime-verified for this sample; not a general stability guarantee |
| Cold-start state | `CMF_PROG_INIT` zeros prognostic arrays, then `STORAGE_SEA_SURFACE` fills river storage to avoid an initial water surface below downstream boundary level. | `src_main/cmf_ctrl_vars_mod.F90` (`CMF_PROG_INIT`, `STORAGE_SEA_SURFACE`) | Code-verified |
| Annual Restart continuity | Each experiment wrote NetCDF Restart files at 2001–2004 Jan 1 boundaries; logs for 2001–2003 show reading the preceding boundary file. | `src_main/cmf_ctrl_restart_mod.F90`; `out/<experiment>/log_CaMa_*.txt`; restart NetCDF signatures | Four-year annual handoffs runtime-verified |
| Runoff NetCDF | Grouping `YEAR` resolves `runoffYYYY.nc`; the reader matches CF time metadata, verifies calendar/interval/coverage and requires the first time record to start at the year boundary. The variable is read as `(NXIN,NYIN,time)` through the Fortran NetCDF interface. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_RESOLVE_FILE`, `FORCING_UTIL_GET_CDF`); `inp/test-mek_1deg_nc/README.md`; `runoff_datasets.csv` | Code-verified; 2000–2005 files tracked, including all Tutorial 02 years |
| Runoff units and matrix | Runoff is converted by dividing by `DROFUNIT`; `CINPMAT` supplies the spatial weights. The annual NetCDF values are documented as daily `mm day-1`; use `86400000` and the existing Binary matrix. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_ROFF_INTERP`); `inp/test-mek_1deg_nc/README.md`; `map/mekong_15min/inpdim_test-mek_1deg.txt` | Code- and data-verified |
| Individual NetCDF map inputs | `LMAPCDF=.FALSE.` selects per-file extension-based readers. The nine map files loaded in every annual segment are NetCDF; the dimension descriptor and input matrix remain text/Binary support inputs. | `src_main/cmf_ctrl_maps_mod.F90`; four experiments' run logs; `map/mekong_15min/netcdf/` | Code-, data-, and runtime-verified |
| Daily mean `outflw` | Registry metadata selects a mean; `CMF_OUTPUT_WRITE` writes `D2OUTFLW_oAVG`. NetCDF output uses float32, `time: mean`, and the `o_` + name + tag + `.nc` naming rule. Actual files have expected 366/365 records, `m3/s`, and interval-ending timestamps. | `variable_table/output_variables.csv`; `src_main/cmf_ctrl_output_mod.F90`; 16 annual output files | Code- and runtime-verified |
| Annual maximum map statistic | The visualization assigns each end-stamped record to the represented daily interval and computes cellwise maxima for 2001–2003. Outputs share coordinates and a river-cell mask; the plotted common logarithmic range includes the observed maximum. | `v03-annual_max_discharge.py`; generated figure; all 12 comparison files | Implemented and output-verified |
| Selected sampling locations | Four GRDC records supply allocated one-based cells; Can Tho uses the MRC WQ station coordinate and a separate model cell. NetCDF downstream indices match the Binary reference at each selected cell; bifurcation paths 337/338 start at Can Tho cell (64,100). | Section 4.2; GRDC allocation files; map NetCDF/Binary; `bifprm.csv`; MRC 2018 WQ report | Geographic locations and model-grid/network mapping verified; scientific interpretation remains open |
| Spin-up effect | The four years ran and the seasonal overlay was generated. Interannual runoff differs, preventing the plotted differences from isolating an initial-condition transient or establishing spin-up sufficiency. | `v01-spinup_comparison.py`; Default annual output | Runtime complete; scientific interpretation unresolved |

### A.2 Decisions and unresolved prerequisites

1. **Interpretation of selected sites:** The five researcher-preferred cells have verified map connectivity and generated hydrographs. The maintainer should review whether the 0.25-degree cells adequately represent the intended Mun, Tonle Sap, mainstream, and Hau/Bassac scientific contrasts.
2. **Spin-up adequacy:** The Default output shows a seasonal overlay but combines initial-condition differences with differing annual runoff. The runs do not establish whether the 2000 cold start is sufficient for later analysis.
3. **Timestep/scientific suitability:** The approved `DT=600` combinations ran to completion without logged errors or non-finite unmasked output. This operational result is not an independent stability or scientific-accuracy assessment.
4. **Observed-flow validation:** No observed discharge series was used. The negative Prek Kdam simulation values and large Can Tho bifurcation differences need scientific interpretation against site and grid limitations, not an accuracy claim.

### A.3 Execution record

- Build: `./gosh_sample/compile_netcdf.sh` succeeded on macOS Apple Silicon with GNU Fortran 14.2.0, NetCDF-Fortran 4.6.2 / NetCDF-C 4.9.3, OpenMP, and `SinglePrec_CMF`. Existing compiler warnings reported unused imports in `cmf_ctrl_restart_mod.F90` and `MAIN_cmf.F90`, plus a duplicate NetCDF link flag; none prevented the build.
- Execution: four scripts ran 2000–2003 sequentially, each with `OMP_NUM_THREADS=4`, producing 16 successful annual model calls. Wall time totals were 231 s (Default), 186 s (No Bifurcation), 115 s (Kinematic Wave), and 110 s (No Floodplain), about 10m42s combined. Per-year elapsed time and status are retained locally under `out/<experiment>/run_time_YEAR.log`.
- Output checks: all 16 NetCDF files contain expected record counts, `(time,latitude,longitude)` dimensions `(days,120,80)`, `outflw` units `m3/s`, no unmasked non-finite values, and a gap-free represented daily timeline of 1,461 days per experiment. The interval-ending January 1 records were assigned to the prior year's December 31 interval.
- Restart checks: each experiment wrote four NetCDF Restart boundary files from 2001-01-01 through 2004-01-01. Logs show each 2001–2003 invocation reading the preceding year's Restart. NetCDF signatures and paths were checked.
- Smoke check: an isolated one-day Default namelist using the same map, runoff, NetCDF output, and Restart interfaces completed and produced one daily output record plus an end Restart.
- Scientific observations: at Prek Kdam, Default had 302 negative days and No Bifurcation 312 of 1,095 days; Kinematic Wave and No Floodplain had none. At Can Tho, Default vs. No Bifurcation differed on all 1,095 daily records, with per-year max absolute differences 21,504 / 20,243 / 12,792 m3/s and mean absolute differences 6,720 / 5,710 / 3,083 m3/s. No observation-based validation was performed.
- Figures: the station-location, spin-up, 2001–2003 comparison, and 3 × 4 annual-maximum figures were generated with the three independent Python scripts and reviewed visually. The annual-max scale is logarithmic, common across experiments, and spans 1–94,305 m3/s; non-river/missing cells are masked.
- Limit: successful execution and data checks establish that this workflow ran as configured, not that the configurations are scientifically accurate or that the coarse map resolves all local river processes.

## Appendix B. Development, distribution, and Benchmark integration

### B.1 GitHub and beginner package

The GitHub tutorial contains the four experiment scripts, specification, visualization tooling, and four figures generated from the completed runs. Generated output, Restart files, and logs remain untracked under the tutorial-local `out/` ignore rule. The beginner-facing README is still deferred.

The beginner package includes only `mek_default.sh` among the experiment scripts, plus tutorial documentation, visualization tools, and verified reference figures. It excludes `mek_nobif.sh`, `mek_kine.sh`, and `mek_nofld.sh` so learners create the variants by copying and editing Default settings. This is a teaching choice, not a packaging limitation. No package assembly change is made by this specification.

### B.2 Optional Benchmark v2 use

The 2001–2003 discharge series may be considered as a small Benchmark v2 dataset after Benchmark v2's scientific and data contracts have been reviewed. No extraction, tracking, or Benchmark integration is authorized here; the tutorial remains complete without that downstream use.
