# Tutorial 02: Mekong Multi-Year NetCDF Simulation and Three-Experiment Comparison

**Status:** Source-reviewed experiment specification for implementation planning. No Tutorial 02 run or scientific result is claimed. Several design and scientific questions, plus reproducible availability of 2002–2003 runoff, remain open; see Appendix A.

## 1. Purpose and learning outcomes

This tutorial uses the CaMa-Flood v5 Mekong sample to demonstrate a four-year simulation with annual Restart handoffs and a controlled comparison of river-routing and floodplain configurations. Users begin with the local-inertial reference experiment, then copy its script and change a small number of clearly identified settings to create two variants.

By the end, users should be able to:

1. Find the map, runoff, input-matrix, experiment, and output directories used by a v5 run.
2. Read the experiment settings in a GOSH script and explain the main physical switches.
3. Run annual NetCDF runoff, model-output, and Restart workflows.
4. Carry model state across annual runs and retain each year’s results.
5. Create independent experiment variants by copying and editing a configuration.
6. Extract discharge at selected Mekong gauge locations and compare model simulations.
7. Examine how initialization and annual runoff variability relate to differences between the spin-up year and later years.

The tutorial uses GRDC station metadata only to locate model grid cells. It does not use or compare observed discharge records.

## 2. Experiment design

The scientific comparison has three configurations. “Default” means the local-inertial routing reference in this experiment; shared option changes required to make the comparison executable are specified below and are not hidden behind that label.

| Experiment | Routing | Floodplain storage representation | `PROJ` | `LKINE` | `LFPLAIN` |
|---|---|---|---|---|---|
| Local-inertial reference | Local inertial equation | Enabled | `mek_default` | `.FALSE.` | `.TRUE.` |
| Kinematic Wave | Kinematic wave | Enabled | `mek_kine` | `.TRUE.` | `.TRUE.` |
| No Floodplain | Kinematic wave | Disabled | `mek_nofld` | `.TRUE.` | `.FALSE.` |

All three experiments must use the same map, runoff, input matrix, simulation dates, output settings, and other physical parameters. The source-supported common settings are:

- `DT=3600` seconds.
- `LLEAPYR=.TRUE.` to keep the model calendar aligned with the Gregorian CF runoff time axis, including February 29, 2000.
- `LADPSTP=.FALSE.` for all three runs. The model warns that adaptive stepping is recommended only with local-inertial routing and tells users to set an appropriate fixed timestep for Kinematic Wave. A one-hour fixed step follows the existing Mekong sample value; its scientific adequacy for both schemes still requires run validation.
- `LPTHOUT=.FALSE.` for all three runs. The current configuration check stops when `LKINE=.TRUE.` and `LPTHOUT=.TRUE.` because bifurcation channel flow is supported only with local-inertial routing. Disabling bifurcation in the reference case keeps this option common across the comparison but differs from `mek01-default_run.sh`, which enables it. Maintainer confirmation is required before scripts are implemented.
- Keep `LFLDOUT=.TRUE.` across the three runs. `LFPLAIN` controls whether floodplain capacity is represented in map initialization; `LFLDOUT` is a separate high-water channel-flow switch. Do not change both to create the No Floodplain case.
- Keep `LSLPMIX=.FALSE.`, reservoir operation off, and all other optional schemes at the same values in every run.

The Kinematic Wave routing code computes channel discharge from Manning velocity using the local water depth and a slope bounded below by `PMINSLP`; available channel storage limits discharge during a timestep. It can also route floodplain flow when `LFLDOUT` is enabled. The No Floodplain experiment turns off the floodplain storage representation through `LFPLAIN=.FALSE.` while retaining the Kinematic Wave routing configuration.

Interpret the comparisons as follows:

- **Local-inertial reference vs. Kinematic Wave:** routing-equation comparison with floodplain representation retained. The shared fixed timestep and bifurcation-off setting are comparison controls, not the unchanged `mek01` sample configuration.
- **Kinematic Wave vs. No Floodplain:** floodplain-storage representation comparison under the same Kinematic Wave routing approximation.
- **Local-inertial reference vs. No Floodplain:** a combined routing and floodplain change; it does not isolate one mechanism.

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

`params.txt`, the runoff-grid descriptor `inpdim_test-mek_1deg.txt`, and the map-specific `inpmat_test-mek_1deg.bin` remain text/Binary support inputs. Reuse the existing 20 × 30 runoff-to-map input matrix; do not regenerate or duplicate it for the NetCDF runoff representation. `bifprm.nc` is not required when the comparison uses `LPTHOUT=.FALSE.`.

`LMAPCDF=.TRUE.` selects the separate legacy consolidated NetCDF map interface and must not be used for these individual files.

### 3.2 Runoff

Use the annual NetCDF dataset `inp/test-mek_1deg_nc/`, derived without rescaling from the tracked daily Binary sample `inp/test-mek_1deg/`. The NetCDF runoff field is `runoff(time, latitude, longitude)`, float32, daily `mm day-1`, on a 20 × 30 grid covering the same 90–110°E, 5–35°N extent. The files have CF daily time bounds and a `proleptic_gregorian` calendar. `DROFUNIT=86400000` converts the stored daily depth units to the model’s `m/s` forcing rate; `CROFFREQ="YEAR"`, `CVNTIME="time"`, and `CVNROF="runoff"` select calendar-year files and records. Use the Binary input matrix above.

Annual inputs are `runoff2000.nc` through `runoff2003.nc`. The repository currently tracks only the 2000 and 2001 NetCDF files. The local checkout inspected for this specification also contains 2002–2003 files as untracked files; their local presence does not make them available in a clean clone. The existing converter can generate years through 2009 from the tracked daily Binary source, but generation or distribution of 2002–2003 is a prerequisite for a reproducible clean-checkout workflow. Do not treat the current local files as committed sample inputs.

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

Write only `outflw`, the river-plus-floodplain discharge mean in `m3/s`, directly through the model’s NetCDF output path. Use `LOUTCDF=.TRUE.`, `LOUTVEC=.FALSE.`, and `IFRQ_OUT=24`. The registry marks `outflw` as a time mean; `CMF_OUTPUT_WRITE` selects `D2OUTFLW_oAVG` and the NetCDF writer records `cell_methods="time: mean"`. The output variable is float32 in NetCDF-4 with the configured compression level.

Set a year-specific `COUTTAG` for each annual run. The current filename rule is `o_` + variable name + tag + `.nc`, so a run with `COUTTAG="2000"` writes `o_outflw2000.nc`. Keep all files for each experiment in its independent directory:

```text
tutorial/tutorial02-mekong_netcdf_3exp/out/
├── mek_default/
├── mek_kine/
└── mek_nofld/
```

Each experiment directory holds its annual discharge files, dated Restarts, year-specific Namelists, and logs. The repository-root `out/` directory is not used. Generated run artifacts remain untracked.

The model labels daily averages at the end of their averaging interval. Since `CMF_TIME_NEXT` advances to the end time before diagnostics are written, the first daily record is time-stamped 2000-01-02 00:00 and represents the interval beginning 2000-01-01; the final record in a segment is stamped at the exclusive January 1 endpoint. A visualization should label or shift these interval-ending timestamps consistently rather than treating them as daily midpoint values.

Each annual output is expected to contain 366 records for 2000 and 365 for each of 2001–2003. The intended total is 1,461 records per experiment, or 12 annual NetCDF discharge files across three experiments. These are expected contracts, not execution-verified results.

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

### 4.4 Three-experiment comparison

Compare daily `outflw` over 2001–2003 at the reviewed gauge locations. Report the model variables and dates actually used, and inspect missing/non-finite values before plotting. The comparison can show differences among the configured simulations; by itself it does not establish which configuration is more accurate because no observed discharge is used.

## 5. Tutorial structure and deliverables

The future beginner-facing README should guide users to inspect the tutorial and input paths, review and run the default configuration, check that annual output and Restart files were produced, copy the default script to create the two variants, change the experiment settings, and visualize the resulting discharge. Commands, sample terminal output, expected files, and troubleshooting guidance must be based on the implemented scripts and actual validation runs.

Planned GitHub development contents are:

- `experiment_spec.md`, `README.md`,
- `mek_default.sh`, `mek_kine.sh`, and `mek_nofld.sh`,
- a visualization script and verified reference figures.

The generated model output, Restart files, and logs are excluded from Git. Scripts, figures, and tutorial README are not part of this specification task; see Appendix B for package policy.

## Appendix A. Technical verification and open questions

### A.1 Verification table

| Setting or requirement | Verified behavior | Evidence | Status |
|---|---|---|---|
| `LKINE` routing | `CMF_PHYSICS_ADVANCE` selects `CMF_CALC_OUTFLW_KINE`; the routine applies a Manning kinematic relation to river depth and slope, with a minimum-slope bound and available-storage discharge cap. | `src_main/cmf_ctrl_physics_mod.F90` (`CMF_PHYSICS_ADVANCE`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| `LFPLAIN` | `CMF_TOPO_INIT` sets river storage capacity to a very large value when false, instead of the channel cross-section capacity used when true. The Kinematic routine separately controls floodplain discharge with `LFLDOUT`. | `src_main/cmf_ctrl_maps_mod.F90` (`CMF_TOPO_INIT`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| Kinematic option compatibility | `LKINE=.TRUE.` with `LPTHOUT=.TRUE.` stops configuration. Adaptive timestep with Kinematic Wave only warns; the model recommends a fixed step. | `src_main/cmf_ctrl_nmlist_mod.F90` (`CMF_CONFIG_CHECK`) | Code-verified; common comparison settings pending maintainer decision |
| Cold-start state | `CMF_PROG_INIT` zeros prognostic arrays, then `STORAGE_SEA_SURFACE` fills river storage to avoid an initial water surface below downstream boundary level. | `src_main/cmf_ctrl_vars_mod.F90` (`CMF_PROG_INIT`, `STORAGE_SEA_SURFACE`) | Code-verified |
| Annual Restart continuity | A Normal Restart restores river/floodplain storage and previous-step flow/depth fields; optional scheme states are conditional. End-only Restart is selected by `IFRQ_RST=0`. | `src_main/cmf_ctrl_restart_mod.F90` (`CMF_RESTART_INIT`, `CMF_RESTART_WRITE`, `RESTART_UTIL_OPEN_WRITE`); `src_main/cmf_drv_advance_mod.F90` | Code-verified; four-year continuity pending execution |
| Runoff NetCDF | Grouping `YEAR` resolves `runoffYYYY.nc`; the reader matches CF time metadata, verifies calendar/interval/coverage and requires the first time record to start at the year boundary. The variable is read as `(NXIN,NYIN,time)` through the Fortran NetCDF interface. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_RESOLVE_FILE`, `FORCING_UTIL_GET_CDF`); `inp/test-mek_1deg_nc/README.md`; `runoff_datasets.csv` | Code- and data-verified for tracked 2000–2001; 2002–2003 reproducible distribution pending |
| Runoff units and matrix | Runoff is converted by dividing by `DROFUNIT`; `CINPMAT` supplies the spatial weights. The annual NetCDF values are documented as daily `mm day-1`; use `86400000` and the existing Binary matrix. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_ROFF_INTERP`); `inp/test-mek_1deg_nc/README.md`; `map/mekong_15min/inpdim_test-mek_1deg.txt` | Code- and data-verified |
| Individual NetCDF map inputs | `LMAPCDF=.FALSE.` selects per-file extension-based readers. The nine map files listed in Section 3.1 are present in the map's NetCDF directory; the dimension descriptor and input matrix are separate support files. | `src_main/cmf_ctrl_maps_mod.F90` (`CMF_MAPS_NMLIST`, `CMF_RIVMAP_INIT`, `CMF_TOPO_INIT`, `MAP_UTIL_READ_*`); `map/mekong_15min/netcdf/`; `map/mekong_15min/params.txt` | Code- and data-verified; Tutorial 02 use pending execution |
| Daily mean `outflw` | Registry metadata selects a mean; `CMF_OUTPUT_WRITE` writes `D2OUTFLW_oAVG`. The NetCDF writer uses float32, `time: mean`, and file pattern `o_` + name + tag + `.nc`. Output time is calculated at `KMINNEXT`, the end of the step/averaging interval. | `variable_table/output_variables.csv`; `src_main/cmf_ctrl_output_mod.F90` (`CMF_OUTPUT_INIT`, `CMF_OUTPUT_WRITE`, `OUTPUT_UTIL_CREATE_OUTCDF`, `OUTPUT_UTIL_WRITE_MAP`); `src_main/cmf_drv_advance_mod.F90`; `src_main/cmf_ctrl_time_mod.F90` | Code-verified; annual count/time labels pending execution |
| GRDC station allocations | Valid Mekong river records and allocated cell indices are present; `Type` counts substitute catchments, not river-order categories. Five candidate station names and cells are listed in Section 4.2. | `map/mekong_15min/GRDC_alloc.txt`; `map/data/GRDC_allocated.csv`; `map/mekong_15min/script_params/README.md` | Data-verified; final selection pending maintainer review |
| Spin-up effect | The source determines initialization behavior but cannot establish a transient's presence, duration, or interpretive strength. | `src_main/cmf_ctrl_vars_mod.F90`; Section 4.3 | Pending simulation and scientific review |

### A.2 Decisions and unresolved prerequisites

1. **Shared bifurcation setting (maintainer decision):** Confirm whether to set `LPTHOUT=.FALSE.` in all three Tutorial 02 experiments. This is required to run Kinematic Wave because the current configuration check stops if bifurcation flow is enabled. It also changes the Local-inertial reference from the bifurcation-on `mek01` sample and may affect discharge pathways. If this is not acceptable, the three-experiment matrix must be redesigned; do not silently run an incompatible case.
2. **Fixed timestep adequacy:** `DT=3600`, `LADPSTP=.FALSE.` is the source-supported common configuration indicated by the existing Mekong step and Kinematic Wave guidance. Its stability and suitability across the map and both routing formulations require run validation; source inspection alone cannot establish this.
3. **Gauge set and spin-up interpretation:** Review the five candidate sites and the proposed leap-day handling before producing reference figures. Actual results must show whether and where initialization effects are distinguishable from interannual runoff variability.
4. **2002–2003 runoff availability:** The clean repository tracks only 2000–2001 NetCDF. The converter can recreate later years from tracked Binary inputs, but Tutorial 02 setup must decide whether it asks users to run the converter for 2002–2003 or whether a later distribution task includes those derived files. The untracked files observed in one local checkout are not part of this specification's reproducible inputs.
5. **Full execution verification:** Build the NetCDF-enabled model, run all 12 annual experiment segments with the settings above, validate Restart handoffs and daily time axes, inspect 1,461 records per experiment, and generate the comparisons. Until then, no comparison, spin-up finding, or reference figure is verified.

## Appendix B. Development, distribution, and Benchmark integration

### B.1 GitHub and beginner package

The intended GitHub tutorial retains all three experiment scripts, the specification and README, visualization tooling, and reference figures only after they are generated from verified runs. Generated output, Restart files, and logs remain untracked.

The beginner package includes the default script, tutorial documentation, visualization script, and verified reference figure. It excludes `mek_kine.sh` and `mek_nofld.sh` so learners create them by copying and editing the default script. This is a teaching choice, not a packaging limitation. No package assembly change is made by this specification.

### B.2 Optional Benchmark v2 use

The 2001–2003 discharge series may be considered as a small Benchmark v2 dataset after Benchmark v2's scientific and data contracts have been reviewed. No extraction, tracking, or Benchmark integration is authorized here; the tutorial remains complete without that downstream use.
