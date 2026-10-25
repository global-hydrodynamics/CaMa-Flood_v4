# Sample execution workflows

`gosh/` contains shell examples for building and running selected CaMa-Flood configurations. The six `test*` scripts demonstrate particular maps, forcing formats, and applications; they are not a catalogue of every supported scheme. Some specialized workflows live under [`etc/`](../etc/).

These scripts expect external map and forcing data and may remove prior outputs in their experiment directory when `SPINUP=0`. Review paths and cleanup behavior before running them. No sample is claimed to have been executed as part of this documentation update.

## Directory contents

| File | Role |
| --- | --- |
| [`compile.sh`](compile.sh) | Builds utility/map components and `src/MAIN_cmf` using the selected `adm/Mkinclude`. |
| [`test1-glb_15min.sh`](test1-glb_15min.sh) – [`test6-reservoir_glb15min.sh`](test6-reservoir_glb15min.sh) | Stand-alone sample experiments described below. |
| [`options_full.sh`](options_full.sh) | A runnable configuration example showing Shell variables, generated runtime namelist settings, and comments about compile-time selection. It is not an exhaustive option reference. |
| [`dev_ERA5_hour.sh`](dev_ERA5_hour.sh) | Development workflow for an hourly ERA5-Land input configuration; inspect its paths, data preparation, and build before use. |
| [`etc/`](etc/) | Additional local examples, including a Heatlink workflow and its namelist. |

`options_full.sh` is not sourced or executed by another tracked script in this repository. It has its own shebang and execution loop, so running it launches its configured experiment. It is best read as a practical script example; for feature behavior, check the current source and relevant component documentation.

## Quick start

From the repository's `gosh/` directory, select an `adm/Mkinclude*` configuration, build, then launch a sample:

```sh
cd /path/to/CaMa-Flood_v4/gosh
./compile.sh
./test1-glb_15min.sh
```

`compile.sh` uses the current working directory to set the repository path. With no argument it cleans and builds `util`, `map/src/src_param`, `map/src/src_region`, and `src`, then asks `make` for `MAIN_cmf`. `./compile.sh no` requests forced rebuilds with `make -B`; `./compile.sh clean` cleans those components without making the executable. The script reads the Make configuration through the repository Makefiles, principally [`adm/Mkinclude`](../adm/Mkinclude). Other platform include files are available under `adm/`.

The `test*` scripts do not compile the model. They expect an existing `src/MAIN_cmf`, create or enter `out/<experiment>`, write `input_cmf.nam`, link the executable into the run directory, and run it. They append run timing to `log.txt`, move the model log to a year-tagged `log_CaMa-YYYY.txt`, and write configured model output and restart files under the run directory. Their `BASE` is based on the current working directory, so invoke them from `gosh/` unless you deliberately adjust the script. Several scripts remove files in `out/<experiment>` on a zero-storage start.

## Sample experiments: test1–test6

The table summarizes settings read from the scripts. All six need map data beyond the source files; the runoff and map datasets are not generally included in a source-only checkout. The configured `map/` and `inp/` paths must exist and contain the matching files.

| Example | Purpose and configuration | Main data and options | Outputs and dependencies |
| --- | --- | --- | --- |
| [`test1-glb_15min.sh`](test1-glb_15min.sh) | Global 15-minute river map, annual loop with spin-up, daily binary runoff. | `map/glb_15min`; `inpmat_test-1deg.bin`; `inp/test_1deg/runoff/`; adaptive time step and bifurcation flow; binary output. | `out/test1-glb_15min/`, daily fields selected by `CVARSOUT`, restart. Needs the global map and 1° runoff/input matrix. |
| [`test2-conus_06min_netcdf.sh`](test2-conus_06min_netcdf.sh) | Regional CONUS 6-minute map, annual loop, daily NetCDF runoff on the sample 15-minute input grid. | `map/conus_06min`; `diminfo_test-15min_nc.txt` and matching input matrix; `inp/test_15min_nc/`; `LINPCDF=.TRUE.`; NetCDF output. | `out/test2-conus_06min/`. Requires a NetCDF-enabled build (`UseCDF_CMF`) and compatible map, matrix, and runoff files. |
| [`test3-jpn_fcast.sh`](test3-jpn_fcast.sh) | Japan 1-minute, forecast-like runs of 39 hours from daily 06:00/18:00 cycles using hourly binary runoff. | `map/tej_01min`; `diminfo_test-tej.txt`, `inpmat_test-tej.bin`; `inp/test_jpn_1hr/`; hourly input and output; adaptive stepping. | `out/test3-jpn_fcast/`. Requires Japan map and hourly forcing files matching the script's date/file naming. |
| [`test4-e2o_glb_15min.sh`](test4-e2o_glb_15min.sh) | Long global 15-minute simulation (1980–2014) using daily E2O WRR2 NetCDF runoff. This is a legacy forcing example. | `map/glb_15min`; sample 15-minute input matrix; annual E2O files under `inp/E2O/`, variable `Runoff`; `LINPCDF=.TRUE.`; binary output. | `out/test4-e2o_ecmwf-glb_15min/`. Requires the large external E2O data archive and a NetCDF-enabled build. The model functionality is not obsolete because this forcing example is old. |
| [`test5_mpi.sh`](test5_mpi.sh) | Global 15-minute annual run using MPI plus OpenMP. | Same map/forcing family as test1 plus `mpireg-<MPINP>.bin`; compile with `UseMPI_CMF`, launch with MPI process count (`MPINP`) and `OMP_NUM_THREADS`. | `out/mpi<processes>omp<threads>/`. Requires an MPI-enabled compiler/runtime and matching region map; inspect the script's launcher and resource settings for the local system. |
| [`test6-reservoir_glb15min.sh`](test6-reservoir_glb15min.sh) | Global 15-minute annual run demonstrating reservoir operations. | Same binary runoff family as test1; `dam_param.csv`; runtime `LDAMOUT=.TRUE.` and `NDAMOUT` settings; binary output. | `out/test6-reservoir/`, selected river/flood/reservoir fields and reservoir text diagnostics. Requires the reservoir map table and global map/forcing data. |

These are example configurations rather than universally available presets. `DT`, `IFRQ_INP`, forcing units, map dimensions, and input matrix must agree with the selected files. Test2 and test4 require NetCDF support at build time; setting a NetCDF namelist flag cannot add NetCDF code to an executable built without it. Test5 likewise requires MPI code compiled in and a working MPI runtime.

## Compilation and configuration

`adm/Mkinclude` selects compiler commands, flags, libraries, and preprocessor macros consumed by `src/Makefile` and component Makefiles. For example, `UseMPI_CMF`, `UseCDF_CMF`, `SinglePrec_CMF`, and `NoAtom_CMF` are compile-time choices. The standard include currently defines `DSINGLE=-DSinglePrec_CMF`; other `Mkinclude*` files have their own selections. Change the active include and rebuild before using a code path that depends on a macro.

The sample scripts assign **Shell variables** such as `BASE`, `FMAP`, `RDIR`, `EXP`, `OMP_NUM_THREADS`, and `SPINUP`. Some are script control or environment settings; others are inserted into Fortran **runtime namelist** groups such as `NRUNVER`, `NMAP`, `NFORCE`, `NOUTPUT`, and `NDAMOUT`. The runtime switch is not the same as a compile-time macro. In `options_full.sh`, `LITRPCDF` is a Shell variable interpolated into the Fortran namelist option of the same name; other Shell variables such as `EXP` do not appear in a namelist at all.

`options_full.sh` demonstrates a subset of common runtime settings: adaptive stepping, floodplain flow, bifurcations, reservoir operation, input/output cadence and format, restart, and map/forcing paths. It also documents representative compile-time switches, but does not set them or rebuild. Its active dam example expects `${FMAP}/dam_param.csv`. It deliberately leaves other optional runtime settings off; feature activation may require additional namelist groups, input data, libraries, and supported combinations.

Avoid enabling multiple alternative flow formulations together. In particular, `LKINE` selects the kinematic path before `LSLPMIX` in the current physics controller; the sample keeps both off to use the default local-inertial path. The source configuration check also rejects some combinations, including levee plus water-evaporation extraction. Check `CMF_CONFIG_CHECK` and the consuming controller before combining less common options.

## Input data and river maps

The examples refer to binary or NetCDF maps, dimension-info text, runoff input matrices, forcing files, optional region maps, and reservoir tables by path. They are not interchangeable across map resolutions or grids. The configured paths show what each example expects; locate or prepare matching data before running it.

Test4 currently reads daily E2O WRR2 runoff NetCDF files with variable `Runoff` from `inp/E2O/`; its name and files are a legacy data workflow, not a statement about the status of the model physics. A newer [`ERA5-Land download/conversion workflow`](../inp/download_ERA5/ReadMe.md) provides `s01-download.sh` and separate NetCDF/binary conversion steps (`s02-convert_CMF_nc.sh`, `s03-convert_CMF_bin.sh`). Migration would require producing the intended CaMa grid, time step and units, setting the input matrix/dimension metadata, matching the actual NetCDF variable and time coordinate (or selecting the binary file pattern), and updating `NFORCE`/time settings in a future test4 change. ERA5 is not a drop-in replacement until those interfaces and sample paths are verified. No forcing was downloaded or substituted here.

## Additional examples under `etc/`

`etc/` contains specialized and developing workflows rather than the main six stand-alone test cases. Examples include:

- [MPI/HPC configurations](../etc/options_HPC/README.md), including host-specific NetCDF and MPI examples.
- [Levee parameter and simulation workflows](../etc/levee_params/s05-run_levee.sh) and a separate [sea-level boundary sample](../etc/sealev_boundary/test5-moz_06min_sealev.sh).
- [Sediment run example](../etc/sediment/run_sediment.sh) and [upstream-inflow preparation](../etc/upstream_inflow/g02-upst_inflow.sh).
- [Discharge, water-surface elevation, and flood-extent validation](../etc/validation/ReadMe.md).
- [Flood-depth downscaling](../etc/downscale_flddph/ReadMe.md), [runoff preprocessing](../etc/runoff_preset/), and [result mapping](../etc/result_mapping/).
- [Storage-to-flood-stage utilities](../etc/storage_to_floodstage/) and [n-year flood-depth analysis](../etc/n-year_flood_depth/).
- [Heatlink execution example](etc/s01-simulation_heatlink.sh) with its sample namelists in `gosh/etc/`.

The presence or absence of a `gosh/test*` example does not establish whether a scheme is supported; check its source, build configuration, and component documentation.

## Known limitations and future updates

- The test4 E2O forcing workflow is legacy. Replacing its data requires the ERA5-Land grid/time/unit/interface checks above; this task does not rewrite test4.
- The six scripts encode particular historical compiler/library paths and data layouts. Review them for your platform and dataset.
- `options_full.sh` is a practical example, not an exhaustive model-option catalogue. Important source options not demonstrated include levee, tracer, Heatlink, sediment, upstream inflow, sea-level boundary, water-evaporation extraction, and vector-processor stage calculation. Some require additional compile-time macros or dedicated input groups/files. A separate reference can be considered later if maintainers need one.

## Related documentation

- [Root README](../README.md) and [development rules](../AGENTS.md)
- [CaMa-Flood user guideline](../doc/CaMa-Flood_Guideline.md) and [main manual](../doc/Manual_CaMa-Flood_v420.docx)
- [High-performance computing note](../doc/Note_HighPerformanceComputing.docx)
- [ERA5-Land data preparation](../inp/download_ERA5/ReadMe.md)
- [Heatlink source and configuration notes](../src/heatlink/README.md)
