# CaMa-Flood v5 sample workflows

This directory is the v5 development copy of the complete legacy [`gosh/`](../gosh/) workflow tree. The legacy `gosh/` directory remains unchanged. The copied `test*.sh`, option, development, and specialized examples are not yet migrated to v5 and may still contain legacy executable paths, package-only data paths, current-working-directory assumptions, and output cleanup behavior. Do not run them as v5 examples until they are individually reviewed and migrated.

## Build the v5 stand-alone model

From any working directory, run:

```sh
./gosh_sample/compile.sh
```

The wrapper resolves the repository location from its own file path and runs `make -C src_main all`. It does not build or clean legacy model or map-tool directories. The output executable is `src_main/MAIN_cmf`. To clean only this v5 build, run `make -C src_main clean` from the repository root.

## Mekong default simulation

[`mek01-default_run.sh`](mek01-default_run.sh) runs one non-MPI simulation from 2000-01-01 00:00 to 2000-02-10 00:00 (40 days; end time exclusive) with adaptive timestep and bifurcation enabled, zero initial storage, and reservoir operation disabled. It uses the daily 1-degree ELSE_GPCC sample with `DROFUNIT=86400000`, the Mekong `rivwth_merit.bin`, and the sample map's `bifprm.txt`. Edit the start/end date settings or `RUNDIR` near the top to change the period or experiment directory.

The map-specific runoff matrix was generated with [`map/mekong_15min/script_params/mek03-generate_inpmat-mek1deg.sh`](../map/mekong_15min/script_params/mek03-generate_inpmat-mek1deg.sh). The generated `inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin` are retained with the Mekong sample. Run this script from `gosh_sample/` so its `cd ..` base-directory setting resolves to the repository root:

```sh
cd gosh_sample
./mek01-default_run.sh
```

The script copies `src_main/MAIN_cmf` into `RUNDIR`, changes to that directory, and writes the namelist. For binary output it then creates planned GrADS CTLs before starting the model, so written records can be inspected during execution; the CTLs use the full configured time axis even while later records are not yet available. `COUTDIR="./"` therefore places the daily outputs, CTLs, logs, and final restart under `out/mek01-default_run/`. The default uses four OpenMP threads. A rerun automatically replaces this sample's known generated files while leaving unrelated files in `RUNDIR` untouched. The latest 40-day run read 40 daily runoff files across January and February (2000-01-01 through 2000-02-09) and wrote 40 records for each gridded daily output, ending at 2000-02-10 00:00. It records model start/end timestamps, elapsed wall-clock seconds, and exit status in `run_time.log`. All checked output values were finite. This is an operational smoke test, not scientific validation or a v4 numerical comparison. Details and limits are in the [workflow plan](../docs/model_source_workflow_plan.md).

## Mekong two-year NetCDF map tests

[`mek02-longterm_netcdf.sh`](mek02-longterm_netcdf.sh) and [`legacy02-longterm_netcdf.sh`](legacy02-longterm_netcdf.sh) run the same 2000–2001 Mekong case with yearly NetCDF runoff, NetCDF model output, a Binary input matrix, and Binary annual restarts. Both carry the 2000 year-end restart into 2001. The map interface is the test variable: `mek02` reads individual NetCDF map files with `LMAPCDF=.FALSE.`, while `legacy02` reads the consolidated `rivclim.nc` and `rivpar.nc` containers with `LMAPCDF=.TRUE.`. Those containers were generated from the selected `nextxy_noedge` and `rivwth_merit` products. Each script writes to a separate directory: `out/mek02-longterm_mapnc/` and `out/legacy02-longterm_netcdf/`.

Build the NetCDF-enabled executable with `./gosh_sample/compile_netcdf.sh` from any directory, then run either sample from `gosh_sample/`:

```sh
./gosh_sample/compile_netcdf.sh
cd gosh_sample
./mek02-longterm_netcdf.sh
./legacy02-longterm_netcdf.sh
```

The `legacy02` script uses `LMAPCDF=.TRUE.` with `CRIVCLINC` and `CRIVPARNC`; its `&NMAP` does not specify individual-map filenames. The generated `rivclim.nc` contains `lon`, `lat`, `nextx`, `nexty`, `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and 10-layer `fldhgt`, while `rivpar.nc` contains `rivwth`, `rivhgt`, and `rivman`. These containers were generated from the selected `nextxy_noedge` and `rivwth_merit` inputs. The sample leaves `gdwdly`, `elevslope`, `mask_slope`, and `meansl` disabled, so their optional map data are not required. Both scripts otherwise use the same simulation years, runoff, physical settings, outputs, and restart handoff.

`mek02` replaces its known generated sample files in its own `out/mek02-longterm_mapnc/` directory. `legacy02` follows the same simple rerun behavior as `mek02`: it removes only the sample-generated outputs in its dedicated run directory before starting a fresh sequence. It retains each year's Namelist, runtime and model logs, console output, NetCDF outputs, and Binary restarts under `out/legacy02-longterm_netcdf/`. It does not keep completion markers or resume an earlier partial run.

The NetCDF-enabled build and `legacy02` both succeeded. Year 2000 produced 366 daily records and `restart2001010100.bin`; year 2001 read that restart and produced 365 daily records and `restart2002010100.bin`. Corresponding Namelists match outside `&NMAP`. All five output variables (`outflw`, `storge`, `sfcelv`, `flddph`, `fldfrc`) matched the individual-NetCDF `mek02` run exactly for both years: zero unequal values, maximum absolute and relative differences of zero, and no NaN/Inf. Missing-value cells were counted separately (2,249,802 of 3,513,600 cells per variable in 2000; 2,243,655 of 3,504,000 in 2001). The current script starts a fresh run and overwrites only its known generated products in the dedicated legacy output directory. This is operational compatibility evidence for these Mekong files and build, not broad scientific validation or proof of every legacy optional path. See the [model workflow plan](../docs/model_source_workflow_plan.md) and dataset [README](../inp/test-mek_1deg_nc/README.md) for earlier comparisons and limitations.
