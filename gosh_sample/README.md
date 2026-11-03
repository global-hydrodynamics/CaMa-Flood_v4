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

## Mekong two-year NetCDF workflow

[`mek02-longterm_netcdf.sh`](mek02-longterm_netcdf.sh) builds and uses a separate NetCDF-enabled executable, consumes yearly runoff files from [`inp/test-mek_1deg_nc/`](../inp/test-mek_1deg_nc/), writes NetCDF model outputs, and carries the binary restart from 2000 into 2001. Build the NetCDF executable with `./gosh_sample/compile_netcdf.sh` from any directory, then run the two-year example from `gosh_sample/`:

```sh
./gosh_sample/compile_netcdf.sh
cd gosh_sample
./mek02-longterm_netcdf.sh
```

The script stores each year's namelist, log, timing/status, NetCDF outputs, restart, and completion signature under `out/mek02-longterm_netcdf/`. Completed years are retained and skipped on an identical rerun; an incomplete year can be rerun without deleting a completed prior year. The current local validation completed 2000 (366 records) and 2001 (365 records), read the 2000-end restart at the 2001 start, and produced 731 daily records across the two annual files. The annual output arrays were checked for shape and finite values. See the [model workflow plan](../docs/model_source_workflow_plan.md) and dataset [README](../inp/test-mek_1deg_nc/README.md) for evidence and limitations.
