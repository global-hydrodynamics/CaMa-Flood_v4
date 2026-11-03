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

The script copies `src_main/MAIN_cmf` into `RUNDIR`, changes to that directory, writes the namelist, and runs the model there. `COUTDIR="./"` therefore places the daily outputs, logs, and final restart under `out/mek01-default_run/`. The default uses four OpenMP threads. The run script stops if its namelist or logs already exist, preserving previous results; set another `RUNDIR` to start a separate run. The latest 40-day run read 40 daily runoff files across January and February (2000-01-01 through 2000-02-09) and wrote 40 records for each gridded daily output, ending at 2000-02-10 00:00. It records model start/end timestamps, elapsed wall-clock seconds, and exit status in `run_time.log`. All checked output values were finite. This is an operational smoke test, not scientific validation or a v4 numerical comparison. Details and limits are in the [workflow plan](../docs/model_source_workflow_plan.md).
