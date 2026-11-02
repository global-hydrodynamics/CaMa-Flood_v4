# CaMa-Flood v5 sample workflows

This directory is the v5 development copy of the complete legacy [`gosh/`](../gosh/) workflow tree. The legacy `gosh/` directory remains unchanged. The copied `test*.sh`, option, development, and specialized examples are not yet migrated to v5 and may still contain legacy executable paths, package-only data paths, current-working-directory assumptions, and output cleanup behavior. Do not run them as v5 examples until they are individually reviewed and migrated.

## Build the v5 stand-alone model

From any working directory, run:

```sh
./gosh_sample/compile.sh
```

The wrapper resolves the repository location from its own file path and runs `make -C src_main all`. It does not build or clean legacy model or map-tool directories. The output executable is `src_main/MAIN_cmf`. To clean only this v5 build, run `make -C src_main clean` from the repository root.

## Mekong default simulation

[`mek01-default_run.sh`](mek01-default_run.sh) runs one non-MPI simulation from 2000-01-01 00:00 to 2001-01-01 00:00 with adaptive timestep and bifurcation enabled, zero initial storage, and reservoir operation disabled. It uses the daily 1-degree ELSE_GPCC sample with `DROFUNIT=86400000`, the Mekong `rivwth_merit.bin`, and the sample map's `bifprm.txt`. Edit `START_YEAR`, `END_YEAR`, or `RUN_DIR` near the top to select another annual period or experiment directory.

Before the first run, generate the map-specific runoff matrix with [`map/mekong_15min/script_params/mek03-generate_inpmat-mek1deg.sh`](../map/mekong_15min/script_params/mek03-generate_inpmat-mek1deg.sh). The generated `inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin` are retained with the Mekong sample. Run the model from any working directory:

```sh
./gosh_sample/mek01-default_run.sh
```

The namelist, model and console logs, daily outputs, bifurcation output, and final restart are written under `out/mek01-default_run/`. The run script will stop if its namelist or logs already exist, preserving previous results; set another `RUN_DIR` to start a separate run. The 2000 run completed with 366 daily runoff inputs and 366 records for each gridded daily output. All checked output values were finite. This is an operational smoke test, not scientific validation or a v4 numerical comparison. Details and limits are in the [workflow plan](../docs/model_source_workflow_plan.md).
