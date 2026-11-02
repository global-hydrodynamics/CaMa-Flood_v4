# CaMa-Flood v5 sample workflows

This directory is the v5 development copy of the complete legacy [`gosh/`](../gosh/) workflow tree. The legacy `gosh/` directory remains unchanged. The copied `test*.sh`, option, development, and specialized examples are not yet migrated to v5 and may still contain legacy executable paths, package-only data paths, current-working-directory assumptions, and output cleanup behavior. Do not run them as v5 examples until they are individually reviewed and migrated.

## Build the v5 stand-alone model

From any working directory, run:

```sh
./gosh_sample/compile.sh
```

The wrapper resolves the repository location from its own file path and runs `make -C src_main all`. It does not build or clean legacy model or map-tool directories. The output executable is `src_main/MAIN_cmf`. To clean only this v5 build, run `make -C src_main clean` from the repository root.

The first runtime example will use `map/mekong_15min/` with `inp/test-mek_1deg/`, in non-MPI mode with standard bifurcation enabled. That Phase 1 simulation has not been run yet. The matching `test-mek_1deg` input matrix is not currently included in the map sample; it will be generated on a disposable map copy so committed sample data are not overwritten. See the [model source and simulation workflow plan](../docs/model_source_workflow_plan.md).
