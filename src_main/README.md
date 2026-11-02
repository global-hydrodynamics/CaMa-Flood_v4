# CaMa-Flood v5 model source

This directory is the v5 development copy of the complete legacy model source tree. The legacy reference remains under [`../src/`](../src/); do not edit it as part of v5 work. The copy includes all existing source components, including `ICI/`, `sediment/`, `heatlink/`, and the supporting libraries. Their presence does not mean every optional configuration has been ported or validated.

## Build the stand-alone model

From any directory in the repository, run:

```sh
make -C src_main all
```

This uses [`../adm/Mkinclude`](../adm/Mkinclude) and the existing source Makefiles without changing shared v4 build settings. The default target is `src_main/MAIN_cmf`; object files, module files, and archives are produced inside `src_main/` and its subdirectories.

To remove only the v5 model build products:

```sh
make -C src_main clean
```

Do not use the legacy [`../gosh/compile.sh`](../gosh/compile.sh) as a v5 build entry point: it cleans and builds `util/`, legacy `map/src/` tools, and `src/`. The `gosh_sample/` build wrapper invokes only this directory's Makefile.

## Current validation status

The copied default stand-alone configuration compiled successfully with the repository's default GNU Fortran/OpenMP settings. No simulation has yet been run from this v5 copy. MPI, NetCDF-specific builds, `heatlink/`, `sediment/`, and `ICI/` configurations remain unvalidated. See the [model source and simulation workflow plan](../docs/model_source_workflow_plan.md) for scope and staged validation.
