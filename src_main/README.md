# CaMa-Flood v5 model source

This directory is the v5 development copy of the complete legacy model source tree. The legacy reference remains under [`../src/`](../src/); do not edit it as part of v5 work. The copy includes all existing source components, including `ICI/`, `sediment/`, `heatlink/`, and the supporting libraries. Their presence does not mean every optional configuration has been ported or validated.

The v5 output-variable inventory is maintained at the repository root in [`output_variables.csv`](../output_variables.csv). It records the `CVARSOUT` names and verified output metadata; future CTL-generation tools should read `${CMF_BASE}/output_variables.csv`.

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

## NetCDF-enabled build

On macOS or Linux, install GNU Fortran and NetCDF-Fortran plus the NetCDF C library so that `nf-config` and `nc-config` are available on `PATH`. From the repository root, build the isolated v5 NetCDF executable with:

```sh
./gosh_sample/compile_netcdf.sh
```

The wrapper combines flags from both configuration tools and builds a temporary source/build copy, leaving the normal `src_main/MAIN_cmf` binary build and object files intact. It writes `out/bin/MAIN_cmf_netcdf`. The currently verified environment is macOS with Homebrew GNU Fortran and NetCDF; other installations should provide compatible GNU Fortran and NetCDF C/Fortran libraries. See the wrapper and [simulation workflow plan](../docs/model_source_workflow_plan.md) for the commands and current validation evidence.

## Current validation status

The default stand-alone Binary build and the separate NetCDF-enabled build both compiled on the maintainer's macOS workstation. The NetCDF executable was checked with Binary runoff/Binary output, NetCDF runoff/Binary output, and Binary runoff/NetCDF output; the selected 40-day output arrays matched bitwise across formats. A two-year annual-NetCDF-runoff/NetCDF-output run completed with restart transfer from 2000 to 2001. These checks establish operational and I/O consistency for this sample and build, not independent scientific validation or broad compiler/platform support. MPI, `heatlink/`, `sediment/`, and `ICI/` configurations remain unvalidated. See the [model source and simulation workflow plan](../docs/model_source_workflow_plan.md).
