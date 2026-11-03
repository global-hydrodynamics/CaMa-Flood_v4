# CaMa-Flood v5 model source

This directory is the v5 development copy of the complete legacy model source tree. The legacy reference remains under [`../src/`](../src/); do not edit it as part of v5 work. The copy includes all existing source components, including `ICI/`, `sediment/`, `heatlink/`, and the supporting libraries. Their presence does not mean every optional configuration has been ported or validated.

The v5 output-variable inventory is maintained in [`variable_table/output_variables.csv`](../variable_table/output_variables.csv). It records the `CVARSOUT` names and verified output metadata; future CTL-generation tools should read `${CMF_BASE}/variable_table/output_variables.csv`.

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

## Map input formats

The existing `LMAPCDF` switch retains its legacy meaning: `.TRUE.` selects the consolidated NetCDF containers (`CRIVCLINC`, `CRIVPARNC`, `CMEANSLNC`, and `CMPIREGNC`) and their historical variable layout. With `LMAPCDF=.FALSE.`, each existing raster path in the namelist is read independently. The final extension selects `.bin` or `.nc`, so a single run can mix the two formats without adding namelist variables. Other extensions fail with an error identifying the file.

For individual NetCDF files, the variable name follows the logical map role rather than the filename: `ctmare`, `elevtn`, `nxtdst`, `rivlen`, `rivwth`, `rivhgt`, `rivman`, `gdwdly`, and `meansl`; `nextxy` contains `nextx` and `nexty`. `fldhgt` is read one layer at a time. Raster values must be float32, except the int32 network-index maps, and NetCDF dimensions must match `(NX,NY)` or `(NX,NY,NLFP)` as seen by the Fortran reader. The companion converter writes the NetCDF dimensions in CDL order `(lat,lon)` or `(layer,lat,lon)`; the Fortran NetCDF interface presents these as `(NX,NY)` or `(NX,NY,NLFP)`. Coordinates are cell centers derived from the configured map bounds. Missing-value and special index codes are preserved as data; input reading does not remap them.

Individual-file support is limited to maps read by the legacy Binary path: the standard river/topography inputs, optional `CGDWDLY` and `CMEANSL`, and MPI `CMPIREG`. `bifprm.txt` remains a structured text input and is not handled as a raster. The ECMWF-specific `elevslope` (`LSLOPEMOUTH`) and `mask_slope` (`LSLPMIX`) inputs exist only in the legacy consolidated NetCDF path; they are not supported as individual files. If either option is enabled with `LMAPCDF=.FALSE.`, initialization stops with an explicit error before the missing arrays can be used. Adding individual-file support for these special inputs requires a separate task and approved data contract. A Binary-only build supports individual `.bin` inputs and reports a clear error for `.nc`; it also rejects `LMAPCDF=.TRUE.` rather than silently skipping legacy initialization. A NetCDF-enabled executable supports both legacy consolidated mode and mixed individual-file mode.

The reusable Binary-to-NetCDF conversion command is documented in [`map/tools/README.md`](../map/tools/README.md#individual-map-netcdf-conversion). Legacy consolidated mode and optional groundwater, mean-sea-level, and MPI inputs were not runtime-tested in this task. The slope options are explicitly rejected in individual-file mode. Their legacy consolidated readers now open and close `CRIVPARNC` around the `elevslope` and `mask_slope` reads; no suitable consolidated fixture was available to runtime-test these special options.

## Restart files

`LRESTART` controls whether the model reads an initial state from `CRESTSTO`. `LRESTCDF=.FALSE.` selects the direct-access Binary format; `LRESTCDF=.TRUE.` selects NetCDF. Binary files use the `.bin` suffix and NetCDF files use `.nc`. `LRESTDBL` selects Float64 (`.TRUE.`) or Float32 (`.FALSE.`) only for Binary Restart. NetCDF Restart variables are written as Float64 regardless of `LRESTDBL`. These file precisions are independent of internal kinds: storage arrays use `JPRD`, while previous-step flow/depth arrays use `JPRB`, which is 4-byte with `SinglePrec_CMF` and 8-byte otherwise.

The main Binary file stores fields as consecutive `NX × NY` direct-access records. `rivsto` and `fldsto` are always records 1 and 2. A normal Restart then stores `rivout_pre`, `fldout_pre`, `rivdph_pre`, and `fldsto_pre`; storage-only Restart omits these four records. Enabled `LGDWDLY`, `LDAMOUT`, and `LLEVEE` states follow in that order, so their physical record numbers depend on the preceding options. NetCDF Restart uses the same variable names but finds fields by name rather than record position. The [`restart_variables.csv`](../variable_table/restart_variables.csv) table lists the variables, units, internal kinds, conditions, and Binary ordering.

`LSTOONLY=.TRUE.` starts from stored water volumes for assimilation-style workflows rather than restoring a complete continuous-run state. River and floodplain storage are read in either format. Previous-step river/floodplain flows and depths are initialized to zero; floodplain previous storage is set from the loaded floodplain storage. If groundwater, reservoir, or levee storage is enabled, its state is also read. Bifurcation previous-step flow is initialized to zero. For Binary Restart, the writer still creates a separate `.bin.pth` file whenever `LPTHOUT` is enabled, but the storage-only reader does not read that file. NetCDF storage-only Restart omits `pthflw_pre`. This is an existing read/write asymmetry; the Restart I/O behavior is documented here and has not been changed.

There is also a read-side difference for normal Restart: the NetCDF reader copies `rivout_pre` and `fldout_pre` into the current outflow arrays after reading, while the Binary reader leaves those current arrays at their zero initialization and restores only the `_pre` arrays. The two-year Mekong comparison described below produced identical tested outputs, but this path difference remains in the source and has not been changed.

Restart filenames combine `CVNREST`, the timestamp `YYYYMMDDHH`, and the selected suffix, for example `restart2001010100.bin` or `restart2001010100.nc`. `CRESTDIR` sets the output directory and `CRESTSTO` names the input file. `IFRQ_RST=0` writes at the end of the run; values 1–24 select hourly restart times, and 30 selects monthly boundaries (the run end is also written). The optional `.bin.pth` file contains `NPTHOUT × NPTHLEV` bifurcation-flow values and is not a geographic `NX × NY` raster.

For visualization, [`tools/generate_restart_ctl.py`](../tools/generate_restart_ctl.py) creates a one-time GrADS CTL for each main Binary Restart and uses the timestamp in its filename. GrADS Binary CTLs read standard Float32 records. For a Float64 Binary Restart, the tool creates a separate Float32 visualization copy and leaves the original Restart intact; this copy is for viewing only. See [`tools/README.md`](../tools/README.md#generate-grads-ctls-for-binary-restart-files) for details.

### NetCDF Restart verification

The two-year Mekong case was run with the same individual NetCDF maps, annual NetCDF runoff, model options, and outputs as the Binary-Restart `mek02` sample, but with `LRESTCDF=.TRUE.` and a separate output directory. Both annual runs completed, produced `restart2001010100.nc` and `restart2002010100.nc`, and the 2001 run read the 2000 restart. `ncdump -h` confirmed the expected six normal state variables plus `pthflw_pre`, all stored as `double`; optional groundwater, reservoir, and levee variables were absent because those schemes were disabled. The 2000 and 2001 `outflw`, `storge`, and `sfcelv` arrays matched the Binary-Restart run exactly at every valid grid/time cell: zero unequal values, zero missing-mask mismatches, zero non-finite values, and maximum absolute and relative differences of zero. This is an operational check for this sample configuration, not validation of storage-only or optional-scheme combinations.

## Current validation status

The default stand-alone Binary build and the separate NetCDF-enabled build both compiled on the maintainer's macOS workstation. The NetCDF executable was checked with Binary runoff/Binary output, NetCDF runoff/Binary output, and Binary runoff/NetCDF output; the selected 40-day output arrays matched bitwise across formats. A two-year annual-NetCDF-runoff/NetCDF-output run completed with restart transfer from 2000 to 2001. These checks establish operational and I/O consistency for this sample and build, not independent scientific validation or broad compiler/platform support. MPI, `heatlink/`, `sediment/`, and `ICI/` configurations remain unvalidated. See the [model source and simulation workflow plan](../docs/model_source_workflow_plan.md).
