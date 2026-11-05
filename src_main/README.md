# CaMa-Flood v5 model source

This directory is the v5 development copy of the complete legacy model source tree. The legacy reference remains under [`../src/`](../src/); do not edit it as part of v5 work. The copy includes all existing source components, including `ICI/`, `sediment/`, `heatlink/`, and the supporting libraries. Their presence does not mean every optional configuration has been ported or validated.

The v5 output-variable inventory is maintained in [`variable_table/output_variables.csv`](../variable_table/output_variables.csv). It supplies metadata for every standard `CVARSOUT` output: long name, units, temporal aggregation, missing value, aliases, and required options. The registry does not select Fortran memory. The output dispatcher explicitly binds each supported name to the array passed to the public generic API, for example `CALL OUTPUT_WRITE('rivout', D2MY_DISCHARGE)`. The caller is responsible for ensuring that the supplied array has the scientific meaning described by the selected metadata. To add an output, add its metadata row and an explicit dispatcher call; no registry parser, conversion, MPI, or writer changes should be needed for an ordinary unit-catchment field.

The `&NOUTPUT` setting `CMF_BASE` identifies the installation/repository root; `COUTVAR_TABLE` optionally overrides the CSV path and defaults to `${CMF_BASE}/variable_table/output_variables.csv`. Both settings remain optional in existing namelists. When `CMF_BASE` is left at its default, the model infers the root from the configured `CDIMINFO` map path when it is under the installation's `map/` directory, or from the executable's standard `src_main/` or `out/bin/` location. Maintained `gosh_sample/` runners set the root explicitly. If the configured registry cannot be opened, initialization reports the attempted path and stops. Registry headers are resolved by name; `alias_of` is authoritative, while `internal_variable` and `internal_dtype` are informational only.

Compatibility aliases, including `outflw` → `totout`, `storge` → `totsto`, `gwsto` → `gdwsto`, `gwout` → `gdwrtn`, `rofsfc` → `runoff`, and `rofsub` → `runoffsub`, inherit descriptive metadata through `alias_of` while retaining the requested filename and NetCDF variable name. Both subsurface runoff names require `LROSPLIT=.TRUE.`; requesting either with split runoff disabled stops initialization before output files are created. `pthflw` uses the bifurcation dimensions `NPTHOUT × NPTHLEV` in both Binary and NetCDF output. The NetCDF variable follows the established `pthflw_pre` Restart dimension order, writes Float32 output-interval means with the standard time coordinate and compression settings, and uses the existing MPI path aggregation. The Binary `.pth` layout is unchanged.

The focused output-registry integration suite uses the one-day Mekong sample and temporary run directories. It checks Binary/NetCDF aliases, pthflw Binary/NetCDF record equality and dimensions, metadata inheritance with reordered and quoted CSV fields, required metadata and duplicate-row errors, omitted Namelist settings, disabled optional runoff, and fixed-double storage arrays. After building the Binary and NetCDF executables (and installing `ncdump`), run `python3 -m unittest gosh_sample.etc.test.test_output_aliases -v` from the repository root.

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

## NetCDF output compression

New model output files and Restart files use lossless NetCDF-4 deflate level 3 with shuffle. `NDLEVEL` controls main and sediment output compression and defaults to 3; set it to 0 to disable deflate. Restart precision and variables are unchanged. See the shared [NetCDF output conventions](../docs/netcdf_output_conventions.md) for the inventory and format exception.

## Restart files

`LRESTART` controls whether the model reads an initial state from `CRESTSTO`. `LRESTCDF=.FALSE.` selects the direct-access Binary format; `LRESTCDF=.TRUE.` selects NetCDF. Binary files use the `.bin` suffix and NetCDF files use `.nc`. `LRESTDBL` selects Float64 (`.TRUE.`) or Float32 (`.FALSE.`) only for Binary Restart. NetCDF Restart variables are written as Float64 regardless of `LRESTDBL`. These file precisions are independent of internal kinds: storage arrays use `JPRD`, while previous-step flow/depth arrays use `JPRB`, which is 4-byte with `SinglePrec_CMF` and 8-byte otherwise.

### Normal and storage-only Restart design

Normal Restart is intended to resume a continuous simulation from its saved state. It restores river and floodplain storage and the previous-step flow/depth fields needed by the next step. Storage-only Restart (`LSTOONLY=.TRUE.`) is a separate initialization mode: it uses available storage states, such as river and floodplain volumes, without restoring the saved previous-step flow/depth fields. The initialization can reconstruct some previous-step quantities from the loaded storage and hydraulic state, but these are not the saved values from the original run. Storage-only is intended for workflows such as Kalman-filter data assimilation, where storage may have been updated but corresponding previous-step flow information is unavailable. A Storage-only continuation is therefore a new simulation from the supplied storage state; its results are not guaranteed to match the original uninterrupted run.

The main Binary file has no header and stores fields as consecutive `NX × NY` direct-access records. `LRESTDBL` selects Float32 or Float64 file records independently of the internal array kinds. `rivsto` and `fldsto` are always records 1 and 2. A normal Restart then stores `rivout_pre`, `fldout_pre`, `rivdph_pre`, and `fldsto_pre`; Storage-only Restart omits these four records. Enabled groundwater delay (`LGDWDLY`), reservoir (`LDAMOUT`), and levee (`LLEVEE`) states follow in that order. Their physical record numbers therefore vary with the selected mode and enabled options. Read and write runs must use matching Restart mode and optional-scheme settings so the reader interprets the same record layout. NetCDF Restart stores variables as Float64 and selects them by name rather than record position. The [`restart_variables.csv`](../variable_table/restart_variables.csv) table lists the variables, units, internal kinds, conditions, and Binary ordering.

Optional schemes require their own Restart state only when enabled. Bifurcation flow is stored separately in a `.bin.pth` companion for Binary Normal Restart. The Binary writer currently creates that companion whenever `LPTHOUT` is enabled, including Storage-only mode, while the Storage-only reader skips it and leaves previous bifurcation flow at zero. NetCDF Storage-only Restart omits `pthflw_pre`. Groundwater, reservoir, and levee storage remain available in Storage-only mode when their corresponding options are enabled. These conditional fields also change Binary record positions; use the registry rather than assuming a fixed record number.

In the stand-alone driver, `CMF_DRV_ADVANCE` writes Restart state after physics and any scheduled output for the current step, then updates the model time. With HeatLink enabled, the coupled driver performs the synchronized Restart write after its end-of-step update. `IFRQ_RST` selects the end of run (`0`), an hourly boundary (`1`–`24`), or a month boundary (`30`); the run end is also written. The Restart mode and optional-scheme settings used when reading must match those used to create the file. `restart_is_write_time()` exposes the existing schedule decision to the coupled driver; its scheduling logic is unchanged here.

Both Normal Restart readers now copy restored `rivout_pre` and `fldout_pre` values into the current outflow arrays, so this initialization is aligned across Binary and NetCDF by source inspection. The two-year Mekong comparison described below was run before the Binary assignment was added; it matched the tested output variables, but the corrected first-step diagnostic state has not been runtime-validated separately. See the implementation history and validation limit in the [model source and simulation workflow plan](../docs/model_source_workflow_plan.md#restart-initialization-review).

Restart filenames combine `CVNREST`, the timestamp `YYYYMMDDHH`, and the selected suffix, for example `restart2001010100.bin` or `restart2001010100.nc`. `CRESTDIR` sets the output directory and `CRESTSTO` names the input file. The optional `.bin.pth` file contains `NPTHOUT × NPTHLEV` bifurcation-flow values and is not a geographic `NX × NY` raster.

For visualization, [`tools/generate_restart_ctl.py`](../tools/generate_restart_ctl.py) creates a one-time GrADS CTL for each main Binary Restart and uses the timestamp in its filename. GrADS Binary CTLs read standard Float32 records. For a Float64 Binary Restart, the tool creates a separate Float32 visualization copy and leaves the original Restart intact; this copy is for viewing only. See [`tools/README.md`](../tools/README.md#generate-grads-ctls-for-binary-restart-files) for details.

### NetCDF Restart verification

The two-year Mekong case was run with the same individual NetCDF maps, annual NetCDF runoff, model options, and outputs as the Binary-Restart `mek02` sample, but with `LRESTCDF=.TRUE.` and a separate output directory. Both annual runs completed, produced `restart2001010100.nc` and `restart2002010100.nc`, and the 2001 run read the 2000 restart. `ncdump -h` confirmed the expected six normal state variables plus `pthflw_pre`, all stored as `double`; optional groundwater, reservoir, and levee variables were absent because those schemes were disabled. The 2000 and 2001 `outflw`, `storge`, and `sfcelv` arrays matched the Binary-Restart run exactly at every valid grid/time cell: zero unequal values, zero missing-mask mismatches, zero non-finite values, and maximum absolute and relative differences of zero. This is an operational check for this sample configuration, not validation of storage-only or optional-scheme combinations.

## Current validation status

The default stand-alone Binary build and the separate NetCDF-enabled build both compiled on the maintainer's macOS workstation. The NetCDF executable was checked with Binary runoff/Binary output, NetCDF runoff/Binary output, and Binary runoff/NetCDF output; the selected 40-day output arrays matched bitwise across formats. A two-year annual-NetCDF-runoff/NetCDF-output run completed with restart transfer from 2000 to 2001. These checks establish operational and I/O consistency for this sample and build, not independent scientific validation or broad compiler/platform support. MPI, `heatlink/`, `sediment/`, and `ICI/` configurations remain unvalidated. See the [model source and simulation workflow plan](../docs/model_source_workflow_plan.md).
