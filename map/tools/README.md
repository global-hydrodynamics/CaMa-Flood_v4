# CaMa-Flood v5 map tools

`map/tools/` contains shared v5 map-processing programs and reusable workflow templates. It is separate from the legacy v4 tools under `map/src/`. The GitHub repository also distributes `map/mekong_15min/` as its official development/test sample; the global source map `glb_15min` and some high-resolution/runoff inputs are distributed separately in CaMa-Flood data packages.

The Mekong map-local regionalization scripts are maintainer-operated **sample-preparation tools**. They create or refresh the distributed sample from package-only global source data. They are not automated tests. Adding or updating a sample does not constitute scientific validation of its river network or hydraulic properties.

## Shared code and build

- `code/` contains shared Fortran programs, the CTL generator, and a Makefile.
- `script_region/` is the reusable v5 regional-map preparation template.
- `script_params/` is the map-preparation and parameterization workflow family: channel parameters, user-configured runoff matrices, bifurcation/MPI setup, and gauge/dam allocation templates.
- `script_analysis/` is for independent map analysis and diagnostic workflows.

All shared Fortran sources and executables remain centralized in `code/`. Executable prefixes identify tool families (`region_*`, `param_*`, `analysis_*`, `allocate_*`); allocation programs do not require a separate `script_allocate/` directory. These are the approved target organization for v5 work. `map/src/` is the historical v4 tool tree and is preserved as a legacy reference; new v5 migration work belongs under `map/tools/`.

The general-purpose runoff input-matrix generator is available as `param_generate_inpmat`, built with `make -C map/tools/code param`. Its map-local wrapper template is [`script_params/s03-generate_inpmat.sh`](script_params/s03-generate_inpmat.sh); copy it into a target map's `script_params/` directory before configuring and running it. One `ROFFTAG` derives the paired `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin` names. `HIRES_TAG` independently selects the high-resolution map source. This generator supports user-selected regular runoff grids and is distinct from `region_default_inpmat`, which creates the standard climatology matrix distributed with prepared maps. See the [parameter workflow guide](script_params/README.md) for required inputs, configuration, outputs, and regression status.

The v5 [`script_params/s01-channel_params.sh`](script_params/s01-channel_params.sh) runs annual mean discharge estimation, power-law channel parameterization, and MERIT Hydro width merging only. It derives the standard paired `inpdim_test-1deg.txt` and `inpmat_test-1deg.bin` from `ROFFTAG=test-1deg`. The v5 tools include `param_calc_outclm`, `param_calc_rivwth`, and `param_set_merit_width`; they preserve the legacy calculations and write the alternate merged width as `rivwth_merit.bin`. The legacy `gosh/` simulation scripts and the historical v4 tools under `map/src/` are preserved; v5 naming and workflow changes belong under `map/tools/`. The current model reader does not automatically select the v5 alternate output. The annual runoff climatology comes from the data-rich distribution package and is not included in the GitHub source repository because its redistribution terms are not established.

The v5 [`script_params/s02-bifurcation.sh`](script_params/s02-bifurcation.sh) generates standard non-MPI bifurcation parameters and basin-connectivity products after `rivhgt.bin` is available. It preserves the legacy default `BIFLAYER=5`, `bifprm.txt` ordering, and the standard names `bifprm.txt`, `bifbsn.bin`, and `bifcol.bin`. If `BIFLAYER` exceeds the layer count recorded in `bifori.txt`, the program warns and uses the available layer count. `bifcol.bin` is always generated because it is routinely used to visualize the merged basin map. Its palette is limited to colors 2–10 (with color 1 reserved for single-cell basins); if all candidate colors are occupied by neighbors, the code falls back to color 10, so neighboring basins can share a display color. This affects visualization only. With `s02-bifurcation.sh check_map`, `param_set_bifparam` additionally writes `check_bif_dphwth.bin` (depth, then width) and `param_set_bif_basin` writes `check_bif_bsnmod.bin` (basin modification class, then connection-point class). Both are optional inspection products, not simulation inputs. Without `check_map`, they are neither created nor overwritten; existing diagnostic files may therefore be stale. The v5 tool no longer creates separate `bifmod.bin` or `bpoint.bin`; those remain in the preserved legacy v4 tree. The v5 workflow does not include obsolete `biftag.bin`. MPI-specific processing remains separate for the future `s04-mpi_map_setup.sh` workflow. Build both programs with `make -C map/tools/code param`; the wrapper uses the map-root and map-local `script_params/` working directories required by the programs.

Run the isolated s02 migration regression with:

```sh
python3 -B -m unittest map.tools.tests.test_bifurcation_migration -v
```

It compiles the shared and legacy tools in temporary directories and compares their outputs using the committed Mekong sample, without overwriting tracked map files. The test for the full s01-to-s02 chain additionally needs compatible annual runoff climatology data; provide it as `CMF_RUNOFF_CLIMATOLOGY=/path/to/ELSE_GPCC_clm-1981-2010.one`. Without that external input, only the full-chain case is explicitly skipped; the isolated s02 regression and missing-input guard still run. This is migration regression evidence, not scientific validation.

Regional executables use the `region_` prefix. Build them with the active compiler settings in `adm/Mkinclude`:

```sh
make -C map/tools/code region
python3 -B -m unittest map.tools.tests.test_region_cut_domain_optional_maps -v
make -C map/tools/code clean
```

Map-local scripts do not compile tools. Regional templates resolve shared executables from the copied `script_region/` directory; parameter templates resolve shared programs from the copied `script_params/` directory and set the CWD required by each utility. See the [regional workflow guide](script_region/README.md) and [parameter workflow guide](script_params/README.md) for configuration, inputs, outputs, and limitations.

## Regional map preparation

The reusable `script_region/` template is intended for preparing a new regional map: copy it into a target map directory, configure `SOURCE` and geographic bounds in `s01-regional_map.sh`, build the shared programs, then run the script from the copied map-local `script_region/` directory. The workflow crops the base map, filters bifurcation paths, derives network products, crops supported high-resolution layers, and creates the standard annual-runoff-climatology input mapping matrix when a supported high-resolution map is available. `region_default_inpmat` produces this distributed map product for channel cross-section parameter estimation; it is distinct from the user-configured runoff-grid matrices generated later by `param_generate_inpmat`. Exact outputs depend on source-map contents and settings.

The sample-local script directory under `map/mekong_15min/` has a different operational role: it records the maintainer's procedure for preparing the official sample. Its original global map input is provided by the distribution package, not GitHub. Do not run it in CI or point its output at the tracked sample when ordinary testing or user workflow examples are intended.

## GrADS CTL files

CTL generation is a separate step; `s01-regional_map.sh` does not call it. Run `s02-write_ctl.sh` from the map-local `script_region/` directory. The wrapper scans binaries in the base map and immediate high-resolution subdirectories with `location.txt`. The generator reads base geometry from `params.txt`, high-resolution geometry from `location.txt`, and registered binary layouts from [`map_variables.csv`](../map_variables.csv). The regionalization input matrix uses the standard `ROFFTAG=test-1deg` pair; legacy `diminfo_*.txt` descriptors are also accepted for existing maps. The generator checks registered layouts and binary sizes, and follows the legacy `yrev little_endian` and cell-center conventions.

```sh
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

The wrapper replaces CTLs by default; use `--no-overwrite` to retain existing descriptors. `map_variables.csv` is the common variable registry; [`map_metadata_spec.md`](../map_metadata_spec.md) defines its schema and the relationship between map metadata files.

### Missing and special values

The registry's `missing_value` field contains one numeric sentinel stored in the binary. `special_values` describes meaningful codes such as river mouths, boundaries, sinks, and categories; the generator does not treat these values as missing or append them to CTL descriptions. Leave `missing_value` blank when the stored sentinel is unknown.

For a blank value, the generator uses a provisional CTL-only default and warns for each affected binary: `float32=-9999`, `int32=-9999`, `int16=-9999`, `int8=-9`. The run summary includes the number of files that used a fallback. The defaults do not update the CSV or binary data. Explicit values are checked for numeric validity, integer form and storage range as applicable. If variables in one binary resolve to different missing values, the generator refuses that file.

GrADS documents byte input as unsigned (`-1,40,1`), two-byte signed input as `-1,40,2,-1`, and four-byte integer input as `-1,40,4`; its documented byte format has no signed one-byte mode. CaMa-Flood int8 binaries are signed, so the generator writes their signed sentinel `-9` as unsigned-byte `undef 247` and leaves the binary unchanged. Other negative int8 data codes display as unsigned values (for example `-1` as `255`). Use a signed-aware tool when those original codes matter. See [GrADS Variable Formats](https://sources.debian.org/data/main/g/grads/3%3A2.2.1-1/doc/variableformats.html) and the [GrADS descriptor file format](https://sources.debian.org/data/main/g/grads/2%3A2.0.2-3/doc/descriptorfile.html#VARS).

## GitHub-only automated tests

Routine tests use code and data included in the repository. They do not execute `s01-regional_map.sh` and do not require or download `glb_15min`.

```sh
python3 -B -m unittest map.tools.tests.test_generate_ctl -v
python3 -B map/tests/validate_map_variables.py
python3 -B map/tests/validate_mekong_sample.py
python3 -B -m unittest map.tools.tests.test_mekong_ctl_integration -v
python3 -B -c 'from pathlib import Path; [compile(p.read_text(), str(p), "exec") for p in [Path("map/tools/code/generate_ctl.py"), *Path("map/tools/tests").glob("*.py"), *Path("map/tests").glob("*.py")]]'
for script in map/tools/script_region/*.sh; do sh -n "$script"; done
for script in map/tools/script_params/*.sh; do sh -n "$script"; done
make -C map/tools/code region
make -C map/tools/code param
make -C map/tools/code clean
```

The synthetic unit tests cover CTL layout, multi-record/multi-layer inputs, errors, fallbacks, conflicting sentinel declarations, overwrite handling, and integer types. A separate small synthetic crop test checks that optional `catmpx.bin` and `class.bin` are copied over the requested rectangle when present and remain optional when absent. The Mekong integration test copies all sample binaries and needed metadata to a temporary tree and invokes the actual map-local CTL wrapper there; it verifies descriptors for base and high-resolution data without modifying the tracked CTLs. The sample validator checks its dimensions, binary inventory, type/layout sizes, workflow status, and registry mapping. The registry validator checks the new metadata columns and numeric sentinel ranges. CI runs these source-only and committed-sample checks; the synthetic Fortran crop test runs when `gfortran` is available. No sample-preparation script or external map dataset is run or downloaded.

**PASS** means the check ran and met its criteria. **FAIL** means it ran but criteria were violated or an implementation defect prevented the required result. **SKIP** is only for an explicitly optional compiler/toolchain that is unavailable; missing package-only map data is not a test and is not reported as SKIP. A successful CTL or file-integrity check confirms metadata/layout compatibility only, not scientific correctness. Scientific validation is a separate activity.
