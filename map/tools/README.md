# CaMa-Flood v5 map tools

`map/tools/` contains shared v5 map-processing programs and reusable workflow templates. It is separate from the legacy v4 tools under `map/src/`. The GitHub repository also distributes `map/mekong_15min/` as its official development/test sample; the global source map `glb_15min` and some high-resolution/runoff inputs are distributed separately in CaMa-Flood data packages.

The Mekong map-local regionalization scripts are maintainer-operated **sample-preparation tools**. They create or refresh the distributed sample from package-only global source data. They are not automated tests. Adding or updating a sample does not constitute scientific validation of its river network or hydraulic properties.

## Shared code and build

- `code/` contains shared Fortran programs, the CTL generator, and a Makefile.
- `script_region/` is the reusable v5 regional-map preparation template.
- `script_params/` is the map-preparation and parameterization workflow family: channel parameters, user-configured runoff matrices, bifurcation/MPI setup, and gauge/dam allocation templates.
- `script_analysis/` is for independent map analysis and diagnostic workflows.

All shared Fortran sources and executables remain centralized in `code/`. Executable prefixes identify tool families (`region_*`, `param_*`, `analysis_*`, `allocate_*`); allocation programs do not require a separate `script_allocate/` directory. These are the approved target organization for migration; existing `map/src/` legacy tools remain unchanged.

The general-purpose runoff input-matrix generator is available as `param_generate_inpmat`, built with `make -C map/tools/code param`. Its map-local wrapper template is [`script_params/s02-generate_inpmat.sh`](script_params/s02-generate_inpmat.sh); copy it into a target map's `script_params/` directory before configuring and running it. This generator supports user-selected regular runoff grids and is distinct from `region_default_inpmat`, which creates the standard climatology matrix distributed with prepared maps. See the [parameter workflow guide](script_params/README.md) for required inputs, configuration, outputs, and regression status.

Regional executables use the `region_` prefix. Build them with the active compiler settings in `adm/Mkinclude`:

```sh
make -C map/tools/code region
python3 -B -m unittest map.tools.tests.test_region_cut_domain_optional_maps -v
make -C map/tools/code clean
```

Map-local scripts do not compile tools. They invoke shared executables through paths relative to the target map's `script_region/` working directory. See the [regional workflow guide](script_region/README.md) for user configuration, required source inputs, outputs, and limitations.

## Regional map preparation

The reusable `script_region/` template is intended for preparing a new regional map: copy it into a target map directory, configure `SOURCE` and geographic bounds in `s01-regional_map.sh`, build the shared programs, then run the script from the copied map-local `script_region/` directory. The workflow crops the base map, filters bifurcation paths, derives network products, crops supported high-resolution layers, and creates the standard annual-runoff-climatology input mapping matrix when a supported high-resolution map is available. `region_default_inpmat` produces this distributed map product for channel cross-section parameter estimation; it is distinct from the user-configured runoff-grid matrices generated later by `param_generate_inpmat`. Exact outputs depend on source-map contents and settings.

The sample-local script directory under `map/mekong_15min/` has a different operational role: it records the maintainer's procedure for preparing the official sample. Its original global map input is provided by the distribution package, not GitHub. Do not run it in CI or point its output at the tracked sample when ordinary testing or user workflow examples are intended.

## GrADS CTL files

CTL generation is a separate step; `s01-regional_map.sh` does not call it. Run `s02-write_ctl.sh` from the map-local `script_region/` directory. The wrapper scans binaries in the base map and immediate high-resolution subdirectories with `location.txt`. The generator reads base geometry from `params.txt`, high-resolution geometry from `location.txt`, and registered binary layouts from [`map_variables.csv`](../map_variables.csv). Paired `diminfo_<TAG>.txt` metadata supports the regionalization input matrix. The generator checks registered layouts and binary sizes, and follows the legacy `yrev little_endian` and cell-center conventions.

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
make -C map/tools/code region
make -C map/tools/code clean
```

The synthetic unit tests cover CTL layout, multi-record/multi-layer inputs, errors, fallbacks, conflicting sentinel declarations, overwrite handling, and integer types. A separate small synthetic crop test checks that optional `catmpx.bin` and `class.bin` are copied over the requested rectangle when present and remain optional when absent. The Mekong integration test copies all sample binaries and needed metadata to a temporary tree and invokes the actual map-local CTL wrapper there; it verifies descriptors for base and high-resolution data without modifying the tracked CTLs. The sample validator checks its dimensions, binary inventory, type/layout sizes, workflow status, and registry mapping. The registry validator checks the new metadata columns and numeric sentinel ranges. CI runs these source-only and committed-sample checks; the synthetic Fortran crop test runs when `gfortran` is available. No sample-preparation script or external map dataset is run or downloaded.

**PASS** means the check ran and met its criteria. **FAIL** means it ran but criteria were violated or an implementation defect prevented the required result. **SKIP** is only for an explicitly optional compiler/toolchain that is unavailable; missing package-only map data is not a test and is not reported as SKIP. A successful CTL or file-integrity check confirms metadata/layout compatibility only, not scientific correctness. Scientific validation is a separate activity.
