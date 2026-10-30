# CaMa-Flood v5 map tools

`map/tools/` contains the v5 map-processing framework. It is separate from the legacy v4 workflows under `map/src/`, which remain available and unchanged. The regionalization implementation has been run manually on the `map/mekong_15min/` development/test sample. File generation and format checks do not establish scientific validity; no regional scientific validation or v4/v5 equivalence test is claimed. Channel-parameter generation under `script_params/` is a later, separate workflow.

## Shared code and build

- `code/` contains the shared Fortran and Python implementations, one unified Makefile, and compiled executables.
- `script_region/` contains the regionalization template copied into a map directory.
- `script_params/` is reserved for the subsequent map-local channel-parameter workflow.

Regional sources and executables use `region_` prefixes; parameter tools use `param_`. The Makefile builds directly in `map/tools/code/`, and all map datasets use the same executables. Build explicitly after configuring the compiler in `adm/Mkinclude`; map-local scripts do not compile tools:

```sh
make -C map/tools/code region
make -C map/tools/code clean
```

The `params` Make target is reserved for parameter tools as they are added. Build products are ignored by Git. See the [regional workflow guide](script_region/README.md) for configuration, inputs, outputs, and checks.

## Regionalization workflow

Copy `script_region/` into the target map directory, configure `SOURCE` and the geographic bounds in `s01-regional_map.sh`, then run that script once from the copied `script_region/` directory. It calls the shared executables through `../../tools/code/`; keeping the map-local script directory as the working directory is required because the Fortran programs use relative data paths. Its sequence is crop the base map and write dimension metadata, filter bifurcation paths, derive network-analysis products, crop any supported high-resolution layers, and optionally make the sample-runoff input matrix used for channel parameterization. The exact outputs depend on the source map and available high-resolution data.

The shell script checks that all five shared executables exist before starting. It does not stop on every later program error, so review each program's terminal output and check the expected products and dimensions. The working example is `map/mekong_15min/`; its files are a development/test example, not an official or scientifically validated production map.

## GrADS CTL files

CTL generation is a separate manual step; `s01-regional_map.sh` does not invoke it. The `s02-write_ctl.sh` wrapper scans the base map and immediate child directories containing `location.txt`. The shared generator uses `params.txt` for base-map geometry and the relevant high-resolution directory's `location.txt` for high-resolution files. It follows the legacy `yrev little_endian` and cell-center convention and checks registered record layouts and binary sizes. It can describe the regionalization input matrix using its paired `diminfo_<TAG>.txt` file; the `_TAG` identifies a runoff-data/configuration variant. This matrix supports channel width/depth parameterization from sample annual-mean runoff, not the simulation runoff matrix later prepared by `script_params`.

From the copied map-local `script_region/` directory:

```sh
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

The wrapper replaces CTLs by default; pass `--no-overwrite` to retain existing files. Grid geometry stays in the map's metadata files and is not duplicated in [`map_variables.csv`](../map_variables.csv). The registry specifies shared variable names, types, dimensions, missing values, and record layouts; see the [metadata schema](../map_metadata_spec.md).

The committed Mekong `lonlat.ctl` uses `outlon` and `outlat`, matching the maintainer-confirmed registry names. The recorded wrapper execution predates the last generator fixes; the committed CTLs are present, but a post-fix wrapper run has not been confirmed in the development record. Existing generated CTLs and successful tool execution do not prove scientific correctness.

High-resolution `downxy` files contain per-pixel relative downstream offsets on their own grid. Low-resolution `downxy` uses `-9999` for sea; the matching high-resolution code for land pixels without an assigned unit catchment remains unconfirmed. Record order and interpretation are documented in the [regional workflow guide](script_region/README.md#high-resolution-downstream-links) and [map metadata specification](../map_metadata_spec.md#high-resolution-source-semantics).
