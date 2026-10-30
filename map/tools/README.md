# CaMa-Flood v5 map tools

`map/tools/` contains reusable map-processing code and map-local workflows for v5. The legacy v4 tools and workflows under `map/src/` remain unchanged. The regional workflow is awaiting maintainer execution and scientific validation.

## Directory roles and naming

- `code/` contains shared Fortran/Python implementations, the unified Makefile, and shared compiled executables.
- `script_region/` contains map-local settings and the regional workflow script.
- `script_params/` is reserved for channel-parameter workflows.

Source and executable names use `region_*.F90` → `region_*` for regionalization, and `param_*.F90` → `param_*` for channel-parameter tools. Existing Fortran `PROGRAM` identifiers are retained. Regional sources currently have no module or cross-file source dependencies.

## Build shared tools

Build tools explicitly from `map/tools/code/` after setting compiler options in the repository's `adm/Mkinclude`:

```sh
make -C map/tools/code region
make -C map/tools/code params
```

`region` builds regional programs; `params` builds available parameter programs. Both targets place executables and compiler intermediates directly in `map/tools/code/`. The same executables are shared by all map datasets. Rebuild them after changing compiler settings. Remove generated build products with:

```sh
make -C map/tools/code clean
```

## Run a map-local workflow

Copy the regional script directory into a map dataset, configure its source path and region bounds, then run the script while keeping that directory as the current working directory:

```sh
mkdir -p map/myregion_15min
cp -a map/tools/script_region map/myregion_15min/
cd map/myregion_15min/script_region
./s01-regional_map.sh
```

Build the shared `region` executables before running the script. It calls them through `../../tools/code/` and keeps the map-local script directory as its working directory, as the Fortran programs use relative input and output paths. The script writes `region_info.txt` and working metadata in `script_region/`; map products are written under the parent map directory. See the script-specific README for inputs, outputs, and checks.

## Generate GrADS CTL files

The CTL generator is a separate Python step; regionalization does not invoke it automatically. It scans only `.bin` files directly inside the selected directory, matches them to `map/map_variables.csv`, checks the declared type, record layout, numeric missing-value convention, grid dimensions, and exact file size, then writes a sibling `.ctl` file. Unregistered files and incomplete or unsupported registry entries are reported and skipped; size or layout mismatches are reported as failures.

After copying the template into a map dataset and reviewing regionalization outputs, run its wrapper from that map's `script_region/` directory. It scans the base map and all immediate child directories that contain `location.txt`:

```sh
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

The wrapper uses `params.txt` for the base map and each high-resolution directory's own `location.txt`, matching each `<AREA>` filename to the corresponding `carea` row. Geometry stays in those map-local files, not in the shared CSV. `--dry-run` validates and lists planned files without writing. Existing CTLs are replaced by default; pass `--no-overwrite` to preserve them. For direct generator use or a different registry, call `code/generate_ctl.py` with `--map-dir` and optionally `--metadata-csv PATH`. The generator supports the legacy regular-grid convention (`yrev little_endian`) and only explicitly recognized record layouts. It scans one directory per invocation; the wrapper handles the base directory and high-resolution children.

`map_variables.csv` must describe the binary filename or template, primitive type, symbolic dimensions, numeric missing-value convention, and record layout. Independent records can use one row per variable with `record_index` and `record_count`, or a structured `record_layout` such as `record 1 downx; record 2 downy`. A layered variable uses `dimensions=NX,NY,NLFP`, `record_count=NLFP`, and `records 1..NLFP; record k is layer k`. The regionalization input matrix is also supported: the shared suffix (for example `_test-1deg`) is a runoff-data/configuration tag, and pairs `inpmat_<TAG>.bin` with `diminfo_<TAG>.txt` as written by `region_generate_inpmat.F90`. The generator reads `INPN` and the paired binary name from that description, checks map dimensions and bounds against `params.txt`, and describes the two integer-index fields plus the floating-point overlap-area field. A same-basename `.txt` is also accepted. Unrecognized layouts and unsupported metadata are skipped; binary-size or declared-grid mismatches are failures. The current Mekong sample has metadata/layout candidates among base files `ctmare.bin`, `elevtn.bin`, `fldhgt.bin`, `nextxy.bin`, `downxy.bin`, `lonlat.bin`, `nextxy_noedge.bin`, `nxtdst.bin`, `rivlen.bin`, `grdare.bin`, `basin.bin`, `bsncol.bin`, `rivseq.bin`, `uparea.bin`, `upgrid.bin`, `uparea_grid.bin`, `nxtdst_grid.bin`, and `rivlen_grid.bin`; its `1min/` directory has corresponding registry entries for all twelve present layers. The tool still checks each binary size when run. `width.bin` with unrecognized dimensions is skipped until they are specified; `lsmask.bin` uses `-9999` as its CTL missing value.

The input matrix created by regionalization supports later channel width/depth parameter calculation using sample runoff; it is not a simulation-specific runoff input matrix. These tools are experimental development files, and successful CTL generation is not scientific validation.
