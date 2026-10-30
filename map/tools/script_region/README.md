# Regional map preparation

This guide documents the reusable v5 regional-map preparation template under `map/tools/`. It prepares a regional map from a compatible source map. The legacy v4 workflow remains under `map/src/src_region/` and is not modified here. Configure and execute `s01-regional_map.sh` for a target map; the individual Fortran programs are implementation details and should not be run separately for the standard workflow.

The scripts under `map/mekong_15min/` are maintained separately to create or update the official GitHub sample using the original global map distributed with CaMa-Flood packages. They are sample-preparation tools, not test scripts. GitHub CI does not run them. The reusable template documented here can be used to prepare other regional maps.

## Inputs and settings

The source directory must contain a compatible `params.txt`, `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, and `bifori.txt`. `catmpx.bin` and `class.bin` are optional supplementary int32 rasters; when present, `region_cut_domain` crops their values over the full selected grid, including non-river cells. `cut_bifway` filters `bifori.txt` using the crop metadata and regional network. Confirm the binary formats and dimensions match the selected source map.

High-resolution cropping is optional. For each supported tag (`1min`, `30sec`, `15sec`, or `3sec`), the source needs that directory's `location.txt`, which lists area names, bounds, dimensions, and cell size, plus the corresponding source layer files read by the combiner. If present, the workflow crops the first available high-resolution tag in the order `1min`, `30sec`, `15sec`, `3sec` and creates the standard climatology input mapping matrix. Matrix generation uses map geometry and high-resolution lookup/area layers; it does not read runoff data. A compatible annual runoff climatology is needed later to use this matrix for channel cross-section parameter estimation. These are external/source datasets; the repository's workflow code does not supply a replacement global map or runoff archive.

Copy the template into a map directory and edit the settings at the top of `s01-regional_map.sh`:

```sh
mkdir -p map/myregion_15min
cp -a map/tools/script_region map/myregion_15min/
```

```sh
SOURCE="../../glb_15min/"
WEST="90.0"
EAST="110.0"
SOUTH="5.0"
NORTH="35.0"
```

`SOURCE` is relative to the copied `script_region/` directory or absolute; keep the trailing slash. Choose bounds contained in the source map's coverage. The Mekong example uses the v400 global map and 90–110°E, 5–35°N.

## Build and run

Build the shared executables explicitly from the repository root, using the active compiler configuration in `adm/Mkinclude`:

```sh
make -C map/tools/code region
cd map/myregion_15min/script_region
./s01-regional_map.sh
```

Do not run it from the map root: the Fortran programs use paths relative to the map-local `script_region/` working directory. The shell verifies the five shared executables exist under `../../tools/code/` but does not compile them. The same binaries serve all map datasets. Rebuild them after changing compiler settings; clean shared build products with `make -C map/tools/code clean` from the repository root.

`s01-regional_map.sh` runs the following sequence internally: `region_cut_domain`, `region_cut_bifway`, `region_set_map`, `region_combine_hires` for available source tags, and `region_default_inpmat` when a suitable high-resolution map exists. `region_default_inpmat` creates the standard input mapping matrix for the annual runoff climatology used to estimate river channel cross-sections. The matrix (`inpmat_test-1deg.bin`) and paired `diminfo_test-1deg.txt` are standard map products and should be distributed with prepared regional maps. The program reads high-resolution `grdare` in km2 and converts positive values to m2 for the overlap matrix; FLOW generates `grdare` from land-pixel areas, so coastal cells contribute only their land area. This fixed-grid product is distinct from the simulation-specific runoff input matrix generated later by the general-purpose `param_generate_inpmat` workflow in `script_params/`.

## Outputs and checks

| Stage | Outputs | Role |
| --- | --- | --- |
| Script and crop metadata | `region_info.txt` and `dim_change.txt` in `script_region/` | Requested source/bounds and crop offsets/dimensions used by subsequent programs. |
| `cut_domain` | Parent map: `params.txt`; cropped `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, `lsmask.bin`; optional `catmpx.bin` and `class.bin` when supplied by the source; `nextxy_noedge.bin` | Cropped base-resolution map fields and bifurcation-filter input. The optional supplementary rasters retain their source int32 values across every cell in the selected rectangle. `nextxy_noedge.bin` is in the parent map directory. |
| `cut_bifway` | Parent map: `bifori.txt` | Source bifurcation paths that remain within the regional network. |
| `set_map` | Parent map: `rivseq.bin`, `basin.bin`, `bsncol.bin`, `upgrid.bin`, `uparea_grid.bin`, `nxtdst_grid.bin`, `rivlen_grid.bin` | Derived river ordering, basin/visualization, and grid-based analysis products. `rivlen_grid.bin` intentionally contains the same downstream-distance array as `nxtdst_grid.bin` for rectangular-grid simulations. |
| `combine_hires` | Regional high-resolution tag directory, including layers and its `location.txt` | Cropped high-resolution layers and geometry metadata, when corresponding source data are available. |
| `region_default_inpmat` | Parent map: `inpmat_test-1deg.bin` and `diminfo_test-1deg.txt` | Standard runoff-climatology mapping product for channel cross-section parameter estimation; distributed with prepared maps when generated. |
| Separate CTL step | `.ctl` files beside eligible binaries | GrADS descriptors based on the base `params.txt` or high-resolution `location.txt`; not written by `s01`. |

Consult [`map_variables.csv`](../../map_variables.csv) for per-product type, dimensions, record layout, numeric missing value, special codes, and model/runtime role. Core model inputs and preprocessing/analysis products are distinguished there; the table above names outputs without duplicating every variable definition.

## High-resolution downstream links

In high-resolution `downxy` files, record 1 (`downx`) and record 2 (`downy`) are signed 16-bit offsets on that same high-resolution grid. For a source pixel `(ix, iy)` with 1-based indices, add the two offsets to recover its downstream pixel: `(ix + downx, iy + downy)`. These are relative offsets, not absolute indices. FLOW adjusts the x offset for periodic wrapping at the global longitude seam. The paired sentinel values are `-999` for a river mouth and `-1000` for an inland termination. The output array starts at `-9999`; source inspection shows this remains when no downstream offset is assigned. The maintainer confirms `-9999` means sea in the low-resolution layer, but the high-resolution layer may use a different value for land pixels without an assigned unit catchment; that encoding remains unconfirmed. Consumers treat `downx <= -900` as a stop condition. The regional combiner preserves the offsets as-is. The common registry distinguishes the resolutions and records the unresolved high-resolution sentinel.

Review terminal output after each internal program. The shell checks for build products before starting but does not stop on every processing failure. Confirm expected outputs exist, dimensions agree with the generated `params.txt` / `location.txt`, and binary sizes match the declared record layout. These checks establish file consistency only; they do not validate network correctness or scientific suitability. Run the separate CTL wrapper from the same `script_region/` directory after reviewing map outputs:

```sh
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

The wrapper uses `params.txt` for the base map and each immediate child directory's `location.txt` for its own grid; existing CTLs are overwritten by default. A high-resolution `location.txt` starts with the area count, then a column header, then one row per area containing its identifier/name, bounds, dimensions, and cell size.

## Automated checks and scientific validation

The automated Mekong CTL check copies the tracked binary files and metadata to a temporary directory and runs the `s02-write_ctl.sh` wrapper there. It verifies CTL generation and file-format declarations without overwriting the checked-in sample. Other synthetic CTL generator checks and the sample/registry validators are listed in the [map-tools overview](../README.md#github-only-automated-tests). GitHub Actions does not execute this regionalization template or the Mekong sample-preparation scripts.

Successful regional product generation, binary size checks, and CTL generation do not establish that the river network or hydraulic fields are scientifically correct. No full simulation, independent river-network validation, or v4/v5 numerical equivalence test is claimed. Check the registry for variables whose meaning, special codes, or units remain provisional. The regionalization shell does not stop after every failed Fortran command, so inspect the complete output and each expected product rather than relying only on the final exit code.
