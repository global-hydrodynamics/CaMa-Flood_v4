# Regional map preparation

This guide describes the v5 regionalization workflow under `map/tools/`. It prepares a regional map from a compatible source map. The legacy v4 workflow remains under `map/src/src_region/` and is not modified here. Configure and execute `s01-regional_map.sh` once; the individual Fortran programs are implementation details and should not be run separately for the standard workflow.

## Inputs and settings

The source directory must contain a compatible `params.txt`, `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, and `bifori.txt`. `cut_domain` reads the map geometry and listed grids; `cut_bifway` filters `bifori.txt` using the crop metadata and regional network. Confirm the binary formats and dimensions match the selected source map.

High-resolution cropping is optional. For each supported tag (`1min`, `30sec`, `15sec`, or `3sec`), the source needs that directory's `location.txt`, which lists area names, bounds, dimensions, and cell size, plus the corresponding source layer files read by the combiner. If present, the workflow crops the first available high-resolution tag in the order `1min`, `30sec`, `15sec`, `3sec` to make the channel-parameterization input matrix. This matrix step also requires the sample annual-mean runoff data under `map/data`. These are external/source datasets; the repository's workflow code does not supply a replacement global map or runoff archive.

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

`s01-regional_map.sh` runs the following sequence internally: `region_cut_domain`, `region_cut_bifway`, `region_set_map`, `region_combine_hires` for available source tags, and `region_generate_inpmat` when a suitable high-resolution map exists. The generated `inpmat_test-1deg.bin` and its `diminfo_test-1deg.txt` description support channel width/depth calculations using sample runoff. `generate_inpmat` reads high-resolution `grdare` as km² and converts positive values to m² for the overlap matrix; FLOW generates `grdare` from land-pixel areas, so coastal cells contribute only their land area. These files are not the simulation-specific runoff input matrix, which belongs to the later `script_params/` workflow.

## Outputs and checks

| Stage | Outputs | Role |
| --- | --- | --- |
| Script and crop metadata | `region_info.txt` and `dim_change.txt` in `script_region/` | Requested source/bounds and crop offsets/dimensions used by subsequent programs. |
| `cut_domain` | Parent map: `params.txt`; cropped `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, `lsmask.bin`; `nextxy_noedge.bin` | Cropped base-resolution map fields and bifurcation-filter input. `nextxy_noedge.bin` is in the parent map directory. |
| `cut_bifway` | Parent map: `bifori.txt` | Source bifurcation paths that remain within the regional network. |
| `set_map` | Parent map: `rivseq.bin`, `basin.bin`, `bsncol.bin`, `upgrid.bin`, `uparea_grid.bin`, `nxtdst_grid.bin`, `rivlen_grid.bin` | Derived river ordering, basin/visualization, and grid-based analysis products. `rivlen_grid.bin` intentionally contains the same downstream-distance array as `nxtdst_grid.bin` for rectangular-grid simulations. |
| `combine_hires` | Regional high-resolution tag directory, including layers and its `location.txt` | Cropped high-resolution layers and geometry metadata, when corresponding source data are available. |
| `generate_inpmat` | Parent map: `inpmat_test-1deg.bin` and `diminfo_test-1deg.txt` | Preprocessing input for channel width/depth parameterization when the required high-resolution map and sample runoff are available. |
| Separate CTL step | `.ctl` files beside eligible binaries | GrADS descriptors based on the base `params.txt` or high-resolution `location.txt`; not written by `s01`. |

Consult [`map_variables.csv`](../../map_variables.csv) for per-product type, dimensions, record layout, missing-value convention, and model/runtime role. Core model inputs and preprocessing/analysis products are distinguished there; the table above names outputs without duplicating every variable definition.

## High-resolution downstream links

In high-resolution `downxy` files, record 1 (`downx`) and record 2 (`downy`) are signed 16-bit offsets on that same high-resolution grid. For a source pixel `(ix, iy)` with 1-based indices, add the two offsets to recover its downstream pixel: `(ix + downx, iy + downy)`. These are relative offsets, not absolute indices. FLOW adjusts the x offset for periodic wrapping at the global longitude seam. The paired sentinel values are `-999` for a river mouth and `-1000` for an inland termination. The output array starts at `-9999`; source inspection shows this remains when no downstream offset is assigned. The maintainer confirms `-9999` means sea in the low-resolution layer, but the high-resolution layer may use a different value for land pixels without an assigned unit catchment; that encoding remains unconfirmed. Consumers treat `downx <= -900` as a stop condition. The regional combiner preserves the offsets as-is. The common registry distinguishes the resolutions and records the unresolved high-resolution sentinel.

Review terminal output after each internal program. The shell checks for build products before starting but does not stop on every processing failure. Confirm expected outputs exist, dimensions agree with the generated `params.txt` / `location.txt`, and binary sizes match the declared record layout. These checks establish file consistency only; they do not validate network correctness or scientific suitability. Run the separate CTL wrapper from the same `script_region/` directory after reviewing map outputs:

```sh
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

The wrapper uses `params.txt` for the base map and each immediate child directory's `location.txt` for its own grid; existing CTLs are overwritten by default. A high-resolution `location.txt` starts with the area count, then a column header, then one row per area containing its identifier/name, bounds, dimensions, and cell size.

## Verification status and limitations

The regionalization workflow and the CTL wrapper have been manually used with the Mekong development/test sample. Source inspection establishes the described file paths and program behavior; the repository records an initial CTL run with 18 base and 12 high-resolution descriptors, including failures for `inpmat_test-1deg.bin` and `width.bin` and a skip for `lsmask.bin`. Subsequent generator changes passed synthetic tests, but a post-fix Mekong wrapper run has not been confirmed in the development record. The committed Mekong `lonlat.ctl` uses the `outlon` / `outlat` labels now recorded in the registry.

Presence of files or successful generation is not scientific validation. No full simulation, river-network validation, or v4/v5 numerical equivalence test is claimed. High-resolution scientific meaning and selected units remain provisional where indicated in the registry. The shell does not stop after every failed Fortran command, so inspect its complete output and do not treat a final zero exit status alone as proof that every stage succeeded.
