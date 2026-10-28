# Regional map preparation

`src_region/` contains Fortran programs and sample shell scripts for deriving a regional map from an existing source map. The repository copy at `map/src/src_region/` is a reusable template. Copy it into the target map directory and run the workflow from that map-local `src_region/` working directory; its programs use relative paths to read a source map and write regional products in the parent map directory. Review those paths and settings for the actual source and target maps before execution.

This is a data-processing workflow, not a ready-made regional dataset. It requires a compatible source map and, for high-resolution products, the corresponding source high-resolution layers. The supplied scripts contain sample domains and assumptions; they do not establish universal coverage or compatibility.

## Main workflow

The orchestration example is `s01-regional_map.sh`. It writes `region_info.txt` with five lines: source-map directory, west edge, east edge, south edge, and north edge. The default values in the script describe a CONUS example sourced from `glb_06min`; `sample_tej_s01-regional_map.sh` shows a Japan-domain configuration sourced from `jpn_01min`. Set these values for the intended output and confirm that the source map exists at the relative path from the map-local work directory.

The script then runs the core programs in this order:

1. `cut_domain.F90` extracts the selected region and writes regional map arrays, updated map parameters, and `dim_change.txt`.
2. `cut_bifway.F90` adjusts bifurcation pathways to the extracted domain.
3. `set_map.F90` derives regional network lookup products, including river sequence, upstream-grid/area, basin, and related grids.
4. When source `location.txt` files are present for supported high-resolution tags, `combine_hires.F90` copies/intersects those layers into the regional map's corresponding high-resolution directories.
5. `generate_inpmat.F90` creates a regional runoff input matrix using the first available tag in the script's priority order. Its implementation has its own grid assumptions; inspect the source and generated dimension metadata for the chosen case.
6. The control-file scripts create GrADS `.ctl` visualization metadata for map and available high-resolution products. These files describe data for visualization and are not themselves the binary map data.

The main map outputs include the cropped network/topography arrays and derived files such as `rivseq.bin`, `uparea_grid.bin`, `upgrid.bin`, `nxtdst_grid.bin`, `rivlen_grid.bin`, `basin.bin`, and `bsncol.bin`. Exact products vary with source contents and the workflow settings. High-resolution layers and `.ctl` files are conditional on available inputs and script paths.

## Programs and interfaces

| Program or script | Responsibility |
| --- | --- |
| `s01-regional_map.sh` | Sample orchestration; sets source map and bounds, then invokes the processing and metadata steps. |
| `cut_domain` | Reads `region_info.txt` and source `params.txt`; writes regional base-map arrays, `../params.txt`, and `dim_change.txt`. |
| `cut_bifway` | Reads the domain-change mapping and source bifurcation information; writes region-adjusted bifurcation pathways. |
| `set_map` | Derives map network indices, river sequence, basin and upstream diagnostic grids from the regional map. |
| `combine_hires <tag>` | Combines a source high-resolution layer set into a regional tag directory when source data are present. |
| `generate_inpmat <tag> ...` | Generates the regional input matrix. The sample shell wrapper relies on its compiled interface and source-specific assumptions; consult the Fortran usage and settings before calling it directly. |
| `s02-wrte_ctl_map.sh`, `s03-wrte_ctl_hires.sh` | Call `gradsinfo`, `wrte_ctl_map`, and `wrte_ctl_hires` to write visualization control files for available products. |

Build targets are listed in this directory's `Makefile`; it includes the repository's active `adm/Mkinclude` configuration. Compile in the map-local copy using the compiler configuration appropriate for the project. No regionalization executable or dataset is built by the model's main `src/` Makefile.

## Product metadata

Regional readers and writers may crop core map files and derive products such as `rivseq.bin`, `uparea_grid.bin`, `upgrid.bin`, `nxtdst_grid.bin`, `rivlen_grid.bin`, `basin.bin`, and `bsncol.bin`. Their verified roles, symbolic layouts, and confidence states are tracked in [`map/map_variables.csv`](../../map_variables.csv), with schema notes in [`map/map_metadata_spec.md`](../../map_metadata_spec.md). Do not treat these regional/diagnostic outputs as original global-map generators or as universally required simulation inputs.

## Practical checks and limitations

- Preserve the source map's resolution, grid alignment, and required metadata. The output bounds must be compatible with the source grid and downstream network.
- Confirm relative paths from the working directory before running. The scripts call executables in the current directory and refer to the parent directory as the output map root.
- Check which high-resolution tags actually exist. Although the scripts enumerate tags, they only process a tag when the expected source `location.txt` exists; regional products are not guaranteed for every listed resolution.
- Review the generated `params.txt`, `dim_change.txt`, input-matrix metadata, and required map files before using the regional map in a simulation.
- The `.ctl` generation steps are optional visualization support. Their presence does not validate the regional map or replace binary outputs.

For parameter preparation after regionalization, see [`../src_param/README.md`](../src_param/README.md). For map data acquisition and map/model compatibility context, see the [map overview](../../README.md).
