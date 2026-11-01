# Portable map-analysis toolkit

`script_analysis/` is a portable, educational toolkit for map-analysis executables. Scientific calculations remain in the shared Fortran programs; `a01`–`a05` show editable settings and terminal commands that they print before running. Users can copy those displayed commands from the shown working directory.

## Build and prepare the portable directory

Build the analysis programs in the shared code directory, then copy all compiled `analysis_*` executables into either the shared template or a map-local example directory:

```sh
make -C map/tools/code analysis
cd map/tools/script_analysis
./a00-copy_codes.sh
```

`a00-copy_codes.sh` locates `map/tools/code/` relative to its own location. It works from both `map/tools/script_analysis/` and `map/<map>/script_analysis/` inside this repository. It copies every regular executable matching `analysis_*`, preserves permissions, and lists only the files copied during that run with `ls -lh`. A new analysis executable is included automatically; no name list in `a00` needs updating. If the shared code directory is not present, or no compiled analysis executables are available, the script reports the issue and points to the shared Makefile. It never compiles programs.

For the Mekong sample, the map-local copy can be prepared directly from the repository checkout:

```sh
cd map/mekong_15min/script_analysis
./a00-copy_codes.sh
```

This map-local route needs the repository's `map/tools/code/` directory. A standalone map-only package without that shared source/build directory cannot use `a00` to locate binaries.

The copied executables are platform and architecture dependent. Rebuild and prepare them on a different or incompatible system. Compiled binaries are excluded from Git.

## Direct command-line use

Run the programs from a map-local `script_analysis/` directory so their `../` input paths resolve to the map root:

```sh
cd map/mekong_15min/script_analysis
./analysis_river_point_info xy 62 93
./analysis_upbasin 62 93
./analysis_subbasin 50000
./analysis_river_node_latlon 90 110 5 35 > river_node_latlon.txt
```

| Executable | Arguments and output |
| --- | --- |
| `analysis_river_point_info` | `xy IX IY` or `latlon LAT LON`; prints cell and catchment details. Lat/lon lookup may use `15sec/` or `1min/` `catmxy.bin`. |
| `analysis_upbasin` | `IX IY [OUTPUT]`; indices are one-based Fortran map cells. Without `OUTPUT`, writes `analysis_upbasin_IX_IY.bin` in the map root, keeping the standard `upbasin.bin` untouched. Values are 1 upstream of the selected cell, 0 for other mapped river cells, and -9999 outside the network. |
| `analysis_subbasin` | `THRESHOLD_KM2 [SUBBSN_OUTPUT SUBCOL_OUTPUT]`; without output paths, writes threshold-labeled `analysis_subbsn_<threshold>.bin` and `analysis_subcol_<threshold>.bin` in the map root. The threshold applies to upstream-area increases across links, not a simple basin-area cutoff. |
| `analysis_river_node_latlon` | `WEST EAST SOUTH NORTH` in degrees; prints a header followed by directed connections between CaMa-Flood river-cell representative points. A link is selected when either endpoint is inside the box. Terminal links (`-9`/`-10`) use `-999` for downstream coordinates. These points are not a high-resolution river centerline. |

For `analysis_river_node_latlon`, the header is the first stdout line and remaining lines are data rows. Redirect stdout to retain both; no GIS export format is produced.

## Example shell scripts

The examples change to their required working directory, print it, show the exact shell-quoted command, then invoke the local analysis executable directly. Most use `./analysis_*` from `script_analysis/`; `a05` uses `./script_analysis/analysis_check_mpi_bifurcation` from the map root to preserve that program's input-path contract. They do not compile programs:

- [`a01-river_point_info.sh`](a01-river_point_info.sh) demonstrates querying a configured cell or geographic point. `REPORT_FILE` optionally redirects output.
- [`a02-upbasin.sh`](a02-upbasin.sh) writes an arbitrary-cell mask and report under the map's `analysis/` directory, separate from the station-selected product.
- [`a03-subbasin.sh`](a03-subbasin.sh) demonstrates a 50,000 km2 threshold and writes both rasters under `analysis/`, separate from standard products.
- [`a04-river_node_latlon.sh`](a04-river_node_latlon.sh) demonstrates extraction for a Mekong-region bounding box and saves the header plus data rows as text.
- [`a05-check_mpi_bifurcation.sh`](a05-check_mpi_bifurcation.sh) checks an existing MPI region map and can save dropped pathways as CSV.

Edit the visible settings near the start of each script. Re-running replaces the same named example outputs. For scripts that accept file paths, the displayed command quotes paths so it can be copied safely even when the path contains spaces or shell-special characters.

## Standard map-preparation workflows

`script_params/s06-subbasin.sh` and `s07-upbasin.sh` are independent map-preparation scripts. They use executables from the shared `map/tools/code/` directory, not binaries copied into `script_analysis/`. `s06` writes the standard `subbsn.bin` and `subcol.bin`; `s07` selects the valid allocated GRDC gauge with the largest `area_CaMa` and writes `upbasin-<GRDC_ID>.bin` plus reports. The Mekong sample wrappers use a 30,000 km2 labeled subbasin example and GRDC ID 2569002. See [`script_params/README.md`](../script_params/README.md#river-network-analysis).

## MPI diagnostics

`a00` includes compiled MPI analysis executables such as `analysis_check_mpi_bifurcation` in its automatic copy. The `a05` example runs from the map root because that program reads `params.txt` and map inputs relative to the current directory; it prints this working directory and the corresponding `./script_analysis/analysis_check_mpi_bifurcation ...` command. `analysis_mpi_region_color` is a separate manual visualization utility that writes `bsncol_mpi-N.bin`; s04 does not invoke it. See the [MPI map-preparation guide](../script_params/README.md#mpi-map-preparation).

Optional analysis outputs are not standard simulation inputs. Automatic CTL generation requires a matching filename row in `map/map_variables.csv`; it does not infer metadata for arbitrary output names.
