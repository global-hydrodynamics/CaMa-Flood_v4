# Portable map-analysis toolkit

`script_analysis/` contains educational command-line examples and their generated outputs. Scientific calculations remain in the shared Fortran programs under `map/tools/code/`.

## Build and link the shared executables

Build the analysis programs first, then create the local relative link to the shared code directory:

```sh
make -C map/tools/code analysis
cd map/tools/script_analysis
./a00-link_codes.sh
```

The canonical link is `code -> ../code`. In a map-local directory such as `map/mekong_15min/script_analysis/`, it is `code -> ../../tools/code`. `a00-link_codes.sh` works from either directory, does not compile or copy executables, and will repair a wrong or broken symlink. It refuses to replace a real file or directory. The link only works while its target exists; it is not suitable for a standalone map package without `map/tools/code/`.

For the Mekong example:

```sh
cd map/mekong_15min/script_analysis
./a00-link_codes.sh
```

Compiled executables are platform-specific and remain excluded from Git. If you previously copied `analysis_*` executables into a map-local directory, inspect them with `ls -lh analysis_*` and remove only files you have confirmed were copied build products. The link script does not delete files.

## Output directory

The `a01`–`a04` examples write their files under `script_analysis/out_analysis/` by default. `a05` prints its diagnostic to the terminal and writes a dropped-path CSV there when requested. Each script has an editable `OUT_DIR="out_analysis"` setting near the top and creates that directory when it writes output. Set `OUT_DIR="../out_analysis"` to save under the map root instead. The default and map-root output directories are ignored by Git.

`a01` writes a text report; `a02` writes an outlet-specific upstream mask and report; `a03` writes threshold-labeled subbasin and color rasters; `a04` writes a text table of river-node coordinate pairs. Call `a05` with `--csv <filename>` to save dropped pathways; relative CSV filenames go under `OUT_DIR`.

## Direct command-line use

Run most programs from the map-local `script_analysis/` directory so their `../` map-input paths resolve:

```sh
cd map/mekong_15min/script_analysis
./code/analysis_river_point_info xy 62 93
./code/analysis_upbasin 62 93
./code/analysis_subbasin 50000
./code/analysis_river_node_latlon 90 110 5 35
```

`analysis_upbasin` and `analysis_subbasin` accept optional output paths; without them, the direct executables use their own documented analysis filenames. The example scripts provide explicit output paths under `OUT_DIR` so they do not replace standard products.

| Executable | Arguments and output |
| --- | --- |
| `analysis_river_point_info` | `xy IX IY` or `latlon LAT LON`; prints cell and catchment details. Lat/lon lookup may use `15sec/` or `1min/` `catmxy.bin`. |
| `analysis_upbasin` | `IX IY [OUTPUT]`; indices are one-based Fortran map cells. Its default output is a coordinate-labeled mask in the map root. |
| `analysis_subbasin` | `THRESHOLD_KM2 [SUBBSN_OUTPUT SUBCOL_OUTPUT]`; its default outputs are threshold-labeled rasters in the map root. The threshold applies to upstream-area increases across links, not a simple basin-area cutoff. |
| `analysis_river_node_latlon` | `WEST EAST SOUTH NORTH` in degrees; prints directed connections between CaMa-Flood river-cell representative points. These points are not a high-resolution river centerline. |

## Example shell scripts

The `a01`–`a05` scripts show the working directory and print each command before running it through `./code/analysis_*`. Their settings are editable near the beginning. Most examples run from `script_analysis/`; `a05` runs from the map root because `analysis_check_mpi_bifurcation` reads `params.txt` and map files relative to its current directory. It invokes the executable through `./script_analysis/code/` and prints that exact command.

- [`a01-river_point_info.sh`](a01-river_point_info.sh) demonstrates grid-index or geographic queries.
- [`a02-upbasin.sh`](a02-upbasin.sh) creates an exploratory mask for one configured cell.
- [`a03-subbasin.sh`](a03-subbasin.sh) demonstrates an exploratory 50,000 km2 threshold.
- [`a04-river_node_latlon.sh`](a04-river_node_latlon.sh) extracts connections in a configured geographic rectangle.
- [`a05-check_mpi_bifurcation.sh`](a05-check_mpi_bifurcation.sh) checks an existing `mpireg-N.bin` and can write dropped pathways as CSV.

These independent examples do not replace standard preparation workflows. `script_params/s06-subbasin.sh` and `s07-upbasin.sh` use the shared executables directly and write their intended standard/sample products. See the [parameter workflow guide](../script_params/README.md#river-network-analysis).

The separate `analysis_mpi_region_color` utility writes optional `bsncol_mpi-N.bin` maps; s04 does not invoke it. See the [MPI map-preparation guide](../script_params/README.md#mpi-map-preparation).
