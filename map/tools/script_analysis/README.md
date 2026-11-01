# Portable map-analysis toolkit

`script_analysis/` is a portable collection of four compiled analysis programs and readable example scripts. Scientific calculations remain in the shared Fortran programs; scripts show editable settings and invocation examples. Use a copied map-local directory either by running an executable directly or by editing and running its paired shell example.

## Build and prepare the portable directory

Build the analysis programs in the shared code directory, then copy the four required executables into this template:

```sh
make -C map/tools/code analysis
cd map/tools/script_analysis
./a00-copy_codes.sh
```

`a00-copy_codes.sh` checks that all four source programs exist and are executable before copying any, then preserves their executable permissions. It never compiles programs. Run it in this canonical template before copying the prepared directory; a copied map-local directory may not have access to the shared `map/tools/code/` path.

```sh
cp -a map/tools/script_analysis map/mekong_15min/
```

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

Each example uses the executable stored beside it, changes to its own directory before execution, and does not compile programs:

- [`a01-river_point_info.sh`](a01-river_point_info.sh) demonstrates querying a configured cell or geographic point.
- [`a02-upbasin.sh`](a02-upbasin.sh) writes an arbitrary-cell mask and report under the map's `analysis/` directory, separate from the station-selected product.
- [`a03-subbasin.sh`](a03-subbasin.sh) demonstrates a 50,000 km2 threshold and writes both rasters under `analysis/`, separate from standard products.
- [`a04-river_node_latlon.sh`](a04-river_node_latlon.sh) demonstrates extraction for a Mekong-region bounding box and saves the header plus data rows as text.

Edit the visible settings near the start of each script. Re-running replaces the same named example outputs.

## Standard map-preparation workflows

`script_params/s06-subbasin.sh` and `s07-upbasin.sh` are independent map-preparation scripts. They use executables from the shared `map/tools/code/` directory, not binaries copied into `script_analysis/`. `s06` writes the standard `subbsn.bin` and `subcol.bin`; `s07` selects the valid allocated GRDC gauge with the largest `area_CaMa` and writes `upbasin-<GRDC_ID>.bin` plus reports. The Mekong sample wrappers use a 30,000 km2 labeled subbasin example and GRDC ID 2569002. See [`script_params/README.md`](../script_params/README.md#river-network-analysis).

## MPI diagnostics

The additional [`a05-check_mpi_bifurcation.sh`](a05-check_mpi_bifurcation.sh) inspects an existing MPI region raster and uses the shared `analysis_check_mpi_bifurcation` executable. Build it beforehand with `make -C map/tools/code analysis`. It is separate from the four-program portable bundle copied by `a00` and requires access to the shared tools directory.

`analysis_mpi_region_color` is a separate manual visualization utility that writes `bsncol_mpi-N.bin`; s04 does not invoke it. The MPI color raster and basin aggregation products are documented in the [MPI map-preparation guide](../script_params/README.md#mpi-map-preparation).

Optional analysis outputs are not standard simulation inputs. Automatic CTL generation requires a matching filename row in `map/map_variables.csv`; it does not infer metadata for arbitrary output names.
