# Mekong sample analysis examples

This directory holds the four portable analysis examples configured for the Mekong map, plus the optional MPI connectivity wrapper. Platform-specific compiled executables are not tracked. The four analysis programs are compiled centrally under [`map/tools/code/`](../../tools/code/) and copied into the canonical [`map/tools/script_analysis/`](../../tools/script_analysis/) template before that prepared directory is copied into a map.

Build and prepare the local binaries from the repository root:

```sh
make -C map/tools/code analysis
cd map/tools/script_analysis
./a00-copy_codes.sh
for program in analysis_river_point_info analysis_upbasin analysis_subbasin analysis_river_node_latlon; do
  cp -p "$program" ../../mekong_15min/script_analysis/
done
```

`a00-copy_codes.sh` is a template-preparation step. Do not run the copy inside this map-local directory: it may not have the canonical `../code/` source directory. Rebuild executables on other operating systems or incompatible architectures.

After the four binaries have been copied, direct commands and the paired example scripts can be run from this directory:

```sh
cd map/mekong_15min/script_analysis
./analysis_river_point_info xy 62 93
./analysis_upbasin 62 93
./analysis_subbasin 50000
./analysis_river_node_latlon 90 110 5 35 > river_node_latlon.txt
```

The executables read map inputs through `../`. Run them from this map-local `script_analysis/` directory. The node-coordinate output begins with a header and then lists directed connections between CaMa-Flood cell representative points; it is not a high-resolution river centerline.

## Configured examples

- [`a01-river_point_info.sh`](a01-river_point_info.sh) inspects the one-based cell `(62,93)`; edit its query mode to use geographic coordinates instead.
- [`a02-upbasin.sh`](a02-upbasin.sh) writes an arbitrary-cell mask under `analysis/`, separate from `upbasin-2569002.bin`.
- [`a03-subbasin.sh`](a03-subbasin.sh) uses a 50,000 km2 exploratory threshold and writes under `analysis/`, separate from the sample's `subbsn-30000.bin` and `subcol-30000.bin`.
- [`a04-river_node_latlon.sh`](a04-river_node_latlon.sh) extracts Mekong-region river-node coordinate pairs to a text file.

These examples do not compile programs. Their calculations are in the shared Fortran executables. Re-running an example replaces its same-named output. These outputs are optional analyses, not standard simulation inputs.

## MPI connectivity wrapper

[`a05-check_mpi_bifurcation.sh`](a05-check_mpi_bifurcation.sh) calls the shared `analysis_check_mpi_bifurcation` executable for an existing `mpireg-N.bin`. Build that separate diagnostic with `make -C map/tools/code analysis`; it is not one of the four binaries copied by `a00`.

For standard 30,000 km2 Mekong subbasin and station-selected upstream products, use the independent `../script_params/s06-subbasin.sh` and `s07-upbasin.sh` workflows. See the [map-tools guide](../../tools/README.md) and [parameter workflow guide](../script_params/README.md#river-network-analysis).
