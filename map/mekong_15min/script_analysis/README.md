# Mekong sample analysis examples

This directory contains Mekong-configured command-line examples for map-analysis programs. Platform-specific compiled executables are not tracked. Programs are compiled centrally under [`map/tools/code/`](../../tools/code/); `a00-copy_codes.sh` copies available executables into this directory.

Build the programs from the repository root, then prepare the Mekong-local binaries:

```sh
make -C map/tools/code analysis
cd map/mekong_15min/script_analysis
./a00-copy_codes.sh
```

`a00-copy_codes.sh` locates the shared code directory relative to its own location and copies all regular executable files matching `analysis_*`, preserving their permissions. It prints the copied filenames and sizes. New `analysis_*` executables are included automatically, and the script never compiles programs. This map-local workflow requires the repository's `map/tools/code/` directory; a standalone map package without that tree cannot use `a00` to find the compiled programs.

After the binaries have been copied, direct commands and the paired example scripts can be run from this directory:

```sh
cd map/mekong_15min/script_analysis
./analysis_river_point_info xy 62 93
./analysis_upbasin 62 93
./analysis_subbasin 50000
./analysis_river_node_latlon 90 110 5 35 > river_node_latlon.txt
```

The direct commands above use the map-local `script_analysis/` working directory. The `a01`–`a05` shell examples show that directory, print the exact command before running it, and invoke the corresponding `./analysis_*` executable. You can copy the displayed command into a terminal. The MPI connectivity program is an exception to the map-local working-directory rule: it reads map inputs from the map root, so `a05` changes there and prints a `./script_analysis/analysis_check_mpi_bifurcation ...` command. The node-coordinate output begins with a header and then lists directed connections between CaMa-Flood cell representative points; it is not a high-resolution river centerline.

## Configured examples

- [`a01-river_point_info.sh`](a01-river_point_info.sh) inspects the one-based cell `(62,93)`; edit its query mode to use geographic coordinates instead.
- [`a02-upbasin.sh`](a02-upbasin.sh) writes an arbitrary-cell mask under `analysis/`, separate from `upbasin-2569002.bin`.
- [`a03-subbasin.sh`](a03-subbasin.sh) uses a 50,000 km2 exploratory threshold and writes under `analysis/`, separate from the sample's `subbsn-30000.bin` and `subcol-30000.bin`.
- [`a04-river_node_latlon.sh`](a04-river_node_latlon.sh) extracts Mekong-region river-node coordinate pairs to a text file.
- [`a05-check_mpi_bifurcation.sh`](a05-check_mpi_bifurcation.sh) checks an existing MPI region map and optionally records dropped pathways in CSV.

These examples do not compile programs. Their calculations are in the shared Fortran executables. Re-running an example replaces its same-named output. These outputs are optional analyses, not standard simulation inputs.

## MPI connectivity wrapper

[`a05-check_mpi_bifurcation.sh`](a05-check_mpi_bifurcation.sh) calls the map-local `analysis_check_mpi_bifurcation` executable for an existing `mpireg-N.bin`; `a00` copies it along with other compiled `analysis_*` programs.

For standard 30,000 km2 Mekong subbasin and station-selected upstream products, use the independent `../script_params/s06-subbasin.sh` and `s07-upbasin.sh` workflows. See the [map-tools guide](../../tools/README.md) and [parameter workflow guide](../script_params/README.md#river-network-analysis).
