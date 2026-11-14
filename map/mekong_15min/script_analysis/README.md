# Mekong sample analysis examples

This directory contains Mekong-configured command-line examples for the shared CaMa-Flood map-analysis programs. The Fortran executables are built centrally in `map/tools/code/`; the local `code` symlink points to those shared executables.

Build the programs from the repository root, then create the relative link:

```sh
make -C map/tools/code analysis
cd map/mekong_15min/script_analysis
./a00-link_codes.sh
```

The sample link is `code -> ../../tools/code`. `a00-link_codes.sh` does not compile, copy, or delete executables. It can repair an incorrect or broken symlink, but refuses to replace a real file or directory. The link depends on the repository's shared `map/tools/code/` directory, so it is not portable to a standalone map package. Compiled executables remain ignored by Git.

Previously copied executable files are not needed when using the link. Inspect any local `analysis_*` files with `ls -lh analysis_*` and remove only files confirmed as old copied build products; the setup script leaves them untouched.

## Output directory

`a01`–`a04` write their files under `script_analysis/out_analysis/` by default. `a05` prints its diagnostic to the terminal and writes a dropped-path CSV there when requested. Each relevant script has an editable `OUT_DIR="out_analysis"` setting and creates the selected directory when needed. Set `OUT_DIR="../out_analysis"` to use `map/mekong_15min/out_analysis/`. Both output locations are ignored by Git.

- `a01-river_point_info.sh` writes the selected cell or geographic query report.
- `a02-upbasin.sh` writes an exploratory outlet mask and report, separate from `upbasin-2569002.bin`.
- `a03-subbasin.sh` writes a 50,000 km2 exploratory subbasin and color map, separate from `subbsn-sample.bin` and `subcol-sample.bin`.
- `a04-river_node_latlon.sh` writes a text table for the configured Mekong-region bounds.
- `a05-check_mpi_bifurcation.sh` optionally writes dropped pathways as CSV; a relative filename is placed in `OUT_DIR`.

## Running examples

Run from this directory:

```sh
cd map/mekong_15min/script_analysis
./a01-river_point_info.sh
./a02-upbasin.sh
./a03-subbasin.sh
./a04-river_node_latlon.sh
./a05-check_mpi_bifurcation.sh mpireg-16.bin
```

Each example prints the working directory and actual command before execution. Most call `./code/analysis_*` from `script_analysis/`. The MPI connectivity program reads `params.txt` and map files relative to the map root, so `a05` runs there using `./script_analysis/code/analysis_check_mpi_bifurcation`.

For standard subbasin and GRDC-selected upstream products, use the independent [`script_params/s06-subbasin.sh`](../script_params/s06-subbasin.sh) and [`s07-upbasin.sh`](../script_params/s07-upbasin.sh) workflows. See the [map-tools guide](../../tools/README.md) and [parameter workflow guide](../script_params/README.md#river-network-analysis).
