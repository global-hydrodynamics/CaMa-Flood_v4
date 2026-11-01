#!/bin/sh
# Independently inspect bifurcation connectivity for an existing MPI region map.
# Copy into <map>/script_analysis/. Relative filenames are interpreted at map root.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)
[ "$#" -ge 1 ] || { echo "Usage: $0 <region-map.bin> [--csv <dropped.csv>]" >&2; exit 2; }
[ "$#" -le 3 ] || { echo "Usage: $0 <region-map.bin> [--csv <dropped.csv>]" >&2; exit 2; }
make -C "$CODE_DIR" analysis || { echo "Failed to build analysis tool" >&2; exit 1; }
[ -x "$CODE_DIR/analysis_check_mpi_bifurcation" ] || { echo "Missing shared executable: $CODE_DIR/analysis_check_mpi_bifurcation" >&2; exit 1; }
[ -s "$MAP_DIR/$1" ] || { echo "Region map not found: $MAP_DIR/$1" >&2; exit 1; }
cd "$MAP_DIR"
"$CODE_DIR/analysis_check_mpi_bifurcation" "$@"
