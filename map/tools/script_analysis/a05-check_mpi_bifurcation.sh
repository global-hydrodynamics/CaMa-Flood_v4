#!/bin/sh
# Independently inspect bifurcation connectivity for an existing MPI region map.
# Copy into <map>/script_analysis/. Relative filenames are interpreted at map root.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)
[ -x "$CODE_DIR/analysis_check_mpi_bifurcation" ] || {
    printf '\n[ERROR] Missing shared executable: %s\n' "$CODE_DIR/analysis_check_mpi_bifurcation" >&2
    echo "Build it beforehand with: make -C map/tools/code analysis" >&2
    exit 1
}
[ "$#" -ge 1 ] || { printf '\n[ERROR] Usage: %s <region-map.bin> [--csv <dropped.csv>]\n' "$0" >&2; exit 2; }
[ "$#" -le 3 ] || { printf '\n[ERROR] Usage: %s <region-map.bin> [--csv <dropped.csv>]\n' "$0" >&2; exit 2; }
case $1 in
    /*) REGION_MAP=$1 ;;
    *) REGION_MAP="$MAP_DIR/$1" ;;
esac
[ -s "$REGION_MAP" ] || { printf '\n[ERROR] Region map not found: %s\n' "$REGION_MAP" >&2; exit 1; }

cd "$MAP_DIR"
printf '\n==================================================\n'
printf '[STEP 1/1] Check bifurcation connectivity for %s\n' "$REGION_MAP"
printf '==================================================\n\n'
if [ "$#" -eq 1 ]; then
    "$CODE_DIR/analysis_check_mpi_bifurcation" "$REGION_MAP"
else
    "$CODE_DIR/analysis_check_mpi_bifurcation" "$REGION_MAP" "$2" "$3"
fi

printf '\n==================================================\n'
printf '[SUCCESS] MPI bifurcation connectivity check complete.\n'
printf '==================================================\n'
