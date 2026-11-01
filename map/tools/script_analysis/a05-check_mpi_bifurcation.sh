#!/bin/sh
# Example: inspect bifurcation connectivity for an existing MPI region map.
# The executable reads map inputs from the map root, so this example runs there.
set -eu

# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# Analysis output directory, relative to script_analysis/.
OUT_DIR="out_analysis"  # Change to ../out_analysis to use the map-root output folder.

# Command-line inputs: an existing region map and optional dropped-path CSV filename.
if [ "$#" -lt 1 ] || [ "$#" -gt 3 ] || [ "$#" -eq 2 ]; then
    printf '\n[ERROR] Usage: %s <region-map.bin> [--csv <dropped.csv>]\n' "$0" >&2
    exit 2
fi

# The second and third positional arguments are optional; :- supplies an empty default.
# A relative CSV filename is placed in OUT_DIR; an absolute path is used as given.
REGION_MAP=$1
CSV_OPTION=${2:-}
CSV_ARGUMENT=${3:-}
CSV_IN_OUT_DIR=0

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# The sed expression escapes apostrophes so printed paths can be pasted into a shell.
quote_shell_arg() {
    printf "'"
    printf '%s' "$1" | sed "s/'/'\\\\''/g"
    printf "'"
}

if [ "$#" -eq 3 ] && [ "$CSV_OPTION" != "--csv" ]; then
    printf '\n[ERROR] Usage: %s <region-map.bin> [--csv <dropped.csv>]\n' "$0" >&2
    exit 2
fi

if [ -n "$CSV_ARGUMENT" ]; then
    case "$CSV_ARGUMENT" in
        /*) CSV_FILE=$CSV_ARGUMENT ;;
        *)
            CSV_FILE="$SCRIPT_DIR/$OUT_DIR/$CSV_ARGUMENT"
            CSV_IN_OUT_DIR=1
            ;;
    esac
fi

# Keep absolute paths; resolve relative filenames from the map root.
case "$REGION_MAP" in
    /*) ;;
    *) REGION_MAP="$MAP_DIR/$REGION_MAP" ;;
esac

if [ ! -s "$REGION_MAP" ]; then
    printf '\n[ERROR] Region map not found: %s\n' "$REGION_MAP" >&2
    exit 1
fi

if [ "$CSV_IN_OUT_DIR" -eq 1 ]; then
    mkdir -p "$SCRIPT_DIR/$OUT_DIR"
fi

# This diagnostic reads params.txt and map files relative to the map root.
cd "$MAP_DIR"

printf '\nWorking directory: %s\n' "$PWD"

STEP="Check bifurcation connectivity for $REGION_MAP"
printf '\n==================================================\n'
printf '[STEP 1/1] Check bifurcation connectivity\n'
printf '==================================================\n\n'

if [ "$#" -ge 3 ]; then
    printf './script_analysis/code/analysis_check_mpi_bifurcation '
    quote_shell_arg "$REGION_MAP"
    printf ' '
    quote_shell_arg "$CSV_OPTION"
    printf ' '
    quote_shell_arg "$CSV_FILE"
    printf '\n'
    ./script_analysis/code/analysis_check_mpi_bifurcation "$REGION_MAP" "$CSV_OPTION" "$CSV_FILE"
else
    printf './script_analysis/code/analysis_check_mpi_bifurcation '
    quote_shell_arg "$REGION_MAP"
    printf '\n'
    ./script_analysis/code/analysis_check_mpi_bifurcation "$REGION_MAP"
fi

printf '\n==================================================\n'
printf '[SUCCESS] MPI bifurcation connectivity check complete.\n'
printf '==================================================\n'
