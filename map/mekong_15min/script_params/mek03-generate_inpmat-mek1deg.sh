#!/bin/sh
# Generate the Mekong map-to-runoff matrix for the 1-degree runoff sample.
#
# Inputs: params.txt and 1min/location.txt with 1min.catmxy.bin.
# Outputs: inpdim_test-mek_1deg.txt and inpmat_test-mek_1deg.bin in the map root.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Runoff grid and output tag; these describe runoff data, not the river map grid.
ROFFTAG="test-mek_1deg"  # Runoff sample ID used in the paired output filenames.
GRSIZEIN=1                # Runoff grid-cell size (degrees).
WESTIN=90                 # Western boundary (degrees longitude).
EASTIN=110                # Eastern boundary (degrees longitude).
NORTHIN=35                # Northern boundary (degrees latitude).
SOUTHIN=5                 # Southern boundary (degrees latitude).
OLAT="NtoS"               # Runoff latitude rows are ordered north to south.

# High-resolution map selection is independent of the runoff-grid tag.
HIRES_TAG="1min"  # High-resolution source used to map runoff cells to river catchments.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Select high-resolution map input"
# The exit trap reports failures and preserves status; signal traps use standard nonzero statuses.
trap '
    EXIT_STATUS=$?
    trap - 0
    if [ "$EXIT_STATUS" -ne 0 ]; then
        printf "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
        printf "!!! ERROR: %s FAILED !!!\n" "$SCRIPT_NAME" >&2
        printf "!!! Step: %s\n" "$STEP" >&2
        printf "!!! Exit status: %s\n" "$EXIT_STATUS" >&2
        printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
    fi
    exit "$EXIT_STATUS"
' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

# ----------------------------------------------------------------------
# End of error handling.
# ----------------------------------------------------------------------

# Select the map-resolution input automatically when HIRES_TAG is not set.
if [ -z "$HIRES_TAG" ]; then
    if [ -f "$MAP_DIR/1min/location.txt" ]; then
        HIRES_TAG=1min
    elif [ -f "$MAP_DIR/15sec/location.txt" ]; then
        HIRES_TAG=15sec
    else
        echo "Set HIRES_TAG to an available map directory (expected 1min or 15sec)." >&2
        exit 1
    fi
fi

INPDIM="inpdim_${ROFFTAG}.txt"
INPMAT="inpmat_${ROFFTAG}.bin"

if [ -e "$MAP_DIR/$INPDIM" ] || [ -e "$MAP_DIR/$INPMAT" ]; then
    printf 'ERROR: refusing to overwrite existing %s or %s\n' "$INPDIM" "$INPMAT" >&2
    exit 1
fi

EXEC_INPMAT="$CODE_DIR/param_inpmat_generate"

# For each river cell, find its runoff-grid source cell and write the matrix and descriptor.
cd "$MAP_DIR"

STEP="Map river cells to the configured runoff grid"
printf '\n==================================================\n[STEP 1/1] Generate runoff input matrix\n==================================================\n\n'
"$EXEC_INPMAT" "$HIRES_TAG" "$GRSIZEIN" "$WESTIN" "$EASTIN" \
    "$NORTHIN" "$SOUTHIN" "$OLAT" "$INPDIM" "$INPMAT"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
