#!/bin/sh
# Generate a map-to-runoff matrix for a regular runoff grid.
# Copy to <map>/script_params/, edit the grid settings, and run from any CWD.
#
# Inputs: params.txt and <HIRES_TAG>/location.txt with each area's catmxy.bin.
# Outputs: inpdim_<ROFFTAG>.txt and inpmat_<ROFFTAG>.bin in the map root.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Configuration: these values describe the runoff grid, not the map grid.
ROFFTAG="${ROFFTAG:-test-15min_nc}"
GRSIZEIN=0.25
WESTIN=-180.0
EASTIN=180.0
NORTHIN=90.0
SOUTHIN=-60.0
OLAT="NtoS" # NtoS: north row first; StoN: south row first.

# HIRES_TAG selects map-resolution input and is distinct from ROFFTAG.
HIRES_TAG=${HIRES_TAG:-}

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Select high-resolution map input"
# On exit, report failures and return the original status; signals use standard nonzero statuses.
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

EXEC_INPMAT="$CODE_DIR/param_inpmat_generate"

# For each river cell, find its runoff-grid source cell and write the matrix and descriptor.
cd "$MAP_DIR"

STEP="Map river cells to the configured runoff grid"
printf '\n==================================================\n[STEP 1/1] Generate runoff input matrix\n==================================================\n\n'
"$EXEC_INPMAT" "$HIRES_TAG" "$GRSIZEIN" "$WESTIN" "$EASTIN" \
    "$NORTHIN" "$SOUTHIN" "$OLAT" "$INPDIM" "$INPMAT"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
