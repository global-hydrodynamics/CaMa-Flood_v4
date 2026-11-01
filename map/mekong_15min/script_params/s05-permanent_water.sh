#!/bin/sh
# Estimate permanent-water area per CaMa-Flood unit catchment.
# Mekong sample workflow; run from any working directory.
#
# Inputs: params.txt, nextxy.bin, ctmare.bin, and
#         <HIRES_TAG>/location.txt plus each area's catmxy, rivwth, flddif,
#         and optional grdare binary files.
# Output: prmwat.bin (float32, NX x NY, one record, m2) in the map root.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Configuration: select the high-resolution map explicitly; no auto-detection.
HIRES_TAG=${HIRES_TAG:-1min}

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Prepare permanent-water area calculation"
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

EXEC_PRMWAT="$CODE_DIR/param_water_calc_prmwat"

# The program reads map inputs relative to the map root.
cd "$MAP_DIR"
STEP="Aggregate permanent-water pixels from $HIRES_TAG into unit catchments"
printf '==================================================\n[STEP 1/1] Estimate permanent-water area (%s)\n==================================================\n' "$HIRES_TAG"
"$EXEC_PRMWAT" "$HIRES_TAG"
printf '==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
