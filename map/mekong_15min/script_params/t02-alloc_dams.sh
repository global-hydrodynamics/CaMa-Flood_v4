#!/bin/sh
# Map GRanD reservoirs from MERIT Hydro to CaMa-Flood unit catchments.
# Inputs: GRanD_allocated.csv, params.txt, uparea.bin, ctmare.bin, nextxy.bin, and selected HIRES_TAG files.
# Outputs: GRanD_river.txt, GRanD_small.txt, and GRanD_error.txt in the map root.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# User settings
DAM_IN="../../data/GRanD_allocated.csv"
DAM_RIVER_OUT="$MAP_DIR/GRanD_river.txt"
DAM_SMALL_OUT="$MAP_DIR/GRanD_small.txt"
DAM_ERROR_OUT="$MAP_DIR/GRanD_error.txt"
HIRES_TAG="1min"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare dam-allocation workflow"
# Report the failed stage with the original command status.
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

EXEC_ALLOCATE="$CODE_DIR/allocate_dam"

cd "$SCRIPT_DIR"
STEP="Allocate dams and separate river, small, and error records"
printf '\n==================================================\n[STEP 1/2] Allocate dams (%s)\n==================================================\n\n' "$HIRES_TAG"
"$EXEC_ALLOCATE" "$DAM_IN" "$HIRES_TAG"

STEP="Move allocated dam tables into the map root"
printf '\n==================================================\n[STEP 2/2] Save GRanD allocation tables\n==================================================\n\n'
mv ./dam_alloc_river.txt "$DAM_RIVER_OUT"
mv ./dam_alloc_small.txt "$DAM_SMALL_OUT"
mv ./dam_alloc_error.txt "$DAM_ERROR_OUT"
printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
