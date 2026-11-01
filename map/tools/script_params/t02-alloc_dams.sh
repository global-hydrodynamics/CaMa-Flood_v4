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

EXEC_ALLOCATE="$CODE_DIR/allocate_dam"

cd "$SCRIPT_DIR"
STEP="Allocate dams and separate river, small, and error records"
printf '[STEP 1/1] Allocate dams (%s)\n' "$HIRES_TAG"
"$EXEC_ALLOCATE" "$DAM_IN" "$HIRES_TAG"
mv ./dam_alloc_river.txt "$DAM_RIVER_OUT"
mv ./dam_alloc_small.txt "$DAM_SMALL_OUT"
mv ./dam_alloc_error.txt "$DAM_ERROR_OUT"
printf '[SUCCESS] %s\n' "$SCRIPT_NAME"
