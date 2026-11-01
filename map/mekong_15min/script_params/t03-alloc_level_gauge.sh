#!/bin/sh
# Map water-level gauges from MERIT Hydro to CaMa-Flood unit catchments.
# Inputs: MLIT_LevelGauge_allocated.csv, params.txt, uparea.bin, elevtn.bin, nextxy.bin, and selected HIRES_TAG files.
# Output: MLIT_levelgauge.txt in the map root.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# User settings
GAUGE_IN="../../data/MLIT_LevelGauge_allocated.csv"
GAUGE_OUT="$MAP_DIR/MLIT_levelgauge.txt"
HIRES_TAG="1min"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare level-gauge allocation"
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

EXEC_ALLOCATE="$CODE_DIR/allocate_level_gauge"

cd "$SCRIPT_DIR"
STEP="Allocate water-level gauges and calculate river-path attributes"
printf '[STEP 1/1] Allocate level gauges (%s)\n' "$HIRES_TAG"
"$EXEC_ALLOCATE" "$GAUGE_IN" "$HIRES_TAG"
mv ./gauge_alloc.txt "$GAUGE_OUT"
printf '[SUCCESS] %s\n' "$SCRIPT_NAME"
