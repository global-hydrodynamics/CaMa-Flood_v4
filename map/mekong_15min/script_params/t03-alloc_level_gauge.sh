#!/bin/sh
# Map water-level gauges from MERIT Hydro to CaMa-Flood unit catchments.
# Inputs: MLIT_LevelGauge_allocated.csv, params.txt, uparea.bin, elevtn.bin, nextxy.bin, and selected HIRES_TAG files.
# Output: MLIT_levelgauge.txt in the map root.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Water-level gauge allocation input and map-root output.
GAUGE_IN="../../data/MLIT_LevelGauge_allocated.csv"  # MERIT Hydro-preallocated MLIT level-gauge table.
GAUGE_OUT="$MAP_DIR/MLIT_levelgauge.txt"             # CaMa-Flood level-gauge table written in the map root.
HIRES_TAG="1min"                                     # High-resolution map directory tag (for example, 1min or 15sec).

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare level-gauge allocation"
# The exit trap reports the failed stage and preserves the original command status.
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

EXEC_ALLOCATE="$CODE_DIR/allocate_level_gauge"

cd "$SCRIPT_DIR"

STEP="Allocate water-level gauges and calculate river-path attributes"
printf '\n==================================================\n[STEP 1/2] Allocate level gauges (%s)\n==================================================\n\n' "$HIRES_TAG"
"$EXEC_ALLOCATE" "$GAUGE_IN" "$HIRES_TAG"

STEP="Move allocated level-gauge table into the map root"
printf '\n==================================================\n[STEP 2/2] Save MLIT_levelgauge.txt\n==================================================\n\n'
mv ./gauge_alloc.txt "$GAUGE_OUT"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
