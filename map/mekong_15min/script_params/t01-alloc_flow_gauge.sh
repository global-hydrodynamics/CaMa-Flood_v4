#!/bin/sh
# Map GRDC flow gauges from MERIT Hydro to CaMa-Flood unit catchments.
# Inputs: GRDC_allocated.csv, params.txt, uparea.bin, nextxy.bin, and selected HIRES_TAG files.
# Output: GRDC_alloc.txt in the map root.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# User settings
GAUGE_IN="../../data/GRDC_allocated.csv"
GAUGE_OUT="$MAP_DIR/GRDC_alloc.txt"
GAUGE_MODE="multi"
HIRES_TAG="1min"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare flow-gauge allocation"
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

EXEC_ALLOCATE="$CODE_DIR/allocate_flow_gauge"

cd "$SCRIPT_DIR"

STEP="Allocate gauges to the CaMa-Flood river network"
printf '\n==================================================\n[STEP 1/2] Allocate flow gauges (%s, %s)\n==================================================\n\n' "$GAUGE_MODE" "$HIRES_TAG"
"$EXEC_ALLOCATE" "$GAUGE_IN" "$GAUGE_MODE" "$HIRES_TAG"

STEP="Move allocated flow-gauge table into the map root"
printf '\n==================================================\n[STEP 2/2] Save GRDC_alloc.txt\n==================================================\n\n'
mv ./gauge_alloc.txt "$GAUGE_OUT"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
