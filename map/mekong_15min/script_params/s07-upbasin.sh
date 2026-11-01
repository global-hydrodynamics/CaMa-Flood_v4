#!/bin/sh
# Select the largest valid allocated GRDC gauge, report its river cell, and create its upstream mask.
# Copy to <map>/script_params/ and run from any working directory.
# Input: map-local GRDC_alloc.txt from t01-alloc_flow_gauge.sh; standard river-map files.
# Outputs: upbasin-<GRDC_ID>.bin plus a concise station-selection report and river information.
# Existing output files are overwritten. This script does not run t01 automatically.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# User settings: choose the GRDC allocation table if it is not in the map root.
GRDC_ALLOC="$MAP_DIR/GRDC_alloc.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Locate the GRDC allocation table"
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

EXEC_RIVINFO="$CODE_DIR/analysis_river_point_info"
EXEC_UPBASIN="$CODE_DIR/analysis_upbasin"
STATION_REPORT="$MAP_DIR/grdc_station_selection.txt"
RIVER_INFO_REPORT="$MAP_DIR/grdc_river_info.txt"

if [ ! -r "$GRDC_ALLOC" ]; then
    printf '[ERROR] %s is not readable. Run t01-alloc_flow_gauge.sh or provide GRDC_alloc.txt.\n' "$GRDC_ALLOC" >&2
    exit 1
fi

# GRDC_alloc.txt has a fixed column layout; strict > keeps the first station on ties.
STEP="Select the largest valid allocated GRDC station"
if ! SELECTED=$(awk '
NR == 1 { next }
$8 != -9 && $9 > 0 && $10 > 0 && $5 > max {
    max = $5
    id = $1
    ix = $9
    iy = $10
}
END {
    if (id == "") exit 1
    print id, ix, iy, max
}' "$GRDC_ALLOC"); then
    printf '[ERROR] %s contains no valid allocated station.\n' "$GRDC_ALLOC" >&2
    exit 1
fi

set -- $SELECTED
GRDC_ID=$1
IX=$2
IY=$3
AREA_CAMA=$4
UPBASIN_OUTPUT="$MAP_DIR/upbasin-${GRDC_ID}.bin"

STEP="Save the selected station record"
printf 'ID\tix1\tiy1\tarea_CaMa_km2\n' > "$STATION_REPORT"
printf '%s\t%s\t%s\t%s\n' \
    "$GRDC_ID" "$IX" "$IY" "$AREA_CAMA" \
    >> "$STATION_REPORT"

# Use the allocation's primary grid cell; re-mapping lat/lon can select a different cell.
cd "$SCRIPT_DIR"
STEP="Write river information for GRDC $GRDC_ID at cell ($IX,$IY)"
printf '==================================================\n[STEP 1/2] Inspect selected GRDC river cell\n==================================================\n'
"$EXEC_RIVINFO" xy "$IX" "$IY" > "$RIVER_INFO_REPORT"

STEP="Create the single-cell upstream-basin mask for GRDC $GRDC_ID"
printf '==================================================\n[STEP 2/2] Create upstream-basin mask\n==================================================\n'
"$EXEC_UPBASIN" "$IX" "$IY" "$UPBASIN_OUTPUT"
printf 'Selected GRDC station: %s; cell=(%s,%s); area_CaMa=%s km2\n' \
    "$GRDC_ID" "$IX" "$IY" "$AREA_CAMA"
printf 'Station summary: %s\nRiver information: %s\nUpstream mask: %s\n' \
    "$STATION_REPORT" "$RIVER_INFO_REPORT" "$UPBASIN_OUTPUT"
printf '==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
