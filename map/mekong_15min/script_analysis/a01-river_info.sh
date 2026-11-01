#!/bin/sh
# Inspect river-network information at a configured map cell or geographic point.
# Copy to <map>/script_analysis/ and run from any working directory.
# Inputs: standard river-map files; latlon mode may also use 15sec/ or 1min/ catmxy data.
# Output: optional text report; otherwise details are printed to the terminal.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Configuration: xy indices are one-based Fortran coordinates; lat/lon values are degrees.
QUERY_MODE=xy
IX=62
IY=93
LAT=11.579
LON=104.946
REPORT_FILE="$MAP_DIR/river_info.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Prepare river-cell query"
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

EXEC_RIVINFO="$CODE_DIR/analysis_get_rivinfo"
cd "$SCRIPT_DIR"
STEP="Inspect river information using $QUERY_MODE coordinates"
printf '==================================================\n[STEP 1/1] Inspect river cell\n==================================================\n'
case "$QUERY_MODE" in
    xy)
        if [ -n "$REPORT_FILE" ]; then
            "$EXEC_RIVINFO" xy "$IX" "$IY" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            "$EXEC_RIVINFO" xy "$IX" "$IY"
        fi
        ;;
    latlon)
        if [ -n "$REPORT_FILE" ]; then
            "$EXEC_RIVINFO" latlon "$LAT" "$LON" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            "$EXEC_RIVINFO" latlon "$LAT" "$LON"
        fi
        ;;
    *)
        printf 'QUERY_MODE must be xy or latlon.\n' >&2
        exit 2
        ;;
esac
printf '==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
