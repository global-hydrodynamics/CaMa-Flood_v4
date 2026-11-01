#!/bin/sh
# Inspect river-network and unit-catchment information at a selected point.
# Copy together with the local executable from a prepared script_analysis/ directory.
# Inputs: standard river-map files; latlon mode may use 15sec/ or 1min/ catmxy data.
# Output: terminal text, or REPORT_FILE when configured.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# User settings: xy coordinates are one-based Fortran indices; lat/lon values are degrees.
QUERY_MODE=xy
IX=62
IY=93
LAT=11.579
LON=104.946
REPORT_FILE=""

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

EXEC_RIVER_INFO="$SCRIPT_DIR/analysis_river_point_info"
cd "$SCRIPT_DIR"
printf '\n==================================================\n'
printf '[STEP 1/1] Inspect river information using %s coordinates\n' "$QUERY_MODE"
printf '==================================================\n\n'
case "$QUERY_MODE" in
    xy)
        if [ -n "$REPORT_FILE" ]; then
            "$EXEC_RIVER_INFO" xy "$IX" "$IY" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            "$EXEC_RIVER_INFO" xy "$IX" "$IY"
        fi
        ;;
    latlon)
        if [ -n "$REPORT_FILE" ]; then
            "$EXEC_RIVER_INFO" latlon "$LAT" "$LON" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            "$EXEC_RIVER_INFO" latlon "$LAT" "$LON"
        fi
        ;;
    *)
        printf '\n[ERROR] QUERY_MODE must be xy or latlon.\n' >&2
        exit 2
        ;;
esac
printf '\n==================================================\n'
printf '[SUCCESS] River-point inspection complete.\n'
printf '==================================================\n'
