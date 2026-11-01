#!/bin/sh
# Example: inspect river-network and unit-catchment information at a selected point.
# Run from the map-local script_analysis/ directory; a00 prepares its local executable.
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

# Quote report paths in the copyable command printed below.
quote_shell_arg() {
    printf "'"
    printf '%s' "$1" | sed "s/'/'\\\\''/g"
    printf "'"
}

cd "$SCRIPT_DIR"

printf '\nWorking directory: %s\n' "$PWD"

printf '\n==================================================\n'
printf '[STEP 1/1] Inspect river information using %s coordinates\n' "$QUERY_MODE"
printf '==================================================\n\n'
case "$QUERY_MODE" in
    xy)
        if [ -n "$REPORT_FILE" ]; then
            printf './analysis_river_point_info xy %s %s > ' "$IX" "$IY"
            quote_shell_arg "$REPORT_FILE"
            printf '\n'
            ./analysis_river_point_info xy "$IX" "$IY" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            printf './analysis_river_point_info xy %s %s\n' "$IX" "$IY"
            ./analysis_river_point_info xy "$IX" "$IY"
        fi
        ;;
    latlon)
        if [ -n "$REPORT_FILE" ]; then
            printf './analysis_river_point_info latlon %s %s > ' "$LAT" "$LON"
            quote_shell_arg "$REPORT_FILE"
            printf '\n'
            ./analysis_river_point_info latlon "$LAT" "$LON" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            printf './analysis_river_point_info latlon %s %s\n' "$LAT" "$LON"
            ./analysis_river_point_info latlon "$LAT" "$LON"
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
