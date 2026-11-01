#!/bin/sh
# Example: inspect river-network and unit-catchment information at a selected point.
# Run from the map-local script_analysis/ directory; a00 prepares its local executable.
# Inputs: standard river-map files; latlon mode may use 15sec/ or 1min/ catmxy data.
# Output: terminal text, or REPORT_FILE when configured.
set -eu

# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# Choose a query mode and location; grid indices are one-based, coordinates are degrees.
QUERY_MODE=xy   # Query type: xy for grid indices or latlon for geographic coordinates.
IX=62           # CaMa-Flood grid column for xy mode.
IY=93           # CaMa-Flood grid row for xy mode.
LAT=11.579      # Latitude for latlon mode (degrees north).
LON=104.946     # Longitude for latlon mode (degrees east).
REPORT_FILE=""  # Optional output text path; empty prints the report to the terminal.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# The sed expression escapes apostrophes so printed paths can be pasted into a shell.
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
