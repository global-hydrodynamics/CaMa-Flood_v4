#!/bin/sh
# Example: inspect river-network and unit-catchment information at a selected point.
# Run from script_analysis/ after a00-link_codes.sh creates the shared-code link.
# Inputs: standard river-map files; latlon mode may use 15sec/ or 1min/ catmxy data.
# Output: river-point report in OUT_DIR.
set -eu

# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# Analysis output directory, relative to script_analysis/.
OUT_DIR="out_analysis"  # Change to ../out_analysis to use the map-root output folder.

# Choose a query mode and location; grid indices are one-based, coordinates are degrees.
QUERY_MODE=xy   # Query type: xy for grid indices or latlon for geographic coordinates.
IX=62           # CaMa-Flood grid column for xy mode.
IY=93           # CaMa-Flood grid row for xy mode.
LAT=11.579      # Latitude for latlon mode (degrees north).
LON=104.946     # Longitude for latlon mode (degrees east).
REPORT_FILE="$OUT_DIR/river_point_info.txt"  # Text report path; overwritten on each run.

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
if [ -n "$REPORT_FILE" ]; then
    mkdir -p "$OUT_DIR"
fi

printf '\nWorking directory: %s\n' "$PWD"

printf '\n==================================================\n'
printf '[STEP 1/1] Inspect river information using %s coordinates\n' "$QUERY_MODE"
printf '==================================================\n\n'
case "$QUERY_MODE" in
    xy)
        if [ -n "$REPORT_FILE" ]; then
            printf './code/analysis_river_point_info xy %s %s > ' "$IX" "$IY"
            quote_shell_arg "$REPORT_FILE"
            printf '\n'
            ./code/analysis_river_point_info xy "$IX" "$IY" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            printf './code/analysis_river_point_info xy %s %s\n' "$IX" "$IY"
            ./code/analysis_river_point_info xy "$IX" "$IY"
        fi
        ;;
    latlon)
        if [ -n "$REPORT_FILE" ]; then
            printf './code/analysis_river_point_info latlon %s %s > ' "$LAT" "$LON"
            quote_shell_arg "$REPORT_FILE"
            printf '\n'
            ./code/analysis_river_point_info latlon "$LAT" "$LON" > "$REPORT_FILE"
            printf 'River-information report: %s\n' "$REPORT_FILE"
        else
            printf './code/analysis_river_point_info latlon %s %s\n' "$LAT" "$LON"
            ./code/analysis_river_point_info latlon "$LAT" "$LON"
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
