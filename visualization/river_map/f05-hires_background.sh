#!/usr/bin/env bash
# Draw six high-resolution background variants over the same vector network.
set -euo pipefail

export MPLBACKEND=Agg
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"
MAP_DIR="$REPO_ROOT/map/mekong_15min"
HIRES_DIR="$MAP_DIR/1min"
NETCDF_DIR="$MAP_DIR/netcdf"
LOCATION_FILE="$HIRES_DIR/location.txt"
PARAMS_FILE="$MAP_DIR/params.txt"
UPAREA_FILE="$NETCDF_DIR/uparea.nc"
NEXTXY_FILE="$NETCDF_DIR/nextxy_noedge.nc"
LONLAT_FILE="$NETCDF_DIR/lonlat.nc"
BIFPRM_FILE="$NETCDF_DIR/bifprm.nc"
CATMXY_FILE="$HIRES_DIR/1min.catmxy.bin"
ELEVATION_FILE="$HIRES_DIR/1min.elevtn.bin"
FLOOD_DIFFERENCE_FILE="$HIRES_DIR/1min.flddif.bin"

GAP=24
MARGIN=24
FIG_WIDTH=0
FIG_HEIGHT=10
DPI=200
PYTHON="python3"

FIGURE_DIR="$SCRIPT_DIR/figure"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"
PANEL_WIDTH=$(awk -v width="$FIG_WIDTH" -v dpi="$DPI" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (width > 0) printf "%.6f", (width*dpi - 2*margin - 2*gap)/(3*dpi); else print 0 }')
PANEL_HEIGHT=$(awk -v height="$FIG_HEIGHT" -v dpi="$DPI" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (height > 0) printf "%.6f", (height*dpi - 2*margin - gap)/(2*dpi); else print 0 }')
mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/7] Draw the gray elevation-background panel.\n'
"$PYTHON" "$SCRIPT_DIR/script/f05a-hires_background-gray_elevtn.py" \
    --background "$ELEVATION_FILE" --uparea "$UPAREA_FILE" \
    --nextxy "$NEXTXY_FILE" --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$FIGURE_DIR/f05a-hires_background-gray_elevtn.png"

printf '\n[STEP 2/7] Draw the gray maximum-neighbor elevation-difference panel.\n'
"$PYTHON" "$SCRIPT_DIR/script/f05b-hires_background-gray_slope.py" \
    --background "$ELEVATION_FILE" --uparea "$UPAREA_FILE" \
    --nextxy "$NEXTXY_FILE" --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$FIGURE_DIR/f05b-hires_background-gray_slope.png"

printf '\n[STEP 3/7] Draw the gray floodplain-difference panel.\n'
"$PYTHON" "$SCRIPT_DIR/script/f05c-hires_background-gray_flddif.py" \
    --background "$FLOOD_DIFFERENCE_FILE" --elevtn "$ELEVATION_FILE" \
    --uparea "$UPAREA_FILE" \
    --nextxy "$NEXTXY_FILE" --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$FIGURE_DIR/f05c-hires_background-gray_flddif.png"

printf '\n[STEP 4/7] Draw the terrain-palette elevation panel with black river lines.\n'
"$PYTHON" "$SCRIPT_DIR/script/f05d-hires_background-terrain_elevtn.py" \
    --background "$ELEVATION_FILE" --uparea "$UPAREA_FILE" \
    --nextxy "$NEXTXY_FILE" --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$FIGURE_DIR/f05d-hires_background-terrain_elevtn.png"

printf '\n[STEP 5/7] Draw the terrain-palette slope panel with black river lines.\n'
"$PYTHON" "$SCRIPT_DIR/script/f05e-hires_background-terrain_slope.py" \
    --background "$ELEVATION_FILE" --uparea "$UPAREA_FILE" \
    --nextxy "$NEXTXY_FILE" --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$FIGURE_DIR/f05e-hires_background-terrain_slope.png"

printf '\n[STEP 6/7] Draw the terrain-palette floodplain-difference panel.\n'
"$PYTHON" "$SCRIPT_DIR/script/f05f-hires_background-terrain_flddif.py" \
    --background "$FLOOD_DIFFERENCE_FILE" --elevtn "$ELEVATION_FILE" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" \
    --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$FIGURE_DIR/f05f-hires_background-terrain_flddif.png"

printf '\n[STEP 7/7] Combine all six p01-f05 panels in a 3-column, 2-row grid.\n'
"$PYTHON" "$COMBINER" \
    "$FIGURE_DIR/f05a-hires_background-gray_elevtn.png" \
    "$FIGURE_DIR/f05b-hires_background-gray_slope.png" \
    "$FIGURE_DIR/f05c-hires_background-gray_flddif.png" \
    "$FIGURE_DIR/f05d-hires_background-terrain_elevtn.png" \
    "$FIGURE_DIR/f05e-hires_background-terrain_slope.png" \
    "$FIGURE_DIR/f05f-hires_background-terrain_flddif.png" \
    --layout grid --columns 3 --align-map-areas \
    --gap "$GAP" --margin "$MARGIN" \
    --figure-width "$FIG_WIDTH" --figure-height "$FIG_HEIGHT" --dpi "$DPI" \
    --output "$FIGURE_DIR/f05-hires_background.png"
printf '[SUCCESS] Created %s\n' "$FIGURE_DIR/f05-hires_background.png"
