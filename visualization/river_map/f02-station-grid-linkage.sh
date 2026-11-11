#!/usr/bin/env bash
# Draw and combine two panels showing station-to-grid-cell links.
set -euo pipefail

# Resolve project and repository paths relative to this script.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: map, station sample, and displayed region.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"
PARAMS_FILE="$MAP_DIR/params.txt"
STATIONS_FILE="$SCRIPT_DIR/station_grid_links.csv"
WEST=98
EAST=102
SOUTH=8
NORTH=20

# User settings: complete figure layout and output size.
LAYOUT="horizontal" # Choose: horizontal, vertical, or grid.
GAP=24               # Space between panels in pixels before final scaling.
MARGIN=24            # Outer margin in pixels before final scaling.
MAX_WIDTH=12         # Maximum complete-figure width in inches; 0 is unconstrained.
MAX_HEIGHT=8         # Maximum complete-figure height in inches; 0 is unconstrained.
DPI=200
PYTHON="python3"     # Activate the repository .venv first, or set its Python.

# Input and generated panel / complete-figure paths.
PANEL_A="$SCRIPT_DIR/figure/f02a-station-grid-linkage.png"
PANEL_B="$SCRIPT_DIR/figure/f02b-station-grid-area.png"
OUTPUT_FILE="$SCRIPT_DIR/figure/f02-station-grid-linkage.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

mkdir -p "$SCRIPT_DIR/figure"

printf '\n[STEP 1/3] Draw the basic station-to-cell linkage panel.\n'
"$PYTHON" "$SCRIPT_DIR/script/f02a-station-grid-linkage.py" \
    --uparea "$UPAREA_FILE" --params "$PARAMS_FILE" --stations "$STATIONS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --dpi "$DPI" --output "$PANEL_A"
printf '[SUCCESS] Created %s\n' "$PANEL_A"

printf '\n[STEP 2/3] Draw the upstream-area-scaled linkage panel.\n'
"$PYTHON" "$SCRIPT_DIR/script/f02b-station-grid-area.py" \
    --uparea "$UPAREA_FILE" --params "$PARAMS_FILE" --stations "$STATIONS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --dpi "$DPI" --output "$PANEL_B"
printf '[SUCCESS] Created %s\n' "$PANEL_B"

printf '\n[STEP 3/3] Combine the panel images into the complete Figure.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" \
    --layout "$LAYOUT" --gap "$GAP" --margin "$MARGIN" \
    --max-width "$MAX_WIDTH" --max-height "$MAX_HEIGHT" --dpi "$DPI" \
    --output "$OUTPUT_FILE"
printf '[SUCCESS] Created %s\n' "$OUTPUT_FILE"
