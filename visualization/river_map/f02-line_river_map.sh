#!/usr/bin/env bash
# Draw and combine three panels of CaMa-Flood river-network connectivity.
set -euo pipefail

# Resolve project and repository paths relative to this script.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: tracked map inputs and common geographic extent.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"              # Upstream drainage area [m²].
NEXTXY_FILE="$MAP_DIR/netcdf/nextxy_noedge.nc"       # One-based downstream grid indices.
BSNCOL_FILE="$MAP_DIR/netcdf/bsncol.nc"             # Categorical basin display indices.
PARAMS_FILE="$MAP_DIR/params.txt"                    # Grid dimensions, resolution, and bounds.
WEST=99
EAST=109
SOUTH=9
NORTH=19

# User settings: panel layout and complete Figure dimensions.
LAYOUT="horizontal" # Choose: horizontal, vertical, or grid.
COLUMNS=2             # Number of columns when LAYOUT is grid.
GAP=24               # Space between panels [pixels].
MARGIN=24            # Outer margin [pixels].
FIG_WIDTH=0          # Complete Figure width [inches]; 0 selects automatic width.
FIG_HEIGHT=8         # Complete Figure height [inches]; 0 selects automatic height.
DPI=200              # PNG resolution.
PYTHON="python3"     # Activate the repository .venv first, or set its Python.

# Panel and complete-Figure output paths.
FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_A="$FIGURE_DIR/f02a-line_river_map-connect_type.png"
PANEL_B="$FIGURE_DIR/f02b-line_river_map-enhance_tickness.png"
PANEL_C="$FIGURE_DIR/f02c-line_river_map-enhance_color.png"
OUTPUT_FILE="$FIGURE_DIR/f02-line_river_map.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

# Split fixed complete-Figure dimensions across the panel grid. The $(...) command
# substitutions capture awk's per-panel inch calculations, including gaps and margins.
case "$LAYOUT" in
    horizontal) GRID_COLUMNS=3; GRID_ROWS=1 ;;
    vertical) GRID_COLUMNS=1; GRID_ROWS=3 ;;
    grid) GRID_COLUMNS="$COLUMNS"; GRID_ROWS=$(( (3 + COLUMNS - 1) / COLUMNS )) ;;
    *) GRID_COLUMNS=3; GRID_ROWS=1 ;;
esac
PANEL_WIDTH=$(awk -v width="$FIG_WIDTH" -v dpi="$DPI" -v columns="$GRID_COLUMNS" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (width > 0) printf "%.6f", (width*dpi - 2*margin - (columns-1)*gap)/(columns*dpi); else print 0 }')
PANEL_HEIGHT=$(awk -v height="$FIG_HEIGHT" -v dpi="$DPI" -v rows="$GRID_ROWS" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (height > 0) printf "%.6f", (height*dpi - 2*margin - (rows-1)*gap)/(rows*dpi); else print 0 }')

mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/4] Draw every valid nextxy connection by link type.\n'
"$PYTHON" "$SCRIPT_DIR/script/f02a-line_river_map-connect_type.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_A"
printf '[SUCCESS] Created %s\n' "$PANEL_A"

printf '\n[STEP 2/4] Draw black links without arrowheads, using upstream-area thickness.\n'
"$PYTHON" "$SCRIPT_DIR/script/f02b-line_river_map-enhance_tickness.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_B"
printf '[SUCCESS] Created %s\n' "$PANEL_B"

printf '\n[STEP 3/4] Color upstream-area arrows over subdued basin indices.\n'
"$PYTHON" "$SCRIPT_DIR/script/f02c-line_river_map-enhance_color.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" --params "$PARAMS_FILE" \
    --bsncol "$BSNCOL_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_C"
printf '[SUCCESS] Created %s\n' "$PANEL_C"

printf '\n[STEP 4/4] Combine the project-local panels.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" "$PANEL_C" \
    --layout "$LAYOUT" --columns "$COLUMNS" --gap "$GAP" --margin "$MARGIN" \
    --figure-width "$FIG_WIDTH" --figure-height "$FIG_HEIGHT" --dpi "$DPI" \
    --output "$OUTPUT_FILE"
printf '[SUCCESS] Created %s\n' "$OUTPUT_FILE"
