#!/usr/bin/env bash
# Draw the Mekong river network between geographic outlet locations.
set -euo pipefail

# The $(...) expressions resolve paths from this script, independent of the caller's directory.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# User settings: tracked map inputs and displayed geographic extent.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"          # Source-cell upstream area [m²].
NEXTXY_FILE="$MAP_DIR/netcdf/nextxy_noedge.nc"  # One-based downstream cell indices.
LONLAT_FILE="$MAP_DIR/netcdf/lonlat.nc"         # Outlet coordinates for river cells.
PARAMS_FILE="$MAP_DIR/params.txt"               # Map grid dimensions and bounds.
WEST=99
EAST=109
SOUTH=9
NORTH=19

# User settings: output size and file locations.
FIG_WIDTH=0        # Complete Figure width [inches]; 0 selects automatic width.
FIG_HEIGHT=8       # Complete Figure height [inches]; 0 selects automatic height.
DPI=200            # PNG resolution.
PYTHON="python3"   # Use the repository .venv Python after standard setup.
FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_FILE="$FIGURE_DIR/f03a-vector_river_map-upstream_area.png"
OUTPUT_FILE="$FIGURE_DIR/f03-vector_river_map.png"
PANEL_SCRIPT="$SCRIPT_DIR/script/f03a-vector_river_map-upstream_area.py"

mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/2] Draw outlet-to-outlet river connections.\n'
"$PYTHON" "$PANEL_SCRIPT" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" \
    --lonlat "$LONLAT_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --figure-width "$FIG_WIDTH" --figure-height "$FIG_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_FILE"

printf '\n[STEP 2/2] Copy the single panel as the complete Figure.\n'
cp "$PANEL_FILE" "$OUTPUT_FILE"
printf '[SUCCESS] Created %s\n' "$OUTPUT_FILE"
