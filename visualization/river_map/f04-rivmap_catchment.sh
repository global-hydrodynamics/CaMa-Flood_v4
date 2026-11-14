#!/usr/bin/env bash
# Draw Mekong 1-minute unit catchments with outlet-based river connections.
set -euo pipefail

export MPLBACKEND=Agg

# Resolve repository paths relative to this script.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: Mekong map products and the requested delta extent.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"          # Coarse-grid upstream area [m²].
NEXTXY_FILE="$MAP_DIR/netcdf/nextxy_noedge.nc"   # One-based downstream cell indices.
LONLAT_FILE="$MAP_DIR/netcdf/lonlat.nc"          # Coarse-cell outlet coordinates.
BIFPRM_FILE="$MAP_DIR/netcdf/bifprm.nc"          # Bifurcation endpoints and widths [m].
CATMXY_FILE="$MAP_DIR/1min/1min.catmxy.bin"      # 1-minute pixel to coarse-cell assignment.
VISUAL_FILE="$MAP_DIR/1min/1min.visual.bin"      # Signed int8 channel-pixel categories.
LOCATION_FILE="$MAP_DIR/1min/location.txt"       # 1-minute grid geometry.
PARAMS_FILE="$MAP_DIR/params.txt"                # Coarse grid dimensions and bounds.

# User settings: line widths [points], ordered from smallest to largest class.
MAIN_WIDTHS="1.5,2.0,2.5,3.0"       # Main-channel upstream-area classes.
CHANNEL_WIDTHS="1.2,1.6,2.0,2.4" # Channel-bifurcation path-width classes.

WEST=104.5
EAST=107
SOUTH=9.5
NORTH=12

# User settings: horizontal panel spacing and complete-Figure size.
GAP=24               # Space between panels [pixels].
MARGIN=24            # Outer margin around the combined Figure [pixels].
FIG_WIDTH=0          # Complete figure width [inches]; 0 selects automatic sizing.
FIG_HEIGHT=9         # Complete figure height [inches]; 0 selects automatic sizing.
DPI=200              # PNG resolution.
PYTHON="python3"     # Activate the repository .venv first if required.

FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_A="$FIGURE_DIR/f04a-rivmap_catchment-outlet_uparea.png"
PANEL_B="$FIGURE_DIR/f04b-rivmap_catchment-channel_pixel.png"
OUTPUT_FILE="$FIGURE_DIR/f04-rivmap_catchment.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

# Divide fixed complete-Figure dimensions across the two horizontal panels.
PANEL_WIDTH=$(awk -v width="$FIG_WIDTH" -v dpi="$DPI" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (width > 0) printf "%.6f", (width*dpi - 2*margin - gap)/(2*dpi); else print 0 }')
PANEL_HEIGHT=$(awk -v height="$FIG_HEIGHT" -v dpi="$DPI" -v margin="$MARGIN" \
    'BEGIN { if (height > 0) printf "%.6f", (height*dpi - 2*margin)/dpi; else print 0 }')

mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/3] Draw f04a upstream area at unit-catchment.\n'
"$PYTHON" "$SCRIPT_DIR/script/f04a-rivmap_catchment-outlet_uparea.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" \
    --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" \
    --params "$PARAMS_FILE" \
    --main-widths "$MAIN_WIDTHS" --channel-widths "$CHANNEL_WIDTHS" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_A"

printf '\n[SUCCESS] Created %s\n' "$PANEL_A"

printf '\n[STEP 2/3] Draw f04b channel-pixel classes.\n'
"$PYTHON" "$SCRIPT_DIR/script/f04b-rivmap_catchment-channel_pixel.py" \
    --visual "$VISUAL_FILE" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" \
    --lonlat "$LONLAT_FILE" --bifprm "$BIFPRM_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" \
    --params "$PARAMS_FILE" \
    --main-widths "$MAIN_WIDTHS" --channel-widths "$CHANNEL_WIDTHS" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_B"

printf '\n[SUCCESS] Created %s\n' "$PANEL_B"

printf '\n[STEP 3/3] Combine f04a and f04b into the complete Figure.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" \
    --layout horizontal --gap "$GAP" --margin "$MARGIN" \
    --figure-width "$FIG_WIDTH" --figure-height "$FIG_HEIGHT" --dpi "$DPI" \
    --output "$OUTPUT_FILE"

printf '\n[SUCCESS] Created the two-panel Figure: %s\n' "$OUTPUT_FILE"
