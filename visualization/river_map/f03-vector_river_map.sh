#!/usr/bin/env bash
# Draw and combine three Mekong outlet-based river-network panels.
set -euo pipefail

# The Figure is saved to PNG; do not require an interactive desktop backend.
export MPLBACKEND=Agg

# Resolve paths from this script, independent of the caller's directory.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: tracked coarse- and high-resolution Mekong map products.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"          # Source-cell upstream area [m²].
NEXTXY_FILE="$MAP_DIR/netcdf/nextxy_noedge.nc"  # One-based downstream cell indices.
NEXTXY_C_FILE="$MAP_DIR/netcdf/nextxy.nc"       # Standard nextxy links for f03c.
LONLAT_FILE="$MAP_DIR/netcdf/lonlat.nc"          # Unit-catchment outlet coordinates.
SUBCOL_FILE="$MAP_DIR/subcol-sample.bin"         # Subbasin display-color categories.
BSNCOL_FILE="$MAP_DIR/netcdf/bsncol.nc"          # Categorical basin display colors.
BIFPRM_FILE="$MAP_DIR/netcdf/bifprm.nc"          # Bifurcation paths and layer widths.
PARAMS_FILE="$MAP_DIR/params.txt"                # Coarse grid dimensions and bounds.
CATMXY_FILE="$MAP_DIR/1min/1min.catmxy.bin"      # 1-minute to coarse-cell assignment.
LOCATION_FILE="$MAP_DIR/1min/location.txt"       # High-resolution grid bounds and size.

# User settings: independent extents for the three vector-map panels.
A_WEST=99
A_EAST=109
A_SOUTH=9
A_NORTH=19
B_WEST=102
B_EAST=108
B_SOUTH=9
B_NORTH=15
C_WEST=90
C_EAST=110
C_SOUTH=9
C_NORTH=30

# User settings: horizontal panel layout and complete Figure dimensions.
GAP=24               # Space between panels [pixels].
MARGIN=24            # Outer margin around the combined Figure [pixels].
FIG_WIDTH=0         # Complete Figure width [inches]; 0 selects automatic width.
FIG_HEIGHT=8        # Complete Figure height [inches]; 0 selects automatic height.
DPI=200             # PNG resolution.
PYTHON="python3"    # Use the repository .venv Python after standard setup.

# Panel and complete-Figure output paths.
FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_A="$FIGURE_DIR/f03a-vector_river_map-upstream_area.png"
PANEL_B="$FIGURE_DIR/f03b-vector_river_map-extended_network.png"
PANEL_C="$FIGURE_DIR/f03c-vector_river_map-gray_subcol_background.png"
OUTPUT_FILE="$FIGURE_DIR/f03-vector_river_map.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

# Divide fixed complete-Figure dimensions between three horizontal panels.
PANEL_WIDTH=$(awk -v width="$FIG_WIDTH" -v dpi="$DPI" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (width > 0) printf "%.6f", (width*dpi - 2*margin - 2*gap)/(3*dpi); else print 0 }')
PANEL_HEIGHT=$(awk -v height="$FIG_HEIGHT" -v dpi="$DPI" -v margin="$MARGIN" \
    'BEGIN { if (height > 0) printf "%.6f", (height*dpi - 2*margin)/dpi; else print 0 }')

mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/4] Draw f03a subcol background and upstream-area outlet network.\n'
"$PYTHON" "$SCRIPT_DIR/script/f03a-vector_river_map-upstream_area.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" \
    --lonlat "$LONLAT_FILE" --subcol "$SUBCOL_FILE" --params "$PARAMS_FILE" \
    --west "$A_WEST" --east "$A_EAST" --south "$A_SOUTH" --north "$A_NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_A"

printf '\n[STEP 2/4] Draw the f03b extended outlet network.\n'
"$PYTHON" "$SCRIPT_DIR/script/f03b-vector_river_map-extended_network.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_FILE" \
    --lonlat "$LONLAT_FILE" --bsncol "$BSNCOL_FILE" \
    --bifprm "$BIFPRM_FILE" --params "$PARAMS_FILE" \
    --catmxy "$CATMXY_FILE" --location "$LOCATION_FILE" \
    --west "$B_WEST" --east "$B_EAST" --south "$B_SOUTH" --north "$B_NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_B"

printf '\n[STEP 3/4] Draw f03c with a gray subcol background.\n'
"$PYTHON" "$SCRIPT_DIR/script/f03c-vector_river_map-gray_subcol_background.py" \
    --uparea "$UPAREA_FILE" --nextxy "$NEXTXY_C_FILE" \
    --lonlat "$LONLAT_FILE" --subcol "$SUBCOL_FILE" --params "$PARAMS_FILE" \
    --west "$C_WEST" --east "$C_EAST" --south "$C_SOUTH" --north "$C_NORTH" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_C"

printf '\n[STEP 4/4] Combine f03a, f03b, and f03c into the project Figure.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" "$PANEL_C" \
    --layout horizontal --gap "$GAP" --margin "$MARGIN" \
    --figure-width "$FIG_WIDTH" --figure-height "$FIG_HEIGHT" --dpi "$DPI" \
    --output "$OUTPUT_FILE"
printf '[SUCCESS] Created %s\n' "$OUTPUT_FILE"
