#!/usr/bin/env bash
# Draw and combine four Mekong river-map attribute panels.
set -euo pipefail

# Resolve project and repository paths relative to this script.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: tracked map inputs and common geographic extent.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"              # Upstream catchment area [m²].
BASIN_FILE="$MAP_DIR/netcdf/basin.nc"                 # Original basin IDs.
BSNCOL_FILE="$MAP_DIR/netcdf/bsncol.nc"              # Precomputed basin display-color indices.
SUBCOL_FILE="$MAP_DIR/subcol-30000.bin"              # Prepared int32 subbasin display-color indices.
NEXTXY_NOEDGE_FILE="$MAP_DIR/netcdf/nextxy_noedge.nc" # Exclusion mask for the Subbasin Color overlay.
PARAMS_FILE="$MAP_DIR/params.txt"                    # 15-minute grid dimensions and extent.
WEST=90
EAST=110
SOUTH=5
NORTH=35

# User settings: panel appearance, layout, and complete Figure dimensions.
UPAREA_CMAP="YlGnBu"      # Sequential palette for drainage area.
UPAREA_SCALE="log"         # Choose: log or linear.
BASIN_CMAP="tab20"         # Discrete colors repeated across original basin IDs.
BSNCOL_CMAP="tab10"        # Base qualitative palette for display indices.
BSNCOL_WHITE_MIX=0.60      # Opaque white fraction for soft basin backgrounds.
SUBCOL_CMAP="tab10"        # Same base palette as Basin Color.
SUBCOL_WHITE_MIX=0.60      # Same opaque white fraction as Basin Color.
LAYOUT="grid"              # Combiner layout: horizontal, vertical, or grid.
COLUMNS=2                   # Number of columns when LAYOUT is grid.
GAP=28                      # Gap between panels [pixels].
MARGIN=28                   # Outer margin around the assembled Figure [pixels].
FIG_WIDTH=0                 # Complete Figure width [inches]; 0 selects automatic width.
FIG_HEIGHT=8                # Complete Figure height [inches]; 0 selects automatic height.
DPI=200                     # PNG resolution.
PYTHON="python3"            # Activate the repository .venv first, or set its Python.

# Panel and complete-Figure outputs.
FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_A="$FIGURE_DIR/f01a-raster_river_map-upstream_area.png"
PANEL_B="$FIGURE_DIR/f01b-raster_river_map-basin.png"
PANEL_C="$FIGURE_DIR/f01c-raster_river_map-basin_color.png"
PANEL_D="$FIGURE_DIR/f01d-raster_river_map-subbasin_color.png"
OUTPUT_FILE="$FIGURE_DIR/f01-raster_river_map.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

# Split fixed complete-Figure dimensions across the panel grid. The $(...) command
# substitutions capture awk's per-panel inch calculations, including gaps and margins.
case "$LAYOUT" in
    horizontal) GRID_COLUMNS=4; GRID_ROWS=1 ;;
    vertical) GRID_COLUMNS=1; GRID_ROWS=4 ;;
    grid) GRID_COLUMNS="$COLUMNS"; GRID_ROWS=$(( (4 + COLUMNS - 1) / COLUMNS )) ;;
    *) GRID_COLUMNS=4; GRID_ROWS=1 ;;
esac
PANEL_WIDTH=$(awk -v width="$FIG_WIDTH" -v dpi="$DPI" -v columns="$GRID_COLUMNS" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (width > 0) printf "%.6f", (width*dpi - 2*margin - (columns-1)*gap)/(columns*dpi); else print 0 }')
PANEL_HEIGHT=$(awk -v height="$FIG_HEIGHT" -v dpi="$DPI" -v rows="$GRID_ROWS" \
    -v gap="$GAP" -v margin="$MARGIN" \
    'BEGIN { if (height > 0) printf "%.6f", (height*dpi - 2*margin - (rows-1)*gap)/(rows*dpi); else print 0 }')

mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/5] Draw upstream drainage area (uparea).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01a-raster_river_map-upstream_area.py" \
    --input "$UPAREA_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$UPAREA_CMAP" --scale "$UPAREA_SCALE" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_A"

printf '\n[STEP 2/5] Draw original basin IDs (basin).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01b-raster_river_map-basin.py" \
    --input "$BASIN_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$BASIN_CMAP" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_B"

printf '\n[STEP 3/5] Draw soft basin display colors (bsncol).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01c-raster_river_map-basin_color.py" \
    --input "$BSNCOL_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$BSNCOL_CMAP" --white-mix "$BSNCOL_WHITE_MIX" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_C"

printf '\n[STEP 4/5] Draw prepared subbasin display colors (subcol).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01d-raster_river_map-subbasin_color.py" \
    --input "$SUBCOL_FILE" --params "$PARAMS_FILE" \
    --basin "$BASIN_FILE" --nextxy "$NEXTXY_NOEDGE_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$SUBCOL_CMAP" --white-mix "$SUBCOL_WHITE_MIX" \
    --figure-width "$PANEL_WIDTH" --figure-height "$PANEL_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_D"

printf '\n[STEP 5/5] Assemble the four panels into the complete Figure.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" "$PANEL_C" "$PANEL_D" \
    --layout "$LAYOUT" --columns "$COLUMNS" --gap "$GAP" --margin "$MARGIN" \
    --figure-width "$FIG_WIDTH" --figure-height "$FIG_HEIGHT" --dpi "$DPI" \
    --output "$OUTPUT_FILE"

printf '\n[SUCCESS] Created the four-panel Figure: %s\n' "$OUTPUT_FILE"
