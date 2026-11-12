#!/usr/bin/env bash
# Draw and combine three Mekong river-map attribute panels.
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
PARAMS_FILE="$MAP_DIR/params.txt"                    # 15-minute grid dimensions and extent.
WEST=90
EAST=110
SOUTH=5
NORTH=35

# User settings: panel appearance, layout, and generated image dimensions.
UPAREA_CMAP="YlGnBu"      # Sequential palette for drainage area.
UPAREA_SCALE="log"         # Choose: log or linear.
BASIN_CMAP="tab20"         # Discrete colors repeated across original basin IDs.
BSNCOL_CMAP="tab10"        # Base qualitative palette for display indices.
BSNCOL_WHITE_MIX=0.60      # Opaque white fraction for soft basin backgrounds.
LAYOUT="horizontal"        # Combiner layout: horizontal, vertical, or grid.
COLUMNS=2                   # Number of columns when LAYOUT is grid.
GAP=28                      # Gap between panels [pixels].
MARGIN=28                   # Outer margin around the assembled Figure [pixels].
PANEL_MAX_WIDTH=0           # Maximum panel width [inches]; 0 is unconstrained.
PANEL_MAX_HEIGHT=6          # Total panel height [inches]; 0 selects automatic height.
MAX_WIDTH=12                # Maximum complete-Figure width [inches].
MAX_HEIGHT=8                # Maximum complete-Figure height [inches].
DPI=200                     # PNG resolution.
PYTHON="python3"            # Activate the repository .venv first, or set its Python.

# Panel and complete-Figure outputs.
FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_A="$FIGURE_DIR/f01a-raster_river_map-upstream_area.png"
PANEL_B="$FIGURE_DIR/f01b-raster_river_map-basin.png"
PANEL_C="$FIGURE_DIR/f01c-raster_river_map-basin_color.png"
OUTPUT_FILE="$FIGURE_DIR/f01-raster_river_map.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

mkdir -p "$FIGURE_DIR"

printf '\n[STEP 1/4] Draw upstream drainage area (uparea).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01a-raster_river_map-upstream_area.py" \
    --input "$UPAREA_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$UPAREA_CMAP" --scale "$UPAREA_SCALE" \
    --max-width "$PANEL_MAX_WIDTH" --max-height "$PANEL_MAX_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_A"

printf '\n[STEP 2/4] Draw original basin IDs (basin).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01b-raster_river_map-basin.py" \
    --input "$BASIN_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$BASIN_CMAP" \
    --max-width "$PANEL_MAX_WIDTH" --max-height "$PANEL_MAX_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_B"

printf '\n[STEP 3/4] Draw soft basin display colors (bsncol).\n'
"$PYTHON" "$SCRIPT_DIR/script/f01c-raster_river_map-basin_color.py" \
    --input "$BSNCOL_FILE" --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$BSNCOL_CMAP" --white-mix "$BSNCOL_WHITE_MIX" \
    --max-width "$PANEL_MAX_WIDTH" --max-height "$PANEL_MAX_HEIGHT" \
    --dpi "$DPI" --output "$PANEL_C"

printf '\n[STEP 4/4] Assemble the three panels into the complete Figure.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" "$PANEL_C" \
    --layout "$LAYOUT" --columns "$COLUMNS" --gap "$GAP" --margin "$MARGIN" \
    --max-width "$MAX_WIDTH" --max-height "$MAX_HEIGHT" --dpi "$DPI" \
    --output "$OUTPUT_FILE"

printf '\n[SUCCESS] Created the three-panel Figure: %s\n' "$OUTPUT_FILE"
