#!/usr/bin/env bash
# Draw p02-f03 flooded-area diagnostics for 2001-09-01.
set -euo pipefail
export MPLBACKEND=Agg

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: map domain and display date for the existing sample fields.
TARGET_DATE="2001-09-01" # Existing extracted snapshot; no extraction is run here.
WEST=98                    # Western map edge [degrees east].
EAST=110                   # Eastern map edge [degrees east].
SOUTH=9                    # Southern map edge [degrees north].
NORTH=22                   # Northern map edge [degrees north].

# User settings: raw area scale and common unit-catchment fraction scale.
AREA_MIN="0"               # Linear lower bound for fldare [km2].
AREA_MAX="1000"            # Linear upper bound for fldare [km2].
AREA_TICKS="0,100,200,300,400,500,600,700,800,900,1000"
AREA_COLORMAP="PuBu"     # Sequential palette for raw flooded area.
FRACTION_MIN="0"           # Common fraction range for panels b, c, and d.
FRACTION_MAX="1"
FRACTION_TICKS="0,0.2,0.4,0.6,0.8,1"
FRACTION_COLORMAP="YlGnBu" # Shared by flooded, seasonal, and permanent fractions.
PERMANENT_COLORMAP="YlOrBr" # Sequential palette for panel d permanent-water fraction.
ZERO_COLOR="#d9d9d9"       # Valid fldare=0 cells; missing/outside cells stay white.

# User settings: output size and raster resolution.
FIGURE_WIDTH=8.2           # Complete panel width [inches]; keeps map/cbar sizes fixed.
FIGURE_HEIGHT=9.0           # Complete panel height [inches].
DPI=200                     # PNG resolution.
PYTHON="python3"            # Activate the repository .venv before running.

# User settings: typography checked on the assembled four-panel Figure.
TITLE_SIZE=14               # Panel title [pt].
AXIS_LABEL_SIZE=11          # Longitude/latitude labels [pt].
TICK_LABEL_SIZE=9            # Axis and colorbar tick labels [pt].
COLORBAR_LABEL_SIZE=11      # Colorbar quantity and units [pt].

SAMPLE_DIR="$SCRIPT_DIR/out_sample"
MAP_DIR="$REPO_ROOT/map/mekong_15min/netcdf"
PARAMS_FILE="$REPO_ROOT/map/mekong_15min/params.txt"
FIGURE_DIR="$SCRIPT_DIR/figure"
PANEL_A="$FIGURE_DIR/f03a-flooded_area-area_raw.png"
PANEL_B="$FIGURE_DIR/f03b-flooded_area-flood_frac.png"
PANEL_C="$FIGURE_DIR/f03c-flooded_area-season_frac.png"
PANEL_D="$FIGURE_DIR/f03d-flooded_area-prmwat.png"
FIGURE_FILE="$FIGURE_DIR/f03-flooded_area.png"
PLOTTER="$SCRIPT_DIR/script/f03-flooded_area.py"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

printf '\n[STEP 1/5] Draw raw flooded area (fldare).\n'
"$PYTHON" "$PLOTTER" --panel a --date "$TARGET_DATE" \
    --fldare "$SAMPLE_DIR/fldare_${TARGET_DATE}.nc" \
    --fldfrc "$SAMPLE_DIR/fldfrc_${TARGET_DATE}.nc" \
    --ctmare "$MAP_DIR/ctmare.nc" --prmwat "$MAP_DIR/prmwat.nc" \
    --params "$PARAMS_FILE" --output "$PANEL_A" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --area-min "$AREA_MIN" --area-max "$AREA_MAX" --area-ticks "$AREA_TICKS" \
    --area-cmap "$AREA_COLORMAP" --fraction-min "$FRACTION_MIN" \
    --fraction-max "$FRACTION_MAX" --fraction-ticks "$FRACTION_TICKS" \
    --fraction-cmap "$FRACTION_COLORMAP" --permanent-cmap "$PERMANENT_COLORMAP" \
    --zero-color "$ZERO_COLOR" \
    --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --figure-width "$FIGURE_WIDTH" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI"

printf '\n[STEP 2/5] Draw unit-catchment flooded fraction (fldfrc).\n'
"$PYTHON" "$PLOTTER" --panel b --date "$TARGET_DATE" \
    --fldare "$SAMPLE_DIR/fldare_${TARGET_DATE}.nc" \
    --fldfrc "$SAMPLE_DIR/fldfrc_${TARGET_DATE}.nc" \
    --ctmare "$MAP_DIR/ctmare.nc" --prmwat "$MAP_DIR/prmwat.nc" \
    --params "$PARAMS_FILE" --output "$PANEL_B" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --area-min "$AREA_MIN" --area-max "$AREA_MAX" --area-ticks "$AREA_TICKS" \
    --area-cmap "$AREA_COLORMAP" --fraction-min "$FRACTION_MIN" \
    --fraction-max "$FRACTION_MAX" --fraction-ticks "$FRACTION_TICKS" \
    --fraction-cmap "$FRACTION_COLORMAP" --permanent-cmap "$PERMANENT_COLORMAP" \
    --zero-color "$ZERO_COLOR" \
    --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --figure-width "$FIGURE_WIDTH" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI"

printf '\n[STEP 3/5] Draw temporary fraction with prmwat excluded.\n'
"$PYTHON" "$PLOTTER" --panel c --date "$TARGET_DATE" \
    --fldare "$SAMPLE_DIR/fldare_${TARGET_DATE}.nc" \
    --fldfrc "$SAMPLE_DIR/fldfrc_${TARGET_DATE}.nc" \
    --ctmare "$MAP_DIR/ctmare.nc" --prmwat "$MAP_DIR/prmwat.nc" \
    --params "$PARAMS_FILE" --output "$PANEL_C" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --area-min "$AREA_MIN" --area-max "$AREA_MAX" --area-ticks "$AREA_TICKS" \
    --area-cmap "$AREA_COLORMAP" --fraction-min "$FRACTION_MIN" \
    --fraction-max "$FRACTION_MAX" --fraction-ticks "$FRACTION_TICKS" \
    --fraction-cmap "$FRACTION_COLORMAP" --permanent-cmap "$PERMANENT_COLORMAP" \
    --zero-color "$ZERO_COLOR" \
    --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --figure-width "$FIGURE_WIDTH" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI"

printf '\n[STEP 4/5] Draw permanent-water fraction (prmwat / ctmare).\n'
"$PYTHON" "$PLOTTER" --panel d --date "$TARGET_DATE" \
    --fldare "$SAMPLE_DIR/fldare_${TARGET_DATE}.nc" \
    --fldfrc "$SAMPLE_DIR/fldfrc_${TARGET_DATE}.nc" \
    --ctmare "$MAP_DIR/ctmare.nc" --prmwat "$MAP_DIR/prmwat.nc" \
    --params "$PARAMS_FILE" --output "$PANEL_D" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --area-min "$AREA_MIN" --area-max "$AREA_MAX" --area-ticks "$AREA_TICKS" \
    --area-cmap "$AREA_COLORMAP" --fraction-min "$FRACTION_MIN" \
    --fraction-max "$FRACTION_MAX" --fraction-ticks "$FRACTION_TICKS" \
    --fraction-cmap "$FRACTION_COLORMAP" --permanent-cmap "$PERMANENT_COLORMAP" \
    --zero-color "$ZERO_COLOR" \
    --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --figure-width "$FIGURE_WIDTH" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI"

printf '\n[STEP 5/5] Assemble the four map panels.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" "$PANEL_C" "$PANEL_D" \
    --output "$FIGURE_FILE" --layout grid --columns 4 \
    --align-map-areas --gap 20 --margin 20 --dpi "$DPI"

printf '\n[SUCCESS] Created p02-f03 Flooded Area.\n'
printf 'Combined figure: %s\n' "$FIGURE_FILE"
