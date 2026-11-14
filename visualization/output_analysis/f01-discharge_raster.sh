#!/usr/bin/env bash
# Extract the dated map and station series, then draw p02-f01a/b.
set -euo pipefail
export MPLBACKEND=Agg

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: source experiment and selected output timestamp.
SOURCE_DIR="$REPO_ROOT/out/mek02-2year_netcdf" # Annual NetCDF model outputs.
TARGET_DATE="2001-09-01"                     # CF time-coordinate date [model calendar].
PARAMS_FILE="$REPO_ROOT/map/mekong_15min/params.txt" # Full 15-minute grid geometry.

# User settings: signed outflow color scale.
# Ascending comma-separated ticks [m3/s]; the first and last values set the range.
CELV="-3000,-1000,-300,-100,-30,-10,0,10,30,100,300,1000,3000,10000,30000"
LINTHRESH="10"       # Linear data interval is [-LINTHRESH, +LINTHRESH] [m3/s].
LINEAR_FRAC="0.10"    # Colormap fraction assigned to the linear interval, centered on zero.

# User settings: output dimensions and raster resolution.
FIGURE_WIDTH=0  # Complete figure width [inches]; 0 selects automatic width.
FIGURE_HEIGHT=8 # Complete figure height [inches].
DPI=200         # PNG resolution.
PYTHON="python3" # Activate the repository .venv before running this script.

OUT_SAMPLE_DIR="$SCRIPT_DIR/out_sample"
FIGURE_DIR="$SCRIPT_DIR/figure"
SAMPLE_FILE="$OUT_SAMPLE_DIR/outflw_${TARGET_DATE}.nc"
STATION_CSV="$SCRIPT_DIR/station_map_locations.csv"
STATION_SERIES="$OUT_SAMPLE_DIR/station_outflw_daily_2000-2001.nc"
PANEL_A="$FIGURE_DIR/f01a-discharge_raster-map.png"
PANEL_B="$FIGURE_DIR/f01b-discharge_raster-hydrograph.png"
FIGURE_FILE="$FIGURE_DIR/f01-discharge_raster.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

printf '\n[STEP 1/4] Extract the dated outflw field and station series.\n'
"$SCRIPT_DIR/s01-extract_output_variables.sh" \
    --variables "outflw" --dates "$TARGET_DATE" \
    --source-dir "$SOURCE_DIR" --params "$PARAMS_FILE" \
    --output-dir "$OUT_SAMPLE_DIR"

printf '\n[STEP 2/4] Draw the outflw map and station locations.\n'
"$PYTHON" "$SCRIPT_DIR/script/f01a-discharge_raster-map.py" \
    --params "$PARAMS_FILE" --sample "$SAMPLE_FILE" \
    --stations "$STATION_CSV" --output "$PANEL_A" --figure-width "$FIGURE_WIDTH" \
    --figure-height "$FIGURE_HEIGHT" --dpi "$DPI" \
    --celv="$CELV" --linthresh="$LINTHRESH" --linear-frac="$LINEAR_FRAC"

printf '\n[STEP 3/4] Draw the two station discharge series.\n'
"$PYTHON" "$SCRIPT_DIR/script/f01b-discharge_raster-hydrograph.py" \
    --stations "$STATION_CSV" --sample "$STATION_SERIES" \
    --output "$PANEL_B" --figure-width 8 --figure-height "$FIGURE_HEIGHT" \
    --dpi "$DPI"

printf '\n[STEP 4/4] Assemble panels (a) and (b).\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" \
    --output "$FIGURE_FILE" --layout horizontal --gap 24 --margin 24 --dpi "$DPI"

printf '\n[SUCCESS] Created p02-f01 Discharge Raster with map and hydrograph panels.\n'
printf 'Figure: %s\nPanel a: %s\nPanel b: %s\n' \
    "$FIGURE_FILE" "$PANEL_A" "$PANEL_B"
