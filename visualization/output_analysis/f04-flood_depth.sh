#!/usr/bin/env bash
# Draw native-grid and 1-minute downscaled flood depth for 2001-09-01.
set -euo pipefail
export MPLBACKEND=Agg

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: snapshot, map extent, and shared flood-depth color scale.
TARGET_DATE="2001-09-01" # Reuse the existing flddph snapshot.
WEST=102                   # Western edge [degrees east].
EAST=108                   # Eastern edge [degrees east].
SOUTH=9                    # Southern edge [degrees north].
NORTH=15                   # Northern edge [degrees north].
DEPTH_MIN=0                # Common minimum flood depth [m].
DEPTH_MAX=20               # Common maximum; values above remain visible via colorbar extension [m].
DEPTH_TICKS="0,2,5,10,15,20" # Shared depth ticks [m].
COLORMAP="YlGnBu"          # Yellow-Green-Blue sequential palette.

# User settings: output size and typography, reviewed on the combined Figure.
FIGURE_WIDTH=8.2           # Complete panel width [inches].
FIGURE_HEIGHT=9.0          # Complete panel height [inches].
DPI=200                     # PNG resolution.
TITLE_SIZE=14               # Panel title [pt].
AXIS_LABEL_SIZE=11          # Longitude/latitude labels [pt].
TICK_LABEL_SIZE=9           # Map and colorbar ticks [pt].
COLORBAR_LABEL_SIZE=11      # Colorbar quantity and unit [pt].
PYTHON="$REPO_ROOT/.venv/bin/python" # Activate .venv before running this script.
FC="${FC:-gfortran}"        # Compiler for the existing Tutorial downscaler.

SAMPLE_DIR="$SCRIPT_DIR/out_sample"
FIGURE_DIR="$SCRIPT_DIR/figure"
MAP_DIR="$REPO_ROOT/map/mekong_15min"
MAP1_DIR="$MAP_DIR/1min"
PARAMS_FILE="$MAP_DIR/params.txt"
LOCATION_FILE="$MAP1_DIR/location.txt"
SOURCE_NC="$SAMPLE_DIR/flddph_${TARGET_DATE}.nc"
DOWNSCALED_NC="$SAMPLE_DIR/flddph_1min_${TARGET_DATE}.nc"
PANEL_A="$FIGURE_DIR/f04a-flood_depth-raster.png"
PANEL_B="$FIGURE_DIR/f04b-flood_depth-downscale_1min.png"
FIGURE_FILE="$FIGURE_DIR/f04-flood_depth.png"
PLOTTER="$SCRIPT_DIR/script/f04-flood_depth.py"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"
DOWNSCALER_SOURCE="$REPO_ROOT/etc/downscale_flddph/src/downscale_flddph_trib.F90"

WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/cmf-f04-downscale.XXXXXX")"
trap 'rm -rf "$WORK_DIR"' EXIT

printf '\n[STEP 1/4] Prepare the coarse flddph record and build the existing downscaler.\n'
"$PYTHON" "$PLOTTER" prepare --source "$SOURCE_NC" --date "$TARGET_DATE" \
    --params "$PARAMS_FILE" --binary "$WORK_DIR/flddph_input.bin"
"$FC" -O3 -Wall -cpp -ffree-line-length-none -fimplicit-none -ftree-vectorize \
    "$DOWNSCALER_SOURCE" -o "$WORK_DIR/downscale_flddph_trib"

printf '\n[STEP 2/4] Run the guide01 downscaling method over the requested domain.\n'
ln -s "$MAP_DIR" "$WORK_DIR/map"
(
    cd "$WORK_DIR"
    ./downscale_flddph_trib "$WEST" "$EAST" "$SOUTH" "$NORTH" 1min \
        "$WORK_DIR/flddph_input.bin" "$WORK_DIR/flddph_1min.bin" 1
)

printf '\n[STEP 3/4] Save the downscaled sample and render both panels.\n'
"$PYTHON" "$PLOTTER" plot --source "$SOURCE_NC" --date "$TARGET_DATE" \
    --downscaled-bin "$WORK_DIR/flddph_1min.bin" --downscaled-netcdf "$DOWNSCALED_NC" \
    --flddif "$MAP1_DIR/1min.flddif.bin" --elevtn "$MAP1_DIR/1min.elevtn.bin" \
    --location "$LOCATION_FILE" --params "$PARAMS_FILE" \
    --panel-a "$PANEL_A" --panel-b "$PANEL_B" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --depth-min "$DEPTH_MIN" --depth-max "$DEPTH_MAX" --depth-ticks "$DEPTH_TICKS" \
    --cmap "$COLORMAP" --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --figure-width "$FIGURE_WIDTH" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI"

printf '\n[STEP 4/4] Assemble the two map panels side by side.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" --output "$FIGURE_FILE" \
    --layout grid --columns 2 --align-map-areas --gap 20 --margin 20 --dpi "$DPI"

printf '\n[SUCCESS] Created p02-f04 Flood Depth.\n'
printf 'Downscaled sample: %s\n' "$DOWNSCALED_NC"
printf 'Combined figure: %s\n' "$FIGURE_FILE"
