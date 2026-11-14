#!/usr/bin/env bash
# Draw p02-f02 signed discharge on the Mekong river network.
set -euo pipefail
export MPLBACKEND=Agg
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR"
VISUALIZATION_DIR="$(cd "$PROJECT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: CF date and signed-flow color scale.
TARGET_DATE="2001-09-01" # Daily output timestamp.
CELV="-3000,-1000,-300,-100,-30,-10,0,10,30,100,300,1000,3000,10000,30000,50000"
LINTHRESH="10"       # Linear interval half-width [m3/s].
LINEAR_FRAC="0.10"   # Colormap fraction assigned to the full linear interval.
COLORMAP="diverge_asymmetry" # Registered signed-flow color palette.
WEST=103             # Western map edge [degrees east].
EAST=108             # Eastern map edge [degrees east].
SOUTH=9              # Southern map edge [degrees north].
NORTH=14             # Northern map edge [degrees north].
LINE_WIDTH_SCALE=1.4 # Multiplier for all main-channel and bifurcation lines.
FIGURE_HEIGHT=8       # Complete panel height [inches]; width is automatic.
DPI=200               # PNG resolution.
PYTHON="python3"       # Activate the repository .venv before running.

# User settings: typography for panels viewed individually and after assembly.
TITLE_SIZE=14            # Panel title [pt].
AXIS_LABEL_SIZE=12       # Longitude/latitude labels [pt].
TICK_LABEL_SIZE=10       # Axis and colorbar tick labels [pt].
LEGEND_SIZE=9            # Combined-figure legend [pt].
COLORBAR_LABEL_SIZE=11   # Colorbar quantity and units [pt].

SAMPLES="$PROJECT_DIR/out_sample"
FIGURES="$PROJECT_DIR/figure"
PANEL_A="$FIGURES/f02a-discharge-vector_default.png"
PANEL_B="$FIGURES/f02b-discharge-vector_direction-aware.png"
COMBINER="$VISUALIZATION_DIR/script/combine_panels.py"

printf '\n[STEP 1/4] Extract daily outflw and pthflw samples.\n'
"$PROJECT_DIR/s01-extract_output_variables.sh" --variables "outflw,pthflw" \
    --dates "$TARGET_DATE" --output-dir "$SAMPLES"
printf '\n[STEP 2/4] Draw signed main-channel and bifurcation discharge without direction markers.\n'
"$PYTHON" "$PROJECT_DIR/script/f02-discharge_vector.py" \
    --sample "$SAMPLES/outflw_${TARGET_DATE}.nc" \
    --pthflw "$SAMPLES/pthflw_${TARGET_DATE}.nc" \
    --params "$REPO_ROOT/map/mekong_15min/params.txt" \
    --output "$PANEL_A" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI" \
    --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --legend-size "$LEGEND_SIZE" \
    --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --celv="$CELV" --linthresh="$LINTHRESH" --linear-frac="$LINEAR_FRAC" \
    --colormap "$COLORMAP" --west "$WEST" --east "$EAST" \
    --south "$SOUTH" --north "$NORTH" --line-width-scale "$LINE_WIDTH_SCALE"
printf '\n[STEP 3/4] Draw direction-aware main-channel and bifurcation flow.\n'
"$PYTHON" "$PROJECT_DIR/script/f02-discharge_vector.py" \
    --sample "$SAMPLES/outflw_${TARGET_DATE}.nc" \
    --pthflw "$SAMPLES/pthflw_${TARGET_DATE}.nc" \
    --direction-aware \
    --params "$REPO_ROOT/map/mekong_15min/params.txt" \
    --output "$PANEL_B" --figure-height "$FIGURE_HEIGHT" --dpi "$DPI" \
    --title-size "$TITLE_SIZE" --axis-label-size "$AXIS_LABEL_SIZE" \
    --tick-label-size "$TICK_LABEL_SIZE" --legend-size "$LEGEND_SIZE" \
    --colorbar-label-size "$COLORBAR_LABEL_SIZE" \
    --celv="$CELV" --linthresh="$LINTHRESH" --linear-frac="$LINEAR_FRAC" \
    --colormap "$COLORMAP" --west "$WEST" --east "$EAST" \
    --south "$SOUTH" --north "$NORTH" --line-width-scale "$LINE_WIDTH_SCALE"
printf '\n[STEP 4/4] Assemble the two map panels.\n'
"$PYTHON" "$COMBINER" "$PANEL_A" "$PANEL_B" \
    --output "$FIGURES/f02-discharge-vector.png" --layout grid --columns 2 \
    --align-map-areas --gap 24 --margin 24 --dpi "$DPI"
printf '\n[SUCCESS] Created p02-f02 Discharge Vector.\n'
