#!/usr/bin/env bash
# Create a standalone upstream drainage-area map from the tracked Mekong map.
set -euo pipefail

# Resolve paths from this project so the script works from any current directory.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: input data and map geometry.
MAP_DIR="$REPO_ROOT/map/mekong_15min"
UPAREA_FILE="$MAP_DIR/netcdf/uparea.nc"
PARAMS_FILE="$MAP_DIR/params.txt"
WEST=90
EAST=110
SOUTH=5
NORTH=35

# User settings: map appearance and output.
CMAP="YlGnBu"
SCALE="log" # Choose: log or linear.
# --- Figure size (inches) ---
MAX_WIDTH=0   # 0 leaves width unconstrained.
MAX_HEIGHT=3  # 0 leaves height unconstrained.
DPI=200
OUTPUT_FILE="$SCRIPT_DIR/figure/f01-rivmap-uparea.png"
PYTHON="python3" # Activate the repository .venv first, or set this to its Python.

# Resolve any user-supplied relative paths from the repository root.
if [[ "$MAP_DIR" != /* ]]; then MAP_DIR="$REPO_ROOT/$MAP_DIR"; fi
if [[ "$UPAREA_FILE" != /* ]]; then UPAREA_FILE="$REPO_ROOT/$UPAREA_FILE"; fi
if [[ "$PARAMS_FILE" != /* ]]; then PARAMS_FILE="$REPO_ROOT/$PARAMS_FILE"; fi
if [[ "$OUTPUT_FILE" != /* ]]; then OUTPUT_FILE="$REPO_ROOT/$OUTPUT_FILE"; fi

mkdir -p "$(dirname "$OUTPUT_FILE")"
"$PYTHON" "$SCRIPT_DIR/script/f01-rivmap-uparea.py" \
    --uparea "$UPAREA_FILE" \
    --params "$PARAMS_FILE" \
    --west "$WEST" --east "$EAST" --south "$SOUTH" --north "$NORTH" \
    --cmap "$CMAP" --scale "$SCALE" \
    --max-width "$MAX_WIDTH" --max-height "$MAX_HEIGHT" \
    --dpi "$DPI" --output "$OUTPUT_FILE"
