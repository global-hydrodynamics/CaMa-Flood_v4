#!/bin/sh
# Downscale the common Control-selected event with the existing tributary method.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
CMF_BASE=$(CDPATH= cd "$SCRIPT_DIR/../../.." && pwd -P)
PYTHON="$CMF_BASE/.venv/bin/python"
MAPDIR="$CMF_BASE/map/mekong_15min"
SOURCE="$CMF_BASE/etc/downscale_flddph/src/downscale_flddph_trib.F90"
BIN_DIR="$SCRIPT_DIR/out/bin"
EXE="$BIN_DIR/downscale_flddph_trib"

# User-requested Mekong lower-basin extent for Figure 6.
# The Fortran method reads an additional five-model-cell buffer around it.
WEST=102
EAST=108
SOUTH=9
NORTH=15
RES=1min
FORCE=0
if [ "$#" -gt 0 ]; then
    [ "$#" -eq 1 ] && [ "$1" = "--force" ] || { echo "Usage: $0 [--force]" >&2; exit 2; }
    FORCE=1
fi

[ -x "$PYTHON" ] || { echo "[ERROR] Python environment missing: $PYTHON" >&2; exit 1; }
[ -f "$SCRIPT_DIR/analysis/event_selection.csv" ] || {
    echo "[ERROR] Run a01-analyze_outputs.py first to select the event." >&2; exit 1;
}
for layer in catmxy flddif rivwth downxy; do
    [ -s "$MAPDIR/1min/1min.$layer.bin" ] || { echo "[ERROR] Missing 1min layer: $MAPDIR/1min/1min.$layer.bin" >&2; exit 1; }
done
[ -s "$MAPDIR/nextxy.bin" ] && [ -s "$MAPDIR/uparea.bin" ] || {
    echo "[ERROR] Missing binary network maps required by the existing downscaler." >&2; exit 1;
}

if [ ! -x "$EXE" ]; then
    mkdir -p "$BIN_DIR"
    FC=${FC:-gfortran}
    echo "[STEP] Building existing downscaler with $FC"
    "$FC" -O3 -Wall -cpp -ffree-line-length-none -fimplicit-none -ftree-vectorize "$SOURCE" -o "$EXE"
fi

for CASE in control deep_channel; do
    WORKDIR="$SCRIPT_DIR/out/downscaling/$CASE/work"
    INPUT_DIR="$SCRIPT_DIR/out/downscaling/$CASE/input"
    OUTPUT_DIR="$SCRIPT_DIR/out/downscaling/$CASE"
    OUTPUT="$OUTPUT_DIR/flddph_1min_provisional.bin"
    mkdir -p "$WORKDIR" "$INPUT_DIR" "$OUTPUT_DIR"
    if [ -L "$WORKDIR/map" ]; then
        [ "$(readlink "$WORKDIR/map")" = "$MAPDIR" ] || { echo "[ERROR] Unexpected map link in $WORKDIR" >&2; exit 1; }
    elif [ -e "$WORKDIR/map" ]; then
        echo "[ERROR] Refusing to replace existing non-link path: $WORKDIR/map" >&2; exit 1
    else
        ln -s "$MAPDIR" "$WORKDIR/map"
    fi
    if [ -e "$OUTPUT" ] && [ "$FORCE" -eq 0 ]; then
        echo "[ERROR] Output exists (use --force to replace): $OUTPUT" >&2; exit 1
    fi
    [ "$FORCE" -eq 0 ] || rm -f "$OUTPUT"
    INPUT="$INPUT_DIR/flddph_event.bin"
    "$PYTHON" "$SCRIPT_DIR/a02-prepare_downscale_input.py" --case "$CASE" --output "$INPUT"
    echo "[STEP] Downscaling $CASE in requested extent ${WEST}-${EAST}E, ${SOUTH}-${NORTH}N"
    (cd "$WORKDIR" && "$EXE" "$WEST" "$EAST" "$SOUTH" "$NORTH" "$RES" "$INPUT" "$OUTPUT" 1)
    [ -s "$OUTPUT" ] || { echo "[ERROR] Downscaler did not create $OUTPUT" >&2; exit 1; }
    "$PYTHON" - "$OUTPUT" <<'PY'
import sys
from pathlib import Path
import numpy as np
p=Path(sys.argv[1])
a=np.fromfile(p,dtype='<f4')
expected=360*360
if a.size != expected:
    raise SystemExit(f"[ERROR] Expected {expected} Float32 cells, got {a.size}")
valid=np.isfinite(a)&(a!=-9999.0)&(a!=1.0e20)
if not valid.any():
    raise SystemExit('[ERROR] No assigned finite pixels in downscaled output')
if np.any(a[valid] < 0):
    raise SystemExit('[ERROR] Negative inundation depth found')
print(f"[CHECK] cells={a.size}; assigned={valid.sum()}; min={a[valid].min():.6g} m; max={a[valid].max():.6g} m; bytes={p.stat().st_size}")
PY
    echo "[SUCCESS] $CASE downscaling output: $OUTPUT"
done
echo "[WARNING] Reference-point, event, and regional-area choices remain provisional pending researcher review."
