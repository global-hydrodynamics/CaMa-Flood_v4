#!/bin/sh
# Extract daily Mekong runoff from the original ELSE_GPCC global 1-degree data.
# Run by path from any working directory; all paths below are relative to this script.

set -eu

# Source data and grid metadata. Relative paths are resolved from this script directory.
SOURCE_DIR="../ELSE_GPCC/Roff"
SOURCE_CTL="../ELSE_GPCC/Roff.ctl"

# Fixed Mekong sample domain (grid-cell boundaries) and inclusive daily period.
WEST=90
EAST=110
SOUTH=5
NORTH=35
START_DATE="1980-01-01"
END_DATE="2010-12-31"

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)

case "$SOURCE_DIR" in
    /*) ;;
    *) SOURCE_DIR="$SCRIPT_DIR/$SOURCE_DIR" ;;
esac

case "$SOURCE_CTL" in
    /*) ;;
    *) SOURCE_CTL="$SCRIPT_DIR/$SOURCE_CTL" ;;
esac

printf '\n[STEP 1/1] Extracting %s to %s\n' "$SOURCE_DIR" "$SCRIPT_DIR/runoff"
python3 "$SCRIPT_DIR/extract_ELSE_GPCC.py" \
    --source-dir "$SOURCE_DIR" \
    --source-ctl "$SOURCE_CTL" \
    --output-dir "$SCRIPT_DIR" \
    --west "$WEST" --east "$EAST" \
    --south "$SOUTH" --north "$NORTH" \
    --start "$START_DATE" --end "$END_DATE"

printf '\n[SUCCESS] ELSE_GPCC sample extraction and validation completed.\n'
