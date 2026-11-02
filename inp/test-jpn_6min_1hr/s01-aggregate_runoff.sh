#!/bin/sh
# Create the compact 6-arcmin Japan hourly runoff sample from test_jpn_1hr.
# Run this script by path; input/output paths are resolved relative to this file.

set -eu

# Source data and target grid. Keep the source path relative and read-only.
SOURCE_DIR="../test_jpn_1hr"
WEST=123
EAST=148
SOUTH=24
NORTH=46
SOURCE_RESOLUTION="0.01666666666666666667"
TARGET_RESOLUTION="0.1"
START_DATE="2007-09-03"
END_DATE="2007-09-09"
HOURS_PER_DAY=24

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)

case "$SOURCE_DIR" in
    /*) ;;
    *) SOURCE_DIR="$SCRIPT_DIR/$SOURCE_DIR" ;;
esac

printf '\n[STEP 1/1] Aggregating Japan hourly runoff from %s\n' "$SOURCE_DIR"
python3 "$SCRIPT_DIR/aggregate_runoff.py" \
    --source-dir "$SOURCE_DIR" \
    --output-dir "$SCRIPT_DIR" \
    --west "$WEST" --east "$EAST" \
    --south "$SOUTH" --north "$NORTH" \
    --source-resolution "$SOURCE_RESOLUTION" \
    --target-resolution "$TARGET_RESOLUTION" \
    --start "$START_DATE" --end "$END_DATE" \
    --hours-per-day "$HOURS_PER_DAY"

printf '\n[SUCCESS] Japan 6-arcmin hourly runoff aggregation and validation completed.\n'
