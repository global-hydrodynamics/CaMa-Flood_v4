#!/bin/sh
# Reproduce the global 1-degree ELSE_GPCC test sample from the external source data.
# Run this script by path from any working directory.

set -eu

# User settings: source files and inclusive daily sample period.
SOURCE_DIR="../ELSE_GPCC/Roff"       # External source directory; relative to this script.
SOURCE_CTL="../ELSE_GPCC/Roff.ctl"   # Tracked grid metadata for the source files.
START_DATE="2000-01-01"              # First daily record; chosen to match the legacy sample.
END_DATE="2001-12-31"                # Last daily record; chosen to match the legacy sample.

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)

case "$SOURCE_DIR" in
    /*) ;;
    *) SOURCE_DIR="$SCRIPT_DIR/$SOURCE_DIR" ;;
esac

case "$SOURCE_CTL" in
    /*) ;;
    *) SOURCE_CTL="$SCRIPT_DIR/$SOURCE_CTL" ;;
esac

printf '\n[STEP 1/1] Extracting %s through %s from ELSE_GPCC\n' "$START_DATE" "$END_DATE"
python3 "$SCRIPT_DIR/extract_ELSE_GPCC.py" \
    --source-dir "$SOURCE_DIR" \
    --source-ctl "$SOURCE_CTL" \
    --output-dir "$SCRIPT_DIR" \
    --start "$START_DATE" \
    --end "$END_DATE"

printf '\n[SUCCESS] ELSE_GPCC 1-degree sample created and validated.\n'
