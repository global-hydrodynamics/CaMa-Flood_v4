#!/bin/sh
# Example: extract directed river-node coordinate pairs for a geographic rectangle.
# Run from the map-local script_analysis/ directory; a00 prepares its local executable.
# Inputs: params.txt, nextxy.bin, uparea.bin, elevtn.bin, and lonlat.bin.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# User settings: WEST EAST SOUTH NORTH in degrees; this rectangle covers Mekong.
WEST=90
EAST=110
SOUTH=5
NORTH=35
OUTPUT_FILE="$SCRIPT_DIR/../river_node_latlon_${WEST}_${EAST}_${SOUTH}_${NORTH}.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Quote the output path in the copyable command printed below.
quote_shell_arg() {
    printf "'"
    printf '%s' "$1" | sed "s/'/'\\\\''/g"
    printf "'"
}

cd "$SCRIPT_DIR"

printf '\nWorking directory: %s\n' "$PWD"

printf '\n==================================================\n'
printf '[STEP 1/1] Extract river-node coordinate pairs in %s %s %s %s\n' \
    "$WEST" "$EAST" "$SOUTH" "$NORTH"
printf '==================================================\n\n'
printf './analysis_river_node_latlon %s %s %s %s > ' \
    "$WEST" "$EAST" "$SOUTH" "$NORTH"
quote_shell_arg "$OUTPUT_FILE"
printf '\n'
./analysis_river_node_latlon "$WEST" "$EAST" "$SOUTH" "$NORTH" > "$OUTPUT_FILE"
printf 'Output includes one header line followed by data rows: %s\n' "$OUTPUT_FILE"

printf '\n==================================================\n'
printf '[SUCCESS] River-node coordinate extraction complete.\n'
printf '==================================================\n'
