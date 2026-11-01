#!/bin/sh
# Example: extract directed river-node coordinate pairs for a geographic rectangle.
# Run from script_analysis/ after a00-link_codes.sh creates the shared-code link.
# Inputs: params.txt, nextxy.bin, uparea.bin, elevtn.bin, and lonlat.bin.
set -eu

# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# Analysis output directory, relative to script_analysis/.
OUT_DIR="out_analysis"  # Change to ../out_analysis to use the map-root output folder.

# Geographic selection rectangle; bounds are in degrees.
WEST=90                                                        # Western longitude boundary.
EAST=110                                                       # Eastern longitude boundary.
SOUTH=5                                                        # Southern latitude boundary.
NORTH=35                                                       # Northern latitude boundary.
OUTPUT_FILE="$OUT_DIR/river_node_latlon_${WEST}_${EAST}_${SOUTH}_${NORTH}.txt"  # Text output path.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# The sed expression escapes apostrophes so printed paths can be pasted into a shell.
quote_shell_arg() {
    printf "'"
    printf '%s' "$1" | sed "s/'/'\\\\''/g"
    printf "'"
}

cd "$SCRIPT_DIR"
mkdir -p "$OUT_DIR"

printf '\nWorking directory: %s\n' "$PWD"

printf '\n==================================================\n'
printf '[STEP 1/1] Extract river-node coordinate pairs in %s %s %s %s\n' \
    "$WEST" "$EAST" "$SOUTH" "$NORTH"
printf '==================================================\n\n'
printf './code/analysis_river_node_latlon %s %s %s %s > ' \
    "$WEST" "$EAST" "$SOUTH" "$NORTH"
quote_shell_arg "$OUTPUT_FILE"
printf '\n'
./code/analysis_river_node_latlon "$WEST" "$EAST" "$SOUTH" "$NORTH" > "$OUTPUT_FILE"
printf 'Output includes one header line followed by data rows: %s\n' "$OUTPUT_FILE"

printf '\n==================================================\n'
printf '[SUCCESS] River-node coordinate extraction complete.\n'
printf '==================================================\n'
