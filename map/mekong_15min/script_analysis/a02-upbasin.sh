#!/bin/sh
# Example: create a single-outlet upstream-basin mask for a river-grid cell.
# Run from the map-local script_analysis/ directory; a00 prepares its local executable.
# Inputs: params.txt, nextxy.bin, upgrid.bin, and uparea.bin.
# Output: a configurable int32 NX x NY mask, separate from the standard upbasin.bin.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# Configuration: use one-based Fortran grid coordinates and choose a nonstandard output name.
IX=62
IY=93
OUTPUT_FILE="$MAP_DIR/analysis_upbasin_${IX}_${IY}.bin"
REPORT_FILE="$MAP_DIR/analysis_upbasin_${IX}_${IY}.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Prepare upstream-basin analysis"
# On exit, report failures and return the original status; signals use standard nonzero statuses.
trap '
    EXIT_STATUS=$?
    trap - 0
    if [ "$EXIT_STATUS" -ne 0 ]; then
        printf "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
        printf "!!! ERROR: %s FAILED !!!\n" "$SCRIPT_NAME" >&2
        printf "!!! Step: %s\n" "$STEP" >&2
        printf "!!! Exit status: %s\n" "$EXIT_STATUS" >&2
        printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
    fi
    exit "$EXIT_STATUS"
' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

# ----------------------------------------------------------------------
# End of error handling.
# ----------------------------------------------------------------------

# Quote output paths in the copyable command printed below.
quote_shell_arg() {
    printf "'"
    printf '%s' "$1" | sed "s/'/'\\\\''/g"
    printf "'"
}

# The program reads map inputs via ../ from the map-local script directory.
cd "$SCRIPT_DIR"

printf '\nWorking directory: %s\n' "$PWD"

STEP="Extract cells upstream of ($IX,$IY)"
printf '\n==================================================\n[STEP 1/1] Create upstream-basin mask\n==================================================\n\n'
printf './analysis_upbasin %s %s ' "$IX" "$IY"
quote_shell_arg "$OUTPUT_FILE"
printf ' > '
quote_shell_arg "$REPORT_FILE"
printf '\n'
./analysis_upbasin "$IX" "$IY" "$OUTPUT_FILE" > "$REPORT_FILE"
printf 'Upstream-basin report: %s\nMask: %s\n' "$REPORT_FILE" "$OUTPUT_FILE"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
