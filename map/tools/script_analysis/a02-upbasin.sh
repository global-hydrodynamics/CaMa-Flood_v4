#!/bin/sh
# Example: create a single-outlet upstream-basin mask for a river-grid cell.
# Run from the map-local script_analysis/ directory; a00 prepares its local executable.
# Inputs: params.txt, nextxy.bin, upgrid.bin, and uparea.bin.
# Output: a configurable int32 NX x NY mask, separate from the standard upbasin.bin.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# Select a one-based CaMa-Flood river-grid cell and separate analysis outputs from standard products.
IX=62                                                    # CaMa-Flood grid column (one-based Fortran index).
IY=93                                                    # CaMa-Flood grid row (one-based Fortran index).
OUTPUT_FILE="$MAP_DIR/analysis_upbasin_${IX}_${IY}.bin"  # Output path for the int32 upstream-basin raster.
REPORT_FILE="$MAP_DIR/analysis_upbasin_${IX}_${IY}.txt"  # Text report path; the executable output is redirected here.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare upstream-basin analysis"
# The exit trap reports failures and preserves status; signal traps use standard nonzero statuses.
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

# The sed expression escapes apostrophes so printed paths can be pasted into a shell.
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
