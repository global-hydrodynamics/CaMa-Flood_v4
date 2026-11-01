#!/bin/sh
# Example: generate exploratory subbasin maps without replacing standard products.
# Run from the map-local script_analysis/ directory; a00 prepares its local executable.
# Inputs: params.txt, nextxy.bin, and uparea.bin.
# Outputs: threshold-named subbsn/subcol maps in OUTPUT_DIR; existing same-name files are overwritten.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# Exploratory subbasin outputs are kept separate from standard map products.
AREA_THRESHOLD_KM2=50000                                         # Example upstream-area threshold (km2); not calibrated universally.
OUTPUT_DIR="$MAP_DIR/analysis"                                   # Directory for exploratory outputs under the map root.
SUBBSN_OUTPUT="$OUTPUT_DIR/subbsn_${AREA_THRESHOLD_KM2}km2.bin"  # Output path for subbasin IDs, named with the threshold.
SUBCOL_OUTPUT="$OUTPUT_DIR/subcol_${AREA_THRESHOLD_KM2}km2.bin"  # Output path for display colors, named with the threshold.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare exploratory subbasin outputs"
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

mkdir -p "$OUTPUT_DIR"

# The executable reads map inputs through ../ from this map-local directory.
cd "$SCRIPT_DIR"

printf '\nWorking directory: %s\n' "$PWD"

STEP="Generate exploratory subbasins at ${AREA_THRESHOLD_KM2} km2"
printf '\n==================================================\n[STEP 1/1] Generate exploratory subbasin maps (%s km2)\n==================================================\n\n' "$AREA_THRESHOLD_KM2"
printf './analysis_subbasin %s ' "$AREA_THRESHOLD_KM2"
quote_shell_arg "$SUBBSN_OUTPUT"
printf ' '
quote_shell_arg "$SUBCOL_OUTPUT"
printf '\n'
./analysis_subbasin "$AREA_THRESHOLD_KM2" "$SUBBSN_OUTPUT" "$SUBCOL_OUTPUT"
printf 'Subbasin IDs: %s\nDisplay colors: %s\n' "$SUBBSN_OUTPUT" "$SUBCOL_OUTPUT"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
