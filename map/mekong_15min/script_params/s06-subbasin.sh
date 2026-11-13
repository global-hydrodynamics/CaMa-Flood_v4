#!/bin/sh
# Generate standard subbasin segmentation and display-color maps.
# Copy to <map>/script_params/ and run from any working directory.
# Inputs: params.txt, nextxy.bin, uparea.bin.
# Outputs: subbsn-sample.bin and subcol-sample.bin in the map root; existing files are overwritten.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Editable example thresholds, not universal scientific parameters.
AREA_THRESHOLD1_KM2=30000  # First segmentation threshold [km2].
AREA_THRESHOLD2_KM2=5000   # Second threshold [km2]; set empty for single-stage operation.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare subbasin segmentation"
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

EXEC_SUBBASIN="$CODE_DIR/analysis_subbasin"

# The program reads map inputs via ../; explicit paths select the sample outputs.
cd "$SCRIPT_DIR"

STEP="Segment the river network at the ${AREA_THRESHOLD1_KM2}/${AREA_THRESHOLD2_KM2} km2 example thresholds"
printf '\n==================================================\n[STEP 1/1] Generate sample subbasin maps (%s / %s km2)\n==================================================\n\n' "$AREA_THRESHOLD1_KM2" "$AREA_THRESHOLD2_KM2"
if [ -n "$AREA_THRESHOLD2_KM2" ]; then
    "$EXEC_SUBBASIN" "$AREA_THRESHOLD1_KM2" "$AREA_THRESHOLD2_KM2" \
        "$MAP_DIR/subbsn-sample.bin" "$MAP_DIR/subcol-sample.bin"
else
    "$EXEC_SUBBASIN" "$AREA_THRESHOLD1_KM2" \
        "$MAP_DIR/subbsn-sample.bin" "$MAP_DIR/subcol-sample.bin"
fi

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
