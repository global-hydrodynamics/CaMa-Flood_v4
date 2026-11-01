#!/bin/sh
# Generate standard subbasin segmentation and display-color maps.
# Copy to <map>/script_params/ and run from any working directory.
# Inputs: params.txt, nextxy.bin, uparea.bin.
# Outputs: subbsn.bin and subcol.bin in the map root; existing files are overwritten.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Example segmentation threshold; this is not a universal scientific calibration.
AREA_THRESHOLD_KM2=30000  # Upstream-area threshold for subbasin segmentation (km2).

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

# The program reads map inputs and writes the standard products via ../.
cd "$SCRIPT_DIR"

STEP="Segment the river network at the ${AREA_THRESHOLD_KM2} km2 example threshold"
printf '\n==================================================\n[STEP 1/1] Generate subbasin maps (%s km2)\n==================================================\n\n' "$AREA_THRESHOLD_KM2"
"$EXEC_SUBBASIN" "$AREA_THRESHOLD_KM2" ../subbsn.bin ../subcol.bin

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
