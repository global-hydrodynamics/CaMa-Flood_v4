#!/bin/sh
# Generate subbasin segmentation for an exploratory threshold without replacing standard products.
# Run from a prepared map-local script_analysis/; the copied executable reads map inputs through ../.
# Inputs: params.txt, nextxy.bin, and uparea.bin.
# Outputs: threshold-named subbsn/subcol maps in OUTPUT_DIR; existing same-name files are overwritten.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# Configuration: this output directory keeps exploratory products separate from standard map products.
AREA_THRESHOLD_KM2=50000
OUTPUT_DIR="$MAP_DIR/analysis"
SUBBSN_OUTPUT="$OUTPUT_DIR/subbsn_${AREA_THRESHOLD_KM2}km2.bin"
SUBCOL_OUTPUT="$OUTPUT_DIR/subcol_${AREA_THRESHOLD_KM2}km2.bin"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Prepare exploratory subbasin outputs"
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

EXEC_SUBBASIN="$SCRIPT_DIR/analysis_subbasin"
mkdir -p "$OUTPUT_DIR"

# The executable reads map inputs through ../ from this map-local directory.
cd "$SCRIPT_DIR"
STEP="Generate exploratory subbasins at ${AREA_THRESHOLD_KM2} km2"
printf '==================================================\n[STEP 1/1] Generate exploratory subbasin maps (%s km2)\n==================================================\n' "$AREA_THRESHOLD_KM2"
"$EXEC_SUBBASIN" "$AREA_THRESHOLD_KM2" "$SUBBSN_OUTPUT" "$SUBCOL_OUTPUT"
printf 'Subbasin IDs: %s\nDisplay colors: %s\n' "$SUBBSN_OUTPUT" "$SUBCOL_OUTPUT"
printf '==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
