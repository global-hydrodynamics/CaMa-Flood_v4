#!/bin/sh
# Example: generate exploratory subbasin maps without replacing standard products.
# Run from script_analysis/ after a00-link_codes.sh creates the shared-code link.
# Inputs: params.txt, nextxy.bin, and uparea.bin.
# Outputs: threshold-named subbsn/subcol maps in OUT_DIR; existing same-name files are overwritten.
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# Analysis output directory, relative to script_analysis/.
OUT_DIR="out_analysis"  # Change to ../out_analysis to use the map-root output folder.

# The threshold is illustrative; these exploratory products do not replace standard maps.
AREA_THRESHOLD_KM2=50000                                      # Upstream-area threshold (km2), not calibrated universally.
SUBBSN_OUTPUT="$OUT_DIR/subbsn_${AREA_THRESHOLD_KM2}km2.bin"  # Output path for subbasin IDs.
SUBCOL_OUTPUT="$OUT_DIR/subcol_${AREA_THRESHOLD_KM2}km2.bin"  # Output path for display colors.

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

# The executable reads map inputs through ../ from this map-local directory.
cd "$SCRIPT_DIR"
mkdir -p "$OUT_DIR"

printf '\nWorking directory: %s\n' "$PWD"

STEP="Generate exploratory subbasins at ${AREA_THRESHOLD_KM2} km2"
printf '\n==================================================\n[STEP 1/1] Generate exploratory subbasin maps (%s km2)\n==================================================\n\n' "$AREA_THRESHOLD_KM2"
printf './code/analysis_subbasin %s ' "$AREA_THRESHOLD_KM2"
quote_shell_arg "$SUBBSN_OUTPUT"
printf ' '
quote_shell_arg "$SUBCOL_OUTPUT"
printf '\n'
./code/analysis_subbasin "$AREA_THRESHOLD_KM2" "$SUBBSN_OUTPUT" "$SUBCOL_OUTPUT"
printf 'Subbasin IDs: %s\nDisplay colors: %s\n' "$SUBBSN_OUTPUT" "$SUBCOL_OUTPUT"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
