#!/bin/sh
# Generate annual-runoff channel parameters for a map.
# Copy to <map>/script_params/, configure the paths and coefficients, and run from any CWD.
#
# Inputs: inpdim_<ROFFTAG>.txt, inpmat_<ROFFTAG>.bin, annual runoff climatology,
#         nextxy.bin, ctmare.bin, and width.bin.
# Outputs: outclm.bin, rivwth.bin, rivhgt.bin, rivman.bin, rivhgt_inf.bin,
#          and rivwth_merit.bin in the map root.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Configuration (relative input paths are resolved from the map root).
TYPE="bin"
INTERP="inpmat"
ROFFTAG="${ROFFTAG:-test-1deg}"
INPDIM="inpdim_${ROFFTAG}.txt"
INPMAT="inpmat_${ROFFTAG}.bin"
CROFBIN="../data/ELSE_GPCC_clm-1981-2010.one" # Annual mean runoff [mm/s].
# Source: Kim et al. (2009), https://doi.org/10.1029/2009GL039006

# Channel cross-section coefficients from the standard workflow.
HC=0.1
HP=0.50
HO=0.00
HMIN=1.0
WC=2.50
WP=0.60
WO=0.00
WMIN=5.0

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Prepare channel-parameter workflow"
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

# Shared executables (build beforehand with map/tools/code/Makefile).
EXEC_OUTCLM="$CODE_DIR/param_channel_calc_outclm"
EXEC_RIVWTH="$CODE_DIR/param_channel_calc_rivwth"
EXEC_MERIT="$CODE_DIR/param_channel_set_merit_width"

cd "$MAP_DIR"

STEP="Calculate mean river discharge from annual runoff and the input matrix"
printf '\n==================================================\n[STEP 1/3] Calculate mean river discharge\n==================================================\n\n'
"$EXEC_OUTCLM" "$TYPE" "$INTERP" "$INPDIM" "$CROFBIN"

# 2. Estimate width, depth, roughness, and the no-floodplain depth field.
STEP="Estimate channel width, depth, and roughness"
printf '\n==================================================\n[STEP 2/3] Estimate channel parameters\n==================================================\n\n'
"$EXEC_RIVWTH" "$TYPE" "$INPDIM" "$HC" "$HP" "$HO" "$HMIN" \
    "$WC" "$WP" "$WO" "$WMIN"

# 3. Merge MERIT Hydro width estimates with the power-law river widths.
STEP="Merge power-law channel widths with MERIT Hydro widths"
printf '\n==================================================\n[STEP 3/3] Merge MERIT Hydro channel widths\n==================================================\n\n'
"$EXEC_MERIT" "$INPDIM"
printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
