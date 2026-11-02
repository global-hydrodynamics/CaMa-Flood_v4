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
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Runoff format, interpolation, and paired runoff-grid matrix files.
TYPE="bin"                                     # Runoff format: bin for binary or cdf for NetCDF.
INTERP="inpmat"                                # Runoff mapping: inpmat for input matrix or near for nearest-grid.
ROFFTAG="test-glb_1deg"                        # Runoff dataset ID used in both matrix filenames.
INPDIM="inpdim_${ROFFTAG}.txt"                 # Runoff-grid dimensions file derived from ROFFTAG.
INPMAT="inpmat_${ROFFTAG}.bin"                 # Runoff input matrix file derived from ROFFTAG.
CROFBIN="../data/ELSE_GPCC_clm-1981-2010.one"  # Annual-mean runoff input [mm/s], relative to the map root.
# Source: Kim et al. (2009), https://doi.org/10.1029/2009GL039006

# Channel cross-sections use H=max(HMIN,HC*Q**HP+HO) and W=max(WMIN,WC*Q**WP+WO), in m.
HC=0.1    # Depth coefficient [m/(m3/s)^HP].
HP=0.50   # Depth power-law exponent (dimensionless).
HO=0.00   # Depth offset [m].
HMIN=1.0  # Minimum channel depth [m].
WC=2.50   # Width coefficient [m/(m3/s)^WP].
WP=0.60   # Width power-law exponent (dimensionless).
WO=0.00   # Width offset [m].
WMIN=5.0  # Minimum channel width [m].

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare channel-parameter workflow"
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
