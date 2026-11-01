#!/bin/sh
# Generate standard non-MPI bifurcation parameters and basin connectivity.
# Copy to <map>/script_params/, configure BIFLAYER and ROFFTAG, and run from any CWD.
#
# Inputs: params.txt, nextxy.bin, basin.bin, bifori.txt, rivhgt.bin,
#         and inpdim_<ROFFTAG>.txt in the map root.
# Outputs: bifprm.txt, bifbsn.bin, bifcol.bin; optional check maps with check_map.
# Usage: ./s02-bifurcation.sh [check_map]
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Configuration
BIFLAYER=5
ROFFTAG="${ROFFTAG:-test-1deg}"
INPDIM="inpdim_${ROFFTAG}.txt"
RIVHGT="rivhgt.bin"
BIFPRM="bifprm.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Check optional check_map argument"
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
EXEC_SETPARAM="$CODE_DIR/param_bif_set_params"
EXEC_SETBASIN="$CODE_DIR/param_bif_set_basin"

# Keep the optional diagnostic switch explicit.
CHECK_MAP=${1:-}
if [ "$#" -gt 1 ] || { [ -n "$CHECK_MAP" ] && [ "$CHECK_MAP" != check_map ]; }; then
    echo "Usage: $0 [check_map]" >&2
    exit 2
fi

# 1. Filter bifurcation paths by channel width and estimate their bifurcation depth.
cd "$MAP_DIR"

STEP="Filter bifurcation paths and estimate bifurcation-channel depth"
printf '\n==================================================\n[STEP 1/2] Estimate bifurcation parameters\n==================================================\n\n'
if [ "$CHECK_MAP" = check_map ]; then
    "$EXEC_SETPARAM" "$RIVHGT" "$BIFPRM" "$BIFLAYER" "$INPDIM" check_map
else
    "$EXEC_SETPARAM" "$RIVHGT" "$BIFPRM" "$BIFLAYER" "$INPDIM"
fi

# 2. Merge basins connected by the retained bifurcation pathways.
cd "$SCRIPT_DIR"

STEP="Merge basins connected by bifurcation pathways"
printf '\n==================================================\n[STEP 2/2] Merge connected basins\n==================================================\n\n'
if [ "$CHECK_MAP" = check_map ]; then
    "$EXEC_SETBASIN" check_map
else
    "$EXEC_SETBASIN"
fi

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
