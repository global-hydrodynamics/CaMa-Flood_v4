#!/bin/sh
# Generate bifurcation CSV/NetCDF parameters and standard basin connectivity.
# Copy to <map>/script_params/, configure BIFLAYER and ROFFTAG, and run from any CWD.
#
# Inputs: params.txt, nextxy.bin, basin.bin, bifori.txt, rivhgt.bin,
#         and inpdim_<ROFFTAG>.txt in the map root.
# Outputs: bifprm.csv, netcdf/bifprm.nc, bifbsn.bin, bifcol.bin; optional check maps.
# Usage: ./s02-bifurcation.sh [check_map]
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Bifurcation inputs and runoff-grid matrix selection.
BIFLAYER=5                      # Number of bifori.txt layers to process (1-10).
ROFFTAG="test-glb_1deg"         # Runoff dataset ID used to derive INPDIM.
INPDIM="inpdim_${ROFFTAG}.txt"  # Runoff-grid dimensions file derived from ROFFTAG.
RIVHGT="rivhgt.bin"             # Channel-depth map used to estimate bifurcation depth.
BIFPRM_CSV="bifprm.csv"         # Finalized, named bifurcation parameter table.
BIFPRM_NC="netcdf/bifprm.nc"    # NetCDF conversion of the same CSV table.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Check optional check_map argument"
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
EXEC_SETPARAM="$CODE_DIR/param_bif_set_params"
EXEC_SETBASIN="$CODE_DIR/param_bif_set_basin"
EXEC_CONVERT="$CODE_DIR/convert_bifprm_csv_to_netcdf.py"

# Keep the optional diagnostic switch explicit.
# ${1:-} is empty when no optional check_map argument was supplied.
CHECK_MAP=${1:-}
if [ "$#" -gt 1 ] || { [ -n "$CHECK_MAP" ] && [ "$CHECK_MAP" != check_map ]; }; then
    echo "Usage: $0 [check_map]" >&2
    exit 2
fi

# Keep any existing legacy text table visible; this workflow does not modify it.
if [ -f "$MAP_DIR/bifprm.txt" ]; then
    printf 'WARNING: existing bifprm.txt is a legacy product and will not be changed.\n' >&2
fi

# 1. Filter bifurcation paths by channel width and estimate their bifurcation depth.
cd "$MAP_DIR"

STEP="Filter bifurcation paths and estimate bifurcation-channel depth"
printf '\n==================================================\n[STEP 1/3] Generate finalized bifurcation CSV\n==================================================\n\n'
if [ "$CHECK_MAP" = check_map ]; then
    "$EXEC_SETPARAM" "$RIVHGT" "$BIFPRM_CSV" "$BIFLAYER" "$INPDIM" check_map
else
    "$EXEC_SETPARAM" "$RIVHGT" "$BIFPRM_CSV" "$BIFLAYER" "$INPDIM"
fi
printf '[SUCCESS] Generated %s\n' "$BIFPRM_CSV"

# 2. Convert the finalized CSV without reordering or recalculating its rows.
STEP="Convert finalized bifurcation CSV to NetCDF"
printf '\n==================================================\n[STEP 2/3] Convert CSV to NetCDF\n==================================================\n\n'
mkdir -p "$(dirname "$BIFPRM_NC")"
python3 "$EXEC_CONVERT" "$BIFPRM_CSV" "$BIFPRM_NC"
printf '[SUCCESS] Generated %s from %s\n' "$BIFPRM_NC" "$BIFPRM_CSV"

# 3. Merge basins connected by the retained bifurcation pathways.
cd "$SCRIPT_DIR"

STEP="Merge basins connected by bifurcation pathways"
printf '\n==================================================\n[STEP 3/3] Merge connected basins\n==================================================\n\n'
if [ "$CHECK_MAP" = check_map ]; then
    "$EXEC_SETBASIN" check_map
else
    "$EXEC_SETBASIN"
fi
printf '[SUCCESS] Generated basin-connectivity products\n'

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
