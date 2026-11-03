#!/bin/sh
# Combine individual map NetCDF files into legacy consolidated containers.
# Inputs: individual map NetCDF files in <map>/netcdf/.
# Outputs: rivclim.nc and rivpar.nc in <map>/netcdf/.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR="$SCRIPT_DIR/../../mekong_15min"  # Change this path when using another map.
NETCDF_DIR="$MAP_DIR/netcdf"
if [ -f "$SCRIPT_DIR/../create_legacy_netcdf_map.py" ]; then
    TOOL_DIR="$SCRIPT_DIR/.."
else
    TOOL_DIR="$SCRIPT_DIR/../../tools"
fi
PROGRAM="$TOOL_DIR/create_legacy_netcdf_map.py"

# River network connectivity
#NEXTXY="nextxy.nc"            # Include regional-boundary river connections.
NEXTXY="nextxy_noedge.nc"      # Exclude boundary-affected networks (default).

# River channel width
#RIVWTH="rivwth.nc"             # Width estimated from empirical parameterization.
RIVWTH="rivwth_merit.nc"        # MERIT-based channel width (default).

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare legacy NetCDF map containers"
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

FORCE=""
if [ "$#" -gt 1 ] || { [ "$#" -eq 1 ] && [ "$1" != "--force" ]; }; then
    printf 'Usage: %s [--force]\n' "$SCRIPT_NAME" >&2
    exit 2
fi
if [ "$#" -eq 1 ]; then
    FORCE="--force"
fi

if [ ! -d "$NETCDF_DIR" ]; then
    printf 'ERROR: individual NetCDF map directory not found: %s\n' "$NETCDF_DIR" >&2
    exit 1
fi

printf '\n==================================================\n'
printf '[STEP 1/1] Create legacy consolidated map files\n'
printf '==================================================\n\n'
STEP="Create and verify rivclim.nc and rivpar.nc"
STATUS=0
if [ -n "$FORCE" ]; then
    if python3 "$PROGRAM" --map-dir "$MAP_DIR" --nextxy "$NEXTXY" --rivwth "$RIVWTH" "$FORCE"; then
        STATUS=0
    else
        STATUS=$?
    fi
elif python3 "$PROGRAM" --map-dir "$MAP_DIR" --nextxy "$NEXTXY" --rivwth "$RIVWTH"; then
    STATUS=0
else
    STATUS=$?
fi

if [ "$STATUS" -eq 3 ]; then
    exit 0
fi
if [ "$STATUS" -ne 0 ]; then
    exit "$STATUS"
fi

printf '\n==================================================\n'
printf '[SUCCESS] %s\n' "$SCRIPT_NAME"
printf '==================================================\n'
