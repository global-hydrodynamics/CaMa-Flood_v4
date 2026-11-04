#!/bin/sh
# Convert registered high-resolution Binary map tiles to NetCDF4.
set -eu

# ----------------------------------------------------------------------
# User settings.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR="$SCRIPT_DIR/.."  # Map root containing the high-resolution directory.
HIRES="1min"
VARS="catmxy downxy elevtn uparea hand flddif rivwth"
FORCE=0

if [ -f "$SCRIPT_DIR/../code/convert_highres_binary_to_netcdf.py" ]; then
    CONVERTER="$SCRIPT_DIR/../code/convert_highres_binary_to_netcdf.py"
else
    CONVERTER="$SCRIPT_DIR/../../tools/code/convert_highres_binary_to_netcdf.py"
fi
# User settings end here.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Validate high-resolution conversion settings"
trap '
    EXIT_STATUS=$?
    trap - 0
    if [ "$EXIT_STATUS" -ne 0 ]; then
        printf "\n[ERROR] %s failed during: %s (exit %s)\n" "$SCRIPT_NAME" "$STEP" "$EXIT_STATUS" >&2
    fi
    exit "$EXIT_STATUS"
' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

if [ "$#" -gt 1 ] || { [ "$#" -eq 1 ] && [ "$1" != "--force" ]; }; then
    printf 'Usage: %s [--force]\n' "$SCRIPT_NAME" >&2
    exit 2
fi
if [ "$#" -eq 1 ]; then
    FORCE=1
fi
if [ ! -d "$MAP_DIR" ]; then
    printf 'ERROR: map directory does not exist: %s\n' "$MAP_DIR" >&2
    exit 1
fi
if [ ! -f "$CONVERTER" ]; then
    printf 'ERROR: converter not found: %s\n' "$CONVERTER" >&2
    exit 1
fi

STEP="Convert selected high-resolution tiles"
printf '\n==================================================\n'
printf '[STEP 1/1] Convert registered high-resolution variables\n'
printf '==================================================\n'
printf 'Map: %s\nHigh-resolution directories: %s\nVariables: %s\n\n' "$MAP_DIR" "$HIRES" "$VARS"

for re in $HIRES; do
    for var in $VARS; do
        if [ "$FORCE" -eq 1 ]; then
            if python3 "$CONVERTER" --map-dir "$MAP_DIR" --hires "$re" --variable "$var" --force; then
                :
            else
                EXIT_STATUS=$?
                exit "$EXIT_STATUS"
            fi
        else
            if python3 "$CONVERTER" --map-dir "$MAP_DIR" --hires "$re" --variable "$var"; then
                :
            else
                EXIT_STATUS=$?
                exit "$EXIT_STATUS"
            fi
        fi
    done
done

printf '\n[SUCCESS] High-resolution Binary-to-NetCDF conversion completed.\n'
