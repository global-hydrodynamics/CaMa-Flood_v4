#!/bin/sh
# Convert registered high-resolution map Binary files in an external data package.
# The source map stays outside this repository; NetCDF files are written beside it.
set -eu

# ----------------------------------------------------------------------
# User settings.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_ROOT="/Users/yamadai/work/CaMa_v400/cmf_v400_data/map_full"
MAPS="glb_15min"
HIRES="1min 15sec 3sec"
VARS="catmxy downxy elevtn uparea hand flddif rivwth"
FORCE=0

# Resolve the converter and metadata registry from this script's v5 repository.
REPO_ROOT=$(CDPATH= cd "$SCRIPT_DIR/../../.." && pwd -P)
CONVERTER="$REPO_ROOT/map/tools/code/convert_highres_binary_to_netcdf.py"
REGISTRY="$REPO_ROOT/variable_table/map_variables.csv"
# User settings end here.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Validate external map conversion settings"
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

if [ ! -d "$MAP_ROOT" ]; then
    printf '[ERROR] External map root does not exist: %s\n' "$MAP_ROOT" >&2
    exit 1
fi
if [ ! -f "$CONVERTER" ]; then
    printf '[ERROR] Converter not found: %s\n' "$CONVERTER" >&2
    exit 1
fi
if [ ! -f "$REGISTRY" ]; then
    printf '[ERROR] Map variable registry not found: %s\n' "$REGISTRY" >&2
    exit 1
fi

STEP="Convert external high-resolution maps"
printf '\n==================================================\n'
printf '[STEP 1/1] Convert selected external high-resolution map tiles\n'
printf '==================================================\n'
printf 'Map root: %s\nMaps: %s\nHigh-resolution directories: %s\nVariables: %s\n' \
    "$MAP_ROOT" "$MAPS" "$HIRES" "$VARS"

# Traverse map, resolution, and variable selections independently.
for map in $MAPS; do
    MAP_DIR="$MAP_ROOT/$map"
    if [ ! -d "$MAP_DIR" ]; then
        printf '[WARNING] Map directory not found; skipping: %s\n' "$MAP_DIR"
        continue
    fi

    for re in $HIRES; do
        HIRES_DIR="$MAP_DIR/$re"
        if [ ! -d "$HIRES_DIR" ]; then
            printf '[WARNING] High-resolution directory not found; skipping: %s\n' "$HIRES_DIR"
            continue
        fi
        SOURCE_LOCATION="$HIRES_DIR/location.txt"
        if [ ! -f "$SOURCE_LOCATION" ]; then
            printf '[ERROR] High-resolution directory has no location.txt: %s\n' "$HIRES_DIR" >&2
            exit 1
        fi

        # Keep the tile geometry beside its converted NetCDF products.
        OUTPUT_DIR="$MAP_DIR/${re}_nc"
        mkdir -p "$OUTPUT_DIR"
        cp "$SOURCE_LOCATION" "$OUTPUT_DIR/location.txt"
        printf '[INFO] Copied %s to %s/location.txt\n' "$SOURCE_LOCATION" "$OUTPUT_DIR"

        for var in $VARS; do
            printf '\n[INFO] Map=%s resolution=%s variable=%s\n' "$map" "$re" "$var"
            if [ "$FORCE" -eq 1 ]; then
                if python3 "$CONVERTER" --map-dir "$MAP_DIR" --hires "$re" \
                    --variable "$var" --registry "$REGISTRY" --force; then
                    :
                else
                    EXIT_STATUS=$?
                    printf '[ERROR] Conversion failed: map=%s resolution=%s variable=%s\n' \
                        "$map" "$re" "$var" >&2
                    exit "$EXIT_STATUS"
                fi
            else
                if python3 "$CONVERTER" --map-dir "$MAP_DIR" --hires "$re" \
                    --variable "$var" --registry "$REGISTRY"; then
                    :
                else
                    EXIT_STATUS=$?
                    printf '[ERROR] Conversion failed: map=%s resolution=%s variable=%s\n' \
                        "$map" "$re" "$var" >&2
                    exit "$EXIT_STATUS"
                fi
            fi
        done
    done
done

printf '\n[SUCCESS] External high-resolution Binary-to-NetCDF conversion completed.\n'
