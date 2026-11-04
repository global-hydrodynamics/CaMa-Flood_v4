#!/bin/sh
# Convert registered high-resolution Binary map tiles to NetCDF4.
# This map-local copy calls the shared converter under map/tools/code/.
# Run it from any working directory; paths are resolved from this script.
set -eu

# ----------------------------------------------------------------------
# User settings: edit these values for the target map.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
# Map root containing each selected high-resolution directory.
MAP_DIR="$SCRIPT_DIR/.."

# Choose one or more available high-resolution map directories.
# Each selected directory must contain location.txt and registered tile files.
#HIRES="1min 15sec 3sec"
HIRES="1min"

# Choose registered Binary file stems present in the selected directories.
# Options shown here are high-resolution registry products; a map need not
# contain every one of them. Multi-record files (catmxy, downxy) are converted
# with all their registered records together.
#VARS="catmxy catmz100 catmzz downxy elevtn flddif flwdir grdare hand rivwth uparea visual"
VARS="catmxy downxy elevtn uparea hand flddif rivwth"

# Lossless zlib/deflate compression level (0 disables deflate; range 0-9).
COMPRESSION_LEVEL=6

# Existing NetCDF outputs are skipped by default. Pass --force to replace them.
FORCE=0

# Shared converter path: use the local template's code link if present,
# otherwise resolve map/tools/code/ from this map-local script directory.
if [ -f "$SCRIPT_DIR/../code/convert_highres_binary_to_netcdf.py" ]; then
    CONVERTER="$SCRIPT_DIR/../code/convert_highres_binary_to_netcdf.py"
else
    CONVERTER="$SCRIPT_DIR/../../tools/code/convert_highres_binary_to_netcdf.py"
fi
# User settings end here.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Validate high-resolution conversion settings"
# Print a stage-specific error when any command exits unsuccessfully.
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
# Check the user-configured paths before creating output directories.
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
printf 'Map: %s\nHigh-resolution directories: %s\nVariables: %s\nCompression level: %s\n\n' \
    "$MAP_DIR" "$HIRES" "$VARS" "$COMPRESSION_LEVEL"

# Preserve the geometry metadata beside the converted tile rasters.
for re in $HIRES; do
    SOURCE_LOCATION="$MAP_DIR/$re/location.txt"
    OUTPUT_DIR="$MAP_DIR/${re}_nc"
    if [ ! -f "$SOURCE_LOCATION" ]; then
        printf 'ERROR: high-resolution geometry file not found: %s\n' "$SOURCE_LOCATION" >&2
        exit 1
    fi
    mkdir -p "$OUTPUT_DIR"
    cp "$SOURCE_LOCATION" "$OUTPUT_DIR/location.txt"
    printf '[INFO] Copied %s to %s/location.txt\n' "$SOURCE_LOCATION" "$OUTPUT_DIR"

    # Convert one registered Binary stem at a time; Python handles tile layout,
    # registry validation, chunked I/O, NetCDF creation, and read-back checks.
    for var in $VARS; do
        if [ "$FORCE" -eq 1 ]; then
            if python3 "$CONVERTER" --map-dir "$MAP_DIR" --hires "$re" --variable "$var" \
                --compression-level "$COMPRESSION_LEVEL" --force; then
                :
            else
                EXIT_STATUS=$?
                exit "$EXIT_STATUS"
            fi
        else
            if python3 "$CONVERTER" --map-dir "$MAP_DIR" --hires "$re" --variable "$var" \
                --compression-level "$COMPRESSION_LEVEL"; then
                :
            else
                EXIT_STATUS=$?
                exit "$EXIT_STATUS"
            fi
        fi
    done
done

printf '\n[SUCCESS] High-resolution Binary-to-NetCDF conversion completed.\n'
