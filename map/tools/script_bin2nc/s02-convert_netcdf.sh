#!/bin/sh
# Convert selected map-root Binary files to individual NetCDF files.
# Inputs: params.txt, bin2nc_list.txt, registered .bin files, map_variables.csv.
# Outputs: <map>/netcdf/<binary-stem>.nc.
set -eu

# Resolve this template independently of the caller's working directory.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
# Default target when running the shared template; map-local copies use "..".
MAP_DIR="$SCRIPT_DIR/../../mekong_15min"  # Change this to select another map directory.
NETCDF_DIR="$MAP_DIR/netcdf"
LIST_FILE="$NETCDF_DIR/bin2nc_list.txt"
REGISTRY="$SCRIPT_DIR/../../map_variables.csv"
if [ -f "$SCRIPT_DIR/../convert_map_binary_to_netcdf.py" ]; then
    TOOL_DIR="$SCRIPT_DIR/.."
else
    TOOL_DIR="$SCRIPT_DIR/../../tools"
fi
CONVERTER="$TOOL_DIR/convert_map_binary_to_netcdf.py"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Read and validate the selected conversion list"
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

FORCE=0
if [ "$#" -gt 1 ] || { [ "$#" -eq 1 ] && [ "$1" != "--force" ]; }; then
    printf 'Usage: %s [--force]\n' "$SCRIPT_NAME" >&2
    exit 2
fi
if [ "$#" -eq 1 ]; then
    FORCE=1
fi

if [ ! -f "$LIST_FILE" ]; then
    printf 'ERROR: conversion list not found: %s\n' "$LIST_FILE" >&2
    printf 'Run s01-list_binary.sh first.\n' >&2
    exit 1
fi

FILE_COUNT=0
SEEN_FILES=""
while IFS= read -r FILE || [ -n "$FILE" ]; do
    case "$FILE" in
        ""|\#*)
            continue
            ;;
    esac

    case "$FILE" in
        *.bin)
            ;;
        *)
            printf 'ERROR: invalid list entry (expected a .bin filename): %s\n' "$FILE" >&2
            exit 1
            ;;
    esac
    case "$FILE" in
        *[!a-zA-Z0-9_.-]*)
            printf 'ERROR: invalid or non-base filename in conversion list: %s\n' "$FILE" >&2
            exit 1
            ;;
    esac
    if [ ! -f "$MAP_DIR/$FILE" ]; then
        printf 'ERROR: listed Binary file does not exist: %s\n' "$MAP_DIR/$FILE" >&2
        exit 1
    fi
    case " $SEEN_FILES " in
        *" $FILE "*)
            printf 'ERROR: duplicate file in conversion list: %s\n' "$FILE" >&2
            exit 1
            ;;
    esac
    SEEN_FILES="$SEEN_FILES $FILE"
    FILE_COUNT=$((FILE_COUNT + 1))
done < "$LIST_FILE"

if [ "$FILE_COUNT" -eq 0 ]; then
    printf 'ERROR: no files are selected in %s.\n' "$LIST_FILE" >&2
    exit 1
fi

mkdir -p "$NETCDF_DIR"
CONVERTED=0
SKIPPED=0
FILE_NUMBER=0
STEP="Convert selected Binary map files"

# Keep the list on file descriptor 3 so overwrite prompts still read stdin.
while IFS= read -r FILE <&3 || [ -n "$FILE" ]; do
    case "$FILE" in
        ""|\#*)
            continue
            ;;
    esac

    FILE_NUMBER=$((FILE_NUMBER + 1))
    OUTPUT_FILE="$NETCDF_DIR/${FILE%.bin}.nc"
    printf '\n==================================================\n'
    printf '[STEP %s/%s] Convert %s\n' "$FILE_NUMBER" "$FILE_COUNT" "$FILE"
    printf '==================================================\n\n'

    if [ -f "$OUTPUT_FILE" ] && [ "$FORCE" -eq 0 ]; then
        if [ ! -t 0 ]; then
            printf 'Existing output preserved in non-interactive mode: %s\n' "$OUTPUT_FILE"
            SKIPPED=$((SKIPPED + 1))
            continue
        fi

        while :; do
            printf '%s already exists.\nOverwrite? [y/N]: ' "$OUTPUT_FILE"
            if ! IFS= read -r ANSWER; then
                ANSWER=""
            fi
            case "$ANSWER" in
                y|Y)
                    break
                    ;;
                ""|n|N)
                    printf 'Skipping %s.\n' "$FILE"
                    SKIPPED=$((SKIPPED + 1))
                    continue 2
                    ;;
                *)
                    printf 'Please answer y or N.\n' >&2
                    ;;
            esac
        done
    fi

    if [ "$FORCE" -eq 1 ]; then
        python3 "$CONVERTER" \
            --map-dir "$MAP_DIR" \
            --registry "$REGISTRY" \
            --input "$MAP_DIR/$FILE" \
            --output "$OUTPUT_FILE" \
            --force
    else
        python3 "$CONVERTER" \
            --map-dir "$MAP_DIR" \
            --registry "$REGISTRY" \
            --input "$MAP_DIR/$FILE" \
            --output "$OUTPUT_FILE"
    fi
    CONVERTED=$((CONVERTED + 1))
done 3< "$LIST_FILE"

printf '\nConverted: %s; skipped: %s\n' "$CONVERTED" "$SKIPPED"
printf '\n==================================================\n'
printf '[SUCCESS] %s\n' "$SCRIPT_NAME"
printf '==================================================\n'
