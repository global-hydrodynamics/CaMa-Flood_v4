#!/bin/sh
# Create an editable list of registered base-map Binary files for conversion.
# Inputs: <map>/params.txt, variable_table/map_variables.csv, and map-root .bin files.
# Output: bin2nc_list.txt in the current working directory.
set -eu

# Resolve shared tools from the script location; keep the list in the current directory.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
# Default target when running the shared template; map-local copies use "..".
MAP_DIR="$SCRIPT_DIR/../../mekong_15min"  # Change this to select another map directory.
LIST_FILE="bin2nc_list.txt"
REGISTRY="$SCRIPT_DIR/../../../variable_table/map_variables.csv"
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
STEP="Check the existing conversion list"
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

if [ ! -d "$MAP_DIR" ]; then
    printf 'ERROR: map directory does not exist: %s\n' "$MAP_DIR" >&2
    exit 1
fi

if [ -f "$LIST_FILE" ]; then
    if [ ! -t 0 ]; then
        printf 'Existing conversion list preserved in non-interactive mode: %s\n' "$LIST_FILE"
        exit 0
    fi

    while :; do
        printf '\n%s already exists. Regenerate it? [y/N]: ' "$LIST_FILE"
        if ! IFS= read -r ANSWER; then
            ANSWER=""
        fi
        case "$ANSWER" in
            y|Y)
                break
                ;;
            ""|n|N)
                printf 'Keeping the existing conversion list.\n'
                exit 0
                ;;
            *)
                printf 'Please answer y or N.\n' >&2
                ;;
        esac
    done
fi

STEP="Generate the base-map conversion list"
printf '\n==================================================\n'
printf '[STEP 1/1] Generate conversion candidates\n'
printf '==================================================\n\n'
python3 "$CONVERTER" \
    --map-dir "$MAP_DIR" \
    --registry "$REGISTRY" \
    --generate-list \
    --list-file "$LIST_FILE"

printf '\n==================================================\n'
printf '[SUCCESS] %s\n' "$SCRIPT_NAME"
printf '==================================================\n'
