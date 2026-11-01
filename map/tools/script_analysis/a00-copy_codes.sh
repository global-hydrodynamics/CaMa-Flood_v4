#!/bin/sh
# Copy the four shared, prebuilt river-analysis executables into this toolkit.
# Run only in the canonical map/tools/script_analysis/ template directory.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
CODE_DIR="$SCRIPT_DIR/../code"
PROGRAMS="analysis_river_point_info analysis_upbasin analysis_subbasin analysis_river_node_latlon"

if [ ! -d "$CODE_DIR" ]; then
    printf '[ERROR] a00-copy_codes.sh must run in the canonical map/tools/script_analysis/ template.\n' >&2
    printf 'A copied map-local script_analysis/ may not have access to map/tools/code/.\n' >&2
    exit 1
fi
CODE_DIR=$(CDPATH= cd "$CODE_DIR" && pwd -P)

printf '[STEP] Check shared analysis executables in %s\n' "$CODE_DIR"
for PROGRAM in $PROGRAMS; do
    if [ ! -x "$CODE_DIR/$PROGRAM" ]; then
        printf '[ERROR] Missing executable: %s\n' "$CODE_DIR/$PROGRAM" >&2
        printf 'Build the analysis programs first with: make -C map/tools/code analysis\n' >&2
        exit 1
    fi
done

printf '[STEP] Copy analysis executables into %s\n' "$SCRIPT_DIR"
for PROGRAM in $PROGRAMS; do
    cp -p "$CODE_DIR/$PROGRAM" "$SCRIPT_DIR/$PROGRAM"
done
printf '[SUCCESS] Prepared all four local analysis executables.\n'
