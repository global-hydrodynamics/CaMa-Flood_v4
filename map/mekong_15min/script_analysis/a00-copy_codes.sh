#!/bin/sh
# Copy compiled analysis executables from the shared code directory beside this toolkit.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# ----------------------------------------------------------------------
# Locate shared compiled code relative to this script's location.
# ----------------------------------------------------------------------

printf '\n==================================================\n'
printf '[STEP 1/3] Locate shared map-tool code\n'
printf '==================================================\n\n'

if [ -d "$SCRIPT_DIR/../code" ]; then
    CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../code" && pwd -P)
elif [ -d "$SCRIPT_DIR/../../tools/code" ]; then
    CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)
else
    printf '\n[ERROR] Cannot find the shared map/tools/code directory.\n' >&2
    exit 1
fi

printf 'Shared code directory: %s\n' "$CODE_DIR"

# ----------------------------------------------------------------------
# Copy every compiled analysis executable into this directory.
# ----------------------------------------------------------------------

printf '\n==================================================\n'
printf '[STEP 2/3] Copy compiled analysis executables\n'
printf '==================================================\n\n'

COPIED=0
set --
for SRC in "$CODE_DIR"/analysis_*
do
    [ -f "$SRC" ] && [ -x "$SRC" ] || continue

    cp -p "$SRC" "$SCRIPT_DIR/"
    set -- "$@" "$SCRIPT_DIR/${SRC##*/}"
    COPIED=$((COPIED + 1))
done

if [ "$COPIED" -eq 0 ]; then
    printf '\n[ERROR] No compiled analysis executables found in %s.\n' "$CODE_DIR" >&2
    printf 'Build them with: make -C map/tools/code analysis\n' >&2
    exit 1
fi

# List only the files copied by this invocation, not stale destination files.
printf '\n==================================================\n'
printf '[STEP 3/3] List copied executables and file sizes\n'
printf '==================================================\n\n'
ls -lh "$@"

printf '\n==================================================\n'
printf '[SUCCESS] Copied %s analysis executables into %s\n' "$COPIED" "$SCRIPT_DIR"
printf '==================================================\n'
