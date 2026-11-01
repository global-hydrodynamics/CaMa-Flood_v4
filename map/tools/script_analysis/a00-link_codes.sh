#!/bin/sh
# Link this toolkit to the shared map-analysis executables.
set -eu

# Resolve paths from this script, so the command works from any current directory.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
LINK_PATH="$SCRIPT_DIR/code"

printf '\n[STEP 1/1] Configure the shared analysis-code link\n'

# ----------------------------------------------------------------------
# Locate the shared compiled code directory.
# ----------------------------------------------------------------------

if [ -d "$SCRIPT_DIR/../code" ]; then
    CODE_LINK="../code"
elif [ -d "$SCRIPT_DIR/../../tools/code" ]; then
    CODE_LINK="../../tools/code"
else
    printf '\n[ERROR] Cannot find the shared map/tools/code directory.\n' >&2
    exit 1
fi

# Keep an existing correct link; replace only a symlink when its target differs.
if [ -L "$LINK_PATH" ]; then
    EXISTING_LINK=$(readlink "$LINK_PATH")
    if [ "$EXISTING_LINK" = "$CODE_LINK" ]; then
        printf '\n[SUCCESS] Shared code link is already correct:\n'
        ls -ld "$LINK_PATH"
        exit 0
    fi
    printf '\n[WARNING] Replacing code symlink target %s with %s.\n' \
        "$EXISTING_LINK" "$CODE_LINK"
    rm "$LINK_PATH"
elif [ -e "$LINK_PATH" ]; then
    printf '\n[ERROR] Refusing to replace existing file or directory: %s\n' "$LINK_PATH" >&2
    exit 1
fi

# Create a relative link so the repository remains portable across workstations.
ln -s "$CODE_LINK" "$LINK_PATH"

printf '\n[SUCCESS] Linked shared map-analysis executables:\n'
ls -ld "$LINK_PATH"
