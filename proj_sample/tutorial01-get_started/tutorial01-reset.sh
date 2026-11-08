#!/bin/sh
# Restore the Tutorial01 build/output state for documentation testing.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
REPO_ROOT=$(CDPATH= cd "$SCRIPT_DIR/../.." && pwd -P)
EXPECTED_SCRIPT_DIR="$REPO_ROOT/proj_sample/tutorial01-get_started"
SOURCE_DIR="$REPO_ROOT/src_main"
OUTPUT_DIR="$SCRIPT_DIR/out"
CONFIG_FILE="$REPO_ROOT/config/Mkinclude"

if [ "$SCRIPT_DIR" != "$EXPECTED_SCRIPT_DIR" ] || [ ! -f "$SOURCE_DIR/Makefile" ]; then
    printf '[ERROR] Tutorial01 reset paths do not match the expected repository layout.\n' >&2
    exit 1
fi

if [ ! -f "$CONFIG_FILE" ]; then
    printf '[ERROR] Prepare config/Mkinclude before using this developer reset script.\n' >&2
    printf 'The reset script never creates, removes, or edits that configuration.\n' >&2
    exit 1
fi

if [ "$#" -gt 1 ] || { [ "$#" -eq 1 ] && [ "$1" != "--yes" ]; }; then
    printf 'Usage: %s [--yes]\n' "${0##*/}" >&2
    exit 2
fi

if [ "${1:-}" != "--yes" ]; then
    if [ ! -t 0 ] || [ ! -t 1 ]; then
        printf '[ERROR] Confirmation needs a terminal; rerun with --yes in a disposable test copy.\n' >&2
        exit 1
    fi
    printf 'Clean src_main build products and remove only this tutorial output? [y/N]: '
    IFS= read -r ANSWER || ANSWER=""
    case "$ANSWER" in
        y|Y|yes|YES|Yes) ;;
        *) printf 'Reset cancelled; files were not changed.\n'; exit 1 ;;
    esac
fi

CONFIG_BEFORE=$(cksum < "$CONFIG_FILE")

# The reviewed clean target removes generated products under src_main/ only.
make -C "$SOURCE_DIR" EXT_LIBS= clean

# Remove only this tutorial's generated files, never the whole project tree.
if [ -d "$OUTPUT_DIR" ]; then
    rm -rf "$OUTPUT_DIR"
    printf 'Removed Tutorial01 outputs: %s\n' "$OUTPUT_DIR"
else
    printf 'Tutorial01 output directory was already absent: %s\n' "$OUTPUT_DIR"
fi

CONFIG_AFTER=$(cksum < "$CONFIG_FILE")
if [ "$CONFIG_BEFORE" != "$CONFIG_AFTER" ]; then
    printf '[ERROR] config/Mkinclude changed during reset; inspect the local configuration.\n' >&2
    exit 1
fi

if [ -e "$SOURCE_DIR/MAIN_cmf" ] || [ -d "$OUTPUT_DIR" ]; then
    printf '[ERROR] The executable or Tutorial01 output directory remains after reset.\n' >&2
    exit 1
fi

printf '\nReset complete.\n'
printf '  MAIN_cmf: absent\n'
printf '  Tutorial01 out/: absent\n'
printf '  config/Mkinclude: preserved unchanged\n'
