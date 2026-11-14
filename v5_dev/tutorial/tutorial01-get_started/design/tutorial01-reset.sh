#!/bin/sh
# Restore the Tutorial01 build state and one selected project's output directory.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
TUTORIAL_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
REPO_ROOT=$(CDPATH= cd "$TUTORIAL_DIR/../.." && pwd -P)
EXPECTED_SCRIPT_DIR="$REPO_ROOT/tutorial/tutorial01-get_started/design"
SOURCE_DIR="$REPO_ROOT/src_main"
OUTPUT_ROOT="$TUTORIAL_DIR/out"
CONFIG_FILE="$REPO_ROOT/config/Mkinclude"

PROJECT_NAME="tutorial01-get_started"
ASSUME_YES=no

while [ "$#" -gt 0 ]; do
    case "$1" in
        --yes)
            ASSUME_YES=yes
            shift
            ;;
        --project)
            if [ "$#" -lt 2 ]; then
                printf 'Usage: %s [--project NAME] [--yes]\n' "${0##*/}" >&2
                exit 2
            fi
            PROJECT_NAME=$2
            shift 2
            ;;
        *)
            printf 'Usage: %s [--project NAME] [--yes]\n' "${0##*/}" >&2
            exit 2
            ;;
    esac
done

case "$PROJECT_NAME" in
    ""|.|..|*[!A-Za-z0-9._-]*)
        printf '[ERROR] Project name must contain only letters, numbers, dots, underscores, and hyphens.\n' >&2
        exit 2
        ;;
esac

OUTPUT_DIR="$OUTPUT_ROOT/$PROJECT_NAME"

if [ "$SCRIPT_DIR" != "$EXPECTED_SCRIPT_DIR" ] || [ ! -f "$SOURCE_DIR/Makefile" ]; then
    printf '[ERROR] Tutorial01 reset paths do not match the expected repository layout.\n' >&2
    exit 1
fi

if [ ! -f "$CONFIG_FILE" ]; then
    printf '[ERROR] Prepare config/Mkinclude before using this developer reset script.\n' >&2
    printf 'The reset script never creates, removes, or edits that configuration.\n' >&2
    exit 1
fi

if [ -L "$OUTPUT_ROOT" ] || [ -L "$OUTPUT_DIR" ]; then
    printf '[ERROR] Refusing to clean through a symbolic-link output path.\n' >&2
    exit 1
fi
if [ -e "$OUTPUT_ROOT" ] && [ ! -d "$OUTPUT_ROOT" ]; then
    printf '[ERROR] The Tutorial01 out path exists but is not a directory.\n' >&2
    exit 1
fi

if [ "$ASSUME_YES" != yes ]; then
    if [ ! -t 0 ] || [ ! -t 1 ]; then
        printf '[ERROR] Confirmation needs a terminal; rerun with --yes in a disposable test copy.\n' >&2
        exit 1
    fi
    printf 'Clean src_main build products and remove out/%s only? [y/N]: ' "$PROJECT_NAME"
    IFS= read -r ANSWER || ANSWER=""
    case "$ANSWER" in
        y|Y|yes|YES|Yes) ;;
        *) printf 'Reset cancelled; files were not changed.\n'; exit 1 ;;
    esac
fi

CONFIG_BEFORE=$(cksum < "$CONFIG_FILE")

# The clean target is confined to generated build products under src_main/.
make -C "$SOURCE_DIR" EXT_LIBS= clean

# Delete only the validated project directory; preserve the shared out/ parent.
if [ -d "$OUTPUT_DIR" ]; then
    rm -rf "$OUTPUT_DIR"
    printf 'Removed Tutorial01 project outputs: %s\n' "$OUTPUT_DIR"
else
    printf 'Tutorial01 project output directory was already absent: %s\n' "$OUTPUT_DIR"
fi

CONFIG_AFTER=$(cksum < "$CONFIG_FILE")
if [ "$CONFIG_BEFORE" != "$CONFIG_AFTER" ]; then
    printf '[ERROR] config/Mkinclude changed during reset; inspect the local configuration.\n' >&2
    exit 1
fi

if [ -e "$SOURCE_DIR/MAIN_cmf" ] || [ -d "$OUTPUT_DIR" ]; then
    printf '[ERROR] The executable or selected project output directory remains after reset.\n' >&2
    exit 1
fi

printf '\nReset complete.\n'
printf '  MAIN_cmf: absent\n'
printf '  out/%s/: absent\n' "$PROJECT_NAME"
printf '  other out/ projects: preserved\n'
printf '  config/Mkinclude: preserved unchanged\n'
