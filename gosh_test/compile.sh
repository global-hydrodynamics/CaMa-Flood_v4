#!/bin/sh
# Build the v5 stand-alone model without touching legacy build directories.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
REPO_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

make -C "$REPO_DIR/src_main" all
