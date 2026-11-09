#!/bin/sh
# Run the Deep Channel case; all paths are resolved from this script location.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
exec "$SCRIPT_DIR/run-experiment.sh" deep_channel "$@"
