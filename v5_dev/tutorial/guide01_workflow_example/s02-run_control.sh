#!/bin/sh
# Run the provided Control configuration.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
exec "$SCRIPT_DIR/run-experiment.sh" "$@"
