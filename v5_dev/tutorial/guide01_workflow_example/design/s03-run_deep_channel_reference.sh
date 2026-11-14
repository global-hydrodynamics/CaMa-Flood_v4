#!/bin/sh
# Developer reference wrapper for the completed Deep Channel experiment.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
exec "$SCRIPT_DIR/run-experiment-reference.sh" deep_channel "$@"
