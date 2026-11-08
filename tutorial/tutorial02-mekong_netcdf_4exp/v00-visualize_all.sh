#!/bin/sh
# Generate Tutorial 02 map figures and comparisons from map data and completed runs.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
PYTHON=${PYTHON:-python3}

"$PYTHON" "$SCRIPT_DIR/v01-spinup_comparison.py"
"$PYTHON" "$SCRIPT_DIR/v02-discharge_comparison.py"
"$PYTHON" "$SCRIPT_DIR/v03-annual_max_discharge.py"
"$PYTHON" "$SCRIPT_DIR/v04-network_maps.py"
