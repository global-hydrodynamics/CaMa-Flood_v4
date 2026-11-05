#!/bin/sh
# Generate the three Tutorial 02 figures from completed experiment outputs.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
PYTHON=${PYTHON:-python3}

"$PYTHON" "$SCRIPT_DIR/v01-spinup_comparison.py"
"$PYTHON" "$SCRIPT_DIR/v02-discharge_comparison.py"
"$PYTHON" "$SCRIPT_DIR/v03-annual_max_discharge.py"
