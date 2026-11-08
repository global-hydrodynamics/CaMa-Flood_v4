#!/bin/sh
# Run the maintained mek01 sample with outputs isolated to this tutorial.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
REPO_ROOT=$(CDPATH= cd "$SCRIPT_DIR/../.." && pwd -P)
TUTORIAL_OUT="$SCRIPT_DIR/out"

if [ -d "$TUTORIAL_OUT" ]; then
    printf 'Tutorial output directory already exists; the sample will replace its known outputs: %s\n' \
        "$TUTORIAL_OUT"
else
    printf 'Tutorial output directory does not exist yet: %s\n' "$TUTORIAL_OUT"
fi

cd "$REPO_ROOT/gosh_sample"
RUNDIR="$TUTORIAL_OUT" OMP_NUM_THREADS=1 ./mek01-default_run.sh
