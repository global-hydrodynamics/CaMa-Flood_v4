#!/bin/sh

set -eu

# Global 15-minute heatlink example for the year 2000.
# Set ATM_DIR to the directory containing the GSWP3 *.2000.nc files.

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RUN_DIR=${RUN_DIR:-${ROOT}/out/test1-heatlink}
MAP_DIR=$(CDPATH= cd -- "${MAP_DIR:-${ROOT}/map/glb_15min}" && pwd)
RUNOFF_DIR=$(CDPATH= cd -- "${RUNOFF_DIR:-${ROOT}/inp/test_1deg/runoff}" && pwd)
ATM_DIR=${ATM_DIR:?Set ATM_DIR to the GSWP3 atmospheric-forcing directory}
ATM_DIR=$(CDPATH= cd -- "$ATM_DIR" && pwd)

EXE=${ROOT}/src/MAIN_cmf
NML_TEMPLATE=${ROOT}/gosh/etc/heat-link.nml
NML=${RUN_DIR}/input_cmf.nam

if [ ! -x "$EXE" ]; then
    echo "Executable not found: ${EXE}" >&2
    echo "Enable heatlink in adm/Mkinclude and run make all in src/." >&2
    exit 1
fi
if [ -e "$NML" ]; then
    echo "Run directory has already been initialized: ${RUN_DIR}" >&2
    echo "Remove it or set RUN_DIR to a new directory." >&2
    exit 1
fi

mkdir -p "${RUN_DIR}/input"
ln -s "$MAP_DIR" "${RUN_DIR}/input/map"
ln -s "$RUNOFF_DIR" "${RUN_DIR}/input/runoff"
ln -s "$ATM_DIR" "${RUN_DIR}/input/atm"
ln -s "$EXE" "${RUN_DIR}/MAIN_cmf"
cat "$NML_TEMPLATE" > "$NML"

echo "Running heatlink example in ${RUN_DIR}"
(
    cd "$RUN_DIR"
    time ./MAIN_cmf
)
