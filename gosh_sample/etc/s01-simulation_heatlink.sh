#!/usr/bin/env bash

set -euo pipefail

# Global 15-minute heatlink example for the year 2000.
# ATM_DIR contains the GSWP3 *.2000.nc forcing files.
# Only atmospheric paths are expanded; hydrology keeps its input/map and input/runoff links.
ROOT=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
RUN_DIR=${RUN_DIR:-${ROOT}/out/test1-heatlink}
MAP_DIR=$(CDPATH= cd -- "${MAP_DIR:-${ROOT}/map/glb_15min}" && pwd)
RUNOFF_DIR=$(CDPATH= cd -- "${RUNOFF_DIR:-${ROOT}/inp/test_1deg/runoff}" && pwd)
ATM_DIR=${ATM_DIR:?Set ATM_DIR to the GSWP3 atmospheric-forcing directory}
ATM_DIR=$(CDPATH= cd -- "$ATM_DIR" && pwd)
INPMAT_DIR_ATM=${INPMAT_DIR_ATM:-${MAP_DIR}/input_mappings/05deg_s-n_0e-360e/mean}
INPMAT_DIR_ATM=$(CDPATH= cd -- "$INPMAT_DIR_ATM" && pwd)

EXE=${ROOT}/src/MAIN_cmf
NML_COMMON=${NML_COMMON:-${ROOT}/gosh/etc/heat-link.nml}
NML_ATM=${NML_ATM:-${ROOT}/gosh/etc/atm_GSWP3.nml}

if [[ ! -x "$EXE" ]]; then
    echo "Executable not found: ${EXE}" >&2
    echo "Enable heatlink in adm/Mkinclude and run make all in src/." >&2
    exit 1
fi
for template in "$NML_COMMON" "$NML_ATM"; do
    if [[ ! -r "$template" ]]; then
        echo "Namelist template not readable: ${template}" >&2
        exit 1
    fi
done
if [[ -d "$RUN_DIR" ]] && [[ -n "$(find "$RUN_DIR" -mindepth 1 -maxdepth 1 -print -quit)" ]]; then
    echo "Run directory is not empty: ${RUN_DIR}" >&2
    echo "Use a new or empty RUN_DIR to avoid mixing experiment results." >&2
    exit 1
fi

# Escape Fortran single-quoted literals and sed replacement metacharacters.
# No shell evaluation of namelist contents is performed.
escape_nml_path() {
    local value=$1
    if [[ $value == *$'\n'* || $value == *$'\r'* ]]; then
        echo 'Namelist paths must not contain newlines' >&2
        return 1
    fi
    value=${value//\'/\'\'}
    value=${value//\\/\\\\}
    value=${value//&/\\&}
    value=${value//|/\\|}
    printf '%s' "$value"
}
atm_path=$(escape_nml_path "$ATM_DIR")
inpmat_atm_path=$(escape_nml_path "$INPMAT_DIR_ATM")

render_namelist() {
    sed \
        -e "s|@ATM_DIR@|${atm_path}|g" \
        -e "s|@INPMAT_DIR_ATM@|${inpmat_atm_path}|g" \
        "$1"
    printf '\n'
}

mkdir -p "$RUN_DIR"
RUN_DIR=$(CDPATH= cd -- "$RUN_DIR" && pwd)
mkdir -p "${RUN_DIR}/input"
ln -s "$MAP_DIR" "${RUN_DIR}/input/map"
ln -s "$RUNOFF_DIR" "${RUN_DIR}/input/runoff"
NML=${RUN_DIR}/input_cmf.nam
{
    render_namelist "$NML_COMMON"
    render_namelist "$NML_ATM"
} > "$NML"

echo "Run directory: ${RUN_DIR}"
echo "Atmospheric forcing: ${ATM_DIR}"
echo "Atmospheric mapping: ${INPMAT_DIR_ATM}"
(
    cd "$RUN_DIR"
    { time -p "$EXE"; } > run_stdout.log 2> run_stderr.log
)
echo "Completed successfully."
echo "CaMa log: ${RUN_DIR}/log_CaMa.txt"
echo "Execution logs: ${RUN_DIR}/run_stdout.log and run_stderr.log"
