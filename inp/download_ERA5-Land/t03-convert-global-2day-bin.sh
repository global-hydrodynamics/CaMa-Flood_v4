#!/usr/bin/env bash
# Convert the tracked-format-check input (local, ignored CDS sample) into
# two daily plain-binary grids and a matching GrADS/CaMa-Flood CTL.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${SCRIPT_DIR}"

# User settings
PYTHON_CMD="python3"
INPUT_FILE="ERA5-Land_runoff_20000101-02.nc"
INPUT_DIR="./ori_global"
START_DATE="2000-01-01"
END_DATE="2000-01-02"
INPUT_TIMESTEP_HOURS=24
OUTPUT_TIMESTEP_HOURS=24
INPUT_VARIABLE="ro"
OUTDIR="./runoff_global_sample_bin"
CTL_FILE="${OUTDIR}/runoff.ctl"
UNDEF="1.0e20"
ENDIAN="little"
OVERWRITE=0

PYTHON_SCRIPT="./convert_CMF_bin.py"

if [[ ! -s "${INPUT_DIR}/${INPUT_FILE}" ]]; then
  echo "[ERROR] Required two-day NetCDF sample not found: ${INPUT_DIR}/${INPUT_FILE}" >&2
  exit 1
fi
command -v "${PYTHON_CMD}" >/dev/null || {
  echo "[ERROR] Python command not found: ${PYTHON_CMD}" >&2
  exit 1
}
[[ -f "${PYTHON_SCRIPT}" ]] || {
  echo "[ERROR] Binary converter not found: ${PYTHON_SCRIPT}" >&2
  exit 1
}

mkdir -p "${OUTDIR}"
shopt -s nullglob
existing=()
for path in "${OUTDIR}"/runoff_*.bin "${CTL_FILE}"; do
  [[ -e "${path}" ]] && existing+=("${path}")
done
if (( ${#existing[@]} > 0 && OVERWRITE == 0 )); then
  echo "[ERROR] Sample output already exists under ${OUTDIR}: ${existing[*]}. Set OVERWRITE=1 to replace it." >&2
  exit 1
fi

# The input is one file containing both timestamps. The converter's monthly
# file-selection mode visits January once; the literal pattern selects this file.
echo "[STEP] Convert the two-day ERA5-Land sample and write its CTL"
"${PYTHON_CMD}" "${PYTHON_SCRIPT}" \
  --start "${START_DATE}" \
  --end "${END_DATE}" \
  --indir "${INPUT_DIR}" \
  --outdir "${OUTDIR}" \
  --input-file-unit monthly \
  --input-pattern "${INPUT_FILE}" \
  --input-variable "${INPUT_VARIABLE}" \
  --timestep-hours "${INPUT_TIMESTEP_HOURS}" \
  --output-timestep-hours "${OUTPUT_TIMESTEP_HOURS}" \
  --binary-prefix "runoff_" \
  --binary-suffix ".bin" \
  --ctl-file "${CTL_FILE}" \
  --undef "${UNDEF}" \
  --grads-variable "runoff" \
  --grads-title "ERA5-Land global runoff two-day sample" \
  --endian "${ENDIAN}" \
  --skip-existing 0

echo "[SUCCESS] Wrote daily binary sample and matching CTL under ${OUTDIR}"
echo "[INFO] Review KG-0019 before interpreting the first record's physical time attribution."
