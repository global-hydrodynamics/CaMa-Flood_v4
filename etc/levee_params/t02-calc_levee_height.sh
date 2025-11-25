#!/bin/sh

# calculate for CYCLE: sim00, sim01, sim02
CYCLE=$1

## CaMa-Flood: simulation map directory & simulation output dorectory
START_YEAR=1980
END_YEAR=2010
MAP_DIR="./map"              ## CaMa map dir
CMF_DIR="./${CYCLE}_out"     ## CaMa output dir
CMF_FMT="bin"                ## CaMa output file format (bin or nc)
WORK_DIR="./${CYCLE}_calc"   ## Data analysis dir

python src/calculate_levhgt.py \
  --start-year   "${START_YEAR}" \
  --end-year     "${END_YEAR}" \
  --cfm-output   "${CMF_DIR}" \
  --input-format "${CMF_FMT}" \
  --map-dir      "${MAP_DIR}" \
  --var-name       rivdph     \
  --levfrc-map     levfrc.bin \
  --rivhgt-map     rivhgt.bin \
  --target-rp-map-file "${MAP_DIR}/protect.tif" \
  --work-dir "${WORK_DIR}"

