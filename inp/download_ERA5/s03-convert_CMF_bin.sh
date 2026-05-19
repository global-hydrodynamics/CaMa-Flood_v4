#!/bin/bash
#==========================================================
# Convert downloaded ERA5-Land NetCDF runoff files to
# CaMa-Flood/GrADS-style plain binary runoff files.
#
# Output:
#   daily:
#       runoff_YYYYMMDD.bin
#       runoff.ctl with tdef 1dy
#
#   sub-daily:
#       runoff_YYYYMMDDHH.bin
#       runoff.ctl with tdef 1hr, 3hr, 6hr, 12hr, etc.
#
# Downloaded input file unit:
#   daily   -> read ERA5-Land_runoff_YYYYMMDD.nc
#   monthly -> read ERA5-Land_runoff_YYYYMM.nc
#==========================================================

set -u

#==========================================================
# User settings
#==========================================================

# (0) Python command and script
PYTHON_CMD="python3"
PYTHON_SCRIPT="./convert_CMF_bin.py"

# (1) Downloaded NetCDF files
#INDIR="./ori_global"
INDIR="./ori_jpn"
INPUT_FILE_UNIT="monthly"

# Downloaded data timestep in hours.
# If this is 24, output is daily runoff_YYYYMMDD.bin.
# If this is 1, 3, 6, or 12, output is sub-daily runoff_YYYYMMDDHH.bin.
TIMESTEP_HOURS=24 

if [ "${INPUT_FILE_UNIT}" = "daily" ]; then
    INPUT_PATTERN="ERA5-Land_runoff_%Y%m%d.nc"
elif [ "${INPUT_FILE_UNIT}" = "monthly" ]; then
    INPUT_PATTERN="ERA5-Land_runoff_%Y%m.nc"
else
    echo "ERROR: INPUT_FILE_UNIT must be daily or monthly."
    exit 1
fi

# (2) Output file configuration
#OUTDIR="./runoff_bin"
OUTDIR="./runoff_japan_bin"

BINARY_PREFIX="runoff_"
BINARY_SUFFIX=".bin"
CTL_FILE="${OUTDIR}/runoff.ctl"

ENDIAN="little"

# (3) Conversion period, inclusive.
START_DATE="2000-01-01"
END_DATE="2000-01-31"

# (4) Basic settings
INPUT_VARIABLE="runoff"
UNDEF="1.e20"
GRADS_VARIABLE="var"
GRADS_TITLE="ERA5Land Runoff"

SKIP_EXISTING=0

EXTRA_OPTS=""

# Metadata text file for CaMa-Flood settings.
INFO_SCRIPT="./make_CMF_runoff_info.py"
INFO_FILE="${OUTDIR}/runoff_info_CaMaFlood.txt"
INFO_SAMPLE_NAME="ERA5-Land binary runoff forcing"

#==========================================================
# Internal settings
#==========================================================

LOGDIR="./log_convert_bin"
mkdir -p "${OUTDIR}" "${LOGDIR}"

if ! command -v "${PYTHON_CMD}" >/dev/null 2>&1; then
    echo "ERROR: Python command not found: ${PYTHON_CMD}"
    exit 1
fi

if [ ! -f "${PYTHON_SCRIPT}" ]; then
    echo "ERROR: Python converter not found: ${PYTHON_SCRIPT}"
    exit 1
fi

if [ ! -d "${INDIR}" ]; then
    echo "ERROR: input directory not found: ${INDIR}"
    exit 1
fi

if [ ! -f "${INFO_SCRIPT}" ]; then
    echo "ERROR: runoff information script not found: ${INFO_SCRIPT}"
    exit 1
fi

case "${INPUT_FILE_UNIT}" in
    daily|monthly) ;;
    *)
        echo "ERROR: INPUT_FILE_UNIT must be daily or monthly."
        exit 1
        ;;
esac

case "${ENDIAN}" in
    little|big) ;;
    *)
        echo "ERROR: ENDIAN must be little or big."
        exit 1
        ;;
esac

if [ "${TIMESTEP_HOURS}" -lt 1 ] || [ "${TIMESTEP_HOURS}" -gt 24 ]; then
    echo "ERROR: TIMESTEP_HOURS must be between 1 and 24."
    exit 1
fi

if [ $((24 % TIMESTEP_HOURS)) -ne 0 ]; then
    echo "ERROR: TIMESTEP_HOURS must divide 24 exactly."
    exit 1
fi


echo "=========================================================="
echo "ERA5-Land runoff binary conversion"
echo "=========================================================="
echo "Python command        : ${PYTHON_CMD}"
echo "Python script         : ${PYTHON_SCRIPT}"
echo "Input dir             : ${INDIR}"
echo "Input file unit       : ${INPUT_FILE_UNIT}"
echo "Input pattern         : ${INPUT_PATTERN}"
echo "Output dir            : ${OUTDIR}"
echo "CTL file              : ${CTL_FILE}"
echo "Info file             : ${INFO_FILE}"
echo "Period                : ${START_DATE} to ${END_DATE}"
echo "Input timestep hours  : ${TIMESTEP_HOURS}"
echo "Output timestep hours : ${TIMESTEP_HOURS}"
echo "Input variable        : ${INPUT_VARIABLE}"
echo "Binary prefix         : ${BINARY_PREFIX}"
echo "Binary suffix         : ${BINARY_SUFFIX}"
echo "Endian                : ${ENDIAN}"
echo "Undef                 : ${UNDEF}"
echo "Skip existing         : ${SKIP_EXISTING}"
echo "=========================================================="

LOGFILE="${LOGDIR}/convert_binary.log"

"${PYTHON_CMD}" "${PYTHON_SCRIPT}" \
    --start "${START_DATE}" \
    --end "${END_DATE}" \
    --indir "${INDIR}" \
    --outdir "${OUTDIR}" \
    --input-file-unit "${INPUT_FILE_UNIT}" \
    --input-pattern "${INPUT_PATTERN}" \
    --input-variable "${INPUT_VARIABLE}" \
    --timestep-hours "${TIMESTEP_HOURS}" \
    --output-timestep-hours "${TIMESTEP_HOURS}" \
    --binary-prefix "${BINARY_PREFIX}" \
    --binary-suffix "${BINARY_SUFFIX}" \
    --ctl-file "${CTL_FILE}" \
    --undef "${UNDEF}" \
    --grads-variable "${GRADS_VARIABLE}" \
    --grads-title "${GRADS_TITLE}" \
    --endian "${ENDIAN}" \
    --skip-existing "${SKIP_EXISTING}" \
    ${EXTRA_OPTS} > "${LOGFILE}" 2>&1

status="$?"

if [ "${status}" = "0" ]; then
    echo "=========================================================="
    echo "All binary conversion jobs finished successfully."
    echo "Creating CaMa-Flood runoff information file..."
    echo "=========================================================="

    "${PYTHON_CMD}" "${INFO_SCRIPT}" \
        --source-format binary \
        --outdir "${OUTDIR}" \
        --outfile "${INFO_FILE}" \
        --ctl-file "${CTL_FILE}" \
        --binary-prefix "${BINARY_PREFIX}" \
        --binary-suffix "${BINARY_SUFFIX}" \
        --binary-variable "${GRADS_VARIABLE}" \
        --variable "${INPUT_VARIABLE}" \
        --timestep-hours "${TIMESTEP_HOURS}" \
        --merge-unit "monthly" \
        --start "${START_DATE}" \
        --end "${END_DATE}" \
        --sample-name "${INFO_SAMPLE_NAME}" \
        --overwrite

    info_status="$?"
    if [ "${info_status}" != "0" ]; then
        echo "ERROR: failed to create runoff information file."
        exit 1
    fi

    echo "=========================================================="
    echo "CTL file: ${CTL_FILE}"
    echo "Runoff information file: ${INFO_FILE}"
    echo "=========================================================="
    exit 0
else
    echo "=========================================================="
    echo "Binary conversion failed. Check log: ${LOGFILE}"
    echo "=========================================================="
    exit 1
fi
