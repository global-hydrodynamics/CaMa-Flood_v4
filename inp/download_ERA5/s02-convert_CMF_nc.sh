#!/bin/bash
#==========================================================
# Convert downloaded ERA5-Land NetCDF files to CaMa-Flood
# runoff forcing NetCDF files.
#
# This wrapper calls convert_CMF_nc.py for each output period.
#
# Downloaded input file unit:
#   daily   -> ERA5-Land_runoff_YYYYMMDD.nc
#   monthly -> ERA5-Land_runoff_YYYYMM.nc
#
# Output grouping:
#   yearly  -> runoff_YYYY.nc
#   monthly -> runoff_YYYYMM.nc
#
#==========================================================

set -u

#==========================================================
# User settings
#==========================================================

# (0) Python command.
# Use "python3" on systems where "python" is not available.
# If a conda environment is activated and "python" works, this can be "python".
PYTHON_CMD="python3"
# Python conversion script
PYTHON_SCRIPT="./convert_CMF_nc.py"


# (1) Downloaded netCDF File
# Input directory containing downloaded ERA5-Land NetCDF files
#INDIR="./ori_global"
INDIR="./ori_japan"
# Downloaded input file unit:
#   daily   -> read ERA5-Land_runoff_YYYYMMDD.nc for each day
#   monthly -> read ERA5-Land_runoff_YYYYMM.nc for each month
INPUT_FILE_UNIT="monthly"
# Input/output forcing timestep in hours. (Downloaded runoff timestep = Converted runoff timestep)
#   24 -> daily, 3 -> 3-hourly, 1 -> hourly
TIMESTEP_HOURS=24

# Input filename pattern.
# Use Python strftime format.
if [ "${INPUT_FILE_UNIT}" = "daily" ]; then
    INPUT_PATTERN="ERA5-Land_runoff_%Y%m%d.nc"
elif [ "${INPUT_FILE_UNIT}" = "monthly" ]; then
    INPUT_PATTERN="ERA5-Land_runoff_%Y%m.nc"
else
    echo "ERROR: INPUT_FILE_UNIT must be daily or monthly."
    exit 1
fi


# (2) Converted File Configulations
# Output directory for CaMa-Flood-ready runoff files
#OUTDIR="./runoff_month_nc"
#OUTDIR="./runoff_year_nc"
OUTDIR="./runoff_japan_nc"
# Output grouping:
#   yearly  -> runoff_YYYY.nc
#   monthly -> runoff_YYYYMM.nc
MERGE_UNIT="monthly"
#MERGE_UNIT="yearly"


# (3) Conversion period, inclusive.
START_DATE="2000-01-01"
END_DATE="2000-01-31"


# (4) Basic Settings
# Variable names
INPUT_VARIABLE="runoff"
OUTPUT_VARIABLE="runoff"
# Missing value / fill value for output NetCDF
FILL_VALUE="1.0e20"
# CaMa-Flood mapping convention
LAT_ORDER="NtoS"

# If 1, skip existing output files.
# If 0, pass --overwrite to the Python converter.
SKIP_EXISTING=0

# Number of simultaneous conversion jobs.
# Usually 1 is safest because NetCDF merging can use memory.
# For monthly conversion on small domains, 2 or more may be okay.
NCPUS=1

# Extra options passed to Python converter, if needed.
EXTRA_OPTS=""

# Metadata text file for CaMa-Flood settings.
# This file is created after successful conversion.
INFO_SCRIPT="./make_CMF_runoff_info.py"
INFO_FILE="${OUTDIR}/runoff_info_CaMaFlood.txt"
INFO_SAMPLE_NAME="ERA5-Land runoff forcing"

#==========================================================
# Internal settings
#==========================================================

LOGDIR="./log_convert"
mkdir -p "${OUTDIR}" "${LOGDIR}"

if ! command -v "${PYTHON_CMD}" >/dev/null 2>&1; then
    echo "ERROR: Python command not found: ${PYTHON_CMD}"
    echo "       Edit PYTHON_CMD in this script, or activate a Python environment."
    exit 1
fi

if [ ! -f "${PYTHON_SCRIPT}" ]; then
    echo "ERROR: Python converter not found: ${PYTHON_SCRIPT}"
    exit 1
fi

if [ ! -f "${INFO_SCRIPT}" ]; then
    echo "ERROR: runoff information script not found: ${INFO_SCRIPT}"
    exit 1
fi

if [ ! -d "${INDIR}" ]; then
    echo "ERROR: input directory not found: ${INDIR}"
    exit 1
fi

case "${INPUT_FILE_UNIT}" in
    daily|monthly)
        ;;
    *)
        echo "ERROR: INPUT_FILE_UNIT must be daily or monthly."
        exit 1
        ;;
esac

if [ "${NCPUS}" -lt 1 ]; then
    echo "ERROR: NCPUS must be at least 1."
    exit 1
fi

case "${MERGE_UNIT}" in
    yearly|monthly)
        ;;
    *)
        echo "ERROR: MERGE_UNIT must be yearly or monthly."
        exit 1
        ;;
esac

#==========================================================
# Utility functions
#==========================================================

count_running_jobs() {
    jobs -rp | wc -l | awk '{print $1}'
}

wait_for_free_slot() {
    while [ "$(count_running_jobs)" -ge "${NCPUS}" ]; do
        sleep 5
    done
}

check_failed_jobs() {
    failed=0
    nstatus=0

    for status_file in "${LOGDIR}"/status_*.txt; do
        [ -e "${status_file}" ] || continue
        nstatus=$((nstatus + 1))
        status=$(cat "${status_file}")

        if [ "${status}" != "0" ]; then
            echo "ERROR: failed conversion detected: ${status_file}, exit code ${status}"
            failed=1
        fi
    done

    if [ "${nstatus}" -eq 0 ]; then
        echo "ERROR: no status files were created."
        return 1
    fi

    if [ "${failed}" -ne 0 ]; then
        return 1
    fi

    return 0
}

run_one_period() {
    pstart="$1"
    pend="$2"
    outfile="$3"
    tag="$4"

    logfile="${LOGDIR}/convert_${tag}.log"
    statusfile="${LOGDIR}/status_${tag}.txt"

    if [ "${SKIP_EXISTING}" -eq 1 ] && [ -e "${outfile}" ]; then
        echo "Skip existing: ${outfile}"
        echo "0" > "${statusfile}"
        return 0
    fi

    overwrite_opt=""
    if [ "${SKIP_EXISTING}" -eq 0 ]; then
        overwrite_opt="--overwrite"
    fi

    echo "Convert: ${pstart} to ${pend} -> ${outfile}"

    (
        "${PYTHON_CMD}" "${PYTHON_SCRIPT}" \
            --start "${pstart}" \
            --end "${pend}" \
            --indir "${INDIR}" \
            --out "${outfile}" \
            --input-file-unit "${INPUT_FILE_UNIT}" \
            --input-pattern "${INPUT_PATTERN}" \
            --input-variable "${INPUT_VARIABLE}" \
            --output-variable "${OUTPUT_VARIABLE}" \
            --timestep-hours "${TIMESTEP_HOURS}" \
            --fill-value "${FILL_VALUE}" \
            --lat-order "${LAT_ORDER}" \
            ${overwrite_opt} \
            ${EXTRA_OPTS} > "${logfile}" 2>&1

        echo "$?" > "${statusfile}"
    ) &
}

#==========================================================
# Main
#==========================================================

rm -f "${LOGDIR}"/status_*.txt

echo "=========================================================="
echo "ERA5-Land runoff conversion wrapper"
echo "=========================================================="
echo "Python command  : ${PYTHON_CMD}"
echo "Python script   : ${PYTHON_SCRIPT}"
echo "Input dir       : ${INDIR}"
echo "Input file unit : ${INPUT_FILE_UNIT}"
echo "Input pattern   : ${INPUT_PATTERN}"
echo "Output dir      : ${OUTDIR}"
echo "Info file       : ${INFO_FILE}"
echo "Period          : ${START_DATE} to ${END_DATE}"
echo "Merge unit      : ${MERGE_UNIT}"
echo "Timestep hours  : ${TIMESTEP_HOURS}"
echo "Input variable  : ${INPUT_VARIABLE}"
echo "Output variable : ${OUTPUT_VARIABLE}"
echo "Fill value      : ${FILL_VALUE}"
echo "Lat order       : ${LAT_ORDER}"
echo "Parallel jobs   : ${NCPUS}"
echo "Skip existing   : ${SKIP_EXISTING}"
echo "=========================================================="

# Generate output periods using Python.
# Output columns:
#   period_start period_end output_filename tag
PERIODS=$("${PYTHON_CMD}" - <<EOF
from datetime import date, timedelta
import calendar
import sys

try:
    start = date.fromisoformat("${START_DATE}")
    end = date.fromisoformat("${END_DATE}")
except ValueError as exc:
    print(f"ERROR: invalid date: {exc}", file=sys.stderr)
    raise SystemExit(1)

merge_unit = "${MERGE_UNIT}"

if end < start:
    print("ERROR: END_DATE is earlier than START_DATE", file=sys.stderr)
    raise SystemExit(1)

def print_period(s, e):
    if merge_unit == "yearly":
        fname = f"runoff_{s.year:04d}.nc"
        tag = f"{s.year:04d}"
    elif merge_unit == "monthly":
        fname = f"runoff_{s.year:04d}{s.month:02d}.nc"
        tag = f"{s.year:04d}{s.month:02d}"
    else:
        raise SystemExit(f"ERROR: unsupported merge unit: {merge_unit}")

    print(f"{s.isoformat()} {e.isoformat()} {fname} {tag}")

cur = start
while cur <= end:
    if merge_unit == "yearly":
        period_end = date(cur.year, 12, 31)
    elif merge_unit == "monthly":
        last_day = calendar.monthrange(cur.year, cur.month)[1]
        period_end = date(cur.year, cur.month, last_day)
    else:
        raise SystemExit(f"ERROR: unsupported merge unit: {merge_unit}")

    if period_end > end:
        period_end = end

    print_period(cur, period_end)
    cur = period_end + timedelta(days=1)
EOF
)

if [ -z "${PERIODS}" ]; then
    echo "ERROR: no periods generated."
    exit 1
fi

while read -r PSTART PEND FNAME TAG; do
    [ -n "${PSTART}" ] || continue
    wait_for_free_slot
    run_one_period "${PSTART}" "${PEND}" "${OUTDIR}/${FNAME}" "${TAG}"
done <<< "${PERIODS}"

wait

if check_failed_jobs; then
    echo "=========================================================="
    echo "All conversion jobs finished successfully."
    echo "Creating CaMa-Flood runoff information file..."
    echo "=========================================================="

    overwrite_info_opt=""
    if [ "${SKIP_EXISTING}" -eq 0 ]; then
        overwrite_info_opt="--overwrite"
    else
        # Always refresh the metadata file after successful conversion.
        overwrite_info_opt="--overwrite"
    fi

    "${PYTHON_CMD}" "${INFO_SCRIPT}" \
        --outdir "${OUTDIR}" \
        --outfile "${INFO_FILE}" \
        --pattern "runoff_*.nc" \
        --variable "${OUTPUT_VARIABLE}" \
        --timestep-hours "${TIMESTEP_HOURS}" \
        --merge-unit "${MERGE_UNIT}" \
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
    echo "Runoff information file: ${INFO_FILE}"
    echo "=========================================================="
    exit 0
else
    echo "=========================================================="
    echo "Some conversion jobs failed. Check logs in: ${LOGDIR}"
    echo "=========================================================="
    exit 1
fi
