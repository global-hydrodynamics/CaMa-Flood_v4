#/bin/bash
#==========================================================
# Download ERA5-Land runoff using a Python CDS downloader.
#
# - The target period is specified by start/end dates.
# - Time resolution can be selected from 1 to 24 hours.
# - Download area is specified as NORTH, SOUTH, WEST, EAST.
# - Output can be daily files or monthly files.
# - Multiple downloads are executed in parallel.
#
# Expected Python downloader interface:
#
# Daily mode:
#   python download_era5land.py \
#       --year YYYY \
#       --month MM \
#       --day DD \
#       --variable runoff \
#       --time-step TSTEP \
#       --area-nswe NORTH SOUTH WEST EAST \
#       --out output.nc
#
# Monthly mode:
#   python download_era5land.py \
#       --year YYYY \
#       --month MM \
#       --start-day DD \
#       --end-day DD \
#       --variable runoff \
#       --time-step TSTEP \
#       --area-nswe NORTH SOUTH WEST EAST \
#       --out output.nc
#
# Note:
#   CDS API expects area as [North, West, South, East].
#   This shell wrapper passes area as N/S/W/E for readability.
#   The Python downloader should convert it internally.
#==========================================================

set -u

#==========================================================
# PBS settings, optional for HPC users.
# These lines are ignored when running directly on a local PC.
#==========================================================
#PBS -q F10
#PBS -l select=1:ncpus=10:mem=10gb
#PBS -j oe
#PBS -m ea
#PBS -V
#PBS -N ERA5LAND_runoff_download

#==========================================================
# Python environment, edit if needed.
#==========================================================
# source ~/.bashrc
# conda activate py312

#==========================================================
# User settings
#==========================================================

# (0) Python command.
# Use "python3" if "python" is not available in your environment.
PYTHON_CMD="python"
# Python download script
PYTHON_SCRIPT="./download_era5land.py"
# Number of simultaneous download jobs.
# For CDS API, 1 or 2 is usually safer than large values.
# Monthly download is usually more stable with NCPUS=1.
NCPUS=1

# (1) Download file configulations
# Download file unit:
#   daily   -> ERA5-Land_runoff_YYYYMMDD.nc
#   monthly -> ERA5-Land_runoff_YYYYMM.nc   (recommended. faster)
DOWNLOAD_UNIT="monthly"
# Output directory for original NetCDF files
#OUTDIR="./ori_global"
OUTDIR="./ori_japan"
# Input/output forcing timestep in hours. (Downloaded runoff timestep = Converted runoff timestep)
#   24 -> daily, 12 -> 12 hourly, 6 -> 6 hourly, 3 -> 3-hourly, 1 -> hourly
TSTEP=24


# (2) Download period, inclusive. Start and End
SYEAR=2000
SMONTH=1
SDAY=1

EYEAR=2000
EMONTH=1
EDAY=31


# (3) Download area: NORTH, SOUTH, WEST, EAST. 
# Global ERA5-Land domain:
#NORTH=90.0
#SOUTH=-90.0
#WEST=-180.0
#EAST=180.0

# Example: Japan region
 NORTH=46.0
 SOUTH=24.0
 WEST=122.0
 EAST=154.0


# (4) Basic Settings 
# ERA5-Land variable
VARIABLE="runoff"

# If 1, skip files that already exist.
# If 0, pass --overwrite to the Python downloader.
SKIP_EXISTING=1

# Extra options passed to Python downloader, if needed.
EXTRA_OPTS="--area-is edge"

#==========================================================
# Internal settings
#==========================================================

LOGDIR="./log_download"
mkdir -p "${OUTDIR}" "${LOGDIR}"

if ! command -v "${PYTHON_CMD}" >/dev/null 2>&1; then
    echo "ERROR: Python command not found: ${PYTHON_CMD}"
    echo "       Edit PYTHON_CMD in this script, or activate a Python environment."
    exit 1
fi

if [ ! -f "${PYTHON_SCRIPT}" ]; then
    echo "ERROR: Python downloader not found: ${PYTHON_SCRIPT}"
    exit 1
fi

case "${DOWNLOAD_UNIT}" in
    daily|monthly)
        ;;
    *)
        echo "ERROR: DOWNLOAD_UNIT must be daily or monthly."
        exit 1
        ;;
esac

if [ "${TSTEP}" -lt 1 ] || [ "${TSTEP}" -gt 24 ]; then
    echo "ERROR: TSTEP must be between 1 and 24."
    exit 1
fi

if [ $((24 % TSTEP)) -ne 0 ]; then
    echo "ERROR: TSTEP must divide 24 exactly: 1, 2, 3, 4, 6, 8, 12, or 24."
    exit 1
fi

if [ "${NCPUS}" -lt 1 ]; then
    echo "ERROR: NCPUS must be at least 1."
    exit 1
fi

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
            echo "ERROR: failed download detected: ${status_file}, exit code ${status}"
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

run_download() {
    tag="$1"
    outfile="$2"
    logfile="$3"
    statusfile="$4"
    shift 4

    if [ "${SKIP_EXISTING}" -eq 1 ] && [ -e "${outfile}" ]; then
        echo "Skip existing: ${outfile}"
        echo "0" > "${statusfile}"
        return 0
    fi

    overwrite_opt=""
    if [ "${SKIP_EXISTING}" -eq 0 ]; then
        overwrite_opt="--overwrite"
    fi

    echo "Download: ${outfile}"

    (
        "${PYTHON_CMD}" "${PYTHON_SCRIPT}" \
            "$@" \
            --variable "${VARIABLE}" \
            --time-step "${TSTEP}" \
            --area-nswe "${NORTH}" "${SOUTH}" "${WEST}" "${EAST}" \
            --out "${outfile}" \
            ${overwrite_opt} \
            ${EXTRA_OPTS} > "${logfile}" 2>&1

        echo "$?" > "${statusfile}"
    ) &
}

download_one_day() {
    year="$1"
    month="$2"
    day="$3"

    yyyy=$(printf "%04d" "$((10#${year}))")
    mm=$(printf "%02d" "$((10#${month}))")
    dd=$(printf "%02d" "$((10#${day}))")

    tag="${yyyy}${mm}${dd}"
    outfile="${OUTDIR}/ERA5-Land_${VARIABLE}_${tag}.nc"
    logfile="${LOGDIR}/download_${tag}.log"
    statusfile="${LOGDIR}/status_${tag}.txt"

    run_download "${tag}" "${outfile}" "${logfile}" "${statusfile}" \
        --year "${year}" \
        --month "${month}" \
        --day "${day}"
}

download_one_month() {
    year="$1"
    month="$2"
    start_day="$3"
    end_day="$4"

    yyyy=$(printf "%04d" "$((10#${year}))")
    mm=$(printf "%02d" "$((10#${month}))")

    tag="${yyyy}${mm}"
    outfile="${OUTDIR}/ERA5-Land_${VARIABLE}_${tag}.nc"
    logfile="${LOGDIR}/download_${tag}.log"
    statusfile="${LOGDIR}/status_${tag}.txt"

    run_download "${tag}" "${outfile}" "${logfile}" "${statusfile}" \
        --year "${year}" \
        --month "${month}" \
        --start-day "${start_day}" \
        --end-day "${end_day}"
}

#==========================================================
# Main
#==========================================================

rm -f "${LOGDIR}"/status_*.txt

syear4=$(printf "%04d" "$((10#${SYEAR}))")
smonth2=$(printf "%02d" "$((10#${SMONTH}))")
sday2=$(printf "%02d" "$((10#${SDAY}))")
eyear4=$(printf "%04d" "$((10#${EYEAR}))")
emonth2=$(printf "%02d" "$((10#${EMONTH}))")
eday2=$(printf "%02d" "$((10#${EDAY}))")

echo "=========================================================="
echo "ERA5-Land runoff download"
echo "=========================================================="
echo "Python command: ${PYTHON_CMD}"
echo "Python script : ${PYTHON_SCRIPT}"
echo "Download unit : ${DOWNLOAD_UNIT}"
echo "Period        : ${syear4}-${smonth2}-${sday2} to ${eyear4}-${emonth2}-${eday2}"
echo "Time step     : ${TSTEP} hour(s)"
echo "Area N/S/W/E  : ${NORTH} ${SOUTH} ${WEST} ${EAST}"
echo "Variable      : ${VARIABLE}"
echo "Output dir    : ${OUTDIR}"
echo "Log dir       : ${LOGDIR}"
echo "Parallel jobs : ${NCPUS}"
echo "Skip existing : ${SKIP_EXISTING}"
echo "=========================================================="

if [ "${DOWNLOAD_UNIT}" = "daily" ]; then
    # Generate inclusive date list using Python.
    DATES=$("${PYTHON_CMD}" - <<EOF
from datetime import date, timedelta
import sys

try:
    start = date(${SYEAR}, ${SMONTH}, ${SDAY})
    end = date(${EYEAR}, ${EMONTH}, ${EDAY})
except ValueError as exc:
    print(f"ERROR: invalid date: {exc}", file=sys.stderr)
    raise SystemExit(1)

if end < start:
    print("ERROR: end date is earlier than start date", file=sys.stderr)
    raise SystemExit(1)

d = start
while d <= end:
    print(d.strftime("%Y %m %d"))
    d += timedelta(days=1)
EOF
)

    if [ -z "${DATES}" ]; then
        echo "ERROR: no dates generated."
        exit 1
    fi

    while read -r YEAR MONTH DAY; do
        [ -n "${YEAR}" ] || continue
        wait_for_free_slot
        download_one_day "${YEAR}" "${MONTH}" "${DAY}"
    done <<< "${DATES}"

elif [ "${DOWNLOAD_UNIT}" = "monthly" ]; then
    # Generate inclusive month list.
    # Each line:
    #   YEAR MONTH START_DAY END_DAY
    MONTHS=$("${PYTHON_CMD}" - <<EOF
from datetime import date
import calendar
import sys

try:
    start = date(${SYEAR}, ${SMONTH}, ${SDAY})
    end = date(${EYEAR}, ${EMONTH}, ${EDAY})
except ValueError as exc:
    print(f"ERROR: invalid date: {exc}", file=sys.stderr)
    raise SystemExit(1)

if end < start:
    print("ERROR: end date is earlier than start date", file=sys.stderr)
    raise SystemExit(1)

year = start.year
month = start.month

while (year, month) <= (end.year, end.month):
    first_day = 1
    last_day = calendar.monthrange(year, month)[1]

    if year == start.year and month == start.month:
        first_day = start.day
    if year == end.year and month == end.month:
        last_day = end.day

    print(f"{year} {month} {first_day} {last_day}")

    if month == 12:
        year += 1
        month = 1
    else:
        month += 1
EOF
)

    if [ -z "${MONTHS}" ]; then
        echo "ERROR: no months generated."
        exit 1
    fi

    while read -r YEAR MONTH START_DAY END_DAY; do
        [ -n "${YEAR}" ] || continue
        wait_for_free_slot
        download_one_month "${YEAR}" "${MONTH}" "${START_DAY}" "${END_DAY}"
    done <<< "${MONTHS}"
fi

# Wait for all background downloads before exiting.
wait

if check_failed_jobs; then
    echo "=========================================================="
    echo "All download jobs finished successfully."
    echo "=========================================================="
    exit 0
else
    echo "=========================================================="
    echo "Some download jobs failed. Check logs in: ${LOGDIR}"
    echo "=========================================================="
    exit 1
fi
