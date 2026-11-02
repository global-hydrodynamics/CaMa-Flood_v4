#!/usr/bin/env bash
# Download global ERA5-Land runoff as monthly NetCDF files.
# Edit the dates and settings below before starting a large request.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${SCRIPT_DIR}"

# User settings. The defaults request one month as a low-risk example.
PYTHON_CMD="python3"
START_YEAR=2000
START_MONTH=1
START_DAY=1
END_YEAR=2000
END_MONTH=1
END_DAY=31
TIMESTEP_HOURS=24
OUTDIR="./ori_global"
VARIABLE="runoff"
SKIP_EXISTING=1
NORTH=90.0
SOUTH=-90.0
WEST=-180.0
EAST=180.0

mkdir -p "${OUTDIR}" "./log_download_global"
command -v "${PYTHON_CMD}" >/dev/null || { echo "[ERROR] Python not found: ${PYTHON_CMD}" >&2; exit 1; }
[[ -f ./download_era5land.py ]] || { echo "[ERROR] download_era5land.py not found" >&2; exit 1; }

if (( START_YEAR > END_YEAR || (START_YEAR == END_YEAR && START_MONTH > END_MONTH) )); then
  echo "[ERROR] Start date must not be later than end date." >&2
  exit 1
fi

year=${START_YEAR}
month=${START_MONTH}
while (( year < END_YEAR || (year == END_YEAR && month <= END_MONTH) )); do
  days_in_month=$("${PYTHON_CMD}" -c 'import calendar,sys; print(calendar.monthrange(int(sys.argv[1]),int(sys.argv[2]))[1])' "${year}" "${month}")
  first_day=1
  last_day=${days_in_month}
  if (( year == START_YEAR && month == START_MONTH )); then first_day=${START_DAY}; fi
  if (( year == END_YEAR && month == END_MONTH )); then last_day=${END_DAY}; fi
  (( first_day >= 1 && first_day <= days_in_month && last_day >= first_day && last_day <= days_in_month )) || {
    echo "[ERROR] Invalid date selection for ${year}-$(printf '%02d' "${month}")." >&2; exit 1;
  }

  outfile="${OUTDIR}/ERA5-Land_runoff_$(printf '%04d%02d' "${year}" "${month}").nc"
  if [[ -s "${outfile}" && ${SKIP_EXISTING} -eq 1 ]]; then
    echo "[SKIP] Existing non-empty file: ${outfile}"
  else
    overwrite=()
    [[ ${SKIP_EXISTING} -eq 1 ]] || overwrite=(--overwrite)
    echo "[STEP] Global ERA5-Land ${year}-$(printf '%02d' "${month}") days ${first_day}-${last_day}"
    "${PYTHON_CMD}" ./download_era5land.py \
      --year "${year}" --month "${month}" \
      --start-day "${first_day}" --end-day "${last_day}" \
      --file-unit monthly --time-step "${TIMESTEP_HOURS}" \
      --variable "${VARIABLE}" \
      --area-nswe "${NORTH}" "${SOUTH}" "${WEST}" "${EAST}" --area-is edge \
      --out "${outfile}" "${overwrite[@]}" \
      > "./log_download_global/download_$(printf '%04d%02d' "${year}" "${month}").log" 2>&1 || {
        status=$?
        echo "[ERROR] Download failed for ${year}-$(printf '%02d' "${month}"); see log_download_global." >&2
        exit "${status}"
      }
    echo "[SUCCESS] ${outfile}"
  fi

  if (( month == 12 )); then year=$((year + 1)); month=1; else month=$((month + 1)); fi
done

echo "[SUCCESS] Requested global ERA5-Land monthly downloads are present. Verify each NetCDF before conversion."
