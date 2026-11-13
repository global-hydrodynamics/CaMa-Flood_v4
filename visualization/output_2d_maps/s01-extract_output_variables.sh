#!/usr/bin/env bash
# Extract selected variable/date pairs from annual simulation output files.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VISUALIZATION_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$VISUALIZATION_DIR/.." && pwd)"

# User settings: simulation output source and compact sample destination.
SOURCE_DIR="$REPO_ROOT/out/mek02-2year_netcdf" # Annual o_<variable><year>.nc outputs.
PARAMS_FILE="$REPO_ROOT/map/mekong_15min/params.txt" # Grid geometry for spatial checks.
OUTPUT_DIR="$SCRIPT_DIR/out_sample" # One-record NetCDF files and extraction log.
STATION_CSV="$SCRIPT_DIR/station_map_locations.csv" # Selected Mekong station locations.

# User settings: comma-separated variables and requested CF dates.
VARIABLES="outflw" # NetCDF variable(s); extend with comma-separated names.
DATES="2001-09-01" # Requested midnight dates; extend with comma-separated dates.
PYTHON="python3" # Activate the repository .venv before running this script.

while (($#)); do
    case "$1" in
        --variables)
            VARIABLES="$2"
            shift 2
            ;;
        --dates)
            DATES="$2"
            shift 2
            ;;
        --source-dir)
            SOURCE_DIR="$2"
            shift 2
            ;;
        --params)
            PARAMS_FILE="$2"
            shift 2
            ;;
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        *)
            printf '[ERROR] Unknown argument: %s\n' "$1" >&2
            exit 2
            ;;
    esac
done

IFS=',' read -r -a VARIABLE_LIST <<< "$VARIABLES"
IFS=',' read -r -a DATE_LIST <<< "$DATES"
TOTAL=$(( ${#VARIABLE_LIST[@]} * ${#DATE_LIST[@]} ))
STEP=0

for VARIABLE in "${VARIABLE_LIST[@]}"; do
    for TARGET_DATE in "${DATE_LIST[@]}"; do
        STEP=$((STEP + 1))
        OUTPUT_FILE="$OUTPUT_DIR/${VARIABLE}_${TARGET_DATE}.nc"
        printf '\n[STEP %s/%s] Extract %s for %s.\n' "$STEP" "$TOTAL" "$VARIABLE" "$TARGET_DATE"
        printf '  Source:      %s/o_%s%s.nc\n' \
            "$SOURCE_DIR" "$VARIABLE" "${TARGET_DATE:0:4}"
        printf '  Destination: %s\n' "$OUTPUT_FILE"
        "$PYTHON" "$SCRIPT_DIR/script/extract_output_variables.py" \
            --source-dir "$SOURCE_DIR" --variable "$VARIABLE" \
            --date "$TARGET_DATE" --params "$PARAMS_FILE" \
            --output "$OUTPUT_FILE"
    done
done

printf '\n[STEP %s/%s] Extract daily outflw at the station locations.\n' \
    "$((TOTAL + 1))" "$((TOTAL + 1))"
"$PYTHON" "$SCRIPT_DIR/script/extract_station_outflw.py" \
    --source-dir "$SOURCE_DIR" --stations-csv "$STATION_CSV" \
    --output-dir "$OUTPUT_DIR"

printf '\n[SUCCESS] Extracted %s variable/date combination(s) and station daily flows.\n' "$TOTAL"
printf 'Samples and extraction history: %s\n' "$OUTPUT_DIR"
