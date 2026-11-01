#!/bin/sh
# Select the largest valid allocated GRDC gauge, report its river cell, and create its upstream mask.
# Copy to <map>/script_params/ and run from any working directory.
# Input: map-local GRDC_alloc.txt from t01-alloc_flow_gauge.sh; standard river-map files.
# Outputs: upbasin.bin, grdc_station_selection.txt, and grdc_river_info.txt in the map root.
# Existing output files are overwritten. This script does not run t01 automatically.
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# User settings: choose the GRDC allocation table if it is not in the map root.
GRDC_ALLOC="$MAP_DIR/GRDC_alloc.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Locate the GRDC allocation table"
# On exit, report failures and return the original status; signals use standard nonzero statuses.
trap '
    EXIT_STATUS=$?
    trap - 0
    if [ "$EXIT_STATUS" -ne 0 ]; then
        printf "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
        printf "!!! ERROR: %s FAILED !!!\n" "$SCRIPT_NAME" >&2
        printf "!!! Step: %s\n" "$STEP" >&2
        printf "!!! Exit status: %s\n" "$EXIT_STATUS" >&2
        printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
    fi
    exit "$EXIT_STATUS"
' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

EXEC_RIVINFO="$CODE_DIR/analysis_get_rivinfo"
EXEC_UPBASIN="$CODE_DIR/analysis_get_upbasin"
STATION_REPORT="$MAP_DIR/grdc_station_selection.txt"
RIVER_INFO_REPORT="$MAP_DIR/grdc_river_info.txt"
UPBASIN_OUTPUT="$MAP_DIR/upbasin.bin"

if [ ! -r "$GRDC_ALLOC" ]; then
    printf '[ERROR] %s is not readable. Run t01-alloc_flow_gauge.sh or provide GRDC_alloc.txt.\n' "$GRDC_ALLOC" >&2
    exit 1
fi

NX=$(awk 'NR == 1 { print $1; exit }' "$MAP_DIR/params.txt")
NY=$(awk 'NR == 2 { print $1; exit }' "$MAP_DIR/params.txt")

# Select by the documented whitespace-table header. Strictly greater preserves the first row on ties.
STEP="Select the first valid station with the largest area_CaMa"
SELECTED=$(awk -v nx="$NX" -v ny="$NY" '
function is_number(value) {
    return value ~ /^[-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][-+]?[0-9]+)?$/
}
NR == 1 {
    for (i = 1; i <= NF; i++) column[$i] = i
    required[1]="ID"; required[2]="lat"; required[3]="lon"
    required[4]="area_Input"; required[5]="area_CaMa"
    required[6]="Type"; required[7]="ix1"; required[8]="iy1"
    for (i = 1; i <= 8; i++) if (!(required[i] in column)) {
        print "GRDC_alloc.txt is missing required column " required[i] > "/dev/stderr"
        invalid_header=1
        exit 2
    }
    next
}
{
    id=$(column["ID"]); lat=$(column["lat"]); lon=$(column["lon"])
    area_input=$(column["area_Input"]); area_cama=$(column["area_CaMa"])
    type=$(column["Type"]); ix=$(column["ix1"]); iy=$(column["iy1"])
    if (type == "-9") next
    if (!is_number(area_input) || !is_number(area_cama)) next
    if ((area_input + 0) <= 0 || (area_cama + 0) <= 0 || (area_input + 0) >= 1e308 || (area_cama + 0) >= 1e308) next
    if (ix !~ /^[0-9]+$/ || iy !~ /^[0-9]+$/) next
    if (ix < 1 || ix > nx || iy < 1 || iy > ny) next
    if (!found || (area_cama + 0) > best_area) {
        found=1; best_area=area_cama + 0
        best=sprintf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s", id,lat,lon,area_input,area_cama,type,ix,iy)
    }
}
END {
    if (invalid_header) exit 2
    if (!found) {
        print "GRDC_alloc.txt contains no allocated station with valid areas and grid indices" > "/dev/stderr"
        exit 1
    }
    print best
}' "$GRDC_ALLOC")

set -- $SELECTED
GRDC_ID=$1
ALLOC_LAT=$2
ALLOC_LON=$3
AREA_INPUT=$4
AREA_CAMA=$5
TYPE=$6
IX=$7
IY=$8

STEP="Save the selected station record"
printf 'ID\tlat\tlon\tarea_Input_km2\tarea_CaMa_km2\tType\tix1\tiy1\n' > "$STATION_REPORT"
printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$GRDC_ID" "$ALLOC_LAT" "$ALLOC_LON" "$AREA_INPUT" "$AREA_CAMA" "$TYPE" "$IX" "$IY" \
    >> "$STATION_REPORT"

# Use the allocation's primary grid cell; re-mapping lat/lon can select a different cell.
cd "$SCRIPT_DIR"
STEP="Write river information for GRDC $GRDC_ID at cell ($IX,$IY)"
printf '==================================================\n[STEP 1/2] Inspect selected GRDC river cell\n==================================================\n'
"$EXEC_RIVINFO" xy "$IX" "$IY" > "$RIVER_INFO_REPORT"

STEP="Create the single-cell upstream-basin mask for GRDC $GRDC_ID"
printf '==================================================\n[STEP 2/2] Create upstream-basin mask\n==================================================\n'
"$EXEC_UPBASIN" "$IX" "$IY" "$UPBASIN_OUTPUT"
printf 'Selected GRDC station: %s; cell=(%s,%s); area_CaMa=%s km2; Type=%s\n' \
    "$GRDC_ID" "$IX" "$IY" "$AREA_CAMA" "$TYPE"
printf 'Station summary: %s\nRiver information: %s\nUpstream mask: %s\n' \
    "$STATION_REPORT" "$RIVER_INFO_REPORT" "$UPBASIN_OUTPUT"
printf '==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
