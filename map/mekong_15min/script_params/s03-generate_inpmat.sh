#!/bin/sh
# Generate a map-to-runoff input matrix for a user-selected regular runoff grid.
# Copy this template into a map-local script_params/ directory before running.
#
# Inputs:
#   - params.txt in the map root, describing the target CaMa-Flood map grid.
#   - <HIRES_TAG>/location.txt and each listed area's <area>.catmxy.bin high-resolution
#     catchment map. grdare.bin is optional; the Fortran tool has a fallback.
#   - The runoff-grid geometry and row order configured below; these describe
#     the runoff grid, not the CaMa-Flood map.
#
# Outputs in the map root (existing files with these configured names may be replaced):
#   - inpdim_<ROFFTAG>.txt: metadata describing the selected runoff grid and matrix.
#   - inpmat_<ROFFTAG>.bin: binary map-to-runoff input matrix.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd)
PROGRAM="$CODE_DIR/param_generate_inpmat"

# One runoff tag derives both paired output names.
ROFFTAG="${ROFFTAG:-test-15min_nc}"
INPDIM="inpdim_${ROFFTAG}.txt"
INPMAT="inpmat_${ROFFTAG}.bin"

# Runoff input-grid geometry (degrees), retained from the legacy s02 example.
# These values describe the runoff dataset, not the CaMa-Flood map.
GRSIZEIN=0.25
WESTIN=-180.0
EASTIN=180.0
NORTHIN=90.0
SOUTHIN=-60.0
OLAT="NtoS" # NtoS: north row first; StoN: south row first.

fail() {
    echo "s03-generate_inpmat.sh: $*" >&2
    exit 1
}

[ -x "$PROGRAM" ] || fail "shared executable not found or not executable: $PROGRAM (build with make -C map/tools/code param)"
[ -f "$MAP_DIR/params.txt" ] || fail "map parameters not found: $MAP_DIR/params.txt"

# HIRES_TAG selects high-resolution map data and is separate from ROFFTAG.
HIRES_TAG=${HIRES_TAG:-}
if [ -z "$HIRES_TAG" ]; then
    if [ -f "$MAP_DIR/1min/location.txt" ]; then
        HIRES_TAG=1min
    elif [ -f "$MAP_DIR/15sec/location.txt" ]; then
        HIRES_TAG=15sec
    else
        fail "no default high-resolution location.txt found (expected 1min/ or 15sec/); set HIRES_TAG to an available map tag"
    fi
fi

HIRES_DIR="$MAP_DIR/$HIRES_TAG"
LOCATION="$HIRES_DIR/location.txt"
[ -f "$LOCATION" ] || fail "high-resolution location file not found for HIRES_TAG=$HIRES_TAG: $LOCATION"

# The Fortran program requires each area's catchment-index raster. grdare.bin
# is optional; if absent, the original area-calculation fallback is used.
AREAS=$(awk 'NR > 2 && NF >= 2 { print $2 }' "$LOCATION")
[ -n "$AREAS" ] || fail "no high-resolution areas found in $LOCATION"
for AREA in $AREAS; do
    [ -f "$HIRES_DIR/$AREA.catmxy.bin" ] || fail "required catchment map not found: $HIRES_DIR/$AREA.catmxy.bin"
done

cd "$MAP_DIR"
"$PROGRAM" "$HIRES_TAG" "$GRSIZEIN" "$WESTIN" "$EASTIN" "$NORTHIN" "$SOUTHIN" "$OLAT" "$INPDIM" "$INPMAT" \
    || fail "param_generate_inpmat failed"

[ -s "$MAP_DIR/$INPDIM" ] || fail "expected dimension metadata was not created: $MAP_DIR/$INPDIM"
[ -s "$MAP_DIR/$INPMAT" ] || fail "expected input matrix was not created: $MAP_DIR/$INPMAT"
METADATA_INPMAT=$(awk 'NR == 7 { print $1; exit }' "$MAP_DIR/$INPDIM")
[ "$METADATA_INPMAT" = "$INPMAT" ] || fail "line 7 of $INPDIM does not name its paired matrix $INPMAT"
echo "Created $INPDIM and $INPMAT in $MAP_DIR (ROFFTAG=$ROFFTAG, HIRES_TAG=$HIRES_TAG)"
