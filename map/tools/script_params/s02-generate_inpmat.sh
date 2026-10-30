#!/bin/sh
# Generate a map-to-runoff input matrix for a user-selected regular runoff grid.
# Copy this template into a map-local script_params/ directory before running.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd)
PROGRAM="$CODE_DIR/param_generate_inpmat"

# Output metadata and matrix. Existing files with these names may be replaced.
DIMINFO="diminfo_test-15min_nc.txt"
INPMAT="inpmat_test-15min_nc.bin"

# Runoff input-grid geometry (degrees), retained from the legacy s02 example.
# These values describe the runoff dataset, not the CaMa-Flood map.
GRSIZEIN=0.25
WESTIN=-180.0
EASTIN=180.0
NORTHIN=90.0
SOUTHIN=-60.0
OLAT="NtoS" # NtoS: north row first; StoN: south row first.

fail() {
    echo "s02-generate_inpmat.sh: $*" >&2
    exit 1
}

[ -x "$PROGRAM" ] || fail "shared executable not found or not executable: $PROGRAM (build with make -C map/tools/code param)"
[ -f "$MAP_DIR/params.txt" ] || fail "map parameters not found: $MAP_DIR/params.txt"

# Preserve the legacy preference for 1min, then 15sec; TAG can select another
# available high-resolution directory (for example 30sec) explicitly.
if [ -z "${TAG:-}" ]; then
    if [ -f "$MAP_DIR/1min/location.txt" ]; then
        TAG=1min
    elif [ -f "$MAP_DIR/15sec/location.txt" ]; then
        TAG=15sec
    else
        fail "no default high-resolution location.txt found (expected 1min/ or 15sec/); set TAG to an available map tag"
    fi
fi

HIRES_DIR="$MAP_DIR/$TAG"
LOCATION="$HIRES_DIR/location.txt"
[ -f "$LOCATION" ] || fail "high-resolution location file not found for TAG=$TAG: $LOCATION"

# The Fortran program requires each area's catchment-index raster. grdare.bin
# is optional; if absent, the original area-calculation fallback is used.
AREAS=$(awk 'NR > 2 && NF >= 2 { print $2 }' "$LOCATION")
[ -n "$AREAS" ] || fail "no high-resolution areas found in $LOCATION"
for AREA in $AREAS; do
    [ -f "$HIRES_DIR/$AREA.catmxy.bin" ] || fail "required catchment map not found: $HIRES_DIR/$AREA.catmxy.bin"
done

cd "$MAP_DIR"
"$PROGRAM" "$TAG" "$GRSIZEIN" "$WESTIN" "$EASTIN" "$NORTHIN" "$SOUTHIN" "$OLAT" "$DIMINFO" "$INPMAT" \
    || fail "param_generate_inpmat failed"

[ -s "$MAP_DIR/$DIMINFO" ] || fail "expected dimension metadata was not created: $MAP_DIR/$DIMINFO"
[ -s "$MAP_DIR/$INPMAT" ] || fail "expected input matrix was not created: $MAP_DIR/$INPMAT"
echo "Created $DIMINFO and $INPMAT in $MAP_DIR"
