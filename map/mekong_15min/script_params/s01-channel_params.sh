#!/bin/sh
# Map-local template for annual-runoff channel parameter generation.
# Copy into <map>/script_params/, edit configuration below, and run from any CWD.
#
# Inputs (relative paths are resolved from the map root):
#   - inpdim_<ROFFTAG>.txt: runoff-grid dimensions and input-matrix metadata.
#   - inpmat_<ROFFTAG>.bin: the paired map-to-runoff input matrix.
#   - CROFBIN: annual-mean runoff climatology (default: ../data/ELSE_GPCC_clm-1981-2010.one).
#   - nextxy.bin and ctmare.bin: river-network maps used by the parameter programs.
#   - width.bin: MERIT Hydro channel-width estimates used for the MERIT merge.
#
# Outputs in the map root (existing files with these names may be replaced):
#   - outclm.bin: climatological discharge calculated from runoff and the input matrix.
#   - rivwth.bin, rivhgt.bin, rivman.bin, rivhgt_inf.bin: power-law channel parameters.
#   - rivwth_merit.bin: width.bin estimates merged with the power-law river widths.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Workflow configuration. Relative paths are interpreted from the map root.
TYPE="bin"                                      # binary runoff input
INTERP="inpmat"                                 # use the standard map input matrix
ROFFTAG="${ROFFTAG:-test-1deg}"                # runoff/input-grid configuration identifier
INPDIM="inpdim_${ROFFTAG}.txt"
INPMAT="inpmat_${ROFFTAG}.bin"
CROFBIN="../data/ELSE_GPCC_clm-1981-2010.one"  # annual mean runoff [mm/s]
# Runoff-climatology data source: Kim, Yeh, Oki, and Kanae (2009),
# "Role of rivers in the seasonal variations of terrestrial water storage over global basins."
# Geophysical Research Letters. https://doi.org/10.1029/2009GL039006

# Channel cross-section coefficients from the legacy standard workflow.
HC=0.1
HP=0.50
HO=0.00
HMIN=1.0
WC=2.50
WP=0.60
WO=0.00
WMIN=5.0

OUTCLM="$CODE_DIR/param_calc_outclm"
RIVWTH="$CODE_DIR/param_calc_rivwth"
MERIT="$CODE_DIR/param_set_merit_width"

case $INPDIM in
    /*) INPDIM_PATH=$INPDIM ;;
    *) INPDIM_PATH="$MAP_DIR/$INPDIM" ;;
esac

fail() {
    echo "s01-channel_params.sh: $*" >&2
    exit 1
}

run_step() {
    label=$1
    shift
    echo "@@@ $label"
    if "$@"; then
        return 0
    else
        status=$?
        fail "$label failed (exit status $status)"
    fi
}

[ -x "$OUTCLM" ] || fail "shared executable missing: $OUTCLM (build with make -C map/tools/code param)"
[ -x "$RIVWTH" ] || fail "shared executable missing: $RIVWTH (build with make -C map/tools/code param)"
[ -x "$MERIT" ] || fail "shared executable missing: $MERIT (build with make -C map/tools/code param)"
[ -s "$INPDIM_PATH" ] || fail "input-grid metadata missing: $INPDIM_PATH"
[ -s "$MAP_DIR/nextxy.bin" ] || fail "map input missing: $MAP_DIR/nextxy.bin"
[ -s "$MAP_DIR/ctmare.bin" ] || fail "map input missing: $MAP_DIR/ctmare.bin"
[ -s "$MAP_DIR/width.bin" ] || fail "MERIT Hydro width input missing: $MAP_DIR/width.bin"

# The seventh line is retained in the format and must name the paired matrix.
METADATA_INPMAT=$(awk 'NR == 7 { print $1; exit }' "$INPDIM_PATH")
[ "$METADATA_INPMAT" = "$INPMAT" ] || fail "line 7 of $INPDIM does not name its paired matrix $INPMAT"
case $INPMAT in
    /*) INPMAT_PATH=$INPMAT ;;
    *) INPMAT_PATH="$MAP_DIR/$INPMAT" ;;
esac
[ -s "$INPMAT_PATH" ] || fail "input matrix missing: $INPMAT_PATH"

case $CROFBIN in
    /*) CROFBIN_PATH=$CROFBIN ;;
    *) CROFBIN_PATH="$MAP_DIR/$CROFBIN" ;;
esac
[ -s "$CROFBIN_PATH" ] || fail "annual runoff climatology missing: $CROFBIN_PATH (provide the package map/data input or edit CROFBIN)"
[ "${#INPDIM}" -le 128 ] || fail "INPDIM exceeds the legacy Fortran argument limit of 128 characters; use a shorter path"
[ "${#CROFBIN}" -le 128 ] || fail "CROFBIN exceeds the legacy Fortran argument limit of 128 characters; use a shorter path"

cd "$MAP_DIR"
run_step "param_calc_outclm" "$OUTCLM" "$TYPE" "$INTERP" "$INPDIM" "$CROFBIN"
run_step "param_calc_rivwth" "$RIVWTH" "$TYPE" "$INPDIM" "$HC" "$HP" "$HO" "$HMIN" "$WC" "$WP" "$WO" "$WMIN"
run_step "param_set_merit_width" "$MERIT" "$INPDIM"

for output in outclm.bin rivwth.bin rivhgt.bin rivman.bin rivhgt_inf.bin rivwth_merit.bin; do
    [ -s "$MAP_DIR/$output" ] || fail "expected output was not created: $MAP_DIR/$output"
done
echo "Created channel-parameter products in $MAP_DIR"
