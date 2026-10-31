#!/bin/sh
# Generate standard non-MPI bifurcation hydraulic and basin-connectivity maps.
# Copy into <map>/script_params/, edit BIFLAYER and ROFFTAG if needed, and run
# from any caller working directory. The executable CWDs below preserve the
# relative-path expectations of the legacy programs.
#
# Inputs in the map root:
#   - params.txt, nextxy.bin, basin.bin, bifori.txt, rivhgt.bin
#   - inpdim_<ROFFTAG>.txt (default ROFFTAG=test-1deg), including map dimensions.
#
# Standard outputs in the map root (existing files with these names are replaced):
#   - bifprm.txt: filtered standard bifurcation parameter table.
#   - bifbsn.bin: basin IDs updated for ordinary bifurcation connectivity.
#   - bifcol.bin: basin-color indices for visualization.
# Optional argument [check_map] asks param_set_bifparam to write the two-record
# check_bif_dphwth.bin (depth, width), and param_set_bif_basin to write the
# two-record check_bif_bsnmod.bin (bifmod, bpoint). Both are inspection-only.
# Usage: ./s02-bifurcation.sh [check_map]
# Without check_map neither diagnostic is created or overwritten. Existing
# separate bifmod.bin/bpoint.bin files are legacy products and are not updated.
#
# This is the standard non-MPI workflow. MPI-specific sorting and map changes
# belong in s04-mpi_map_setup.sh. The programs preserve legacy overwrite behavior.
set -eu

# Resolve paths from this copied map-local script, not from the caller's CWD.
# The shared executables remain in the repository's map/tools/code directory.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Workflow settings. Paths are interpreted from the map root.
BIFLAYER=5
ROFFTAG="${ROFFTAG:-test-1deg}"
INPDIM="inpdim_${ROFFTAG}.txt"
INPMAT="inpmat_${ROFFTAG}.bin"
RIVHGT="rivhgt.bin"
BIFPRM="bifprm.txt"

SETPARAM="$CODE_DIR/param_set_bifparam"
SETBASIN="$CODE_DIR/param_set_bif_basin"

fail() {
    echo "s02-bifurcation.sh: $*" >&2
    exit 1
}

CHECK_MAP=${1:-}
[ "$#" -le 1 ] || fail "usage: $0 [check_map]"
case $CHECK_MAP in
    ''|check_map) ;;
    *) fail "unknown argument '$CHECK_MAP' (use check_map for the optional diagnostic map)" ;;
esac

run_step() {
    # Report which executable failed, then stop before running dependent steps.
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

# The legacy program accepts a requested number of bifurcation layers. Its
# supported source dataset has at most 10 layers; keep the wrapper in that range.
case $BIFLAYER in
    ''|*[!0-9]*) fail "BIFLAYER must be an integer from 1 to 10" ;;
    *) [ "$BIFLAYER" -ge 1 ] && [ "$BIFLAYER" -le 10 ] || fail "BIFLAYER must be an integer from 1 to 10" ;;
esac
[ "${#INPDIM}" -le 128 ] || fail "INPDIM exceeds the legacy Fortran argument limit of 128 characters"

case $INPDIM in
    /*) INPDIM_PATH=$INPDIM ;;
    *) INPDIM_PATH="$MAP_DIR/$INPDIM" ;;
esac

# Fail early if either executable, map input, or dimension descriptor is absent.
[ -x "$SETPARAM" ] || fail "shared executable missing: $SETPARAM (build with make -C map/tools/code param)"
[ -x "$SETBASIN" ] || fail "shared executable missing: $SETBASIN (build with make -C map/tools/code param)"
[ -s "$MAP_DIR/params.txt" ] || fail "map input missing: $MAP_DIR/params.txt"
[ -s "$MAP_DIR/nextxy.bin" ] || fail "map input missing: $MAP_DIR/nextxy.bin"
[ -s "$MAP_DIR/basin.bin" ] || fail "map input missing: $MAP_DIR/basin.bin"
[ -s "$MAP_DIR/bifori.txt" ] || fail "bifurcation input missing: $MAP_DIR/bifori.txt"
[ -s "$MAP_DIR/$RIVHGT" ] || fail "channel-depth input missing: $MAP_DIR/$RIVHGT (run s01-channel_params.sh first)"
[ -s "$INPDIM_PATH" ] || fail "input-grid metadata missing: $INPDIM_PATH"
[ "$(awk 'NR == 7 { print $1; exit }' "$INPDIM_PATH")" = "$INPMAT" ] \
    || fail "line 7 of $INPDIM does not name its paired matrix $INPMAT"

# The bifurcation utility reads NX and NY from INPDIM while the basin utility
# reads them from params.txt. Confirm they describe the same base-map grid.
MAP_NX=$(awk 'NR == 1 { print $1; exit }' "$MAP_DIR/params.txt")
MAP_NY=$(awk 'NR == 2 { print $1; exit }' "$MAP_DIR/params.txt")
DIM_NX=$(awk 'NR == 1 { print $1; exit }' "$INPDIM_PATH")
DIM_NY=$(awk 'NR == 2 { print $1; exit }' "$INPDIM_PATH")
case $MAP_NX:$MAP_NY:$DIM_NX:$DIM_NY in
    *[!0-9:]*|:*) fail "could not read positive integer map dimensions from params.txt and $INPDIM" ;;
esac
[ "$MAP_NX" -gt 0 ] && [ "$MAP_NY" -gt 0 ] || fail "params.txt contains invalid map dimensions"
[ "$MAP_NX" -eq "$DIM_NX" ] && [ "$MAP_NY" -eq "$DIM_NY" ] \
    || fail "map dimensions in $INPDIM ($DIM_NX x $DIM_NY) do not match params.txt ($MAP_NX x $MAP_NY)"

# Warn before the programs open their regular output files in replacement mode.
for output in "$BIFPRM" bifbsn.bin bifcol.bin; do
    if [ -e "$MAP_DIR/$output" ]; then
        echo "s02-bifurcation.sh: existing output will be replaced: $MAP_DIR/$output" >&2
    fi
done
if [ "$CHECK_MAP" = check_map ] && [ -e "$MAP_DIR/check_bif_dphwth.bin" ]; then
    echo "s02-bifurcation.sh: existing check-only output will be replaced: $MAP_DIR/check_bif_dphwth.bin" >&2
fi
if [ "$CHECK_MAP" = check_map ] && [ -e "$MAP_DIR/check_bif_bsnmod.bin" ]; then
    echo "s02-bifurcation.sh: existing check-only output will be replaced: $MAP_DIR/check_bif_bsnmod.bin" >&2
fi

# set_bifparam reads map-root-relative inputs and writes outputs in that root.
cd "$MAP_DIR"
if [ "$CHECK_MAP" = check_map ]; then
    run_step "param_set_bifparam (BIFLAYER=$BIFLAYER, check map enabled)" \
        "$SETPARAM" "$RIVHGT" "$BIFPRM" "$BIFLAYER" "$INPDIM" check_map
else
    run_step "param_set_bifparam (BIFLAYER=$BIFLAYER)" \
        "$SETPARAM" "$RIVHGT" "$BIFPRM" "$BIFLAYER" "$INPDIM"
fi
[ -s "$MAP_DIR/$BIFPRM" ] || fail "expected output was not created: $MAP_DIR/$BIFPRM"
if [ "$CHECK_MAP" = check_map ]; then
    [ -s "$MAP_DIR/check_bif_dphwth.bin" ] || fail "expected check-only output was not created: $MAP_DIR/check_bif_dphwth.bin"
fi

# set_bif_basin expects CWD=map/script_params because its paths start with ../.
cd "$SCRIPT_DIR"
if [ "$CHECK_MAP" = check_map ]; then
    run_step "param_set_bif_basin (check map enabled)" "$SETBASIN" check_map
else
    run_step "param_set_bif_basin" "$SETBASIN"
fi

for output in "$BIFPRM" bifbsn.bin bifcol.bin; do
    [ -s "$MAP_DIR/$output" ] || fail "expected output was not created: $MAP_DIR/$output"
done
if [ "$CHECK_MAP" = check_map ]; then
    [ -s "$MAP_DIR/check_bif_dphwth.bin" ] || fail "expected check-only output was not created: $MAP_DIR/check_bif_dphwth.bin"
    [ -s "$MAP_DIR/check_bif_bsnmod.bin" ] || fail "expected check-only output was not created: $MAP_DIR/check_bif_bsnmod.bin"
fi

if [ "$CHECK_MAP" = check_map ]; then
    echo "Created standard bifurcation products and optional check maps in $MAP_DIR"
else
    echo "Created standard non-MPI bifurcation products in $MAP_DIR"
fi
