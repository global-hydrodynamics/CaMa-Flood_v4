#!/bin/sh
# Create MPI region maps from the standard bifprm.txt and map basin raster.
# Copy to <map>/script_params/ and edit settings below. Run from any CWD.
# Standard bifprm.txt is read-only for this workflow; MPI-only tables are not retained.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

MPIMODE=normal                 # normal (6% merge threshold) or MaxMPI (3%)
MPI_COUNTS="4 8 16"            # MPI process counts to create
DEFAULT_MPI=16                 # mpireg.bin aliases this generated partition

fail() { echo "s04-mpi_map_setup.sh: $*" >&2; exit 1; }
case $MPIMODE in normal|MaxMPI) ;; *) fail "MPIMODE must be normal or MaxMPI" ;; esac
[ -s "$MAP_DIR/params.txt" ] || fail "map input missing: $MAP_DIR/params.txt"
[ -s "$MAP_DIR/basin.bin" ] || fail "map input missing: $MAP_DIR/basin.bin"
[ -s "$MAP_DIR/bifprm.txt" ] || fail "standard bifurcation input missing: $MAP_DIR/bifprm.txt (run s02-bifurcation.sh first)"
# Counts are a whitespace-separated list of distinct positive integers.
seen=" "
for count in $MPI_COUNTS; do
    case $count in ''|*[!0-9]*) fail "invalid MPI count '$count'" ;; esac
    [ "$count" -gt 0 ] || fail "MPI counts must be positive"
    case $seen in *" $count "*) fail "duplicate MPI count: $count" ;; esac
    seen="$seen$count "
done
case $seen in *" $DEFAULT_MPI "*) ;; *) fail "DEFAULT_MPI=$DEFAULT_MPI must be included in MPI_COUNTS='$MPI_COUNTS'" ;; esac

# Build or refresh shared tools from their centralized sources.
make -C "$CODE_DIR" param analysis || fail "shared MPI tools failed to build"
for exe in param_set_bif_basin_mpi param_set_mpi_region analysis_check_mpi_bifurcation; do
    [ -x "$CODE_DIR/$exe" ] || fail "shared executable missing: $CODE_DIR/$exe"
done

for output in .bifbsn_mpi.tmp mpireg.bin; do
    [ ! -e "$MAP_DIR/$output" ] || echo "WARNING: existing output will be replaced: $MAP_DIR/$output" >&2
done
for count in $MPI_COUNTS; do
    [ ! -e "$MAP_DIR/mpireg-$count.bin" ] || echo "WARNING: existing output will be replaced: $MAP_DIR/mpireg-$count.bin" >&2
done
# The temporary basin raster is removed on success, error, or interruption.
cleanup() { rm -f "$MAP_DIR/.bifbsn_mpi.tmp"; }
trap cleanup EXIT
trap 'cleanup; exit 1' HUP INT TERM
BIFPRM_CHECKSUM_BEFORE=$(cksum "$MAP_DIR/bifprm.txt")
cd "$SCRIPT_DIR"
echo "MPI mode: $MPIMODE; requested partitions: $MPI_COUNTS; default: $DEFAULT_MPI"
"$CODE_DIR/param_set_bif_basin_mpi" "$MPIMODE"
[ -s "$MAP_DIR/.bifbsn_mpi.tmp" ] || fail "MPI basin aggregation did not create its temporary raster"
for count in $MPI_COUNTS; do
    "$CODE_DIR/param_set_mpi_region" "$count"
    [ -s "$MAP_DIR/mpireg-$count.bin" ] || fail "expected output missing: mpireg-$count.bin"
done
cp "$MAP_DIR/mpireg-$DEFAULT_MPI.bin" "$MAP_DIR/mpireg.bin"
[ -s "$MAP_DIR/mpireg.bin" ] || fail "could not create mpireg.bin"
BIFPRM_CHECKSUM_AFTER=$(cksum "$MAP_DIR/bifprm.txt")
[ "$BIFPRM_CHECKSUM_BEFORE" = "$BIFPRM_CHECKSUM_AFTER" ] || fail "bifprm.txt changed unexpectedly; restore from backup before proceeding"
rm -f "$MAP_DIR/.bifbsn_mpi.tmp"
trap - EXIT HUP INT TERM

# Report the default map's hydraulic connectivity exclusions immediately.
echo "@@@ Bifurcation connectivity for mpireg-$DEFAULT_MPI.bin"
(cd "$MAP_DIR" && "$CODE_DIR/analysis_check_mpi_bifurcation" "mpireg-$DEFAULT_MPI.bin")
echo ""
echo "To analyze another partition from the map root:"
echo "  ./script_analysis/a01-check_mpi_bifurcation.sh mpireg-8.bin"
echo "  ./script_analysis/a01-check_mpi_bifurcation.sh mpireg-8.bin --csv dropped_bifurcations.csv"
echo "Created MPI region maps in $MAP_DIR"
