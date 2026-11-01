#!/bin/sh
# Generate MPI region maps for CaMa-Flood.
# Copy to <map>/script_params/, edit the settings below, and run from any CWD.
#
# Inputs:  params.txt, basin.bin, bifprm.txt, nextxy.bin
# Outputs: mpireg-N.bin for each MPI count, plus mpireg.bin for DEFAULT_MPI
# Usage:   ./s04-mpi_map_setup.sh
set -eu

# Paths
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# Configuration
MPIMODE=normal                 # normal uses 6%; MaxMPI uses 3% for basin merging
MPI_COUNTS="4 8 16"
DEFAULT_MPI=16

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep the script name and current step for the failure summary below.
SCRIPT_NAME=${0##*/}
STEP="Prepare MPI map-preprocessing workflow"
# On exit, remove the temporary basin map, report failures, and return the original status.
trap '
    EXIT_STATUS=$?
    if [ -n "${TEMP_BASIN:-}" ]; then
        rm -f "$TEMP_BASIN" || :
    fi
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

# Programs (compile these with the shared Makefile before running this script).
EXEC_SETBASIN_MPI="$CODE_DIR/param_set_bif_basin_mpi"
EXEC_SETREGION_MPI="$CODE_DIR/param_set_mpi_region"
EXEC_CHECKBIF="$CODE_DIR/analysis_check_mpi_bifurcation"
TEMP_BASIN="$MAP_DIR/bifbsn_mpi.tmp"

# Both preprocessing programs read map inputs relative to script_params/.
cd "$SCRIPT_DIR"

# 1. Merge connected basins for MPI decomposition. bifprm.txt is read only.
STEP="Aggregate basins for MPI mode $MPIMODE"
printf '==================================================\n[STEP 1/4] Aggregate basins for MPI decomposition (%s)\n==================================================\n' "$MPIMODE"
"$EXEC_SETBASIN_MPI" "$MPIMODE"

# 2. Assign the merged basins to each requested number of regions.
for N in $MPI_COUNTS; do
    STEP="Generate mpireg-$N.bin"
    printf '==================================================\n[STEP 2/4] Generate mpireg-%s.bin\n==================================================\n' "$N"
    "$EXEC_SETREGION_MPI" "$N"
done

# 3. Set the standard runtime map to the selected partition count.
STEP="Select mpireg-$DEFAULT_MPI.bin as the default partition"
printf '==================================================\n[STEP 3/4] Select default partition mpireg-%s.bin\n==================================================\n' "$DEFAULT_MPI"
cp "$MAP_DIR/mpireg-$DEFAULT_MPI.bin" "$MAP_DIR/mpireg.bin"

# 4. Report bifurcation pathways excluded by the default partition.
STEP="Analyze bifurcation connectivity for mpireg-$DEFAULT_MPI.bin"
printf '==================================================\n[STEP 4/4] Analyze default-partition bifurcation connectivity\n==================================================\n'
(cd "$MAP_DIR" && "$EXEC_CHECKBIF" "mpireg-$DEFAULT_MPI.bin")

# 5. Analyze another partition from the map root when needed.
echo "To analyze another partition from the map root:"
echo "  ./script_analysis/a01-check_mpi_bifurcation.sh mpireg-8.bin"
echo "  ./script_analysis/a01-check_mpi_bifurcation.sh mpireg-8.bin --csv dropped_bifurcations.csv"
STEP="Remove temporary MPI basin-group map"
rm -f "$TEMP_BASIN"
printf '==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
