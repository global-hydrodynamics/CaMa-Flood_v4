#!/bin/sh
# Generate MPI region maps for CaMa-Flood.
# Copy to <map>/script_params/, edit the settings below, and run from any CWD.
#
# Inputs:  params.txt, basin.bin, bifprm.csv, nextxy.bin
# Outputs: bifcol_mpi.bin; mpireg-N.bin for each MPI count; default connectivity report
# Optional: ./s04-mpi_map_setup.sh check_map also writes check_bif_bsnmod_mpi.bin
# Usage:   ./s04-mpi_map_setup.sh [check_map]
set -eu

# Paths
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
MAP_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
CODE_DIR=$(CDPATH= cd "$SCRIPT_DIR/../../tools/code" && pwd -P)

# MPI partition configuration for this map.
MPIMODE=normal       # Basin merging mode: normal (6%) or MaxMPI (3%).
MPI_COUNTS="4 8 16"  # Requested number of MPI regions; separate values with spaces.
DEFAULT_MPI=16       # Partition count used for the connectivity analysis; include it above.

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare MPI map-preprocessing workflow"
# The exit trap removes the temporary basin map, reports failures, and preserves status. ${TEMP_BASIN:-} is empty if the path was not set before an early exit.
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

# ----------------------------------------------------------------------
# End of error handling.
# ----------------------------------------------------------------------

# Programs (compile these with the shared Makefile before running this script).
EXEC_SETBASIN_MPI="$CODE_DIR/param_mpi_set_basin"
EXEC_SETREGION_MPI="$CODE_DIR/param_mpi_set_region"
EXEC_CHECKBIF="$CODE_DIR/analysis_check_mpi_bifurcation"
TEMP_BASIN="$MAP_DIR/bifbsn_mpi.tmp"

# Both preprocessing programs read map inputs relative to script_params/.
cd "$SCRIPT_DIR"

# 1. Merge connected basins for MPI decomposition. bifprm.csv is read only.
STEP="Aggregate basins for MPI mode $MPIMODE"
printf '\n==================================================\n[STEP 1/3] Aggregate basins for MPI decomposition (%s)\n==================================================\n\n' "$MPIMODE"
if [ "${1:-}" = "check_map" ]; then
    "$EXEC_SETBASIN_MPI" "$MPIMODE" check_map
else
    "$EXEC_SETBASIN_MPI" "$MPIMODE"
fi

# 2. Assign the merged basins to each requested number of regions.
# Split the space-separated configuration into one MPI count per loop.
for N in $MPI_COUNTS; do
    STEP="Generate mpireg-$N.bin"
    printf '\n==================================================\n[STEP 2/3] Generate mpireg-%s.bin\n==================================================\n\n' "$N"
    "$EXEC_SETREGION_MPI" "$N"
done

# 3. Analyze bifurcation pathways for the selected default partition.
STEP="Analyze bifurcation connectivity for mpireg-$DEFAULT_MPI.bin"
printf '\n==================================================\n[STEP 3/3] Analyze default partition mpireg-%s.bin\n==================================================\n\n' "$DEFAULT_MPI"
(cd "$MAP_DIR" && "$EXEC_CHECKBIF" "mpireg-$DEFAULT_MPI.bin")

# Optional manual visualization and analysis for another partition from the map root.
echo "To visualize and analyze another partition from the map root:"
echo "  ../tools/code/analysis_mpi_region_color mpireg-8.bin"
echo "  ./script_analysis/a05-check_mpi_bifurcation.sh mpireg-8.bin"
echo "  ./script_analysis/a05-check_mpi_bifurcation.sh mpireg-8.bin --csv dropped_bifurcations.csv"

STEP="Remove temporary MPI basin-group map"
rm -f "$TEMP_BASIN"

printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
