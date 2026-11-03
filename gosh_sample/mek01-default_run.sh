#!/bin/sh
# Run a 40-day, non-MPI CaMa-Flood simulation on the Mekong sample.
# Map: map/mekong_15min; forcing: inp/test-mek_1deg (daily mm/day).
# Output, executable, and namelist: out/mek01-default_run/.
#
# Execution requirement:
#   Run this script from the gosh_sample/ directory.
#
# Example:
#   cd gosh_sample
#   ./mek01-default_run.sh
set -eu

# CaMa-Flood base directory (contains src_main/, map/, and inp/).
CMF_BASE=$(cd .. && pwd)

# Simulation working directory; the model executable and outputs are placed here.
RUNDIR="${CMF_BASE}/out/mek01-default_run"

# -------------------------- User settings ------------------------------
# Simulation interval. The end time is exclusive; this interval is 40 days.
START_YEAR=2000               # Start year.
START_MONTH=1                 # Start month.
START_DAY=1                   # Start day.
START_HOUR=0                  # Start hour (00:00).
END_YEAR=2000                 # End year.
END_MONTH=2                   # End month.
END_DAY=10                    # End day, at 00:00 (exclusive).
END_HOUR=0                    # End hour (00:00).


# CPU execution settings; this sample runs with OpenMP and without MPI.
OMP_NUM_THREADS=4             # Number of OpenMP threads (non-MPI execution).

# Model features and initial conditions.
LADPSTP=".TRUE."             # Let the model adjust the timestep to its stability criteria.
LPTHOUT=".TRUE."             # Enable bifurcation flow paths using bifprm.txt.
LDAMOUT=".FALSE."            # Disable reservoir operation.
LRESTART=".FALSE."           # Initialize storage to zero; do not read a restart.
DT=3600                       # Maximum timestep setting, in seconds.

# Daily binary runoff sample and its matching map-specific input matrix.
ROFFTAG="test-mek_1deg"      # Select inp/test-mek_1deg/runoff and its input matrix.
IFRQ_INP=24                   # Read runoff once every 24 hours.
DROFUNIT=86400000             # Convert daily runoff in mm/day to the model input rate.
LINPCDF=".FALSE."             # Read binary runoff files rather than NetCDF.
LINPDAY=".TRUE."              # Read one dated runoff file per day.
LINTERP=".TRUE."              # Map runoff using inpmat_test-mek_1deg.bin.
CROFPRE="Roff____"            # Daily filename prefix: Roff____YYYYMMDD.one.
CROFSUF=".one"                # Daily filename suffix.

# Daily binary output settings.
CVARSOUT="outflw,storge,sfcelv,flddph,pthflw"  # Variables saved in binary output.
IFRQ_OUT=24                   # Write output every 24 hours.
LOUTCDF=".FALSE."            # Write binary files rather than NetCDF.
# Model output directory, relative to the model execution directory.
# "./" means outputs are saved in RUNDIR,
# because CaMa-Flood is executed after "cd ${RUNDIR}".
COUTDIR="./"

# Restart output remains enabled at the model's standard end-of-run behavior.
CVNREST="restart"            # Prefix used for the final restart files.
IFRQ_RST=0                    # Do not write intermediate periodic restart files.
# ----------------------------------------------------------------------
# User settings end here. Paths, namelist generation, and execution follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare the Mekong experiment directory"
trap '
    EXIT_STATUS=${?}
    trap - 0
    if [ "${EXIT_STATUS}" -ne 0 ]; then
        printf "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
        printf "!!! ERROR: %s FAILED !!!\n" "${SCRIPT_NAME}" >&2
        printf "!!! Step: %s\n" "${STEP}" >&2
        printf "!!! Exit status: %s\n" "${EXIT_STATUS}" >&2
        printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
    fi
    exit "${EXIT_STATUS}"
' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

# ----------------------------------------------------------------------
# End of error handling.
# ----------------------------------------------------------------------

# Reference input directories: river map and daily runoff files.
MAPDIR="${CMF_BASE}/map/mekong_15min"
RUNOFF_DIR="${CMF_BASE}/inp/${ROFFTAG}/runoff"

# The dimension descriptor and matrix were generated for this map/runoff pair.
INPDIM="${MAPDIR}/inpdim_${ROFFTAG}.txt"
INPMAT="${MAPDIR}/inpmat_${ROFFTAG}.bin"

# Source executable; a copy is placed in RUNDIR and executed there.
EXEC_CMF="${CMF_BASE}/src_main/MAIN_cmf"
NAMELIST="${RUNDIR}/input_cmf.nam"
MODEL_LOG="${RUNDIR}/log_CaMa.txt"
CONSOLE_LOG="${RUNDIR}/run.stdout.log"
RUN_TIME_LOG="${RUNDIR}/run_time.log"

# Show resolved input and output locations before preparing the model.
printf 'CMF_BASE = %s\n' "${CMF_BASE}"
printf 'RUNDIR = %s\n' "${RUNDIR}"
printf 'MAPDIR = %s\n' "${MAPDIR}"
printf 'RUNOFF_DIR = %s\n' "${RUNOFF_DIR}"

# Keep prior results intact. Choose another RUNDIR or archive this directory to rerun.
if [ -e "${NAMELIST}" ] || [ -e "${MODEL_LOG}" ] || [ -e "${CONSOLE_LOG}" ] || [ -e "${RUN_TIME_LOG}" ]; then
    printf 'ERROR: experiment files already exist in %s\n' "${RUNDIR}" >&2
    printf 'Set RUNDIR to a new directory or archive the existing experiment first.\n' >&2
    exit 1
fi

mkdir -p "${RUNDIR}"
cp "${EXEC_CMF}" "${RUNDIR}/MAIN_cmf"

# Run in RUNDIR so the namelist, model logs, restart, and default outputs stay together.
cd "${RUNDIR}"

# Write the namelist in the run directory; the model reads input_cmf.nam from its CWD.
STEP="Write the Mekong run namelist"
printf '\n==================================================\n[STEP 1/2] Configure the Mekong simulation\n==================================================\n\n'
cat > input_cmf.nam <<EOF
&NRUNVER
LADPSTP  = ${LADPSTP}
LPTHOUT  = ${LPTHOUT}
LDAMOUT  = ${LDAMOUT}
LRESTART = ${LRESTART}
/
&NDIMTIME
CDIMINFO = "${INPDIM}"
DT       = ${DT}
IFRQ_INP = ${IFRQ_INP}
/
&NPARAM
PMANRIV  = 0.03D0
PMANFLD  = 0.10D0
PDSTMTH  = 10000.D0
PCADP    = 0.7D0
/
&NSIMTIME
SYEAR = ${START_YEAR}
SMON  = ${START_MONTH}
SDAY  = ${START_DAY}
SHOUR = ${START_HOUR}
EYEAR = ${END_YEAR}
EMON  = ${END_MONTH}
EDAY  = ${END_DAY}
EHOUR = ${END_HOUR}
/
&NMAP
LMAPCDF = .FALSE.
CNEXTXY = "${MAPDIR}/nextxy.bin"
CGRAREA = "${MAPDIR}/ctmare.bin"
CELEVTN = "${MAPDIR}/elevtn.bin"
CNXTDST = "${MAPDIR}/nxtdst.bin"
CRIVLEN = "${MAPDIR}/rivlen.bin"
CFLDHGT = "${MAPDIR}/fldhgt.bin"
CRIVWTH = "${MAPDIR}/rivwth_merit.bin"
CRIVHGT = "${MAPDIR}/rivhgt.bin"
CRIVMAN = "${MAPDIR}/rivman.bin"
CPTHOUT = "${MAPDIR}/bifprm.txt"
/
&NRESTART
CRESTDIR = "./"
CVNREST  = "${CVNREST}"
LRESTCDF = .FALSE.
IFRQ_RST = ${IFRQ_RST}
/
&NFORCE
LINPCDF  = ${LINPCDF}
LINPDAY  = ${LINPDAY}
LINTERP  = ${LINTERP}
CINPMAT  = "${INPMAT}"
DROFUNIT = ${DROFUNIT}
CROFDIR  = "${RUNOFF_DIR}"
CROFPRE  = "${CROFPRE}"
CROFSUF  = "${CROFSUF}"
/
&NOUTPUT
COUTDIR  = "${COUTDIR}"
CVARSOUT = "${CVARSOUT}"
COUTTAG  = "${START_YEAR}"
LOUTVEC  = .FALSE.
LOUTCDF  = ${LOUTCDF}
IFRQ_OUT = ${IFRQ_OUT}
/
EOF

# Run one continuous period so the model reads daily files across the month boundary.
STEP="Run the non-MPI model with bifurcation enabled"
printf '\n==================================================\n[STEP 2/2] Run CaMa-Flood v5 from %s to %s-%02d-%02d\n==================================================\n\n' \
    "${START_YEAR}" "${END_YEAR}" "${END_MONTH}" "${END_DAY}"

# Measure only the model process; setup and executable-copy time are excluded.
START_DATETIME=$(date)
printf 'Start: %s\n' "${START_DATETIME}" | tee "${RUN_TIME_LOG}"
START_TIME=$(date +%s)

if OMP_NUM_THREADS="${OMP_NUM_THREADS}" ./MAIN_cmf > "${CONSOLE_LOG}" 2>&1; then
    RUN_STATUS=0
else
    RUN_STATUS=${?}
fi

END_TIME=$(date +%s)
END_DATETIME=$(date)
ELAPSED_SECONDS=$((END_TIME - START_TIME))
{
    printf 'End: %s\n' "${END_DATETIME}"
    printf 'Elapsed time: %s seconds\n' "${ELAPSED_SECONDS}"
    printf 'Exit status: %s\n' "${RUN_STATUS}"
} | tee -a "${RUN_TIME_LOG}"

if [ "${RUN_STATUS}" -ne 0 ]; then
    exit "${RUN_STATUS}"
fi

printf '\n==================================================\n[SUCCESS] %s completed\n' "${SCRIPT_NAME}"
printf 'Results: %s\n' "${RUNDIR}"
printf 'Model log: %s\n' "${MODEL_LOG}"
printf 'Console log: %s\n' "${CONSOLE_LOG}"
printf '==================================================\n'
