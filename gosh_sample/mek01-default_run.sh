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
export CMF_BASE

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

# River map configuration.
MAPDIR="${CMF_BASE}/map/mekong_15min"

# River network connectivity.
# nextxy.bin:        Includes rivers draining outside the regional domain.
# nextxy_noedge.bin: Excludes river networks affected by regional boundaries (default).
#NEXTXY="${MAPDIR}/nextxy.bin"
NEXTXY="${MAPDIR}/nextxy_noedge.bin"

# River channel width.
# rivwth_merit.bin: Satellite-derived river width (default).
# rivwth.bin:       Power-law river width estimated from upstream area.
#RIVWTH="${MAPDIR}/rivwth.bin"
RIVWTH="${MAPDIR}/rivwth_merit.bin"

# CPU execution settings; this sample runs with OpenMP and without MPI.
OMP_NUM_THREADS=4             # Number of OpenMP threads (non-MPI execution).

# Model features and initial conditions.
LADPSTP=".TRUE."              # TRUE: use adaptive time stepping.
LPTHOUT=".TRUE."              # TRUE: activate bifurcation routing.
LDAMOUT=".FALSE."             # TRUE: enable reservoir operation; FALSE: disable.
LRESTART=".FALSE."            # TRUE: read restart initial conditions; FALSE: initialize without restart data.
DT=3600                       # Base timestep [s], multiple of 60; adaptive stepping may adjust it.

# Daily binary runoff sample and its matching map-specific input matrix.
ROFFTAG="test-mek_1deg"       # Select inp/test-mek_1deg/runoff and its input matrix.
IFRQ_INP=24                   # Runoff update interval [hours]; examples: 1, 2, 3, 6, 12, 24.
DROFUNIT=86400000             # Input runoff units / DROFUNIT -> m/s; 86400000 converts mm/day.
LINPCDF=".FALSE."             # TRUE: read NetCDF runoff; FALSE: read binary runoff.
CROFFREQ="DAY"                 # Explicit daily grouping; records are indexed from 00:00 each date.
LINTERP=".TRUE."              # TRUE: map runoff with input matrix; FALSE: use nearest-grid conversion.
CROFPRE="Roff____"            # Daily filename prefix: Roff____YYYYMMDD.one.
CROFSUF=".one"                # Daily filename suffix.

# Daily binary output settings.
CVARSOUT="outflw,storge,sfcelv,flddph,fldfrc"  # Variables saved in binary output.
IFRQ_OUT=24                   # Output interval [hours]; examples: 1, 2, 3, 6, 12, 24.
LOUTCDF=".FALSE."             # TRUE: write NetCDF; FALSE: write binary output.

# Model output directory, relative to the model execution directory.
# "./" means outputs are saved in RUNDIR,
# because CaMa-Flood is executed after "cd ${RUNDIR}".
COUTDIR="./"

# Restart output remains enabled at the model's standard end-of-run behavior.
CVNREST="restart"            # Prefix used for restart-output filenames.
IFRQ_RST=0                    # 0=end only; 1-24=hour interval; 30=monthly (nonzero also writes at end).
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

# Reference input directory for daily runoff files.
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

# Replace only files generated by this sample; leave other files in RUNDIR untouched.
printf 'CMF_BASE = %s\n' "${CMF_BASE}"
printf 'RUNDIR = %s\n' "${RUNDIR}"
printf 'MAPDIR = %s\n' "${MAPDIR}"
printf 'RUNOFF_DIR = %s\n' "${RUNOFF_DIR}"
printf 'Overwriting previous sample results in: %s\n' "${RUNDIR}"

mkdir -p "${RUNDIR}"
rm -f "${RUNDIR}/MAIN_cmf" \
    "${RUNDIR}/input_cmf.nam" "${RUNDIR}/log_CaMa.txt" \
    "${RUNDIR}/mapdata.txt" "${RUNDIR}/run.stdout.log" "${RUNDIR}/run_time.log"

# Remove this sample's output variables, including old tags and the former pthflw output.
rm -f "${RUNDIR}/outflw"* "${RUNDIR}/storge"* "${RUNDIR}/sfcelv"* \
    "${RUNDIR}/flddph"* "${RUNDIR}/fldfrc"* "${RUNDIR}/pthflw"* \
    "${RUNDIR}/o_outflw"* "${RUNDIR}/o_storge"* "${RUNDIR}/o_sfcelv"* \
    "${RUNDIR}/o_flddph"* "${RUNDIR}/o_fldfrc"* "${RUNDIR}/o_pthflw"*

# Restart files use the configured prefix and may include a bifurcation .pth companion.
rm -f "${RUNDIR}/${CVNREST}"*

cp "${EXEC_CMF}" "${RUNDIR}/MAIN_cmf"

# Run in RUNDIR so the namelist, model logs, restart, and default outputs stay together.
cd "${RUNDIR}"

# Write the namelist in the run directory; the model reads input_cmf.nam from its CWD.
STEP="Write the Mekong run namelist"
printf '\n==================================================\n[STEP 1/2] Configure the Mekong simulation\n==================================================\n\n'
cat > input_cmf.nam <<EOF
&NRUNVER
LADPSTP  = ${LADPSTP}        ! TRUE: use adaptive time stepping.
LPTHOUT  = ${LPTHOUT}        ! TRUE: activate bifurcation routing.
LDAMOUT  = ${LDAMOUT}        ! TRUE: enable reservoir operation; FALSE: disable.
LRESTART = ${LRESTART}       ! TRUE: read restart initial conditions; FALSE: initialize without restart data.
/
&NDIMTIME
CDIMINFO = "${INPDIM}"       ! Map and runoff-grid dimensions and input-matrix metadata.
DT       = ${DT}             ! Base timestep [s], multiple of 60; adaptive stepping may adjust it.
IFRQ_INP = ${IFRQ_INP}       ! Runoff update interval [hours]; examples: 1, 2, 3, 6, 12, 24.
/
&NPARAM
PMANRIV  = 0.03D0            ! River Manning roughness coefficient [s m-1/3].
PMANFLD  = 0.10D0            ! Floodplain Manning roughness coefficient [s m-1/3].
PDSTMTH  = 10000.D0          ! Downstream distance at river mouth [m].
PCADP    = 0.7D0             ! CFL coefficient for adaptive time-step control [-].
/
&NSIMTIME
SYEAR = ${START_YEAR}        ! Simulation start year.
SMON  = ${START_MONTH}       ! Simulation start month.
SDAY  = ${START_DAY}         ! Simulation start day.
SHOUR = ${START_HOUR}        ! Simulation start hour [0-23].
EYEAR = ${END_YEAR}          ! Simulation end year (end time is exclusive).
EMON  = ${END_MONTH}         ! Simulation end month (end time is exclusive).
EDAY  = ${END_DAY}           ! Simulation end day (end time is exclusive).
EHOUR = ${END_HOUR}          ! Simulation end hour (end time is exclusive).
/
&NMAP
LMAPCDF = .FALSE.                   ! TRUE: read NetCDF maps; FALSE: read binary maps.
CNEXTXY = "${NEXTXY}"               ! River-network downstream connectivity.
CGRAREA = "${MAPDIR}/ctmare.bin"    ! Unit catchment area [m2].
CELEVTN = "${MAPDIR}/elevtn.bin"    ! River-bank elevation [m].
CNXTDST = "${MAPDIR}/nxtdst.bin"    ! Distance to downstream outlet [m].
CRIVLEN = "${MAPDIR}/rivlen.bin"    ! River channel length [m].
CFLDHGT = "${MAPDIR}/fldhgt.bin"    ! Floodplain elevation profile [m].
CRIVWTH = "${RIVWTH}"               ! River channel width [m].
CRIVHGT = "${MAPDIR}/rivhgt.bin"    ! River channel depth [m].
CRIVMAN = "${MAPDIR}/rivman.bin"    ! River Manning roughness [s m-1/3].
CPTHOUT = "${MAPDIR}/bifprm.txt"    ! Bifurcation channel parameters.
/
&NRESTART
CRESTDIR = "./"              ! Restart-output directory relative to the run directory.
CVNREST  = "${CVNREST}"      ! Prefix for restart-output filenames.
LRESTCDF = .FALSE.           ! TRUE: write NetCDF restart; FALSE: write binary restart.
IFRQ_RST = ${IFRQ_RST}       ! 0=end only; 1-24=hour interval; 30=monthly (nonzero also writes at end).
/
&NFORCE
LINPCDF  = ${LINPCDF}         ! TRUE: read NetCDF runoff; FALSE: read binary runoff.
CROFFREQ = "${CROFFREQ}"     ! Explicit grouping mode; DAY/MON/YEAR/SEQ.
LINTERP  = ${LINTERP}         ! TRUE: map runoff with input matrix; FALSE: use nearest-grid conversion.
CINPMAT  = "${INPMAT}"        ! Runoff-to-river input-mapping matrix.
DROFUNIT = ${DROFUNIT}        ! Input runoff units / DROFUNIT -> m/s; 86400000 converts mm/day.
CROFDIR  = "${RUNOFF_DIR}"    ! Directory containing runoff input files.
CROFPRE  = "${CROFPRE}"       ! Runoff filename prefix before the date.
CROFSUF  = "${CROFSUF}"       ! Runoff filename suffix.
/
&NOUTPUT
COUTDIR  = "${COUTDIR}"       ! Output directory relative to the model run directory.
CVARSOUT = "${CVARSOUT}"      ! Comma-separated list of output variables.
COUTTAG  = "${START_YEAR}"    ! Tag appended to output filenames.
LOUTVEC  = .FALSE.            ! TRUE: write vector output; FALSE: write gridded XY output.
LOUTCDF  = ${LOUTCDF}         ! TRUE: write NetCDF; FALSE: write binary output.
IFRQ_OUT = ${IFRQ_OUT}        ! Output interval [hours]; examples: 1, 2, 3, 6, 12, 24.
/
EOF

# Create descriptors before the model starts so GrADS can inspect written records during a run.
if [ "${LOUTCDF}" = ".FALSE." ]; then
    STEP="Generate planned GrADS CTL files"
    printf '\n==================================================\n[STEP 2/3] Generate planned binary-output CTLs\n==================================================\n\n'
    if python3 "${CMF_BASE}/tools/generate_output_ctl.py" \
        --run-dir "${RUNDIR}" \
        --mode planned; then
        :
    else
        CTL_STATUS=${?}
        printf '[ERROR] CTL generation failed; the CaMa-Flood model was not started.\n' >&2
        exit "${CTL_STATUS}"
    fi
fi

# Run one continuous period so the model reads daily files across the month boundary.
STEP="Run the non-MPI model with bifurcation enabled"
if [ "${LOUTCDF}" = ".FALSE." ]; then
    MODEL_STEP="3/3"
else
    MODEL_STEP="2/2"
fi
printf '\n==================================================\n[STEP %s] Run CaMa-Flood v5 from %s to %s-%02d-%02d\n==================================================\n\n' \
    "${MODEL_STEP}" "${START_YEAR}" "${END_YEAR}" "${END_MONTH}" "${END_DAY}"

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

STEP="Generate GrADS CTLs for Binary Restart files"
printf '\n==================================================\n'
printf '[STEP 3/3] Generate GrADS CTLs for Binary Restart files\n'
printf '==================================================\n\n'
python3 "${CMF_BASE}/tools/generate_restart_ctl.py" --run-dir "${RUNDIR}"

printf '\n==================================================\n[SUCCESS] %s completed\n' "${SCRIPT_NAME}"
printf 'Results: %s\n' "${RUNDIR}"
printf 'Model log: %s\n' "${MODEL_LOG}"
printf 'Console log: %s\n' "${CONSOLE_LOG}"
printf '==================================================\n'
