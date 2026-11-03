#!/bin/sh
# Run yearly Mekong simulations with individual NetCDF maps.
# NetCDF runoff/output and annual Binary restarts connect the two runs.
# Run from gosh_sample/: ./mek02-longterm_netcdf.sh
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
CMF_BASE=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# -------------------------- User settings ------------------------------
# Simulation years are inclusive; the model ends at January 1 of END_YEAR+1.
START_YEAR=2000                  # First simulation year; starts at January 1 00:00.
END_YEAR=2001                    # Last simulation year; extend to continue the annual sequence.

# Runoff dataset and its matching map-specific input matrix.
ROFFTAG="test-mek_1deg_nc"     # Selects inp/test-mek_1deg_nc/ with one NetCDF file per year.
INPMAT_TAG="test-mek_1deg"     # Selects the existing input matrix for the same 1-degree grid.

# Map input files; LMAPCDF=.FALSE. makes the reader select Binary or NetCDF from each extension.
MAPDIR="${CMF_BASE}/map/mekong_15min"  # Mekong river-map root containing params.txt and bifprm.txt.
MAP_NETCDF_DIR="${MAPDIR}/netcdf"      # Individual NetCDF map products.
#NEXTXY="${MAP_NETCDF_DIR}/nextxy.nc"       # Includes river networks affected by regional edges.
NEXTXY="${MAP_NETCDF_DIR}/nextxy_noedge.nc" # Excludes boundary-affected river networks (default).
#RIVWTH="${MAP_NETCDF_DIR}/rivwth.nc"       # Empirical power-law channel width.
RIVWTH="${MAP_NETCDF_DIR}/rivwth_merit.nc"  # MERIT Hydro-merged channel width (default).

# Model execution and physical options; these retain the mek01 sample configuration.
OMP_NUM_THREADS=4                # OpenMP threads for this non-MPI sample.
DT=3600                          # Base model timestep [s]; adaptive stepping may reduce it.
LADPSTP=".TRUE."                 # TRUE: enable adaptive time stepping.
LPTHOUT=".TRUE."                 # TRUE: enable bifurcation routing.
LDAMOUT=".FALSE."                # TRUE: enable reservoir operation; FALSE: disable it.
PMANRIV="0.03D0"                # River Manning roughness coefficient [s m-1/3].
PMANFLD="0.10D0"                # Floodplain Manning roughness coefficient [s m-1/3].
PDSTMTH="10000.D0"              # Downstream distance assigned at a river mouth [m].
PCADP="0.7D0"                    # CFL coefficient used by adaptive timestep control [-].

# Runoff input timing and units.
IFRQ_INP=24                      # Runoff interval [hours]; 24 means daily input.
DROFUNIT=86400000                # Divide input units by this factor to obtain m/s; mm/day example.
CVNROF="runoff"                 # NetCDF runoff variable name.
CVNTIME="time"                  # NetCDF CF time-coordinate variable name.

# Output and restart products.
IFRQ_OUT=24                      # Model-output interval [hours]; 24 means daily output.
CVARSOUT="outflw,storge,sfcelv,flddph,fldfrc" # Comma-separated model output variable names.
LOUTCDF=".TRUE."                # TRUE: write NetCDF model output; FALSE: write Binary output.
IFRQ_RST=0                       # 0: write restart at end only; 1-24: hourly; 30: monthly (also at end).
CVNREST="restart"               # Prefix for binary restart files and their date stamps.
LRESTCDF=".FALSE."              # TRUE: NetCDF restart; FALSE: Binary restart.
RUNDIR="${CMF_BASE}/out/mek02-longterm_mapnc" # Dedicated individual-map test output directory.

# ----------------------------------------------------------------------
# User settings end here. Paths and annual model runs follow.
# ----------------------------------------------------------------------

NAMELIST="${RUNDIR}/input_cmf.nam"
EXEC_CMF="${CMF_BASE}/out/bin/MAIN_cmf_netcdf"
RUNOFF_DIR="${CMF_BASE}/inp/${ROFFTAG}"
INPDIM="${MAPDIR}/inpdim_${INPMAT_TAG}.txt"
INPMAT="${MAPDIR}/inpmat_${INPMAT_TAG}.bin"

printf '\n==================================================\n'
printf '[STEP 1/3] Prepare the individual map test\n'
printf '==================================================\n'
printf 'CMF_BASE = %s\n' "$CMF_BASE"
printf 'RUNDIR = %s\n' "$RUNDIR"
printf 'MAPDIR = %s\n' "$MAPDIR"
printf 'RUNOFF_DIR = %s\n' "$RUNOFF_DIR"
printf '\nOverwriting previous sample results in: %s\n' "$RUNDIR"

mkdir -p "$RUNDIR"
rm -f "$RUNDIR"/o_outflw*.nc "$RUNDIR"/o_storge*.nc \
      "$RUNDIR"/o_sfcelv*.nc "$RUNDIR"/o_flddph*.nc \
      "$RUNDIR"/o_fldfrc*.nc "$RUNDIR"/restart*.bin* \
      "$RUNDIR"/input_cmf*.nam "$RUNDIR"/console_*.log \
      "$RUNDIR"/log_CaMa*.txt "$RUNDIR"/run_time_*.log \
      "$RUNDIR"/MAIN_cmf "$RUNDIR"/mapdata.txt \
      "$RUNDIR"/year*.complete "$RUNDIR"/signature_*.tmp \
      "$RUNDIR"/summary.tsv
cp "$EXEC_CMF" "$RUNDIR/MAIN_cmf"

# Run each calendar year separately, carrying the end-of-year restart forward.
YEAR=$START_YEAR
while [ "$YEAR" -le "$END_YEAR" ]; do
    NEXT_YEAR=$((YEAR + 1))
    RUNOFF_FILE="${RUNOFF_DIR}/runoff${YEAR}.nc"
    YEAR_NAMELIST="${RUNDIR}/input_cmf_${YEAR}.nam"

    printf '\n==================================================\n'
    printf '[STEP 2/3] Configure year %s\n' "$YEAR"
    printf '==================================================\n'

    if [ "$YEAR" -eq "$START_YEAR" ]; then
        LRESTART=".FALSE."
        CRESTSTO="NONE"
    else
        PREVIOUS_RESTART="${RUNDIR}/${CVNREST}$(printf '%04d010100' "$YEAR").bin"
        LRESTART=".TRUE."
        CRESTSTO="$PREVIOUS_RESTART"
    fi

    # Save a year-specific Namelist beside the active one used by the model.
    cat > "$NAMELIST" <<EOF
&NRUNVER
LADPSTP = ${LADPSTP}       ! TRUE enables adaptive time stepping.
LPTHOUT = ${LPTHOUT}       ! TRUE enables bifurcation routing.
LDAMOUT = ${LDAMOUT}       ! TRUE enables reservoir operation; FALSE disables it.
LRESTART = ${LRESTART}     ! TRUE reads initial state from CRESTSTO; FALSE starts without a restart.
/
&NDIMTIME
CDIMINFO = "${INPDIM}"    ! Map and runoff-grid dimensions for input mapping.
DT = ${DT}                 ! Base timestep [s]; adaptive stepping may reduce it.
IFRQ_INP = ${IFRQ_INP}     ! Runoff update interval [hours].
/
&NPARAM
PMANRIV = ${PMANRIV}       ! River Manning roughness coefficient [s m-1/3].
PMANFLD = ${PMANFLD}       ! Floodplain Manning roughness coefficient [s m-1/3].
PDSTMTH = ${PDSTMTH}       ! Downstream distance assigned at a river mouth [m].
PCADP = ${PCADP}           ! CFL coefficient used by adaptive timestep control [-].
/
&NSIMTIME
SYEAR = ${YEAR}            ! First simulation year for this annual run.
SMON = 1                   ! Simulation start month.
SDAY = 1                   ! Simulation start day.
SHOUR = 0                  ! Simulation start hour [0-23].
EYEAR = ${NEXT_YEAR}       ! Exclusive simulation end year (January 1 of the next year).
EMON = 1                   ! Exclusive simulation end month.
EDAY = 1                   ! Exclusive simulation end day.
EHOUR = 0                  ! Exclusive simulation end hour.
/
&NMAP
LMAPCDF = .FALSE.                     ! FALSE uses individual .bin/.nc files; TRUE selects legacy consolidated NetCDF maps.
CNEXTXY = "${NEXTXY}"                ! Downstream river connectivity (nextx/nexty).
CGRAREA = "${MAP_NETCDF_DIR}/ctmare.nc" ! Unit-catchment area [m2].
CELEVTN = "${MAP_NETCDF_DIR}/elevtn.nc" ! Channel-top elevation [m].
CNXTDST = "${MAP_NETCDF_DIR}/nxtdst.nc" ! Distance to downstream unit catchment [m].
CRIVLEN = "${MAP_NETCDF_DIR}/rivlen.nc" ! River-channel length within unit catchment [m].
CFLDHGT = "${MAP_NETCDF_DIR}/fldhgt.nc" ! Floodplain elevation-profile thresholds [m].
CRIVWTH = "${RIVWTH}"                 ! River-channel width [m], selected above.
CRIVHGT = "${MAP_NETCDF_DIR}/rivhgt.nc" ! River-channel depth [m].
CRIVMAN = "${MAP_NETCDF_DIR}/rivman.nc" ! River Manning roughness coefficient.
CPTHOUT = "${MAPDIR}/bifprm.txt"     ! Bifurcation channel parameters.
/
&NRESTART
CRESTSTO = "${CRESTSTO}"  ! Input restart path when LRESTART is true; NONE means no input restart.
CRESTDIR = "./"           ! Directory for restart outputs, relative to the model run directory.
CVNREST = "${CVNREST}"    ! Prefix for dated restart output files.
LRESTCDF = ${LRESTCDF}     ! TRUE writes NetCDF restart; FALSE writes Binary restart.
IFRQ_RST = ${IFRQ_RST}     ! 0=end only; 1-24=hourly; 30=monthly (also writes at end).
/
&NFORCE
LINPCDF = .TRUE.           ! TRUE reads NetCDF runoff; independent of the model-output format.
LINPDAY = .FALSE.          ! FALSE uses one time-series NetCDF file for this annual run.
LINTERP = .TRUE.           ! TRUE maps runoff through CINPMAT; FALSE uses nearest-grid mapping.
CINPMAT = "${INPMAT}"     ! Binary runoff-to-river input-mapping matrix.
DROFUNIT = ${DROFUNIT}     ! Divide input units by this factor to obtain runoff depth rate [m/s].
CROFCDF = "${RUNOFF_FILE}" ! NetCDF runoff file for this simulation year.
CVNROF = "${CVNROF}"      ! Runoff data variable within CROFCDF.
CVNTIME = "${CVNTIME}"    ! CF time-coordinate variable within CROFCDF.
SYEARIN = 0                ! 0 uses CF time metadata to locate the first input record.
/
&NOUTPUT
COUTDIR = "./"            ! Output directory relative to the model run directory.
CVARSOUT = "${CVARSOUT}"  ! Comma-separated list of output variables to write.
COUTTAG = "${YEAR}"       ! Year tag appended to each output filename.
LOUTVEC = .FALSE.          ! TRUE writes vector output; FALSE writes gridded XY output.
LOUTCDF = ${LOUTCDF}       ! TRUE writes NetCDF model output; FALSE writes Binary model output.
IFRQ_OUT = ${IFRQ_OUT}     ! Model-output interval [hours], separate from IFRQ_INP.
/
EOF
    cp "$NAMELIST" "$YEAR_NAMELIST"

    printf '\n==================================================\n'
    printf '[STEP 3/3] Run year %s\n' "$YEAR"
    printf '==================================================\n'
    RUN_START_SECONDS=$(date +%s)
    printf 'Start: %s\n' "$(date)" | tee "${RUNDIR}/run_time_${YEAR}.log"

    if (cd "$RUNDIR" && OMP_NUM_THREADS="$OMP_NUM_THREADS" ./MAIN_cmf > "console_${YEAR}.log" 2>&1); then
        RUN_STATUS=0
    else
        RUN_STATUS=$?
    fi

    ELAPSED_SECONDS=$(($(date +%s) - RUN_START_SECONDS))
    {
        printf 'End: %s\n' "$(date)"
        printf 'Elapsed time: %s seconds\n' "$ELAPSED_SECONDS"
        printf 'Exit status: %s\n' "$RUN_STATUS"
    } | tee -a "${RUNDIR}/run_time_${YEAR}.log"

    if [ "$RUN_STATUS" -ne 0 ]; then
        printf '[ERROR] Model run failed for year %s (status %s).\n' "$YEAR" "$RUN_STATUS" >&2
        exit "$RUN_STATUS"
    fi

    cp "${RUNDIR}/log_CaMa.txt" "${RUNDIR}/log_CaMa_${YEAR}.txt"
    printf '[SUCCESS] Year %s completed.\n' "$YEAR"
    YEAR=$NEXT_YEAR
done

printf '\n[SUCCESS] Annual simulations %s-%s completed.\n' "$START_YEAR" "$END_YEAR"
printf 'Results: %s\n' "$RUNDIR"
