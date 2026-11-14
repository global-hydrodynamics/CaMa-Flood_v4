#!/bin/sh
# Run yearly Mekong simulations with legacy consolidated NetCDF maps.
# NetCDF runoff/output and annual Binary restarts connect the two runs.
# Run from gosh_test/: ./legacy02-longterm_netcdf.sh
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

# Legacy consolidated NetCDF map inputs.
MAPDIR="${CMF_BASE}/map/mekong_15min"   # Mekong map root containing params.txt and bifprm.txt.
MAP_NETCDF_DIR="${MAPDIR}/netcdf"       # Directory containing rivclim.nc and rivpar.nc.
LEGACY_RIVCLIM="${MAP_NETCDF_DIR}/rivclim.nc" # River network and topography container.
LEGACY_RIVPAR="${MAP_NETCDF_DIR}/rivpar.nc"   # River-channel parameter container.

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
CVNREST="restart"               # Prefix for dated restart files.
LRESTCDF=".TRUE."               # TRUE: NetCDF restart; FALSE: Binary restart.
RUNDIR="${CMF_BASE}/out/legacy02-longterm_netcdf" # Dedicated legacy-map test output directory.

# ----------------------------------------------------------------------
# User settings end here. Paths and annual model runs follow.
# ----------------------------------------------------------------------

NAMELIST="${RUNDIR}/input_cmf.nam"
EXEC_CMF="${CMF_BASE}/out/bin/MAIN_cmf_netcdf"
RUNOFF_DIR="${CMF_BASE}/inp/${ROFFTAG}"
INPDIM="${MAPDIR}/inpdim_${INPMAT_TAG}.txt"
INPMAT="${MAPDIR}/inpmat_${INPMAT_TAG}.bin"

printf '\n==================================================\n'
printf '[STEP 1/3] Prepare the legacy consolidated map test\n'
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
      "$RUNDIR"/restart*.nc \
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
    if [ ! -f "$RUNOFF_FILE" ]; then
        printf '[ERROR] Missing annual runoff file: %s\n' "$RUNOFF_FILE" >&2
        exit 1
    fi

    printf '\n==================================================\n'
    printf '[STEP 2/3] Configure year %s\n' "$YEAR"
    printf '==================================================\n'

    if [ "$YEAR" -eq "$START_YEAR" ]; then
        LRESTART=".FALSE."
        CRESTSTO="NONE"
    else
        PREVIOUS_RESTART="${RUNDIR}/${CVNREST}$(printf '%04d010100' "$YEAR").nc"
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
LMAPCDF = .TRUE.                      ! TRUE selects the legacy consolidated NetCDF map interface.
CRIVCLINC = "${LEGACY_RIVCLIM}"      ! Consolidated connectivity and topography maps.
CRIVPARNC = "${LEGACY_RIVPAR}"       ! Consolidated river-channel parameters.
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
CROFFREQ = "YEAR"          ! One NetCDF file per calendar year; the reader switches at January 1.
LINTERP = .TRUE.           ! TRUE maps runoff through CINPMAT; FALSE uses nearest-grid mapping.
CINPMAT = "${INPMAT}"     ! Binary runoff-to-river input-mapping matrix.
DROFUNIT = ${DROFUNIT}     ! Divide input units by this factor to obtain runoff depth rate [m/s].
CVNTIME = "${CVNTIME}"    ! CF time-coordinate variable within each annual file.
CVNROF = "${CVNROF}"      ! Runoff data variable within each annual file.
CROFDIR = "${RUNOFF_DIR}" ! Directory containing annual runoff NetCDF files.
CROFPRE = "runoff"         ! Annual filename prefix: runoffYYYY.nc.
CROFSUF = ".nc"            ! Annual filename suffix.
/
&NOUTPUT
CMF_BASE = "${CMF_BASE}"     ! CaMa-Flood repository/package root.
COUTVAR_TABLE = ""            ! Defaults to CMF_BASE/variable_table/output_variables.csv.
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
