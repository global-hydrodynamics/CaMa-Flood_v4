#!/bin/sh
# Run a one-year, non-MPI CaMa-Flood simulation on the Mekong sample.
# Map: map/mekong_15min; forcing: inp/test-mek_1deg (daily mm/day).
# Output and namelist: out/mek01-default_run/.
set -eu

# Resolve the repository from this script so it can be launched from any CWD.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
REPO_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# -------------------------- User settings ------------------------------
START_YEAR=2000              # Simulation start: January 1 at 00:00.
END_YEAR=2001                # Simulation end: January 1 at 00:00 (exclusive).
RUN_DIR="$REPO_DIR/out/mek01-default_run"
OMP_NUM_THREADS=1             # Serial CPU execution; MPI is not enabled.

# Model features and initial conditions.
LADPSTP=".TRUE."             # Adaptive timestep.
LPTHOUT=".TRUE."             # Standard bifurcation flow using bifprm.txt.
LDAMOUT=".FALSE."            # Reservoir operation is disabled.
LRESTART=".FALSE."           # Start from zero storage.
DT=3600                       # Maximum timestep setting (seconds).

# Daily binary runoff sample and its matching map-specific input matrix.
ROFFTAG="test-mek_1deg"
IFRQ_INP=24                   # Runoff update interval (hours).
DROFUNIT=86400000             # Convert daily mm of runoff depth to m/s.
LINPCDF=".FALSE."             # Read plain binary runoff.
LINPDAY=".TRUE."              # Read a separate file for each day.
LINTERP=".TRUE."              # Apply the input matrix.
CROFPRE="Roff____"
CROFSUF=".one"

# Daily binary outputs.
CVARSOUT="outflw,storge,sfcelv,flddph,pthflw"
IFRQ_OUT=24                   # Output interval (hours).
LOUTCDF=".FALSE."
# ----------------------------------------------------------------------
# User settings end here. Namelist generation and model execution follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare the Mekong experiment directory"
trap '
    EXIT_STATUS=$?
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

FMAP="$REPO_DIR/map/mekong_15min"
RUNOFF_DIR="$REPO_DIR/inp/$ROFFTAG/runoff"
INPDIM="$FMAP/inpdim_$ROFFTAG.txt"
INPMAT="$FMAP/inpmat_$ROFFTAG.bin"
EXEC_CMF="$REPO_DIR/src_main/MAIN_cmf"
NAMELIST="$RUN_DIR/input_cmf.nam"
MODEL_LOG="$RUN_DIR/log_CaMa.txt"
CONSOLE_LOG="$RUN_DIR/run.stdout.log"

# Keep prior results intact. Choose another RUN_DIR or archive this directory to rerun.
if [ -e "$NAMELIST" ] || [ -e "$MODEL_LOG" ] || [ -e "$CONSOLE_LOG" ]; then
    printf 'ERROR: experiment files already exist in %s\n' "$RUN_DIR" >&2
    printf 'Set RUN_DIR to a new directory or archive the existing experiment first.\n' >&2
    exit 1
fi

mkdir -p "$RUN_DIR"
cd "$RUN_DIR"

# Write the namelist in the run directory; the model reads input_cmf.nam from its CWD.
STEP="Write the Mekong run namelist"
printf '\n==================================================\n[STEP 1/2] Configure the Mekong simulation\n==================================================\n\n'
cat > input_cmf.nam <<EOF
&NRUNVER
LADPSTP  = $LADPSTP
LPTHOUT  = $LPTHOUT
LDAMOUT  = $LDAMOUT
LRESTART = $LRESTART
/
&NDIMTIME
CDIMINFO = "$INPDIM"
DT       = $DT
IFRQ_INP = $IFRQ_INP
/
&NPARAM
PMANRIV  = 0.03D0
PMANFLD  = 0.10D0
PDSTMTH  = 10000.D0
PCADP    = 0.7D0
/
&NSIMTIME
SYEAR = $START_YEAR
SMON  = 1
SDAY  = 1
SHOUR = 0
EYEAR = $END_YEAR
EMON  = 1
EDAY  = 1
EHOUR = 0
/
&NMAP
LMAPCDF = .FALSE.
CNEXTXY = "$FMAP/nextxy.bin"
CGRAREA = "$FMAP/ctmare.bin"
CELEVTN = "$FMAP/elevtn.bin"
CNXTDST = "$FMAP/nxtdst.bin"
CRIVLEN = "$FMAP/rivlen.bin"
CFLDHGT = "$FMAP/fldhgt.bin"
CRIVWTH = "$FMAP/rivwth_merit.bin"
CRIVHGT = "$FMAP/rivhgt.bin"
CRIVMAN = "$FMAP/rivman.bin"
CPTHOUT = "$FMAP/bifprm.txt"
/
&NRESTART
CRESTDIR = "./"
CVNREST  = "restart"
LRESTCDF = .FALSE.
IFRQ_RST = 0
/
&NFORCE
LINPCDF  = $LINPCDF
LINPDAY  = $LINPDAY
LINTERP  = $LINTERP
CINPMAT  = "$INPMAT"
DROFUNIT = $DROFUNIT
CROFDIR  = "$RUNOFF_DIR"
CROFPRE  = "$CROFPRE"
CROFSUF  = "$CROFSUF"
/
&NOUTPUT
COUTDIR  = "./"
CVARSOUT = "$CVARSOUT"
COUTTAG  = "$START_YEAR"
LOUTVEC  = .FALSE.
LOUTCDF  = $LOUTCDF
IFRQ_OUT = $IFRQ_OUT
/
EOF

# A single run from zero storage covers the leap year 2000 (366 daily records).
STEP="Run the non-MPI model with bifurcation enabled"
printf '\n==================================================\n[STEP 2/2] Run CaMa-Flood v5 for %s\n==================================================\n\n' "$START_YEAR"
OMP_NUM_THREADS="$OMP_NUM_THREADS" "$EXEC_CMF" > "$CONSOLE_LOG" 2>&1

printf '\n==================================================\n[SUCCESS] %s completed\n' "$SCRIPT_NAME"
printf 'Results: %s\n' "$RUN_DIR"
printf 'Model log: %s\n' "$MODEL_LOG"
printf 'Console log: %s\n' "$CONSOLE_LOG"
printf '==================================================\n'
