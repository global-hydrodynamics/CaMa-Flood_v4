#!/bin/sh
# Shared implementation for the controlled 2000 experiments.
set -eu
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
CMF_BASE=$(CDPATH= cd "$SCRIPT_DIR/../../../../" && pwd -P)
PYTHON="$CMF_BASE/.venv/bin/python"
MODEL="$CMF_BASE/out/bin/MAIN_cmf_netcdf"
MAPDIR="$CMF_BASE/map/mekong_15min"
MAP_NC="$MAPDIR/netcdf"
RUNOFF_DIR="$CMF_BASE/inp/test-mek_1deg_nc"
INPMAT="$MAPDIR/inpmat_test-mek_1deg.bin"
INPDIM="$MAPDIR/inpdim_test-mek_1deg.txt"
OMP_NUM_THREADS=4
DT=600
YEAR=2000

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 control|deep_channel [--force]" >&2
    exit 2
fi
CASE=$1
shift
FORCE=0
if [ "$#" -gt 0 ]; then
    [ "$#" -eq 1 ] && [ "$1" = "--force" ] || { echo "Only --force is supported." >&2; exit 2; }
    FORCE=1
fi
case "$CASE" in
  control)
    PROJ=control
    RIVHGT="$MAP_NC/rivhgt.nc"
    ;;
  deep_channel)
    PROJ=deep_channel
    RIVHGT="$SCRIPT_DIR/../map_modify/rivhgt_deep.nc"
    ;;
  *) echo "Unknown experiment: $CASE" >&2; exit 2 ;;
esac
RUNDIR="$SCRIPT_DIR/../out/$PROJ"

for required in "$PYTHON" "$MODEL" "$INPMAT" "$INPDIM" "$RUNOFF_DIR/runoff2000.nc" \
                "$MAP_NC/nextxy_noedge.nc" "$MAP_NC/ctmare.nc" "$MAP_NC/elevtn.nc" \
                "$MAP_NC/nxtdst.nc" "$MAP_NC/rivlen.nc" "$MAP_NC/fldhgt.nc" \
                "$MAP_NC/rivwth_merit.nc" "$MAP_NC/rivman.nc" "$MAP_NC/bifprm.nc" "$RIVHGT"; do
    [ -f "$required" ] || { echo "[ERROR] Required input is missing: $required" >&2; exit 1; }
done
[ -x "$MODEL" ] || { echo "[ERROR] Model executable is not executable: $MODEL" >&2; exit 1; }

mkdir -p "$RUNDIR"
if [ "$FORCE" -eq 0 ] && { [ -e "$RUNDIR/o_outflw2000.nc" ] || [ -e "$RUNDIR/o_rivdph2000.nc" ]; }; then
    echo "[ERROR] Outputs already exist in $RUNDIR (use --force to replace this experiment's known outputs)." >&2
    exit 1
fi
if [ "$FORCE" -eq 1 ]; then
    rm -f "$RUNDIR"/o_outflw2000.nc "$RUNDIR"/o_rivdph2000.nc \
          "$RUNDIR"/o_sfcelv2000.nc "$RUNDIR"/o_fldfrc2000.nc \
          "$RUNDIR"/restart*.nc "$RUNDIR"/input_cmf*.nam \
          "$RUNDIR"/console_2000.log "$RUNDIR"/log_CaMa*.txt \
          "$RUNDIR"/run_time_2000.log "$RUNDIR"/MAIN_cmf "$RUNDIR"/mapdata.txt
fi
cp "$MODEL" "$RUNDIR/MAIN_cmf"
cat > "$RUNDIR/input_cmf.nam" <<EOF
&NRUNVER
LKINE = .FALSE.
LFPLAIN = .TRUE.
LPTHOUT = .TRUE.
LDAMOUT = .FALSE.
LFLDOUT = .TRUE.
LSLPMIX = .FALSE.
LADPSTP = .TRUE.
LRESTART = .FALSE.
LLEAPYR = .TRUE.
/
&NDIMTIME
CDIMINFO = "$INPDIM"
DT = $DT
IFRQ_INP = 24
/
&NPARAM
PMANRIV = 0.03D0
PMANFLD = 0.10D0
PDSTMTH = 10000.D0
PCADP = 0.7D0
/
&NSIMTIME
SYEAR = $YEAR
SMON = 1
SDAY = 1
SHOUR = 0
EYEAR = 2001
EMON = 1
EDAY = 1
EHOUR = 0
/
&NMAP
LMAPCDF = .FALSE.
CNEXTXY = "$MAP_NC/nextxy_noedge.nc"
CGRAREA = "$MAP_NC/ctmare.nc"
CELEVTN = "$MAP_NC/elevtn.nc"
CNXTDST = "$MAP_NC/nxtdst.nc"
CRIVLEN = "$MAP_NC/rivlen.nc"
CFLDHGT = "$MAP_NC/fldhgt.nc"
CRIVWTH = "$MAP_NC/rivwth_merit.nc"
CRIVHGT = "$RIVHGT"
CRIVMAN = "$MAP_NC/rivman.nc"
CPTHOUT = "$MAP_NC/bifprm.nc"
/
&NRESTART
CRESTSTO = "NONE"
CRESTDIR = "$RUNDIR/"
CVNREST = "restart"
LRESTCDF = .TRUE.
IFRQ_RST = 0
/
&NFORCE
LINPCDF = .TRUE.
CROFFREQ = "YEAR"
LINTERP = .TRUE.
CINPMAT = "$INPMAT"
DROFUNIT = 86400000
CVNTIME = "time"
CVNROF = "runoff"
CROFDIR = "$RUNOFF_DIR"
CROFPRE = "runoff"
CROFSUF = ".nc"
/
&NOUTPUT
CMF_BASE = "$CMF_BASE"
COUTVAR_TABLE = ""
COUTDIR = "$RUNDIR/"
CVARSOUT = "outflw,rivdph,sfcelv,fldfrc"
COUTTAG = "2000"
LOUTVEC = .FALSE.
LOUTCDF = .TRUE.
IFRQ_OUT = 24
/
EOF

echo "[STEP] Running $CASE with CRIVHGT=$RIVHGT"
START=$(date +%s)
if (cd "$RUNDIR" && OMP_NUM_THREADS="$OMP_NUM_THREADS" ./MAIN_cmf > console_2000.log 2>&1); then
    STATUS=0
else
    STATUS=$?
fi
ELAPSED=$(($(date +%s) - START))
printf 'Elapsed seconds: %s\nExit status: %s\n' "$ELAPSED" "$STATUS" > "$RUNDIR/run_time_2000.log"
if [ "$STATUS" -ne 0 ]; then
    echo "[ERROR] $CASE failed; inspect $RUNDIR/console_2000.log" >&2
    tail -80 "$RUNDIR/console_2000.log" >&2 || true
    exit "$STATUS"
fi
cp "$RUNDIR/input_cmf.nam" "$RUNDIR/input_cmf_2000.nam"
cp "$RUNDIR/log_CaMa.txt" "$RUNDIR/log_CaMa_2000.txt"
for var in outflw rivdph sfcelv fldfrc; do
    [ -s "$RUNDIR/o_${var}2000.nc" ] || { echo "[ERROR] Missing model output: $RUNDIR/o_${var}2000.nc" >&2; exit 1; }
done
echo "[SUCCESS] $CASE completed in ${ELAPSED}s; outputs are under $RUNDIR"
