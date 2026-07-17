#!/bin/sh

set -eu

# Run the liquid-water-only heatlink baseline.
# Usage: run_heatlink_baseline.sh [smoke|annual]
#
# Environment variables:
#   RUN_DIR       Output and working directory for this run.
#   OMP_NUM_THREADS Number of OpenMP threads used by CaMa-Flood.
#   FLOW_MAP_DIR  Directory containing the 15-minute river map files.
#   RUNOFF_DIR    Directory containing daily runoff files [mm day-1].
#   ATM_DIR       Directory containing the 3-hourly atmospheric forcing files.
#   TOOLCHAIN_ROOT Directory containing the Fortran/netCDF runtime libraries.
#   SDKROOT       macOS SDK selected by the conda Fortran toolchain.

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
RUN_MODE=${1:-smoke}
TOOLCHAIN_ROOT=${TOOLCHAIN_ROOT:-/Users/dtokuda/miniconda3/envs/ils}
SDKROOT=${SDKROOT:-$(xcrun --show-sdk-path)}
FLOW_MAP_DIR=${FLOW_MAP_DIR:-/Users/dtokuda/work/data/flow/FLOW/out/glob_15min_nolake}
RUNOFF_DIR=${RUNOFF_DIR:-/Users/dtokuda/work/data/cmf_v420_pkg/inp/test_1deg/runoff}
ATM_DIR=${ATM_DIR:-/Users/dtokuda/work/data/ils/ILS_data_20241118/test/frc}
DIMINFO=${DIMINFO:-${ROOT}/map/glb_15min/diminfo_test-1deg.txt}
RUNOFF_INPMAT=${RUNOFF_INPMAT:-${ROOT}/map/glb_15min/inpmat_test-1deg.bin}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-16}

case "$RUN_MODE" in
    smoke)
        END_YEAR=2000
        END_MONTH=1
        END_DAY=2
        EXPECTED_STEPS=24
        EXPECTED_RECORDS=1
        ;;
    annual)
        END_YEAR=2001
        END_MONTH=1
        END_DAY=1
        EXPECTED_STEPS=8784
        EXPECTED_RECORDS=366
        ;;
    *)
        echo "Usage: $0 [smoke|annual]" >&2
        exit 2
        ;;
esac

RUN_DIR=${RUN_DIR:-${ROOT}/out/step1-heatlink-${RUN_MODE}}
EXE=${ROOT}/src/MAIN_cmf
NML=${RUN_DIR}/input_cmf.nam
LOG=${RUN_DIR}/log_CaMa.txt
RIVWAT_OUTPUT=${RUN_DIR}/rivwattmp2000.bin
FINAL_RESTART=${RUN_DIR}/restart${END_YEAR}$(printf '%02d' "${END_MONTH}")$(printf '%02d' "${END_DAY}")00.heatlink.bin

if [ ! -x "$EXE" ]; then
    echo "Executable not found: ${EXE}" >&2
    echo "Run gosh/compile_heatlink.sh first." >&2
    exit 1
fi

for required_path in \
    "$DIMINFO" \
    "$RUNOFF_INPMAT" \
    "${FLOW_MAP_DIR}/nextxy.bin" \
    "${FLOW_MAP_DIR}/ctmare.bin" \
    "${FLOW_MAP_DIR}/elevtn.bin" \
    "${FLOW_MAP_DIR}/nxtdst.bin" \
    "${FLOW_MAP_DIR}/rivlen.bin" \
    "${FLOW_MAP_DIR}/fldhgt.bin" \
    "${FLOW_MAP_DIR}/rivwth.bin" \
    "${FLOW_MAP_DIR}/rivhgt.bin" \
    "${FLOW_MAP_DIR}/rivman.bin" \
    "${FLOW_MAP_DIR}/bifprm.txt" \
    "${RUNOFF_DIR}/Roff____20000101.one" \
    "${ATM_DIR}/GSWP3.BC.LWdown.3hrMap.ILS.2000.nc" \
    "${ATM_DIR}/GSWP3.BC.PSurf.3hrMap.ILS.2000.nc" \
    "${ATM_DIR}/GSWP3.BC.Qair.3hrMap.ILS.2000.nc" \
    "${ATM_DIR}/GSWP3.BC.SWdown.3hrMap.ILS.2000.nc" \
    "${ATM_DIR}/GSWP3.BC.Tair.3hrMap.ILS.2000.nc" \
    "${ATM_DIR}/GSWP3.BC.Wind.3hrMap.ILS.2000.nc"
do
    if [ ! -e "$required_path" ]; then
        echo "Required input was not found: ${required_path}" >&2
        exit 1
    fi
done

if [ -d "$RUN_DIR" ] && [ -n "$(find "$RUN_DIR" -mindepth 1 -maxdepth 1 -print -quit)" ]; then
    echo "Run directory is not empty: ${RUN_DIR}" >&2
    echo "Set RUN_DIR to a new or empty directory." >&2
    exit 1
fi
mkdir -p "$RUN_DIR"

cat > "$NML" <<EOF
&NRUNVER
LADPSTP   = .TRUE.                 ! Use the adaptive hydraulic time step.
LPTHOUT   = .TRUE.                 ! Enable bifurcation flow.
LDAMOUT   = .FALSE.                ! Disable reservoir operation.
LOUTPUT   = .FALSE.                ! Disable standard output; heatlink output remains enabled.
LRESTART  = .FALSE.                ! Initialize without a restart file.
LHEATLINK = .TRUE.                 ! Enable river water thermodynamics.
/
&NDIMTIME
CDIMINFO = "${DIMINFO}"            ! Grid dimensions and geographic extent.
DT       = 3600                    ! [s] Coupling time step.
IFRQ_INP = 24                      ! [hour] Runoff update interval.
/
&NPARAM
PMANRIV = 0.03D0                   ! [s m-1/3] River Manning roughness.
PMANFLD = 0.10D0                   ! [s m-1/3] Floodplain Manning roughness.
PDSTMTH = 10000.D0                 ! [m] Downstream distance at river mouths.
PCADP   = 0.7                      ! [-] Adaptive-step CFL coefficient.
/
&NSIMTIME
SYEAR = 2000
SMON  = 1
SDAY  = 1
SHOUR = 0
EYEAR = ${END_YEAR}
EMON  = ${END_MONTH}
EDAY  = ${END_DAY}
EHOUR = 0
/
&NMAP
LMAPCDF = .FALSE.
CNEXTXY = "${FLOW_MAP_DIR}/nextxy.bin"  ! Downstream grid indices.
CGRAREA = "${FLOW_MAP_DIR}/ctmare.bin"  ! [m2] Unit-catchment area.
CELEVTN = "${FLOW_MAP_DIR}/elevtn.bin"  ! [m] Channel-bank elevation.
CNXTDST = "${FLOW_MAP_DIR}/nxtdst.bin"  ! [m] Distance to the next outlet.
CRIVLEN = "${FLOW_MAP_DIR}/rivlen.bin"  ! [m] River-channel length.
CFLDHGT = "${FLOW_MAP_DIR}/fldhgt.bin"  ! [m] Floodplain height profile.
CRIVWTH = "${FLOW_MAP_DIR}/rivwth.bin"  ! [m] River-channel width.
CRIVHGT = "${FLOW_MAP_DIR}/rivhgt.bin"  ! [m] River-channel depth.
CRIVMAN = "${FLOW_MAP_DIR}/rivman.bin"  ! [s m-1/3] River Manning roughness.
CPTHOUT = "${FLOW_MAP_DIR}/bifprm.txt"  ! Bifurcation-channel table.
/
&NRESTART
CRESTSTO = ""
CRESTDIR = "./"
CVNREST  = "restart"
LRESTCDF = .FALSE.
LRESTDBL = .TRUE.
IFRQ_RST = 0                       ! Write restart state only at the end of the run.
/
&NFORCE
LINPCDF  = .FALSE.
LINTERP  = .TRUE.
CINPMAT  = "${RUNOFF_INPMAT}"
DROFUNIT = 86400000                ! Convert runoff from [mm day-1] to [m s-1].
CROFDIR  = "${RUNOFF_DIR}"
CROFPRE  = "Roff____"
CROFSUF  = ".one"
/

&intrp_map
LINTRP = .TRUE.
inpmat_names = '01', '02', '03', '04', '05', '06', '07', '08'
/

&nml_inpmat item='01', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_01' &end
&nml_inpmat item='02', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_02' &end
&nml_inpmat item='03', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_03' &end
&nml_inpmat item='04', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_04' &end
&nml_inpmat item='05', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_05' &end
&nml_inpmat item='06', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_06' &end
&nml_inpmat item='07', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_07' &end
&nml_inpmat item='08', dir='${FLOW_MAP_DIR}/inpmat', prefix='inpmat_08' &end

&output_default
dt = 86400                          ! [s] Heatlink output interval.
/
&nml_out item='RIVWAT_TMP', path='./rivwattmp2000' &end

&restart_default
initial_state_is_dumped = .false.
/
&restart_config item='RIVWAT_TMP', file='.heatlink.bin', recnum=1, mapfmt=.true. &end

&input_item item='LWDN', fmt='nc', path='${ATM_DIR}/GSWP3.BC.LWdown.3hrMap.ILS.2000.nc' /
&input_item item='PSRF', fmt='nc', path='${ATM_DIR}/GSWP3.BC.PSurf.3hrMap.ILS.2000.nc', scale=1.0e-2 /
&input_item item='QAIR', fmt='nc', path='${ATM_DIR}/GSWP3.BC.Qair.3hrMap.ILS.2000.nc' /
&input_item item='SWDN', fmt='nc', path='${ATM_DIR}/GSWP3.BC.SWdown.3hrMap.ILS.2000.nc' /
&input_item item='TAIR', fmt='nc', path='${ATM_DIR}/GSWP3.BC.Tair.3hrMap.ILS.2000.nc' /
&input_item item='TROF', fmt='nc', path='${ATM_DIR}/GSWP3.BC.Tair.3hrMap.ILS.2000.nc' /
&input_item item='WIND', fmt='nc', path='${ATM_DIR}/GSWP3.BC.Wind.3hrMap.ILS.2000.nc' /

&input_nc item='LWDN', var_name='LWdown' /
&input_nc item='PSRF', var_name='PSurf' /
&input_nc item='QAIR', var_name='Qair' /
&input_nc item='SWDN', var_name='SWdown' /
&input_nc item='TAIR', var_name='Tair' /
&input_nc item='TROF', var_name='Tair' /
&input_nc item='WIND', var_name='Wind' /
EOF

export PATH="${TOOLCHAIN_ROOT}/bin:${PATH}"
export DYLD_LIBRARY_PATH="${TOOLCHAIN_ROOT}/lib:${DYLD_LIBRARY_PATH:-}"
export SDKROOT
export OMP_NUM_THREADS

echo "Running ${RUN_MODE} liquid-water baseline"
echo "  period: 2000-01-01 00:00 to ${END_YEAR}-$(printf '%02d' "${END_MONTH}")-$(printf '%02d' "${END_DAY}") 00:00"
echo "  threads: ${OMP_NUM_THREADS}"
echo "  run dir: ${RUN_DIR}"

START_EPOCH=$(date +%s)
(
    cd "$RUN_DIR"
    /usr/bin/time -p "$EXE" > run.stdout 2> run.stderr
)
END_EPOCH=$(date +%s)
ELAPSED_SECONDS=$((END_EPOCH - START_EPOCH))

if [ ! -f "$LOG" ]; then
    echo "Run failed: model log was not created." >&2
    exit 1
fi
if [ ! -f "$RIVWAT_OUTPUT" ]; then
    echo "Run failed: river water temperature output was not created." >&2
    exit 1
fi
if [ ! -f "$FINAL_RESTART" ]; then
    echo "Run failed: final heatlink restart was not created." >&2
    exit 1
fi

STEP_COUNT=$(grep -c '^\[MAIN_cmf\] Time step:' "$LOG" || true)
if [ "$STEP_COUNT" -ne "$EXPECTED_STEPS" ]; then
    echo "Unexpected time-step count: ${STEP_COUNT} (expected ${EXPECTED_STEPS})." >&2
    exit 1
fi

if stat -f %z "$RIVWAT_OUTPUT" >/dev/null 2>&1; then
    OUTPUT_BYTES=$(stat -f %z "$RIVWAT_OUTPUT")
else
    OUTPUT_BYTES=$(stat -c %s "$RIVWAT_OUTPUT")
fi
RECORD_BYTES=$((1440 * 720 * 4))
OUTPUT_RECORDS=$((OUTPUT_BYTES / RECORD_BYTES))
if [ "$OUTPUT_RECORDS" -ne "$EXPECTED_RECORDS" ] || [ $((OUTPUT_BYTES % RECORD_BYTES)) -ne 0 ]; then
    echo "Unexpected RIVWAT_TMP size: ${OUTPUT_BYTES} bytes (${OUTPUT_RECORDS} records)." >&2
    exit 1
fi

if grep -Eiq '(^|[^[:alpha:]])(nan|infinity)([^[:alpha:]]|$)' "$LOG"; then
    echo "Run failed: non-finite value found in the model log." >&2
    exit 1
fi

FINAL_MINMAX=$(grep '  min/max = ' "$LOG" | tail -n 1 | sed 's/^ *min\/max = *//')
if [ -z "$FINAL_MINMAX" ]; then
    echo "Run failed: final temperature range was not found in the model log." >&2
    exit 1
fi

OUTPUT_SHA256=$(shasum -a 256 "$RIVWAT_OUTPUT" | awk '{print $1}')
RESTART_SHA256=$(shasum -a 256 "$FINAL_RESTART" | awk '{print $1}')
COMPILER_VERSION=$(gfortran --version | head -n 1)

cat > "${RUN_DIR}/summary.txt" <<EOF
run_mode=${RUN_MODE}
period_start=2000-01-01T00:00:00
period_end=${END_YEAR}-$(printf '%02d' "${END_MONTH}")-$(printf '%02d' "${END_DAY}")T00:00:00
omp_num_threads=${OMP_NUM_THREADS}
compiler=${COMPILER_VERSION}
elapsed_seconds=${ELAPSED_SECONDS}
time_steps=${STEP_COUNT}
rivwattmp_records=${OUTPUT_RECORDS}
rivwattmp_bytes=${OUTPUT_BYTES}
rivwattmp_sha256=${OUTPUT_SHA256}
final_restart_sha256=${RESTART_SHA256}
final_rivwattmp_minmax_K=${FINAL_MINMAX}
EOF

echo "Baseline completed successfully."
cat "${RUN_DIR}/summary.txt"
