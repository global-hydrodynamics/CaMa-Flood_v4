#!/bin/sh

set -eu

# Run a heatlink regression case with optional prognostic river ice.
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
#   LHEATLINK      Fortran logical enabling river thermodynamics; defaults to .TRUE.
#   LICE          Fortran logical enabling river ice; defaults to .FALSE.
#   START_*       Optional start date fields (YEAR, MONTH, DAY, HOUR).
#   END_*         Optional end date fields overriding the selected run mode.
#   EXPECTED_STEPS, EXPECTED_RECORDS Optional expected counts for overridden dates.
#   MODEL_DT      CaMa/heatlink coupling time step [s]; defaults to 3600.
#   OUTPUT_DT     Heatlink output interval [s]; defaults to 86400.
#   RESTART_SOURCE_DIR Directory containing restart files at the requested start date.

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
LHEATLINK=${LHEATLINK:-.TRUE.}
LICE=${LICE:-.FALSE.}
MODEL_DT=${MODEL_DT:-3600}
OUTPUT_DT=${OUTPUT_DT:-86400}
START_YEAR=${START_YEAR:-2000}
START_MONTH=${START_MONTH:-1}
START_DAY=${START_DAY:-1}
START_HOUR=${START_HOUR:-0}
RESTART_SOURCE_DIR=${RESTART_SOURCE_DIR:-}

case "$LHEATLINK" in
    .TRUE.|.true.|TRUE|true)
        LHEATLINK_NML=.TRUE.
        HEATLINK_ENABLED=1
        ;;
    .FALSE.|.false.|FALSE|false)
        LHEATLINK_NML=.FALSE.
        HEATLINK_ENABLED=0
        ;;
    *)
        echo "LHEATLINK must be a Fortran or shell logical: ${LHEATLINK}" >&2
        exit 2
        ;;
esac

case "$LICE" in
    .TRUE.|.true.|TRUE|true)
        LICE_NML=.TRUE.
        ICE_ENABLED=1
        ;;
    .FALSE.|.false.|FALSE|false)
        LICE_NML=.FALSE.
        ICE_ENABLED=0
        ;;
    *)
        echo "LICE must be a Fortran or shell logical: ${LICE}" >&2
        exit 2
        ;;
esac

if [ "$HEATLINK_ENABLED" -eq 0 ] && [ "$ICE_ENABLED" -eq 1 ]; then
    echo "LICE=.TRUE. requires LHEATLINK=.TRUE." >&2
    exit 2
fi

case "$RUN_MODE" in
    smoke)
        DEFAULT_END_YEAR=2000
        DEFAULT_END_MONTH=1
        DEFAULT_END_DAY=2
        DEFAULT_END_HOUR=0
        DEFAULT_EXPECTED_STEPS=24
        DEFAULT_EXPECTED_RECORDS=1
        ;;
    annual)
        DEFAULT_END_YEAR=2001
        DEFAULT_END_MONTH=1
        DEFAULT_END_DAY=1
        DEFAULT_END_HOUR=0
        DEFAULT_EXPECTED_STEPS=8784
        DEFAULT_EXPECTED_RECORDS=366
        ;;
    *)
        echo "Usage: $0 [smoke|annual]" >&2
        exit 2
        ;;
esac

END_YEAR=${END_YEAR:-${DEFAULT_END_YEAR}}
END_MONTH=${END_MONTH:-${DEFAULT_END_MONTH}}
END_DAY=${END_DAY:-${DEFAULT_END_DAY}}
END_HOUR=${END_HOUR:-${DEFAULT_END_HOUR}}
EXPECTED_STEPS=${EXPECTED_STEPS:-${DEFAULT_EXPECTED_STEPS}}
EXPECTED_RECORDS=${EXPECTED_RECORDS:-${DEFAULT_EXPECTED_RECORDS}}

RUN_DIR=${RUN_DIR:-${ROOT}/out/step1-heatlink-${RUN_MODE}}
EXE=${ROOT}/src/MAIN_cmf
NML=${RUN_DIR}/input_cmf.nam
LOG=${RUN_DIR}/log_CaMa.txt
RIVWAT_OUTPUT=${RUN_DIR}/rivwattmp2000.bin
START_STAMP=${START_YEAR}$(printf '%02d' "${START_MONTH}")$(printf '%02d' "${START_DAY}")$(printf '%02d' "${START_HOUR}")
END_STAMP=${END_YEAR}$(printf '%02d' "${END_MONTH}")$(printf '%02d' "${END_DAY}")$(printf '%02d' "${END_HOUR}")
FINAL_CORE_RESTART=${RUN_DIR}/restart${END_STAMP}.bin
FINAL_RESTART=${RUN_DIR}/restart${END_STAMP}.heatlink.bin

if [ -n "$RESTART_SOURCE_DIR" ]; then
    LRESTART_NML=.TRUE.
    CRESTSTO_NML="./restart${START_STAMP}.bin"
else
    LRESTART_NML=.FALSE.
    CRESTSTO_NML=""
fi

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
    "${RUNOFF_DIR}/Roff____${START_YEAR}$(printf '%02d' "${START_MONTH}")$(printf '%02d' "${START_DAY}").one" \
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

if [ -n "$RESTART_SOURCE_DIR" ]; then
    for restart_file in \
        "restart${START_STAMP}.bin" \
        "restart${START_STAMP}.bin.pth"
    do
        if [ ! -f "${RESTART_SOURCE_DIR}/${restart_file}" ]; then
            echo "Restart source was not found: ${RESTART_SOURCE_DIR}/${restart_file}" >&2
            exit 1
        fi
        cp "${RESTART_SOURCE_DIR}/${restart_file}" "${RUN_DIR}/${restart_file}"
    done
    if [ "$HEATLINK_ENABLED" -eq 1 ]; then
        restart_file="restart${START_STAMP}.heatlink.bin"
        if [ ! -f "${RESTART_SOURCE_DIR}/${restart_file}" ]; then
            echo "Restart source was not found: ${RESTART_SOURCE_DIR}/${restart_file}" >&2
            exit 1
        fi
        cp "${RESTART_SOURCE_DIR}/${restart_file}" "${RUN_DIR}/${restart_file}"
    fi
fi

cat > "$NML" <<EOF
&NRUNVER
LADPSTP   = .TRUE.                 ! Use the adaptive hydraulic time step.
LPTHOUT   = .TRUE.                 ! Enable bifurcation flow.
LDAMOUT   = .FALSE.                ! Disable reservoir operation.
LOUTPUT   = .FALSE.                ! Disable standard output; heatlink output remains enabled.
LRESTART  = ${LRESTART_NML}        ! Initialize from the requested restart source when true.
LHEATLINK = ${LHEATLINK_NML}       ! Enable river water thermodynamics.
LICE      = ${LICE_NML}            ! Enable river ice state and diagnostics.
/
&NDIMTIME
CDIMINFO = "${DIMINFO}"            ! Grid dimensions and geographic extent.
DT       = ${MODEL_DT}             ! [s] CaMa/heatlink coupling time step.
IFRQ_INP = 24                      ! [hour] Runoff update interval.
/
&NPARAM
PMANRIV = 0.03D0                   ! [s m-1/3] River Manning roughness.
PMANFLD = 0.10D0                   ! [s m-1/3] Floodplain Manning roughness.
PDSTMTH = 10000.D0                 ! [m] Downstream distance at river mouths.
PCADP   = 0.7                      ! [-] Adaptive-step CFL coefficient.
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
CRESTSTO = "${CRESTSTO_NML}"
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
dt = ${OUTPUT_DT}                   ! [s] Heatlink output interval.
/
&nml_out item='RIVWAT_TMP', path='./rivwattmp2000' &end
&nml_out item='RIVICE_VOL', path='./rivicevol2000', is_mean=.false. &end
&nml_out item='RIVICE_ARE', path='./riviceare2000', is_mean=.false. &end
&nml_out item='RIVICE_THK', path='./rivicethk2000', is_mean=.false. &end
&nml_out item='RIVICE_FRC', path='./rivicefrc2000', is_mean=.false. &end
&nml_out item='RIVICE_VOL_EXCESS', path='./rivicevolexcess2000', is_mean=.false. &end
&nml_out item='RIVICE_SRF_TMP', path='./rivicetmpsrf2000', is_mean=.false. &end
&nml_out item='RIVICE_MEAN_TMP', path='./rivicetmpmean2000', is_mean=.false. &end
&nml_out item='RIVICE_COND_FLX', path='./rivicecondflx2000', is_mean=.false. &end
&nml_out item='RIVICE_EXCESS_TMP', path='./riviceexcesstmp2000', is_mean=.false. &end

&restart_default
initial_state_is_dumped = .false.
/
&restart_config item='RIVWAT_TMP', file='.heatlink.bin', recnum=1, mapfmt=.true. &end
&restart_config item='RIVICE_VOL', file='.heatlink.bin', recnum=2, mapfmt=.true. &end
&restart_config item='RIVICE_VOL_EXCESS', file='.heatlink.bin', recnum=3, mapfmt=.true. &end

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

echo "Running ${RUN_MODE} heatlink regression"
echo "  period: ${START_YEAR}-$(printf '%02d' "${START_MONTH}")-$(printf '%02d' "${START_DAY}") $(printf '%02d' "${START_HOUR}"):00 to ${END_YEAR}-$(printf '%02d' "${END_MONTH}")-$(printf '%02d' "${END_DAY}") $(printf '%02d' "${END_HOUR}"):00"
echo "  heatlink: ${LHEATLINK_NML}"
echo "  river ice: ${LICE_NML}"
echo "  threads: ${OMP_NUM_THREADS}"
echo "  model time step: ${MODEL_DT} s"
echo "  output interval: ${OUTPUT_DT} s"
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
if [ ! -f "$FINAL_CORE_RESTART" ]; then
    echo "Run failed: final CaMa restart was not created." >&2
    exit 1
fi

RESTART_RECORDS=0
RESTART_BYTES=0
if [ "$HEATLINK_ENABLED" -eq 1 ]; then
    if [ ! -f "$RIVWAT_OUTPUT" ]; then
        echo "Run failed: river water temperature output was not created." >&2
        exit 1
    fi
    if [ ! -f "$FINAL_RESTART" ]; then
        echo "Run failed: final heatlink restart was not created." >&2
        exit 1
    fi

    if stat -f %z "$FINAL_RESTART" >/dev/null 2>&1; then
        RESTART_BYTES=$(stat -f %z "$FINAL_RESTART")
    else
        RESTART_BYTES=$(stat -c %s "$FINAL_RESTART")
    fi
    RESTART_RECORD_BYTES=$((1440 * 720 * 8))
    EXPECTED_RESTART_RECORDS=$((1 + 2 * ICE_ENABLED))
    RESTART_RECORDS=$((RESTART_BYTES / RESTART_RECORD_BYTES))
    if [ "$RESTART_RECORDS" -ne "$EXPECTED_RESTART_RECORDS" ] || \
        [ $((RESTART_BYTES % RESTART_RECORD_BYTES)) -ne 0 ]; then
        echo "Unexpected heatlink restart size: ${RESTART_BYTES} bytes (${RESTART_RECORDS} records)." >&2
        exit 1
    fi
fi

STEP_COUNT=$(grep -c '^\[MAIN_cmf\] Time step:' "$LOG" || true)
if [ "$STEP_COUNT" -ne "$EXPECTED_STEPS" ]; then
    echo "Unexpected time-step count: ${STEP_COUNT} (expected ${EXPECTED_STEPS})." >&2
    exit 1
fi

RESTART_WRITE_COUNT=$(grep -c 'CMF::RESTART_WRITE: write time:' "$LOG" || true)
if [ "$RESTART_WRITE_COUNT" -ne 1 ]; then
    echo "Unexpected CaMa restart write count: ${RESTART_WRITE_COUNT} (expected 1)." >&2
    exit 1
fi

OUTPUT_RECORDS=0
OUTPUT_BYTES=0
if [ "$HEATLINK_ENABLED" -eq 1 ]; then
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

if [ "$ICE_ENABLED" -eq 1 ]; then
    for ice_output in \
        rivicevol2000.bin \
        riviceare2000.bin \
        rivicethk2000.bin \
        rivicefrc2000.bin \
        rivicevolexcess2000.bin \
        rivicetmpsrf2000.bin \
        rivicetmpmean2000.bin \
        rivicecondflx2000.bin \
        riviceexcesstmp2000.bin
    do
        ice_path=${RUN_DIR}/${ice_output}
        if [ ! -f "$ice_path" ]; then
            echo "Run failed: river-ice output was not created: ${ice_path}" >&2
            exit 1
        fi
        if [ "$(stat -f %z "$ice_path")" -ne "$OUTPUT_BYTES" ]; then
            echo "Unexpected river-ice output size: ${ice_path}" >&2
            exit 1
        fi
        if ! LC_ALL=C od -An -v -t f4 "$ice_path" | awk '
        {
            for (i = 1; i <= NF; i++) {
                value = $i + 0.0
                is_missing = value >= 0.999e20 && value <= 1.001e20
                if (!is_missing && value < 0.0) exit 1
            }
        }
        '; then
            echo "Found a negative river-ice diagnostic: ${ice_path}" >&2
            exit 1
        fi
    done

    if ! (LC_ALL=C od -An -v -t f4 "${RUN_DIR}/rivicetmpsrf2000.bin"; \
          LC_ALL=C od -An -v -t f4 "${RUN_DIR}/rivicetmpmean2000.bin"; \
          LC_ALL=C od -An -v -t f4 "${RUN_DIR}/riviceexcesstmp2000.bin") | awk '
        {
            for (i = 1; i <= NF; i++) {
                value = $i + 0.0
                is_missing = value >= 0.999e20 && value <= 1.001e20
                if (!is_missing && value > 273.151) exit 1
            }
        }
    '; then
        echo "Found a river-ice temperature above the melting point." >&2
        exit 1
    fi

    if ! (LC_ALL=C od -An -v -t f4 "${RUN_DIR}/rivicevol2000.bin"; \
          LC_ALL=C od -An -v -t f4 "${RUN_DIR}/rivicevolexcess2000.bin") | awk '
        {
            for (i = 1; i <= NF; i++) {
                value = $i + 0.0
                if (value > 0.0 && value < 0.999e20) found = 1
            }
        }
        END { exit(found ? 0 : 1) }
    '; then
        echo "Expected nonzero river-ice volume in the ice-enabled regression." >&2
        exit 1
    fi
fi
fi

if grep -Eiq '(^|[^[:alpha:]])(nan|infinity)([^[:alpha:]]|$)' "$LOG"; then
    echo "Run failed: non-finite value found in the model log." >&2
    exit 1
fi

OUTPUT_SHA256=not_applicable
RESTART_SHA256=not_applicable
FINAL_MINMAX=not_applicable
if [ "$HEATLINK_ENABLED" -eq 1 ]; then
    FINAL_MINMAX=$(grep -A8 'item   = RIVWAT_TMP' "$LOG" | \
        grep '  min/max = ' | tail -n 1 | sed 's/^ *min\/max = *//')
    if [ -z "$FINAL_MINMAX" ]; then
        echo "Run failed: final temperature range was not found in the model log." >&2
        exit 1
    fi

    OUTPUT_SHA256=$(shasum -a 256 "$RIVWAT_OUTPUT" | awk '{print $1}')
    RESTART_SHA256=$(shasum -a 256 "$FINAL_RESTART" | awk '{print $1}')
fi

CORE_RESTART_SHA256=$(shasum -a 256 "$FINAL_CORE_RESTART" | awk '{print $1}')
COMPILER_VERSION=$(gfortran --version | head -n 1)

cat > "${RUN_DIR}/summary.txt" <<EOF
run_mode=${RUN_MODE}
period_start=${START_YEAR}-$(printf '%02d' "${START_MONTH}")-$(printf '%02d' "${START_DAY}")T$(printf '%02d' "${START_HOUR}"):00:00
period_end=${END_YEAR}-$(printf '%02d' "${END_MONTH}")-$(printf '%02d' "${END_DAY}")T$(printf '%02d' "${END_HOUR}"):00:00
lheatlink=${LHEATLINK_NML}
lice=${LICE_NML}
restart_source_dir=${RESTART_SOURCE_DIR}
omp_num_threads=${OMP_NUM_THREADS}
model_dt_seconds=${MODEL_DT}
output_dt_seconds=${OUTPUT_DT}
compiler=${COMPILER_VERSION}
elapsed_seconds=${ELAPSED_SECONDS}
time_steps=${STEP_COUNT}
cama_restart_write_count=${RESTART_WRITE_COUNT}
cama_restart_sha256=${CORE_RESTART_SHA256}
rivwattmp_records=${OUTPUT_RECORDS}
rivwattmp_bytes=${OUTPUT_BYTES}
rivwattmp_sha256=${OUTPUT_SHA256}
heatlink_restart_records=${RESTART_RECORDS}
heatlink_restart_bytes=${RESTART_BYTES}
final_restart_sha256=${RESTART_SHA256}
final_rivwattmp_minmax_K=${FINAL_MINMAX}
EOF

echo "Baseline completed successfully."
cat "${RUN_DIR}/summary.txt"
