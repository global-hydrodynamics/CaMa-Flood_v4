#!/bin/sh
# Run consecutive yearly Mekong simulations with NetCDF runoff/output and binary restart.
# Run from gosh_sample/: ./mek02-longterm_netcdf.sh
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
CMF_BASE=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

# Simulation years are inclusive; extend END_YEAR to continue the yearly sequence.
START_YEAR=2000
END_YEAR=2001

# Model, map, runoff, input matrix, and experiment output paths.
EXEC_CMF="${CMF_BASE}/out/bin/MAIN_cmf_netcdf"
MAPDIR="${CMF_BASE}/map/mekong_15min"
ROFFTAG="test-mek_1deg_nc"
INPMAT_TAG="test-mek_1deg"
RUNOFF_DIR="${CMF_BASE}/inp/${ROFFTAG}"
INPDIM="${MAPDIR}/inpdim_${INPMAT_TAG}.txt"
INPMAT="${MAPDIR}/inpmat_${INPMAT_TAG}.bin"
RUNDIR="${CMF_BASE}/out/mek02-longterm_netcdf"

# Keep the mek01 numerical and physical configuration.
OMP_NUM_THREADS=4
DT=3600
LADPSTP=".TRUE."
LPTHOUT=".TRUE."
LDAMOUT=".FALSE."
PMANRIV="0.03D0"
PMANFLD="0.10D0"
PDSTMTH="10000.D0"
PCADP="0.7D0"
CVARSOUT="outflw,storge,sfcelv,flddph,fldfrc"
IFRQ_OUT=24
IFRQ_RST=0
CVNREST="restart"

# Runoff is daily depth in mm/day. The existing input matrix matches this grid.
IFRQ_INP=24
DROFUNIT=86400000
CVNROF="runoff"
CVNTIME="time"

# ----------------------------------------------------------------------
# User settings end here. Paths, annual namelists, and processing follow.
# ----------------------------------------------------------------------

SCRIPT_NAME=${0##*/}
STEP="Prepare the annual NetCDF experiment"
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

NAMELIST="${RUNDIR}/input_cmf.nam"
SUMMARY="${RUNDIR}/summary.tsv"

file_sha256() {
    if command -v shasum >/dev/null 2>&1; then
        LC_ALL=C shasum -a 256 "$1" | awk '{print $1}'
    elif command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    else
        printf 'ERROR: shasum or sha256sum is required to protect completed annual results.\n' >&2
        return 1
    fi
}

year_days() {
    YEAR_CHECK=$1
    if [ "$((YEAR_CHECK % 400))" -eq 0 ] || \
       { [ "$((YEAR_CHECK % 4))" -eq 0 ] && [ "$((YEAR_CHECK % 100))" -ne 0 ]; }; then
        printf '366\n'
    else
        printf '365\n'
    fi
}

year_end_stamp() {
    NEXT_YEAR=$(( $1 + 1 ))
    printf '%04d010100\n' "$NEXT_YEAR"
}

annual_outputs_exist() {
    CHECK_YEAR=$1
    EXPECTED_DAYS=$(year_days "$CHECK_YEAR")
    for OUTPUT_VAR in outflw storge sfcelv flddph fldfrc; do
        OUTPUT_FILE="${RUNDIR}/o_${OUTPUT_VAR}${CHECK_YEAR}.nc"
        [ -s "$OUTPUT_FILE" ] || return 1
        ncdump -h "$OUTPUT_FILE" | grep -Fq "(${EXPECTED_DAYS} currently)" || return 1
    done
    END_STAMP=$(year_end_stamp "$CHECK_YEAR")
    [ -s "${RUNDIR}/${CVNREST}${END_STAMP}.bin" ] || return 1
    [ -s "${RUNDIR}/${CVNREST}${END_STAMP}.bin.pth" ] || return 1
}

mkdir -p "$RUNDIR"
if ! command -v ncdump >/dev/null 2>&1; then
    printf 'ERROR: ncdump is required to validate annual NetCDF output.\n' >&2
    exit 1
fi
if [ ! -x "$EXEC_CMF" ]; then
    printf 'ERROR: NetCDF-enabled model executable not found: %s\n' "$EXEC_CMF" >&2
    printf 'Build it with: ./gosh_sample/compile_netcdf.sh\n' >&2
    exit 1
fi
if [ ! -d "$RUNOFF_DIR" ] || [ ! -f "$INPDIM" ] || [ ! -f "$INPMAT" ]; then
    printf 'ERROR: runoff dataset or matching Mekong input matrix is missing.\n' >&2
    exit 1
fi

if [ ! -f "$SUMMARY" ]; then
    printf 'year\tstatus\tstart\tend\telapsed_seconds\texit_status\n' > "$SUMMARY"
fi

printf 'CMF_BASE = %s\n' "$CMF_BASE"
printf 'RUNDIR = %s\n' "$RUNDIR"
printf 'MAPDIR = %s\n' "$MAPDIR"
printf 'RUNOFF_DIR = %s\n' "$RUNOFF_DIR"

# Each annual run has an exclusive end at January 1 of the following year.
YEAR=$START_YEAR
while [ "$YEAR" -le "$END_YEAR" ]; do
    NEXT_YEAR=$((YEAR + 1))
    EXPECTED_DAYS=$(year_days "$YEAR")
    RUNOFF_FILE="${RUNOFF_DIR}/runoff${YEAR}.nc"
    END_STAMP=$(year_end_stamp "$YEAR")
    MARKER="${RUNDIR}/year${YEAR}.complete"
    YEAR_NAMELIST="${RUNDIR}/input_cmf_${YEAR}.nam"

    STEP="Configure year ${YEAR}"
    printf '\n==================================================\n[STEP] Configure %s (%s records)\n==================================================\n' \
        "$YEAR" "$EXPECTED_DAYS"
    if [ ! -s "$RUNOFF_FILE" ]; then
        printf 'ERROR: yearly runoff file is missing: %s\n' "$RUNOFF_FILE" >&2
        exit 1
    fi

    if [ "$YEAR" -eq 2000 ]; then
        LRESTART=".FALSE."
        CRESTSTO="NONE"
        PREVIOUS_RESTART="NONE"
    else
        PREVIOUS_YEAR_END=$(printf '%04d010100' "$YEAR")
        PREVIOUS_RESTART="${RUNDIR}/${CVNREST}${PREVIOUS_YEAR_END}.bin"
        if [ ! -s "$PREVIOUS_RESTART" ] || [ ! -s "${PREVIOUS_RESTART}.pth" ]; then
            printf 'ERROR: restart from the previous year is required: %s\n' "$PREVIOUS_RESTART" >&2
            exit 1
        fi
        LRESTART=".TRUE."
        CRESTSTO="$PREVIOUS_RESTART"
    fi

    cat > "$NAMELIST" <<EOF
&NRUNVER
LADPSTP = ${LADPSTP}
LPTHOUT = ${LPTHOUT}
LDAMOUT = ${LDAMOUT}
LRESTART = ${LRESTART}
/
&NDIMTIME
CDIMINFO = "${INPDIM}"
DT = ${DT}
IFRQ_INP = ${IFRQ_INP}
/
&NPARAM
PMANRIV = ${PMANRIV}
PMANFLD = ${PMANFLD}
PDSTMTH = ${PDSTMTH}
PCADP = ${PCADP}
/
&NSIMTIME
SYEAR = ${YEAR}
SMON = 1
SDAY = 1
SHOUR = 0
EYEAR = ${NEXT_YEAR}
EMON = 1
EDAY = 1
EHOUR = 0
/
&NMAP
LMAPCDF = .FALSE.
CNEXTXY = "${MAPDIR}/nextxy_noedge.bin"
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
CRESTSTO = "${CRESTSTO}"
CRESTDIR = "./"
CVNREST = "${CVNREST}"
LRESTCDF = .FALSE.
IFRQ_RST = ${IFRQ_RST}
/
&NFORCE
LINPCDF = .TRUE.
LINPDAY = .FALSE.
LINTERP = .TRUE.
CINPMAT = "${INPMAT}"
DROFUNIT = ${DROFUNIT}
CROFCDF = "${RUNOFF_FILE}"
CVNROF = "${CVNROF}"
CVNTIME = "${CVNTIME}"
SYEARIN = 0
/
&NOUTPUT
COUTDIR = "./"
CVARSOUT = "${CVARSOUT}"
COUTTAG = "${YEAR}"
LOUTVEC = .FALSE.
LOUTCDF = .TRUE.
IFRQ_OUT = ${IFRQ_OUT}
/
EOF

    SIGNATURE_INPUT="${RUNDIR}/signature_${YEAR}.tmp"
    {
        printf 'year=%s\nrestart=%s\n' "$YEAR" "$PREVIOUS_RESTART"
        printf 'model=%s\n' "$(file_sha256 "$EXEC_CMF")"
        printf 'namelist=%s\n' "$(file_sha256 "$NAMELIST")"
        printf 'runoff=%s\n' "$(file_sha256 "$RUNOFF_FILE")"
        for INPUT_FILE in \
            "${MAPDIR}/params.txt" "${MAPDIR}/nextxy_noedge.bin" "${MAPDIR}/ctmare.bin" \
            "${MAPDIR}/elevtn.bin" "${MAPDIR}/nxtdst.bin" "${MAPDIR}/rivlen.bin" \
            "${MAPDIR}/fldhgt.bin" "${MAPDIR}/rivwth_merit.bin" "${MAPDIR}/rivhgt.bin" \
            "${MAPDIR}/rivman.bin" "${MAPDIR}/bifprm.txt" "$INPDIM" "$INPMAT"; do
            printf '%s=%s\n' "$INPUT_FILE" "$(file_sha256 "$INPUT_FILE")"
        done
        if [ "$PREVIOUS_RESTART" != "NONE" ]; then
            printf 'restart_data=%s\n' "$(file_sha256 "$PREVIOUS_RESTART")"
            printf 'restart_bifurcation=%s\n' "$(file_sha256 "${PREVIOUS_RESTART}.pth")"
        fi
    } > "$SIGNATURE_INPUT"
    SIGNATURE=$(file_sha256 "$SIGNATURE_INPUT")
    rm -f "$SIGNATURE_INPUT"

    if [ -f "$MARKER" ]; then
        if ! grep -Fqx "signature=${SIGNATURE}" "$MARKER"; then
            printf 'ERROR: completed year %s has different inputs or executable.\n' "$YEAR" >&2
            printf 'Preserve this result and use a new RUNDIR for changed experiments: %s\n' "$RUNDIR" >&2
            exit 1
        fi
        if ! annual_outputs_exist "$YEAR"; then
            printf 'ERROR: year %s is marked complete but an output or restart is missing/incomplete.\n' "$YEAR" >&2
            exit 1
        fi
        printf '[SUCCESS] Year %s already completed with matching inputs; leaving its outputs unchanged.\n' "$YEAR"
        printf '%s\tSKIPPED\t%s-01-01\t%s-01-01\t0\t0\n' "$YEAR" "$YEAR" "$NEXT_YEAR" >> "$SUMMARY"
        YEAR=$NEXT_YEAR
        continue
    fi

    # Remove only this incomplete year's outputs; completed earlier years stay intact.
    for OUTPUT_VAR in outflw storge sfcelv flddph fldfrc; do
        rm -f "${RUNDIR}/o_${OUTPUT_VAR}${YEAR}.nc"
    done
    rm -f "${RUNDIR}/${CVNREST}${END_STAMP}.bin" "${RUNDIR}/${CVNREST}${END_STAMP}.bin.pth"
    cp "$EXEC_CMF" "${RUNDIR}/MAIN_cmf"
    cp "$NAMELIST" "$YEAR_NAMELIST"

    STEP="Run and validate year ${YEAR}"
    printf '\n==================================================\n[STEP] Run %s-01-01 through %s-01-01\n==================================================\n' \
        "$YEAR" "$NEXT_YEAR"
    RUN_START=$(date)
    RUN_START_SECONDS=$(date +%s)
    printf 'Start: %s\n' "$RUN_START" | tee "${RUNDIR}/run_time_${YEAR}.log"
    printf '%s\tRUNNING\t%s-01-01\t%s-01-01\t\t\n' "$YEAR" "$YEAR" "$NEXT_YEAR" >> "$SUMMARY"

    if (cd "$RUNDIR" && OMP_NUM_THREADS="$OMP_NUM_THREADS" ./MAIN_cmf > "console_${YEAR}.log" 2>&1); then
        RUN_STATUS=0
    else
        RUN_STATUS=$?
    fi
    RUN_END_SECONDS=$(date +%s)
    RUN_END=$(date)
    ELAPSED_SECONDS=$((RUN_END_SECONDS - RUN_START_SECONDS))
    {
        printf 'End: %s\n' "$RUN_END"
        printf 'Elapsed time: %s seconds\n' "$ELAPSED_SECONDS"
        printf 'Exit status: %s\n' "$RUN_STATUS"
    } | tee -a "${RUNDIR}/run_time_${YEAR}.log"
    cp "${RUNDIR}/log_CaMa.txt" "${RUNDIR}/log_CaMa_${YEAR}.txt"

    if [ "$RUN_STATUS" -ne 0 ]; then
        printf '%s\tFAILED\t%s-01-01\t%s-01-01\t%s\t%s\n' \
            "$YEAR" "$YEAR" "$NEXT_YEAR" "$ELAPSED_SECONDS" "$RUN_STATUS" >> "$SUMMARY"
        exit "$RUN_STATUS"
    fi

    if ! annual_outputs_exist "$YEAR"; then
        printf 'ERROR: year %s ended but expected NetCDF records or restart files are missing.\n' "$YEAR" >&2
        printf '%s\tFAILED_VALIDATION\t%s-01-01\t%s-01-01\t%s\t0\n' \
            "$YEAR" "$YEAR" "$NEXT_YEAR" "$ELAPSED_SECONDS" >> "$SUMMARY"
        exit 1
    fi

    {
        printf 'signature=%s\n' "$SIGNATURE"
        printf 'year=%s\nrecords=%s\nrestart=%s\n' "$YEAR" "$EXPECTED_DAYS" "${CVNREST}${END_STAMP}.bin"
    } > "$MARKER"
    printf '%s\tCOMPLETED\t%s-01-01\t%s-01-01\t%s\t0\n' \
        "$YEAR" "$YEAR" "$NEXT_YEAR" "$ELAPSED_SECONDS" >> "$SUMMARY"
    printf '\n[SUCCESS] Year %s completed: %s output records; restart %s\n' \
        "$YEAR" "$EXPECTED_DAYS" "${CVNREST}${END_STAMP}.bin"
    YEAR=$NEXT_YEAR
done

printf '\n[SUCCESS] Annual simulations %s-%s completed or were safely skipped.\n' "$START_YEAR" "$END_YEAR"
printf 'Summary: %s\n' "$SUMMARY"
