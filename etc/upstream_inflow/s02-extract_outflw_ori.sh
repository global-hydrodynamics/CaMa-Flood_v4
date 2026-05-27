#!/bin/bash
set -eu
mkdir -p gauge
mkdir -p data

STAYEAR=2000
ENDYEAR=2001

PARAMS="./map/params.txt"
LIST="./data/list_points.txt"
GAUGEDIR='./gauge'

INPFREQ="--freq hourly"
#INPFREQ="--freq daily"

DAILYMEAN=""
#DAILYMEAN="--daily-mean"

INFCSV='./data/mekong_inflow.csv'

# [1] Convert CaMa-Flood simulation to gauge timeseries text
tail -n +2 "${LIST}" | while read -r NAME IX IY LAT LON UPAREA
do
    [ -z "${NAME}" ] && continue

    echo "[INFO] Processing ${NAME} (x=${IX}, y=${IY})"

    YEAR="${STAYEAR}"
    while [ "${YEAR}" -le "${ENDYEAR}" ]
    do
        OUTFLW="./out_ori/outflw${YEAR}.bin"

        echo "python x01-extract_outflw.py "${PARAMS}" "${OUTFLW}" "${YEAR}" "${IX}" "${IY}" ${INPFREQ} ${DAILYMEAN} > ${GAUGEDIR}/${NAME}_${YEAR}.txt"
        python x01-extract_outflw.py \
            "${PARAMS}" \
            "${OUTFLW}" \
            "${YEAR}" \
            "${IX}" \
            "${IY}" \
            ${INPFREQ} \
            ${DAILYMEAN} \
            > "${GAUGEDIR}/${NAME}_${YEAR}.txt"

        YEAR=$((YEAR + 1))
    done
done

# [2] Merge gauge data to inflow CSV
echo "python x02-merge_gauge_csv.py ${LIST} ${GAUGEDIR} ${STAYEAR} ${ENDYEAR} -o ${INFCSV}"
python x02-merge_gauge_csv.py ${LIST} ${GAUGEDIR} ${STAYEAR} ${ENDYEAR} -o ${INFCSV}

echo "INFLOW_CSV: $INFCSV"
column -s, -t $INFCSV | head