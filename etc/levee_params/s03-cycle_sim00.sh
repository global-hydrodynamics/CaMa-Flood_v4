#!/bin/bash
## CaMa-Flood: simulation map directory & levee data files

CYCLE="sim00"
NEXT="sim01"

./t01-run_cmf_levee.sh $CYCLE

./t02-calc_levee_height.sh $CYCLE

cp ${CYCLE}_calc/levhgt.bin ./map/levhgt_${NEXT}.bin