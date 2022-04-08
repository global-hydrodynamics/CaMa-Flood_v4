#!/bin/sh

TAG=$1
MINUPAREA=$2

echo ""
echo "@@@ src/get_rivinfo_glb "
./src/get_rivinfo_glb
mv ./damloc_tmp.txt ./$TAG/

# temporal dam allocation file : $TAG/damloc_tmp.txt

echo ""
echo "@@@ ./src/modify_damloc.py "
python ./src/modify_damloc.py $TAG $MINUPAREA

# mnodified dam allocation file : $TAG/damloc_modified.csv
