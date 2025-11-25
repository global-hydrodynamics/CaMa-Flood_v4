#!/bin/bash
#
# x01-calc_levbaseheight.sh
# Compute CaMa-Flood levee-related topographic and storage parameters
# using calc_baseheight.py.

# -------- Input files --------
PARAMS="./map/params.txt"        # map parameters (nx, ny, NLFP, etc.)
GRDARE="./map/grdare.bin"        # grid cell area [m^2]
RIVLEN="./map/rivlen.bin"        # river channel length [m]
RIVWTH="./map/rivwth_gwdlr.bin"  # river width [m]
RIVHGT="./map/rivhgt.bin"        # riverbed elevation (sea = -9999)
FLDHGT="./map/fldhgt.bin"        # floodplain relative height layers (nx*ny*NLFP)
LEVFRC="./map/levfrc.bin"        # levee relative location (0–1 from river to floodplain edge)
LEVHGT="./map/levhgt.bin"        # levee crown height, relative height above river channel (elevtn.bin) [m]

# -------- Output files --------
OUT_LEVDST="./map/levdst.bin"        # distance from river to levee [m]
OUT_LEVPHY="./map/levphyhgt.bin"     # physical levee height [m] (= LEVHGT - LEVBASHGT)
OUT_LEVBAS="./map/levbashgt.bin"     # levee base ground height [m]
OUT_LEVHGT="./map/levhgt_mod.bin"    # modified levee crown height [m] (invalid levees -> -9999)
OUT_LEVBASSTO="./map/levbassto.bin"  # storage at levee base (river side only) [m3]
OUT_LEVTOPSTO="./map/levtopsto.bin"  # storage at levee crown (river side only) [m3]
OUT_LEVFILSTO="./map/levfilsto.bin"  # storage at levee crown (river + protected side) [m3]

# -------- Run Python script --------
python ./src/calc_baseheight.py \
  --params       "${PARAMS}" \
  --grdare       "${GRDARE}" \
  --rivlen       "${RIVLEN}" \
  --rivwth       "${RIVWTH}" \
  --rivhgt       "${RIVHGT}" \
  --fldhgt       "${FLDHGT}" \
  --levfrc       "${LEVFRC}" \
  --levhgt       "${LEVHGT}" \
  --out-levdst     "${OUT_LEVDST}" \
  --out-levbashgt  "${OUT_LEVBAS}" \
  --out-levphyhgt  "${OUT_LEVPHY}" \
  --out-levhgt-mod "${OUT_LEVHGT}" \
  --out-levbassto  "${OUT_LEVBASSTO}" \
  --out-levtopsto  "${OUT_LEVTOPSTO}" \
  --out-levfilsto  "${OUT_LEVFILSTO}"
