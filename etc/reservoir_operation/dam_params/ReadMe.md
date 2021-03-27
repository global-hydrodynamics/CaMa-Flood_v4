# scripts to estimate dam parameters for reservoir operation model
2021.3.25, written by Risa Hanazaki & Dai Yamazaki

## input data
- GRanD_v1_1_inp.csv
[damid, damname, lon, lat, total storage capacity, drainage area]

- GRSAD, ReGeom

## Pre-Requirement 
- Download GRanD, GRSAD, ReGeom data
- Execute naturalized simulation without reservoir operation

## How to
0. Please compile ./src/get_rivinfo.F90 and run auto.sh
1. Edit s00-link.sh to specify input data locations, Run ./s00-link.sh 
2. Edit s01-calc_damloc.sh to specify project configulations
3. Run ./s01-calc_damloc.sh
4. Dam parameter csv file will be outputted to ../${TAG}/damparam_${TAG}.csv

## Output data
ix, iy: grid index in CaMa-Flood river map
upreal: real drainage area [km2]
        if not known, -999
uparea_cama: drainage area in CaMa river map [km2]
totalsto_mcm: total storage capacity [million cubic meter]
