2021.3.25, written by Risa Hanazaki

scripts to estimate dam parameters for reservoir operation model

input data
GRanD_v1_1_inp.csv
[damid, damname, lon, lat, total storage capacity, drainage area]

how to
please compile ./src/get_rivinfo.F90 and run auto.sh

procedure
1. run ./s00-link.sh 
2. compile ./src/get_rivinfo.F90
3. run ./s01-calc_damloc.sh
4. run ./s02-mk_damparams.sh
dam parameter csv file will be outputted to ../${TAG}/dam_params_${TAG}.csv

ix, iy: grid index in CaMa-Flood river map
upreal: real drainage area [km2]
        if not known, -999
uparea_cama: drainage area in CaMa river map [km2]
totalsto_mcm: total storage capacity [million cubic meter]
