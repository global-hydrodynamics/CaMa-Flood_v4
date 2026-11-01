dset ^lonlat.bin
undef -9999
title CaMa-Flood map file lonlat.bin
options yrev little_endian
xdef 80 linear 90.125 0.25
ydef 120 linear 5.125 0.25
tdef 1 linear 00Z01jan2000 1yr
zdef 1 linear 1 1
vars 2
outlon 1 99 Unit-catchment outlet longitude [degrees_east]
outlat 1 99 Unit-catchment outlet latitude [degrees_north]
ENDVARS
