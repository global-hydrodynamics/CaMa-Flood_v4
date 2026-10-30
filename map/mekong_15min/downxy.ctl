dset ^downxy.bin
undef -9999
title CaMa-Flood map file downxy.bin
options yrev little_endian
xdef 80 linear 90.125 0.25
ydef 120 linear 5.125 0.25
tdef 1 linear 00Z01jan2000 1yr
zdef 1 linear 1 1
vars 2
downx 1 -1,40,4,-1 Downstream high-resolution grid indices
downy 1 -1,40,4,-1 Downstream high-resolution grid indices
ENDVARS
