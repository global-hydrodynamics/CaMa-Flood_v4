dset  ^./runoff_%y4%m2%d2.bin
undef 1e+20
title ERA5Land Runoff
options yrev little_endian template
xdef    321 linear 122 0.1
ydef    221 linear 24 0.1
tdef     62 linear  00Z01jan2000 1dy
zdef      1 linear  1 1
vars 1
var 0 99       ** runoff [mm/day]
ENDVARS
