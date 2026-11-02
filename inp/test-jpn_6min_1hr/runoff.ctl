dset ^runoff_%y4%m2%d2.bin
undef -9999
title Japan hourly runoff, area-weighted 6-arcmin sample
options yrev little_endian template
xdef 250 linear 123.0500000000 0.1000000000
ydef 220 linear 24.0500000000 0.1000000000
tdef 168 linear 00Z03sep2007 1hr
zdef 1 linear 1 1
vars 1
runoff 0 99 ** runoff [mm/hour]
ENDVARS
