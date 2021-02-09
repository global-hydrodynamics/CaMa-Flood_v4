#!/usr/bin/env python
# coding=utf-8
import sys
import os
import numpy as np

years = int(sys.argv[1])
yeare = int(sys.argv[2])
ysize = int(sys.argv[3])
xsize = int(sys.argv[4])
outdir = sys.argv[5]
var = sys.argv[6]
yearlist= np.arange(years, yeare+1)

nyears = yeare - years + 1

amax_total = np.zeros((nyears, ysize, xsize), 'f')

for i in range(nyears):
#for year in yearlist:
    year = yearlist[i]
    print ( year, )
    sys.stdout.flush()
    filename = var + str(year) + '.bin'
    fdat = outdir + '/inp/' + filename
    fd = np.fromfile(fdat, "float32").reshape(-1,ysize,xsize).max(0)
    fd = np.ma.masked_greater(fd,1.e18).filled(-9999.)
    # write the data to .bin file
    filename2 = var + str(year) + '_anmax.bin'
    fd.astype('float32').tofile(outdir+'/amax/'+filename2)

    amax_total[i,:,:] = fd

amax_ave = np.nanmean(amax_total, axis=0)
filename2 = var + '-' + str(years) + '-' + str(yeare) + '_anmax_ave.bin'
amax_ave.astype('float32').tofile(outdir+'/amax/'+filename2)


##################################################
# calculate the average maximum dph 
##################################################
if var == 'rivdph': 
    rivhgt = np.fromfile(outdir+'/map/rivhgt.bin', np.float32).reshape(ysize,xsize)

    flddph_ave = amax_ave - rivhgt 
    flddph_ave[flddph_ave < 0.] = 0.
    flddph_ave[rivhgt==-9999] = -9999
    filename2 = 'flddph-' + str(years) + '-' + str(yeare) + '_anmax_ave.bin'
    amax_ave.astype('float32').tofile(outdir+'/amax/'+filename2)

        
