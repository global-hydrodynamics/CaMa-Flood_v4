#!/usr/bin/env python
# coding=utf-8
import numpy as np
import sys

years = sys.argv[1]
yeare = sys.argv[2]
ysize = int(sys.argv[3])
xsize = int(sys.argv[4])
outdir = sys.argv[5]
var = sys.argv[6]
rp = sys.argv[7]

afile = 'alpha_'+var+'_'+years+'-'+yeare+'.bin'
zfile = 'zeta_'+var+'_'+years+'-'+yeare+'.bin'

alpha = np.fromfile(outdir+'/G_para/'+afile, np.float64).reshape(ysize,xsize)
zeta = np.fromfile(outdir+'/G_para/'+zfile, np.float64).reshape(ysize,xsize)
rivhgt = np.fromfile(outdir+'/map/rivhgt.bin', np.float32).reshape(ysize,xsize)

Nflddph = np.zeros((ysize,xsize), dtype = np.float64)
RPP = 1.0 - (1.0/float(rp))
Nrivdph = 0.0

for i in range(ysize):
	for j in range(xsize):
		if alpha[i,j] == -9999:
			Nflddph[i,j] = -9999
		elif alpha[i,j] == 0.0:
			Nflddph[i,j] = 0.0
		else:
			if rivhgt[i,j] != -9999:
				Nrivdph = zeta[i,j] - (alpha[i,j] * np.log(-1.0 * np.log(RPP)))
				Nflddph[i,j] = Nrivdph - rivhgt[i,j]
				if Nflddph[i,j] < 0.0:
					Nflddph[i,j] = 0.0

fflddph = 'flddph_RP'+rp+'.bin'
Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 
