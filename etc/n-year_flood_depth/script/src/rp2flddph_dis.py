#!/usr/bin/env python
# coding=utf-8
import numpy as np
import sys
import lmoments as lmom

years = sys.argv[1]
yeare = sys.argv[2]
ysize = int(sys.argv[3])
xsize = int(sys.argv[4])
outdir = sys.argv[5]
var = sys.argv[6]
rp = sys.argv[7]
FUNC = sys.argv[8]

rivhgt = np.fromfile(outdir+'/map/rivhgt.bin', np.float32).reshape(ysize,xsize)

Nflddph = np.zeros((ysize,xsize), dtype = np.float64)
if float(rp) > 1:
    RPP = 1.0 - (1.0/float(rp))
else:
    RPP = 1.0 - float(rp)
Nrivdph = 0.0

# For GEV distribution
if FUNC == "GEV":
    f_para1 = 'GEV_mu_'+var+'_'+years+'-'+yeare+'.bin'
    f_para2 = 'GEV_sigma_'+var+'_'+years+'-'+yeare+'.bin'
    f_para3 = 'GEV_theta_'+var+'_'+years+'-'+yeare+'.bin'

    para1 = np.fromfile(outdir+'/para/'+f_para1, np.float64).reshape(ysize,xsize)
    para2 = np.fromfile(outdir+'/para/'+f_para2, np.float64).reshape(ysize,xsize)
    para3 = np.fromfile(outdir+'/para/'+f_para3, np.float64).reshape(ysize,xsize)

    for i in range(ysize):
        for j in range(xsize):

            if para2[i,j] == -9999:
                Nflddph[i,j] = -9999
            elif para2[i,j] == 0.0 or para2[i,j] == -999.0:
                Nflddph[i,j] = 0.0
            else:
                if rivhgt[i,j] != -9999:
                    Nrivdph = lmom.quagev(RPP, [para1[i,j], para2[i,j], para3[i,j]])
                    Nflddph[i,j] = Nrivdph - rivhgt[i,j]
                    if Nflddph[i,j] < 0.0:
                        Nflddph[i,j] = 0.0

    fflddph = 'flddph_RP'+rp+'_'+ FUNC + '.bin'
    Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 

# For GAM distribution
if FUNC == "GAM":
    f_para1 = 'GAM_alpha_'+var+'_'+years+'-'+yeare+'.bin'
    f_para2 = 'GAM_beta_'+var+'_'+years+'-'+yeare+'.bin'

    para1 = np.fromfile(outdir+'/para/'+f_para1, np.float64).reshape(ysize,xsize)
    para2 = np.fromfile(outdir+'/para/'+f_para2, np.float64).reshape(ysize,xsize)

    for i in range(ysize):
        for j in range(xsize):
            if para2[i,j] == -9999:
                Nflddph[i,j] = -9999
            elif para2[i,j] == 0.0 or para2[i,j] == -999.0:
                Nflddph[i,j] = 0.0
            else:
                if rivhgt[i,j] != -9999:
                    Nrivdph = lmom.quagam(RPP, [para1[i,j], para2[i,j]])
                    Nflddph[i,j] = Nrivdph - rivhgt[i,j]
                    if Nflddph[i,j] < 0.0:
                        Nflddph[i,j] = 0.0

    fflddph = 'flddph_RP'+rp+'_'+ FUNC + '.bin'
    Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 

# For PE3 distribution
if FUNC == "PE3":
    f_para1 = 'PE3_para1_'+var+'_'+years+'-'+yeare+'.bin'
    f_para2 = 'PE3_para2_'+var+'_'+years+'-'+yeare+'.bin'
    f_para3 = 'PE3_gamma_'+var+'_'+years+'-'+yeare+'.bin'

    para1 = np.fromfile(outdir+'/para/'+f_para1, np.float64).reshape(ysize,xsize)
    para2 = np.fromfile(outdir+'/para/'+f_para2, np.float64).reshape(ysize,xsize)
    para3 = np.fromfile(outdir+'/para/'+f_para3, np.float64).reshape(ysize,xsize)

    for i in range(ysize):
        for j in range(xsize):
            if para2[i,j] == -9999. :
                Nflddph[i,j] = -9999.
            elif  para2[i,j] == -999.0 :
                Nflddph[i,j] = 0.0
            else:
                if rivhgt[i,j] != -9999:
                    Nrivdph = lmom.quape3(RPP, [para1[i,j], para2[i,j], para3[i,j]])
                    Nflddph[i,j] = Nrivdph - rivhgt[i,j]
                    if Nflddph[i,j] < 0.0:
                        Nflddph[i,j] = 0.0

    fflddph = 'flddph_RP'+rp+'_'+ FUNC + '.bin'
    Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 

# For GUM distribution
if FUNC == "GUM":
    f_para1 = 'GUM_U_'+var+'_'+years+'-'+yeare+'.bin'
    f_para2 = 'GUM_A_'+var+'_'+years+'-'+yeare+'.bin'

    para1 = np.fromfile(outdir+'/para/'+f_para1, np.float64).reshape(ysize,xsize)
    para2 = np.fromfile(outdir+'/para/'+f_para2, np.float64).reshape(ysize,xsize)

    for i in range(ysize):
        for j in range(xsize):
            if para2[i,j] == -9999:
                Nflddph[i,j] = -9999
            elif para2[i,j] == -999.0:
                Nflddph[i,j] = 0.0
            else:
                if rivhgt[i,j] != -9999:
                    Nrivdph = lmom.quagum(RPP, [para1[i,j], para2[i,j]])
                    Nflddph[i,j] = Nrivdph - rivhgt[i,j]
                    if Nflddph[i,j] < 0.0:
                        Nflddph[i,j] = 0.0

    fflddph = 'flddph_RP'+rp+'_'+ FUNC + '.bin'
    Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 

# For WEI distribution
if FUNC == "WEI":
    f_para1 = 'WEI_para1_'+var+'_'+years+'-'+yeare+'.bin'
    f_para2 = 'WEI_beta_'+var+'_'+years+'-'+yeare+'.bin'
    f_para3 = 'WEI_delta_'+var+'_'+years+'-'+yeare+'.bin'

    para1 = np.fromfile(outdir+'/para/'+f_para1, np.float64).reshape(ysize,xsize)
    para2 = np.fromfile(outdir+'/para/'+f_para2, np.float64).reshape(ysize,xsize)
    para3 = np.fromfile(outdir+'/para/'+f_para3, np.float64).reshape(ysize,xsize)

    for i in range(ysize):
        for j in range(xsize):
            if para2[i,j] == -9999. :
                Nflddph[i,j] = -9999.
            elif  para2[i,j] == -999.0 :
                Nflddph[i,j] = 0.0
            else:
                if rivhgt[i,j] != -9999:
                    Nrivdph = lmom.quawei(RPP, [para1[i,j], para2[i,j], para3[i,j]])
                    Nflddph[i,j] = Nrivdph - rivhgt[i,j]
                    if Nflddph[i,j] < 0.0:
                        Nflddph[i,j] = 0.0

    fflddph = 'flddph_RP'+rp+'_'+ FUNC + '.bin'
    Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 


# For WAK distribution
if FUNC == "WAK":
    f_para1 = 'WAK_XI_'+var+'_'+years+'-'+yeare+'.bin'
    f_para2 = 'WAK_A_'+var+'_'+years+'-'+yeare+'.bin'
    f_para3 = 'WAK_B_'+var+'_'+years+'-'+yeare+'.bin'
    f_para4 = 'WAK_C_'+var+'_'+years+'-'+yeare+'.bin'
    f_para5 = 'WAK_D_'+var+'_'+years+'-'+yeare+'.bin'

    para1 = np.fromfile(outdir+'/para/'+f_para1, np.float64).reshape(ysize,xsize)
    para2 = np.fromfile(outdir+'/para/'+f_para2, np.float64).reshape(ysize,xsize)
    para3 = np.fromfile(outdir+'/para/'+f_para3, np.float64).reshape(ysize,xsize)
    para4 = np.fromfile(outdir+'/para/'+f_para4, np.float64).reshape(ysize,xsize)
    para5 = np.fromfile(outdir+'/para/'+f_para5, np.float64).reshape(ysize,xsize)

    for i in range(ysize):
        for j in range(xsize):
            if para2[i,j] == -9999. :
                Nflddph[i,j] = -9999.
            elif  para2[i,j] == -999.0 :
                Nflddph[i,j] = 0.0
            else:
                if rivhgt[i,j] != -9999:
                    Nrivdph = lmom.quawak(RPP, [para1[i,j], para2[i,j], para3[i,j], para4[i,j], para5[i,j]])
                    Nflddph[i,j] = Nrivdph - rivhgt[i,j]
                    if Nflddph[i,j] < 0.0:
                        Nflddph[i,j] = 0.0

    fflddph = 'flddph_RP'+rp+'_'+ FUNC + '.bin'
    Nflddph.astype(np.float32).tofile(outdir+'/Nyear_flddph/'+fflddph) 
