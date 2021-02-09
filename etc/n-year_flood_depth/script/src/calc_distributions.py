#!/usr/bin/env python
# coding=utf-8
import sys
import os
import numpy as np

import lmoments as lmom
from lmoments import *
#from math import *
from RegscorePy import *
import time

# ==============================================================
#  Arguments
# ==============================================================

years = int(sys.argv[1])
yeare = int(sys.argv[2])
ysize = int(sys.argv[3])
xsize = int(sys.argv[4])
outdir = sys.argv[5]
var = sys.argv[6]
FUNC = sys.argv[7]
try:
    norm = sys.argv[8]
except:
    norm = ''
yearlist= np.arange(years, yeare+1)


# ==============================================================
def progressbar(it, prefix="", size=60, file=sys.stdout):
    count = len(it)
    def show(j):
        x = int(size*j/count)
        file.write("%s[%s%s] %i/%i\r" % (prefix, "#"*x, "."*(size-x), j, count))
        file.flush()        
    show(0)
    for i, item in enumerate(it):
        yield item
        show(i+1)
    file.write("\n")
    file.flush()

# ==============================================================
#  Prepare the maximum values
# ==============================================================

datay = []

for year in yearlist:
    FDAT = outdir+ '/amax/'+ var + str(year) + '_anmax.bin'
    dataa = np.fromfile(FDAT, 'float32').reshape(ysize,xsize)
    datay = np.append(datay,dataa)
    del dataa

def normalization(data, norm):
    data_back = np.copy(data)
    if np.nanmean(data) == -9999:
        data_back[:] = -9999
    else:
        if norm == "_norm":
            data_back = (data - np.nanmean(data)) / np.nanstd(data)
        elif norm == "_maxmin":
            data_back = (data - np.nanmin(data)) / (np.nanmax(data) - np.nanmin(data))
    return data_back

datm = datay.reshape(-1,ysize,xsize)
for i in range(ysize):
    for j in range(xsize):
        #datm[:,i,j] = sorted(datm[:,i,j],reverse=True)
        datm[:,i,j] = sorted(datm[:,i,j])
        if norm != '':
            datm[:,i,j] = normalization(datm[:,i,j], norm)

# ==============================================================
#  Calculate the parameters for each function 
#  1. GEV
# ==============================================================

para1 = np.zeros((ysize, xsize))
para2 = np.zeros((ysize, xsize))
para3 = np.zeros((ysize, xsize))
para4 = np.zeros((ysize, xsize))
para5 = np.zeros((ysize, xsize))
p_AIC = np.zeros((ysize, xsize))
c_AIC = np.zeros((ysize, xsize))
py_AIC = np.zeros((ysize, xsize))
para1[:] = -9999.
para2[:] = -9999.
para3[:] = -9999.
para4[:] = -9999.
para5[:] = -9999.
p_AIC[:] = -9999. 
c_AIC[:] = -9999. 

N = datm.shape[0]
p = np.arange(1./(N+1), 1., 1./(N+1))
daty = np.zeros((N))

def calc_aic( xx, yy):
    resid = yy - xx
    sse = sum(resid ** 2)
    aic = 2 - 2* np.log(sse)
    return aic

##################################################
# 1 GEV (General Extreme V)
# para1 = \mu
# para2 = \sigma
# para3 = \theta
if FUNC == 'GEV':
    #for i in progressbar(range(ysize), "Computing: ", 40):
    for i in range(ysize):
        if i % 10 == 0: print ( i, 'out of ', ysize )
        for j in range(xsize):
            if np.nanmean(datm[:,i,j]) > -9990.:
                # There are many grids with constant values or 
                # there is only one large value but others are constant.
                # We cannot calculate the parameters with this time series.
                if np.std(datm[:-5,i,j]) > 1e-5:
                    lmoms = lmom.samlmu(datm[:,i,j],4)
                    params = lmom.pelgev(lmoms)

                    try:
                        para1[i,j] = params[0]
                        para2[i,j] = params[1]
                        para3[i,j] = params[2]
                        p_AIC[i,j] = lmom.AIC(datm[:,i,j], FUNC)

                        y = lmom.quagev(p, params)
                        c_AIC[i,j] = calc_aic(datm[:,i,j], y) 
                        py_AIC[i,j] = aic.aic(datm[:,i,j], y, len(params)) 

                    except:
                        para1[i,j] = np.nanmean(datm[:,i,j])
                        para2[i,j] = -9999.
                        para3[i,j] = -9999.
                        p_AIC[i,j] = -9999.
                        c_AIC[i,j] = -9999.

    fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
    para1.astype('float64').tofile(outdir+'/para/'+FUNC+'_mu_'+fname)    
    para2.astype('float64').tofile(outdir+'/para/'+FUNC+'_sigma_'+fname)
    para3.astype('float64').tofile(outdir+'/para/'+FUNC+'_theta_'+fname)
    p_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_aic_'+fname)
    c_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_c_aic_'+fname)
    py_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_py_aic_'+fname)

##################################################
# 2 GAM: gamma
# para1 = \alpha
# para2 = \para2 [para2=xmom[0]/alpha]
if FUNC == 'GAM':
    #for i in progressbar(range(ysize), "Computing: ", 40):
    for i in range(ysize):
        if i % 10 == 0: print ( i, 'out of ', ysize )
        for j in range(xsize):
            if np.nanmean(datm[:,i,j]) > -9990.:
                # There are many grids with constant values or 
                # there is only one large value but others are constant.
                # We cannot calculate the parameters with this time series.
                if np.std(datm[:-5,i,j]) > 1e-5:
                    lmoms = lmom.samlmu(datm[:,i,j],4)
                    params = lmom.pelgam(lmoms)
                    try:
                        para1[i,j] = params[0]
                        para2[i,j] = params[1]
                        p_AIC[i,j] = lmom.AIC(datm[:,i,j], FUNC)
                        y = lmom.quagam(p, params)
                        c_AIC[i,j] = calc_aic(datm[:,i,j], y) 
                        py_AIC[i,j] = aic.aic(datm[:,i,j], y, len(params)) 
                    except:
                        para1[i,j] = -9999.
                        para2[i,j] = -9999.
                        p_AIC[i,j] = -9999.
                        c_AIC[i,j] = -9999.

    fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
    para1.astype('float64').tofile(outdir+'/para/'+FUNC+'_alpha_'+fname)    
    para2.astype('float64').tofile(outdir+'/para/'+FUNC+'_beta_'+fname)
    p_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_aic_'+fname)
    c_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_c_aic_'+fname)
    py_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_py_aic_'+fname)

##################################################
# 3 PE3: 
# para1 = \para1
# para2 = \para2 [para2=xmom[0]/alpha]
# para3 = \gamma
if FUNC == 'PE3':
    #for i in progressbar(range(ysize), "Computing: ", 40):
    for i in range(ysize):
        if i % 10 == 0: print ( i, 'out of ', ysize )
        for j in range(xsize):
            if np.nanmean(datm[:,i,j]) > -9990.:
                # There are many grids with constant values or 
                # there is only one large value but others are constant.
                # We cannot calculate the parameters with this time series.
                if np.std(datm[:10,i,j]) > 1e-5:
                    lmoms = lmom.samlmu(datm[:,i,j],4)
                    params = lmom.pelpe3(lmoms)
                    try:
                        para1[i,j] = params[0]
                        para2[i,j] = params[1]
                        para3[i,j] = params[2]
                        p_AIC[i,j] = lmom.AIC(datm[:,i,j], FUNC)
                        y = lmom.quape3(p, params)
                        c_AIC[i,j] = calc_aic(datm[:,i,j], y) 
                        py_AIC[i,j] = aic.aic(datm[:,i,j], y, len(params)) 
                    except:
                        print ( "error in calculating the params",i,j,params )
                        print ( "datm:" , datm[:,i,j] )
                        print ( "daty", lmom.quape3(p, [para1[i,j], para2[i,j], para3[i,j]]) )
                        para1[i,j] = -9999.
                        para2[i,j] = -9999.
                        para3[i,j] = -9999.
                        p_AIC[i,j] = -9999.
                        c_AIC[i,j] = -9999.
            
    fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
    para1.astype('float64').tofile(outdir+'/para/'+FUNC+'_para1_'+fname)    
    para2.astype('float64').tofile(outdir+'/para/'+FUNC+'_para2_'+fname)
    para3.astype('float64').tofile(outdir+'/para/'+FUNC+'_gamma_'+fname)
    p_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_aic_'+fname)
    c_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_c_aic_'+fname)
    py_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_py_aic_'+fname)

###################################################
# 4 GUM: 
# para1 = \U
# para2 = \A 
if FUNC == 'GUM':
    #for i in progressbar(range(ysize), "Computing: ", 40):
    for i in range(ysize):
        if i % 10 == 0: print ( i, 'out of ', ysize )
        for j in range(xsize):
            if np.nanmean(datm[:,i,j]) > -9990.:
                # There are many grids with constant values or 
                # there is only one large value but others are constant.
                # We cannot calculate the parameters with this time series.
                if np.std(datm[:-5,i,j]) > 1e-5:
                    lmoms = lmom.samlmu(datm[:,i,j],4)
                    params = lmom.pelgum(lmoms)
                    try:
                        para1[i,j] = params[0]
                        para2[i,j] = params[1]
                        p_AIC[i,j] = lmom.AIC(datm[:,i,j], FUNC)
                        y = lmom.quagum(p, params)
                        c_AIC[i,j] = calc_aic(datm[:,i,j], y) 
                        py_AIC[i,j] = aic.aic(datm[:,i,j], y, len(params)) 
                    except:
                        print ( "error in calculating the params",i,j,params ) 
                        print ( "datm:" , datm[:,i,j] )
                        print ( "daty", lmom.quagum(p, [para1[i,j], para2[i,j]]) )
                        sys.exit()
                        para1[i,j] = -9999.
                        para2[i,j] = -9999.
                        p_AIC[i,j] = -9999.
                        c_AIC[i,j] = -9999.

    fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
    para1.astype('float64').tofile(outdir+'/para/'+FUNC+'_U_'+fname)    
    para2.astype('float64').tofile(outdir+'/para/'+FUNC+'_A_'+fname)
    p_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_aic_'+fname)
    c_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_c_aic_'+fname)
    py_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_py_aic_'+fname)


##################################################
# 5 WEI: 
# para1 = \para1 [-pg[0]-beta]
# para2 = \beta
# para3 = \delta
if FUNC == 'WEI':
    #for i in progressbar(range(ysize), "Computing: ", 40):
    for i in range(ysize):
        if i % 10 == 0: print ( i, 'out of ', ysize )
        for j in range(xsize):
            if np.nanmean(datm[:,i,j]) > -9990.:
                # There are many grids with constant values or 
                # there is only one large value but others are constant.
                # We cannot calculate the parameters with this time series.
                if np.std(datm[:10,i,j]) > 1e-5:
                    lmoms = lmom.samlmu(datm[:,i,j],4)
                    params = lmom.pelwei(lmoms)
                    try:
                        para1[i,j] = params[0]
                        para2[i,j] = params[1]
                        para3[i,j] = params[2]
                        p_AIC[i,j] = lmom.AIC(datm[:,i,j], FUNC)
                        y = lmom.quawei(p, params)
                        c_AIC[i,j] = calc_aic(datm[:,i,j], y) 
                        py_AIC[i,j] = aic.aic(datm[:,i,j], y, len(params)) 
                        #print ( "success point:", i,j, lmoms, params )
                    except:
                        print ( "error in calculating the params",i,j,lmoms,params )
                        para1[i,j] = -9999.
                        para2[i,j] = -9999.
                        para3[i,j] = -9999.
                        p_AIC[i,j] = -9999.
                        c_AIC[i,j] = -9999.
            
    fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
    para1.astype('float64').tofile(outdir+'/para/'+FUNC+'_para1_'+fname)    
    para2.astype('float64').tofile(outdir+'/para/'+FUNC+'_beta_'+fname)
    para3.astype('float64').tofile(outdir+'/para/'+FUNC+'_delta_'+fname)
    p_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_aic_'+fname)
    c_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_c_aic_'+fname)
    py_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_py_aic_'+fname)

##################################################
# 6 WAK: 
# para1 = \XI
# para2 = \A
# para3 = \B
# para4 = \C
# para5 = \D
if FUNC == 'WAK':
    #for i in progressbar(range(ysize), "Computing: ", 40):
    for i in range(ysize):
        if i % 10 == 0: print ( i, 'out of ', ysize )
        for j in range(xsize):
            if np.nanmean(datm[:,i,j]) > -9990.:
                # There are many grids with constant values or 
                # there is only one large value but others are constant.
                # We cannot calculate the parameters with this time series.
                if np.std(datm[3:13,i,j]) > 1e-5:
                    lmoms = lmom.samlmu(datm[:,i,j],5)
                    params = lmom.pelwak(lmoms)
                    try:
                        para1[i,j] = params[0]
                        para2[i,j] = params[1]
                        para3[i,j] = params[2]
                        para4[i,j] = params[3]
                        para5[i,j] = params[4]
                        p_AIC[i,j] = lmom.AIC(datm[:,i,j], FUNC)
                        y = lmom.quawak(p, params)
                        c_AIC[i,j] = calc_aic(datm[:,i,j], y) 
                        py_AIC[i,j] = aic.aic(datm[:,i,j], y, len(params)) 
                        #print "success point:", i,j, params
                    except:
                        print ("error in calculating the params",i,j,params )
                        print ("datm:" , datm[:,i,j] )
                        print ("daty", lmom.quawak(p, [para1[i,j], para2[i,j], para3[i,j], para4[i,j], para5[i,j]]) )
                        print ("p_AIC", lmom.AIC(datm[:,i,j], FUNC) )
                        sys.exit()
                        para1[i,j] = -9999.
                        para2[i,j] = -9999.
                        para3[i,j] = -9999.
                        para4[i,j] = -9999.
                        para5[i,j] = -9999.
                        p_AIC[i,j] = -9999.
                        c_AIC[i,j] = -9999.
            
    fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
    para1.astype('float64').tofile(outdir+'/para/'+FUNC+'_XI_'+fname)    
    para2.astype('float64').tofile(outdir+'/para/'+FUNC+'_A_'+fname)
    para3.astype('float64').tofile(outdir+'/para/'+FUNC+'_B_'+fname)
    para4.astype('float64').tofile(outdir+'/para/'+FUNC+'_C_'+fname)
    para5.astype('float64').tofile(outdir+'/para/'+FUNC+'_D_'+fname)
    p_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_aic_'+fname)
    c_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_c_aic_'+fname)
    py_AIC.astype('float64').tofile(outdir+'/para/'+FUNC+'_py_aic_'+fname)

