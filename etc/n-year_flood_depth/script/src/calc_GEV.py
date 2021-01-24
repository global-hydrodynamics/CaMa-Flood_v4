#!/usr/bin/env python
# coding=utf-8
# ==============================================================
#  Import
# ==============================================================
import numpy
from numpy import *
from math import *
import math as MyMath
import os
import sys
# ==============================================================
#  Arguments
# ==============================================================

years = int(sys.argv[1])
yeare = int(sys.argv[2])
ysize = int(sys.argv[3])
xsize = int(sys.argv[4])
outdir = sys.argv[5]
var = sys.argv[6]
yearlist= arange(years, yeare+1)

###########################
# setting parameters
###########################

def compute_GEV_Parameters(annMaxVals, numVals, params_GEV):
    #calculate L-moment estimators (pg. 18.6-7, Chapter 18, Handbook of Hydrology, Maidment (Editor))

    #sort annual maximums, in descending order
    annMaxVals2 = sorted(annMaxVals, reverse=True)

    pwm_b0 = float(sum(annMaxVals2))/float(numVals)
    #
    pwm_b1 = 0.0
    for indexi in range(1, numVals, 1):
        pwm_b1 += ((numVals - indexi) * annMaxVals2[indexi - 1])
    pwm_b1 /=  float(numVals * (numVals - 1))
    
    # pwm_b2 = 0.0
    # for indexi in range(1, numVals-1, 1):
    #    pwm_b2 += ((numVals - indexi) * (numVals - indexi - 1) * annMaxVals2[indexi - 1])
    # pwm_b2 /=  float(numVals * (numVals - 1) * (numVals - 2))
    
    # pwm_b3 = 0.0
    # for indexi in range(1, numVals-2, 1):
    #    pwm_b3 += ((numVals - indexi) * (numVals - indexi - 1) * (numVals - indexi - 2) * annMaxVals2[indexi - 1])
    # pwm_b3 /=  float(numVals * (numVals - 1) * (numVals - 2) * (numVals - 3))

    #L-moments
    lmom1 = pwm_b0
    lmom2 = (2*pwm_b1) - pwm_b0
    alpha=lmom2/MyMath.log(2)
    zeta=lmom1-alpha*0.57721
    params_GEV[0] = alpha
    params_GEV[1] = zeta

disch_d = zeros((ysize,xsize))
alpha = zeros((ysize,xsize))
zeta = zeros((ysize,xsize))
gamma = zeros((ysize,xsize))
datay = []

for year in yearlist:
  FDAT = outdir+ '/amax/'+ var + str(year) + '_anmax.bin'
  dataa = fromfile(FDAT, float32).reshape(ysize,xsize)
  datay = append(datay,dataa)
  del dataa
datm = datay.reshape(-1,ysize,xsize)
avg = datm.mean(0)
stdv = numpy.std(datm,axis=0)
for i in range(ysize):
  for j in range(xsize):
    if avg[i,j]==-9999.:
      alpha[i,j] = -9999
      zeta[i,j] = -9999
    else:
      params_GEV_d = [alpha[i,j],zeta[i,j],gamma[i,j]]
      numVals_d = len(datm)
      compute_GEV_Parameters(datm[...,i,j], numVals_d, params_GEV_d)
      alpha[i,j] = params_GEV_d[0]
      zeta[i,j] = params_GEV_d[1]
      fname = var + '_' + str(years) + '-' + str(yeare) + '.bin'
alpha.astype(float64).tofile(outdir+'/G_para/alpha_'+fname)    
zeta.astype(float64).tofile(outdir+'/G_para/zeta_'+fname)


