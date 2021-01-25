#!/usr/bin/env python
# coding=utf-8
# ==============================================================
#  Description
# ==============================================================
#
#  Calcualte flood water depht with a consideration of storage
#  corresponded to flood protection standard.
#
#  Developed for by masatano (181214)
#  Improved by masatano (190111)
#
#  Noted that I applied interporation method for estimating 
#  flddph to reduce steps.
#
# ==============================================================
#  Import
# ==============================================================
import numpy as np
import sys
import argparse
# ==============================================================
#  Get Arguments
# ==============================================================
years = int(sys.argv[1])
yeare = int(sys.argv[2])
ysize = int(sys.argv[3])
xsize = int(sys.argv[4])
outdir = sys.argv[5]
rp = sys.argv[6]
FUNC = sys.argv[7]

# ==============================================================
#  Arguments
# ==============================================================
NL = xsize * ysize
NH = 10
fRivLen = './map/rivlen.bin'
fRivWth = './map/rivwth.bin'
fRivHgt = './map/rivhgt.bin'
fGrArea = './map/ctmare.bin'
fFldHgt = './map/fldhgt.bin'
# ==============================================================
#  Read
# ==============================================================
RivLen = np.fromfile(fRivLen, np.float32)
RivWth = np.fromfile(fRivWth, np.float32)
RivHgt = np.fromfile(fRivHgt, np.float32)
GrArea = np.fromfile(fGrArea, np.float32)
FldHgt = np.fromfile(fFldHgt, np.float32).reshape([NH, NL])
#
#fStorge = outdir + '/amax/storge'+ str(yr) +'_anmax.bin'
fStorge = outdir + '/Nyear_storge/storge_RP' + rp + '_' + FUNC + '.bin'
StorgeAll = np.fromfile(fStorge, np.float32).reshape([-1, NL])
#FPLRivStoMax = np.fromfile(fFPLRivStoMax, np.float32)
FPLRivStoMax = np.copy(RivLen)
FPLRivStoMax[:] = 0
# ==============================================================
#  Pre-process
# ==============================================================
NT = StorgeAll[:, 0].size
FldDph = np.zeros([NT, NL])
# ==============================================================
#  Calcualte StorgeMax and FldGrd
# ==============================================================
FldGrd = np.zeros([NH, NL])
StorgeMax = np.zeros([NH+1, NL])
FPLStorgeMax = np.zeros([NH+1, NL])

WthInc = GrArea / RivLen / float(NH)
RivStoMax = RivHgt * RivLen * RivWth
Storge = RivStoMax
StorgeMax[0, :] = RivStoMax  # add

FPLStorge = FPLRivStoMax
FldHgtPre = np.zeros(NL)
FPLStoHgt = FPLRivStoMax/RivLen/RivWth
FPLStorgeMax[0, :] = FPLRivStoMax
FPLStorgeNow = np.zeros(NL)

for ilev in range(NH):
    FldHgtNow = FldHgt[ilev, :]
    StorgeNow = RivLen * ( RivWth + WthInc * (ilev+0.5) ) * (FldHgtNow-FldHgtPre)
    StorgeNow[np.isnan(StorgeNow)] = 0.0
    StorgeMax[ilev+1, :] = Storge + StorgeNow
    #
    index = np.where((FldHgtNow >= FPLStoHgt) & (FldHgtPre >= FPLStoHgt))
    FPLStorgeNow[index] = StorgeNow[index]
    index = np.where((FldHgtNow >= FPLStoHgt) & (FldHgtPre < FPLStoHgt))
    FPLStorgeNow[index] = StorgeNow[index] - RivLen[index] * RivWth[index] * (FPLStoHgt[index]-FldHgtPre[index])
    index = np.where((FldHgtNow < FPLStoHgt) & (FldHgtPre < FPLStoHgt))
    FPLStorgeNow[index] = StorgeNow[index] - RivLen[index] * RivWth[index] * (FldHgtNow[index]-FldHgtPre[index])
    FPLStorgeMax[ilev+1, :] = FPLStorgeMax[ilev, :] + FPLStorgeNow
    #
    Storge = StorgeMax[ilev+1, :]
    FPLStorge = FPLStorgeMax[ilev+1, :]
    FldGrd[ilev, :] = (FldHgtNow-FldHgtPre) / WthInc
    FldHgtPre = FldHgt[ilev, :]
    #
for ilev in range(NH+1):
    index = np.where(FPLStorgeMax[ilev, :]<StorgeMax[ilev, :])[0]
    FPLStorgeMax[ilev, index] = StorgeMax[ilev, index]
# ==============================================================
#  Main
# ==============================================================
for it in range(NT):

    Storge = StorgeAll[it, :]

    # Calcualte flood stage
    FldStg = np.zeros(NL, np.int32)
    for ilev in range(NH+1):
        FldStg = FldStg + (Storge > FPLStorgeMax[ilev, :])

    WthPre = RivWth + WthInc * (FldStg-1)
    StoMax = np.zeros(NL)
    StoMaxPst = np.zeros(NL)
    index = np.where(FldStg == 0)
    StoMax[index] = FPLStorgeMax[0, index]
    StoMaxPst[index] = FPLStorgeMax[0, index]

    for ilev in range(1, NH+1):
        index = np.where(FldStg == ilev)
        StoMax[index] = FPLStorgeMax[ilev-1, index]
        StoMaxPst[index] = FPLStorgeMax[ilev, index]
    index = np.where(FldStg == NH+1)
    StoMax[index] = FPLStorgeMax[NH, index]
    StoMaxPst[index] = FPLStorgeMax[NH, index]

    DphPre = np.zeros(NL)
    for ilev in range(1, NH+1):
        index = np.where(FldStg > ilev)
        DphPre[index] = DphPre[index] + FldGrd[ilev-1, index] * WthInc[index]



    StoNow = Storge - StoMax
    StoNow = np.ma.masked_less(StoNow, 0).filled(0.)
    WthNow = np.zeros(NL)
    Ratio = np.zeros(NL)

    for ilev in range(1, NH+1):
        index = np.where( FldStg==ilev )
        WthNow[index] = -WthPre[index] \
                      + np.sqrt( np.power(WthPre[index], 2.0) 
                      + 2.0*StoNow[index]/RivLen[index]/FldGrd[ilev-1, index])
        #Ratio[index] = (Storge[index]-StoMax[index]) / (StoMaxPst[index]-StoMax[index])
        #WthNow[index] = Ratio[index] * WthInc[index]
        FldDph[it, index] = DphPre[index] + FldGrd[ilev-1, index] * WthNow[index]
    index = np.where(FldStg == NH+1)
    FldDph[it, index] = DphPre[index] + StoNow[index] / WthPre[index] / RivLen[index]
# ==============================================================
#  Write
# ==============================================================
# flddph
index = np.where(RivWth == -9999.)
FldDph[:, index] = -9999.
fFldDph = outdir + '/Nyear_flddph/sto2flddph_RP' + rp + '_' + FUNC + '.bin'
FldDph.astype(np.float32).tofile(fFldDph)


