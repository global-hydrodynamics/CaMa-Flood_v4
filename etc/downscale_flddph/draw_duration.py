import sys
import os

import numpy as np
from numpy import *

import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import colors
from matplotlib.ticker import FormatStrFormatter
from mpl_toolkits.axes_grid1 import make_axes_locatable

import warnings;warnings.filterwarnings('ignore')

class BoundaryNorm(colors.Normalize):
    def __init__(self, boundaries):
        self.vmin = boundaries[0]
        self.vmax = boundaries[-1]
        self.boundaries = boundaries
        self.N = len(self.boundaries)

    def __call__(self, x, clip=False):
        x = np.asarray(x)
        ret = np.zeros(x.shape, dtype=np.int)
        for i, b in enumerate(self.boundaries):
            ret[np.greater_equal(x, b)] = i
        ret[np.less(x, self.vmin)] = -1
        ret = np.ma.asarray(ret / float(self.N-1))
        return ret

argv  = sys.argv
west  = int( argv[1] )
east  = int( argv[2] )
south = int( argv[3] )
north = int( argv[4] )
fflood = argv[5]
fslope = argv[6]
ngrid = int( argv[7] )
hires = argv[8]
tag = argv[9]

if( hires=="3sec" ):
    csize=1./1200.
if( hires=="15sec" ):
    csize=1./240.
if( hires=="30sec" ):
    csize=1./120.
if( hires=="1min" ):
    csize=1./60.

dx=csize
dy=csize
nx=int( (east -west )/csize/ngrid+0.5 )
ny=int( (north-south)/csize/ngrid+0.5 )
print ( nx, ny )

ssize=int(20)
fsize=int(ssize*1.5)

xlint=1.0
if( east-west>=10   ):
    xlint=5.0
if( north-south>=10 ):
    xlint=5.0
ylint=xlint

fig=plt.figure(figsize=(ssize,ssize*0.8),dpi=100)
fig.subplots_adjust(left=0.05,right=0.95,top=0.90,bottom=0.05)

ax = fig.add_subplot(1,1,1)

#===================================

rfile=fslope
slp=np.fromfile(rfile,float32).reshape(ny,nx)
slp=np.ma.masked_where(slp<-9000,slp)

im=plt.imshow(slp,cmap=cm.gray,extent=(west,east,south,north))

plt.tick_params(labelsize=ssize,pad=5,length=5)
plt.xticks( np.arange(west,east+0.000001,xlint) )
plt.yticks( np.arange(south,north+0.000001,ylint) )

ysf=(north-south)*0.025
ctext="Flood Duration Days [ "+tag+" ]"
plt.text(west,north+ysf,ctext,fontsize=fsize)

#===================================

#rfile="./dph"+cdate+".bin"
rfile=fflood
fldday=np.fromfile(rfile,float32).reshape(ny,nx)
fldday=np.ma.masked_where(fldday<0.1,fldday)

interval = [1,3,5,10,20,30,60,90,120,180,240,300,365]
norml = colors.BoundaryNorm(interval, 256)

im2=plt.imshow(fldday,cmap=cm.YlOrRd,norm=norml,extent=(west,east,south,north))

divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="2%", pad=0.15)

cbar=plt.colorbar(im2, cax=cax)
#cbar=plt.colorbar(im3, cax=cax, extend='max', orientation="horizontal")
cbar.set_label('Flood DUration [Day]', size=ssize*1.2)
cbar.set_ticks(interval)

plt.savefig("./fig/fldprd"+tag+".jpg")

quit()

