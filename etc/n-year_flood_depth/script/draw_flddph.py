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
west  = float( argv[1] )
east  = float( argv[2] )
south = float( argv[3] )
north = float( argv[4] )

ngrid = int( argv[5] )
hires = argv[6]
md = float( argv[7] )
maxdph = int(md)
fflood = argv[8]
RP = float(argv[9])

if RP < 1:
    RP = int(1/RP)

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

ssize=int(12)
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

rfile="./slp.bin"
slp=np.fromfile(rfile,float32).reshape(ny,nx)
slp=np.ma.masked_where(slp<-9000,slp)

im=plt.imshow(slp,cmap=cm.gray,extent=(west,east,south,north))

plt.tick_params(labelsize=ssize,pad=5,length=5)
plt.xticks( np.arange(west,east+0.000001,xlint) )
plt.yticks( np.arange(south,north+0.000001,ylint) )

ysf=(north-south)*0.05
ctext="CaMa-Flood v4.0: flood risk map with return period "+ str(RP) + ' years'
plt.text(west,north+ysf,ctext,fontsize=fsize)

#===================================

#rfile="./dph"+cdate+".bin"
#rfile="./Mekong-e2o/downscaled_flddph/N09E102_RP50_3sec.bin"
rfile = fflood
dph=np.fromfile(rfile,float32).reshape(ny,nx)
dph=np.ma.masked_where(dph<-9000,dph)
#dph=np.ma.masked_where(dph<0.01,dph)

interval=np.array(range(maxdph))
norml=colors.BoundaryNorm(interval,256) 

im2=plt.imshow(dph,cmap=cm.YlGnBu,norm=norml,extent=(west,east,south,north))

divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="2%", pad=0.15)

cbar=plt.colorbar(im2, cax=cax)
#cbar=plt.colorbar(im3, cax=cax, extend='max', orientation="horizontal")
cbar.set_label('Flood Depth [m]', size=ssize*1.2)
cbar.set_ticks(interval)
cbar.ax.tick_params(labelsize=ssize)


#savefig("./fig/flddph_"+cdate+".jpg")
plt.savefig("./fig/fldris_"+str(RP)+".jpg")







