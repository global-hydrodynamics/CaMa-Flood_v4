#!/usr/bin/env python
# coding=utf-8
# store and plot global maps of GSWO
# Option is the Water Suface Fraction Ratio (as a permanent water surface)
# and the spatial resolution to store and plot the maps (0.1 degree, 0.5)
import sys,os 
import numpy as np
import matplotlib
matplotlib.use('Agg')
import glob
from netCDF4 import Dataset
from pylab import *
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import colors
from matplotlib.ticker import FormatStrFormatter
from mpl_toolkits.axes_grid1 import make_axes_locatable
from mpl_toolkits.basemap import Basemap

from matplotlib.backends.backend_pdf import PdfPages

import warnings;warnings.filterwarnings('ignore')


argv=sys.argv
f=argv[1]
vname=argv[2]



# read the data 
o = Dataset(f,'r')
try:
    lat = o.variables['lat'][:]
    lon = o.variables['lon'][:]
except:
    lat = o.variables['latitude'][:]
    lon = o.variables['longitude'][:]

data = o.variables[vname][:]

print 'output shape:', data.shape
data=np.nanmean( data, axis=0)
if lat[1]<lat[0]:
    data = np.flipud(data)

data = np.ma.masked_where(data>1e9, data)
data = np.ma.masked_where(data<0., data)
print 'max, min, mean, sum :', data.max(),data.min(),np.mean(data), np.sum(data)

# plots

ssize=12
pdf = PdfPages('test.pdf')

fig=plt.figure(figsize=(ssize,ssize*0.8),dpi=300)
fig.subplots_adjust(left=0.05,right=0.85,top=0.90,bottom=0.05)

ax = fig.add_subplot(1,1,1)

west = np.min(lon)
east = np.max(lon)
south = np.min(lat)
north = min(np.max(lat),90)

print west, east, south, north

m = Basemap(llcrnrlon=west, llcrnrlat=south, urcrnrlon=east, urcrnrlat=north, resolution='i', projection='cyl')
span = 30
m.drawparallels(np.arange(-90, north+0.01, span), labels=[1,0,0,0], linewidth=0) # draw latitudes
m.drawmeridians(np.arange(-180, east+0.01, span), labels=[0,0,0,1], linewidth=0) # draw longitudes

im2=plt.imshow(data, cmap=cm.YlOrRd, origin='lower', extent=(np.min(lon), np.max(lon), np.min(lat), np.max(lat)), zorder=10)

divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="2%", pad=0.15)

cbar=plt.colorbar(im2, cax=cax)
cbar.set_label('To be set', size=ssize*1.2)
cbar.ax.tick_params(labelsize=ssize)


savefig("test.jpg")
pdf.savefig()
plt.close()

pdf.close()

