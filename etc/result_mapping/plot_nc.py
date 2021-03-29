import sys,os 
import numpy as np
import matplotlib
matplotlib.use('Agg')
import glob
from netCDF4 import Dataset
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import colors
from matplotlib.ticker import FormatStrFormatter
from mpl_toolkits.axes_grid1 import make_axes_locatable

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

print ('output shape:', data.shape)
data=np.nanmean( data, axis=0)
if lat[1]<lat[0]:
    data = np.flipud(data)

data = np.ma.masked_where(data>1e9, data)
data = np.ma.masked_where(data<0., data)
print ('max, min, mean, sum :', data.max(),data.min(),np.mean(data), np.sum(data))

# plots

ssize=12
pdf = PdfPages('test2.pdf')

fig=plt.figure(figsize=(ssize,ssize*0.8),dpi=300)
fig.subplots_adjust(left=0.05,right=0.85,top=0.90,bottom=0.05)

ax = fig.add_subplot(1,1,1)

west = np.min(lon)
east = np.max(lon)
south = np.min(lat)
north = min(np.max(lat),90)

print (west, east, south, north)

im2=plt.imshow(data, cmap=cm.YlOrRd, origin='lower', extent=(west, east, south, north), zorder=10)

divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="2%", pad=0.15)

cbar=plt.colorbar(im2, cax=cax)
cbar.set_label('To be set', size=ssize*1.2)
cbar.ax.tick_params(labelsize=ssize)


plt.savefig("test2.jpg")
pdf.savefig()
plt.close()

pdf.close()

