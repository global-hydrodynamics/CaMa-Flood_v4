import sys,os 
import numpy as np
import matplotlib
matplotlib.use('Agg')
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import colors
from matplotlib.ticker import FormatStrFormatter
from mpl_toolkits.axes_grid1 import make_axes_locatable

from matplotlib.backends.backend_pdf import PdfPages


argv=sys.argv
f=argv[1]
ny=int(argv[2])
nx=int(argv[3])
ftype='float32'


# read the data 
data = np.fromfile(f, ftype).reshape(-1,ny,nx)

print ('output shape:', data.shape)
data=np.nanmean( data, axis=0)

data = np.ma.masked_where(data>1e9, data)
data = np.ma.masked_where(data<0., data)
print ('max, min, mean, sum :', data.max(),data.min(),np.mean(data), np.sum(data))
#data = data/1000.*365.

# plots
ssize=12
pdf = PdfPages('test1.pdf')

fig=plt.figure(figsize=(ssize,ssize*0.8),dpi=300)
fig.subplots_adjust(left=0.05,right=0.85,top=0.90,bottom=0.05)

ax = fig.add_subplot(1,1,1)
im2=plt.imshow(data, cmap=cm.YlOrRd, vmax=50000)

divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="2%", pad=0.15)

cbar=plt.colorbar(im2, cax=cax, extend="max")
cbar.set_label('Label to be set', size=ssize*1.2)
cbar.ax.tick_params(labelsize=ssize)

plt.savefig("test1.jpg")

pdf.savefig()
plt.close()

pdf.close()

