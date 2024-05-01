import os
import numpy as np
import pandas as pd
from scipy.signal import argrelmax
import sys

print(os.path.basename(__file__))

#### initial setting =====================================

syear=int(sys.argv[1])
eyear=int(sys.argv[2])
dt   =int(sys.argv[3])
tag  =sys.argv[4]

outdir   = './inp/natsim/'
mapdir   = './inp/map/'
#dam_file = './'+tag+'/damloc_modified.csv'
dam_file = './inp/damlist.txt'

## get map nx, ny ----------------------------------------

f = open(mapdir+'/params.txt', 'r')
data = f.readline()
nx   = int(data.strip().split(' ')[0])
data = f.readline()
ny   = int(data.strip().split(' ')[0])
f.close()
print('CaMa map dim (nx,ny):', nx,ny)

damcsv = pd.read_csv(dam_file, sep='\s+', header=0, skipinitialspace=True)

ndams = len(damcsv)
print('number of dams:', ndams)

##--------------------------------------------------

maxdays = 1   #number of days to consider extreme values in a year

max_outf = './'+tag+'/tmp_p01_AnnualMax.bin'
mean_outf= './'+tag+'/tmp_p01_AnnualMean.bin'

### calculate annual maximum -------------------------------

years = eyear - syear + 1
max_finarray = np.zeros((years*maxdays, ndams))
mean_yeararray = np.zeros((years, ndams))

dam_output = np.zeros((3, ndams))

x_arr = damcsv['ix'].values - 1
y_arr = damcsv['iy'].values - 1

print(x_arr)
print(y_arr)

for i, year in enumerate(range(syear, eyear+1, 1)):
    print(' ')
    print('read natsim outflw: year=', year)

    ## read NAT outflw
    outflw_file = outdir + '/outflw' + str(year) + '.bin'
    outflw_all = np.fromfile(outflw_file, 'float32').reshape(-1,ny,nx)
    print(outflw_file)

    outflw_dam = outflw_all[:,y_arr,x_arr]
    print('outflw_dam.shape:', outflw_dam.shape)

    ## annual mean
    mean_yeararray[i,:] = np.mean(outflw_dam, axis=0)
    print('mean:', mean_yeararray[i,:5])

    ## annual maximum
    for j, row in damcsv.iterrows():
        outflw = outflw_dam[:,j]
        maxindex = argrelmax(outflw, order=8*7)
        maxarray = outflw[maxindex]
        maxarray_sorted = np.sort(maxarray)[::-1]
        if len(maxarray_sorted) > 0:
            max_finarray[i*maxdays:(i+1)*maxdays, j] = maxarray_sorted[0:maxdays]
        else:
            outflw_sorted = np.sort(outflw)[::-1]
            max_finarray[i*maxdays:(i+1)*maxdays, j] = outflw_sorted[0:maxdays]
    print('max:', max_finarray[i*maxdays,:5])
    
print('save flood and mean discharge at dam grids')
max_finarray.astype('float32').tofile(max_outf)

mean_finarray = np.mean(mean_yeararray, axis=0)
mean_finarray = np.where(mean_finarray<1.E-10, 1.E-10, mean_finarray)

mean_finarray.astype('float32').tofile(mean_outf)

print('Output Plain Binary Files')
print('-- flood discharge [nyear * ndams]', max_outf)
print('-- mean  discharge [nyear * ndams]', mean_outf)
print('###########################################')
print(' ')
    
# %%
