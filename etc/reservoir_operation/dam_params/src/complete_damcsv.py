import calendar
from datetime import datetime
import os
import numpy as np
import pandas as pd
import sys
from collections import defaultdict

print(os.path.basename(__file__))

#### initial setting =======================================
tag = sys.argv[1]
MINUPAREA = int(sys.argv[2])

DAM_FILE    = './'+tag+'/damloc_modified.csv'
output_file = './'+tag+'/dam_params_comp.csv'

#### other file paths -----------------------
Qmean_file   = './'+tag+'/tmp_p01_AnnualMean.bin'
Q100_file    = './'+tag+'/tmp_p02_100year.bin'
storage_file = './'+tag+'/tmp_p03_fldsto.csv'


#### make dam param csv --------------------------------

Qn_all   = np.fromfile(Qmean_file, 'float32')
Q100_all = np.fromfile(Q100_file, 'float32')

damcsv = pd.read_csv(DAM_FILE)
damcsv['Qn'] = Qn_all
damcsv['Qf'] = Q100_all * 0.3

stocsv = pd.read_csv(storage_file)

fldsto_l, consto_l = [], []
Qf_new = []
for index, row in damcsv.iterrows():
    damid = row['damid']

    ## storage capacity
    fldsto   = stocsv.query('damid == @damid')['fldsto_mcm'].values[0]
    totalsto = stocsv.query('damid == @damid')['totalsto_mcm'].values[0]
    if fldsto != fldsto:
        fldsto = totalsto * 0.37
    consto = totalsto - fldsto
    fldsto_l.append(fldsto)
    consto_l.append(consto)

    ## Qf and Qn
    Qf, Qn = row['Qf'], row['Qn']
    if Qf < Qn:
        if Q100_all[index] * 0.4 >= Qn:
            Qf_new.append(Q100_all[index]*0.4)
        else:
            Qf_new.append(Qn*1.1)
    else:
        Qf_new.append(Qf)

damcsv['fldsto_mcm'] = fldsto_l
damcsv['consto_mcm'] = consto_l
damcsv['Qf'] = Qf_new

## calc fldsto/uparea
damcsv['fldsto/uparea'] = damcsv['fldsto_mcm'] * 1e+6 / (damcsv['uparea_cama'] * 1e+6)    #(m3/m2)
# damcsv['fldsto/uparea'] = damcsv['fldsto_mcm'] * 1e+6 / (damcsv['upreal'] * 1e+6)    #(m3/m2)

## remove dams with small drainage area

damcsv = damcsv.query('uparea_cama >= @MINUPAREA')
print(damcsv)
damcsv = damcsv.dropna()
print(damcsv)


#### treat multiple dams in one grid ------------------------

print('')
print('treat multiple dams in one grid')

## count grids with multiple dams allocated
cnt = defaultdict(int)

for index, row in damcsv.iterrows():
    grandid = row['damid']
    ix, iy = row['ix'], row['iy']
    print('')
    print(ix,iy)
    key = tuple([ix,iy])
    cnt[key] += 1

print('cnt=', cnt)


## remove smaller dams in such grids
damcsv2 = damcsv.copy()
for k, v in cnt.items():
    if v > 1:
        print('')
        print('multiple dams on one grid!!:', k, v)
        ix, iy = k
        dams = damcsv.query('ix == @ix & iy == @iy')
        print(dams[['damname', 'ix', 'iy', 'totalsto_mcm']])
        maxsto = np.max(dams['totalsto_mcm'])
        rmdams = dams.query('totalsto_mcm != @maxsto')
        if len(rmdams) == 0:
            maxfsto = np.max(dams['fldsto_mcm'])
            rmdams = dams.query('fldsto_mcm != @maxfsto')
        print('remove:', rmdams)
        damcsv2.drop(index=rmdams.index, inplace=True)
        print(damcsv2.query('ix==@ix & iy==@iy'))

print('')
print(damcsv2)


#### sort by drainage area

# print(' ')
# print('sort by drainage area')

# damcsv2 = damcsv2.sort_values('uparea_cama')

damcsv2 = damcsv2.rename(columns={'damid': 'GRAND_ID', 'damname':'DamName', 'lon':'DamLon', 'lat':'DamLat', 'ix':'DamIX', 'iy':'DamIY', 'fldsto_mcm':'FldVol_mcm', 'consto_mcm':'ConVol_mcm', 'totalsto_mcm':'TotalVol_mcm'})

damcsv2 = damcsv2[['GRAND_ID', 'DamName', 'DamLon', 'DamLat', 'upreal', 'DamIX', 'DamIY', 'FldVol_mcm', 'ConVol_mcm', 'TotalVol_mcm', 'fldsto/uparea', 'Qn', 'Qf']]

damcsv2.to_csv(output_file, index=None)
print(' ')
print('###############################')
print('dam parameters:', output_file)
print(' ')

sys.exit()
    
