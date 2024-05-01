import calendar
import os
import numpy as np
import pandas as pd
import sys
from collections import defaultdict

print(os.path.basename(__file__))

#### initial setting =======================================
tag = sys.argv[1]
MINUPAREA = int(sys.argv[2])

DAM_FILE    = './inp/damlist.txt'
output_file = './'+tag+'/tmp_p04_damparams.csv'

#### other file paths -----------------------
Qmean_file   = './'+tag+'/tmp_p01_AnnualMean.bin'
Q100_file    = './'+tag+'/tmp_p02_100year.bin'
storage_file = './'+tag+'/tmp_p03_fldsto.csv'


#### make dam param csv --------------------------------
# merge Qmean and Q100 from tmp_p01 & tmp_p02
Qn_all   = np.fromfile(Qmean_file,'float32')
Q100_all = np.fromfile(Q100_file, 'float32')

damcsv = pd.read_csv(DAM_FILE, sep='\s+', header=0, skipinitialspace=True)
damcsv['Qn'] = Qn_all
damcsv['Qf'] = Q100_all * 0.3


# merge flood control storage from tmp_p03
stocsv = pd.read_csv(storage_file)

fldsto_l, consto_l = [], []
Qf_new = []
for index, row in damcsv.iterrows():
    damid = row['ID']

    ## storage capacity
    fldsto   = stocsv.query('ID == @damid')['fldsto_mcm'].values[0]
    totalsto = stocsv.query('ID == @damid')['totalsto_mcm'].values[0]
    if fldsto != fldsto:
        fldsto = totalsto * 0.37  # when fldsto is undefined
    if fldsto < -99 :
        fldsto = totalsto * 0.37  # when fldsto cannot be calculaed by GRSAD
    consto = totalsto - fldsto
    fldsto_l.append(fldsto)
    consto_l.append(consto)

    ## Qf and Qn
    Qf, Qn = row['Qf'], row['Qn']
    if Qf < Qn:  ## adjustment for flood discharge smaller than mean
        if Q100_all[index] * 0.4 >= Qn:
            Qf_new.append(Q100_all[index]*0.4)
        else:
            Qf_new.append(Qn*1.1)
    else:
        Qf_new.append(Qf)

damcsv['fldsto_mcm'] = fldsto_l
damcsv['consto_mcm'] = consto_l
damcsv['Qf'] = Qf_new

#### remove dams if needed (post-process)
## remove dams with small drainage area 
damcsv = damcsv.query('area_CaMa >= @MINUPAREA')
damcsv = damcsv.dropna()
#print(damcsv)

##### treat multiple dams in one grid (remove smaller dam)
print('')
print('treat multiple dams in one grid')

## count grids with multiple dams allocated
cnt = defaultdict(int)

for index, row in damcsv.iterrows():
    grandid = row['ID']
    ix, iy = row['ix'], row['iy']
    key = tuple([ix,iy])
    cnt[key] += 1

#print('cnt=', cnt)

## remove smaller dams in such grids
damcsv2 = damcsv.copy()
for k, v in cnt.items():
    if v > 1:
#        print('multiple dams on one grid!!:', k, v)
        ix, iy = k
        dams = damcsv.query('ix == @ix & iy == @iy')
#        print(dams[['damname', 'ix', 'iy', 'cap_mcm']])
        maxsto = np.max(dams['cap_mcm'])
        rmdams = dams.query('cap_mcm != @maxsto')
        if len(rmdams) == 0:
            maxfsto = np.max(dams['fldsto_mcm'])
            rmdams = dams.query('fldsto_mcm != @maxfsto')
#        print('remove:', rmdams)
        damcsv2.drop(index=rmdams.index, inplace=True)
#        print(damcsv2.query('ix==@ix & iy==@iy'))
        remid=rmdams.loc[:,['ID','lat','lon','area_CaMa']]
        print('-- multiple dams on one grid!!:', k, v, 'dams exist. removed below')
        print(remid.to_string(index=False,header=False))

##### Save merged map parameter file
print('')
print(damcsv2)

#### Save Dam CSV ####
damcsv2 = damcsv2.rename(columns={'ID': 'GRAND_ID', 'damname':'DamName', 'lon':'DamLon', 'lat':'DamLat', 'ix':'DamIX', 'iy':'DamIY', 'fldsto_mcm':'FldVol_mcm', 'consto_mcm':'ConVol_mcm', 'cap_mcm':'TotalVol_mcm'})
damcsv2 = damcsv2[['GRAND_ID', 'DamName', 'DamLat', 'DamLon', 'area_CaMa', 'DamIX', 'DamIY', 'FldVol_mcm', 'ConVol_mcm', 'TotalVol_mcm', 'Qn', 'Qf', 'year']]

damcsv2.to_csv(output_file, index=None)
print(' ')
print('###############################')
print('dam parameters:', output_file)
print(' ')

sys.exit()
    
