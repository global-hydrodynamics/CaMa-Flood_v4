# to estimate flood control voluse from ReGeom data
from datetime import datetime
from datetime import date
import os
import numpy as np
import pandas as pd
import sys
from dateutil.relativedelta import relativedelta

print(os.path.basename(__file__))

##### initial setting ------------------------------

tag = sys.argv[1]
dam_file = './'+tag+'/damloc_modified.csv'

## link
GRSADdir  = "./inp/GRSAD/"
ReGeomdir = "./inp/ReGeom/"
ReGeom_ErrorFile = "./inp/ReGeom_Error.csv"

output_file = './'+tag+'/tmp_p03_fldsto.csv'

#### parameters to calculate flood control volume
pc = 75    ## percentile of surface area timeseries
s_yr, s_mon = 1984, 3
e_yr, e_mon = 2018, 12

#### read database --------------------------
grand = pd.read_csv(dam_file)
error = pd.read_csv(ReGeom_ErrorFile)

#### dam loop -----------------------------
cols = ['damid', 'damname', 'ave_area', 'fldsto_mcm', 'totalsto_mcm']
df_new = pd.DataFrame(index=[], columns=cols)

for i in range(len(grand)):
    gr = grand.iloc[i:i+1]
    nm = gr['damid'].values[0]
    damname = gr['damname'].values[0]
    totalsto = gr['totalsto_mcm'].values[0] 
    print('')
    print('------')
    print(nm, damname)

    #if nm > 6820:
    #    continue
    error_i = error.query('GRAND_ID == @nm')

    ## read timeseries file -----
    grsadpath = GRSADdir + '/'+ str(nm) + '_intp'
    if not os.path.isfile(grsadpath):
       print('file not found: ' +str(grsadpath)) 
       df_i = [nm, damname, np.nan, np.nan, totalsto]
       df_i = pd.Series(df_i, index=df_new.columns)
       df_new = df_new.append(df_i, ignore_index=True)
       continue
    import pandas as pd
    df = pd.read_table(grsadpath, index_col=0, parse_dates=True)
    data = df.dropna()
    
    if np.max(df['3water_enh'].value_counts()) > 12:
        rm_df = df['3water_enh'].value_counts()
        rm_df = rm_df[rm_df > 12]
        rm_df = rm_df.index
        for j in range(len(rm_df)):
            rm_val = rm_df[j]
            data['3water_enh'] = data['3water_enh'].replace(rm_val, np.nan)
        data = data.dropna()

    data = data['3water_enh']
    #print(data)

    if len(data) < 2:
        df_i = [nm, damname, np.nan, np.nan, totalsto]
        df_i = pd.Series(df_i, index=df_new.columns)
        df_new = df_new.append(df_i, ignore_index=True)
        continue

    fld_area = np.percentile(data, pc)
    areamax = np.max(data)
    print('fld_area_org', fld_area)


    ## read reservoir bathymetry data --------------
    regeompath = ReGeomdir + '/'+ str(nm) + '.csv'
    if not os.path.isfile(regeompath):
       print('file not found: ' +str(regeompath)) 
       df_i = [nm, damname, fld_area, np.nan, totalsto]
       df_i = pd.Series(df_i, index=df_new.columns)
       df_new = df_new.append(df_i, ignore_index=True)
       continue
    regeom = pd.read_csv(regeompath, header=7)
    regeom.columns = ['Depth', 'Area', 'Storage']
    if len(regeom) <= 1:
        print('ReGeom data was empty!!!')
        df_i = [nm, damname, fld_area, np.nan, totalsto]
        df_i = pd.Series(df_i, index=df_new.columns)
        df_new = df_new.append(df_i, ignore_index=True)
        continue
    fld_area = fld_area * regeom['Area'].values[-1] / areamax
    print('fld_area', fld_area, 'areamax', areamax, 'regeom_max', regeom['Area'].values[-1])

    fld_sto = 0
    sto_max = 0

    for i in range(len(regeom)):
        rg = regeom.iloc[i:i+1]
        if rg['Area'].values[0] < fld_area:
            continue
        elif rg['Area'].values[0] == fld_area:
            #use_sto = rg['Storage'].values[0]
            use_sto = np.mean(regeom.query('Area == @fld_area')['Storage'])
            sto_max = np.mean(regeom.query('Area == @fld_area')['Storage'])

            #use_sto = use_sto * error_i['V_GRanD_mcm'].values[0] / error_i['V_est_mcm'].values[0]
            use_sto = use_sto * error_i['V_GRanD_mcm'].values[0] / regeom['Storage'].values[-1]
            fld_sto = totalsto - use_sto
            break
        elif rg['Area'].values[0] > fld_area:
            sto_max, area_max = rg['Storage'].values[0], rg['Area'].values[0]
            rg_p = regeom.iloc[i-1:i]
            sto_min, area_min = rg_p['Storage'].values[0], rg_p['Area'].values[0]
            use_sto = sto_min + (sto_max - sto_min) * (fld_area - area_min) / (area_max - area_min)
            #print('use_sto', use_sto)
            #use_sto = use_sto * error_i['V_GRanD_mcm'].values[0] / error_i['V_est_mcm'].values[0]
            use_sto = use_sto * error_i['V_GRanD_mcm'].values[0] / regeom['Storage'].values[-1]
            fld_sto = totalsto - use_sto
            break
    
    if sto_max == 0:
        print('sto_max == 0!!!')
        area_max = regeom['Area'].values[-1]
        use_sto = np.mean(regeom.query('Area == @area_max')['Storage'])
        #use_sto = use_sto * error_i['V_GRanD_mcm'].values[0] / error_i['V_est_mcm'].values[0]
        use_sto = use_sto * error_i['V_GRanD_mcm'].values[0] / regeom['Storage'].values[-1]
        fld_sto = totalsto - use_sto
        print(fld_sto, totalsto)
        exit()

    if fld_sto == 0:
        print('error!')
        print(fld_area, rg['Area'].values[0])
        exit()

    if fld_sto < 0:
        fld_sto = 0


    print('fld_sto:', fld_sto, 'total_sto', totalsto)
    df_i = [nm, damname, fld_area, fld_sto, totalsto]
    df_i = pd.Series(df_i, index=df_new.columns)
    df_new = df_new.append(df_i, ignore_index=True)

print('------')
print('save results')
print(df_new)
df_new.to_csv(output_file)
print(output_file)
print('##################################')

sys.exit()
