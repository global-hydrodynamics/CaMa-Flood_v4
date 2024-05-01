# To estimate flood control voluse from ReGeom and GRSAD data
# - aquire reservoir surface area and normal water volume corresponding to normal water level (75% from GSRAD)
import os
import numpy as np
import pandas as pd
import sys

print(os.path.basename(__file__))

##### initial setting ------------------------------

## working directory
tag = sys.argv[1]

## input files
#dam_file = './'+tag+'/damloc_modified.csv'
dam_file = './inp/damlist.txt'

GRSADdir  = "./inp/GRSAD/"
ReGeomdir = "./inp/ReGeom/"

## output files
output_file = './'+tag+'/tmp_p03_fldsto.csv'

#### parameters to calculate flood control volume from GSRAD
pc = 75    ## percentile of surface area timeseries corresponding to Normal Water Level (Water use capacity)
s_yr, s_mon = 1984,  3
e_yr, e_mon = 2018, 12

#### read input DamList 
grand = pd.read_csv(dam_file, sep='\s+', header=0, skipinitialspace=True)

#### dam loop ------------------------------------------------------------
cols = ['ID', 'damname', 'ave_area', 'fldsto_mcm', 'totalsto_mcm']  # output column
df_new = pd.DataFrame(index=[], columns=cols)                       # create empty DataFrame with column info

for i in range(len(grand)):
    # extract i_th dam data from damlist
    gr = grand.iloc[i:i+1]              
    damid    = gr['ID'].values[0]
    damname  = gr['damname'].values[0]
    totalsto = gr['cap_mcm'].values[0]  # total capacity in Million m^3

    # read GRSAD timeseries file as "data" -----
    grsadpath = GRSADdir + '/'+ str(damid) + '_intp'
    if not os.path.isfile(grsadpath):
       print(damid, 'No GRSAD file: ' +str(grsadpath)) 
#       df_i = pd.DataFrame(data=[[damid, damname, np.nan, np.nan, totalsto]], columns=cols) # no GSRAD -> flood storage cannot be defined 
       df_i = pd.DataFrame(data=[[damid, damname, -998, -998, totalsto]], columns=cols) # no GSRAD -> flood storage cannot be defined 
       df_new = pd.concat([df_new,df_i], axis=0)
       continue
    df = pd.read_table(grsadpath, index_col=0, parse_dates=True)
    data = df.dropna()
    
    if np.max(df['3water_enh'].value_counts()) > 12:  # more than 12 redords of same value data --> Remove as this could be suspicious
        rm_df = df['3water_enh'].value_counts()
        rm_df = rm_df[rm_df > 12]
        rm_df = rm_df.index
        for j in range(len(rm_df)):
            rm_val = rm_df[j]
#            data['3water_enh'] = data['3water_enh'].replace(rm_val, np.nan)
            data.loc[:,('3water_enh')] = data['3water_enh'].replace(rm_val, np.nan)
        data = data.dropna()

    data = data['3water_enh']
    #print(data)

    if len(data) < 2:
        print('Too short GRSAD data: ' +str(grsadpath)) 
        #df_i = pd.DataFrame(data=[[damid, damname, np.nan, np.nan, totalsto]], columns=cols) # time series too short, flood storage cannot be defined.
        df_i = pd.DataFrame(data=[[damid, damname, -997, -997, totalsto]], columns=cols) # time series too short, flood storage cannot be defined.
        df_new = pd.concat([df_new,df_i], axis=0)
        continue

    ## extract surface water area at normal water level and max level 
    fld_area = np.percentile(data, pc)
    areamax  = np.max(data)
#    print('fld_area_org', fld_area)

    ## read reservoir bathymetry data --------------
    regeompath = ReGeomdir + '/'+ str(damid) + '.csv'

    if not os.path.isfile(regeompath): ## when no ReGeom data exist, set storaga capacity as NaN
       print(damid, 'No ReGeom file: ' +str(regeompath)) 
       #df_i = pd.DataFrame(data=[[damid, damname, np.nan, np.nan, totalsto]], columns=cols)
       df_i = pd.DataFrame(data=[[damid, damname, -996, -996, totalsto]], columns=cols)
       df_new = pd.concat([df_new,df_i], axis=0)
       continue

    regeom = pd.read_csv(regeompath, header=7)  ## skip first 7 lines, use 8th line as header
    regeom.columns = ['Depth', 'Area', 'Storage']
    if len(regeom) <= 1:
        print(damid, 'ReGeom data was empty!!!')
#        df_i = pd.DataFrame(data=[[damid, damname, np.nan, np.nan, totalsto]], columns=cols)
        df_i = pd.DataFrame(data=[[damid, damname, -999, -999, totalsto]], columns=cols)
        df_new = pd.concat([df_new,df_i], axis=0)
        continue

    fld_area = fld_area * regeom['Area'].values[-1] / areamax    ## adjust area to fit max value to ReGeom (avoid error in GRSAD)
#    print('fld_area', fld_area, 'areamax', areamax, 'regeom_max', regeom['Area'].values[-1])

    # extract water volume at normal water level and max level
    fld_sto = 0
    sto_max = 0

    for i in range(len(regeom)):
        rg = regeom.iloc[i:i+1]
        if rg['Area'].values[0] < fld_area:
            continue
        elif rg['Area'].values[0] == fld_area:
            #use_sto = rg['Storage'].values[0]
            use_sto = np.mean(regeom.query('Area == @fld_area')['Storage'])  ## water use storage (=normal volume)
            sto_max = np.mean(regeom.query('Area == @fld_area')['Storage'])  

            use_sto = use_sto * totalsto / regeom['Storage'].values[-1]
            fld_sto = totalsto - use_sto
            break
        elif rg['Area'].values[0] > fld_area:
            sto_max  = rg['Storage'].values[0] ## storage and area of current step
            area_max = rg['Area'].values[0]

            rg_p     = regeom.iloc[i-1:i]  ## storage and area of previous depth 
            sto_min  = rg_p['Storage'].values[0]
            area_min = rg_p['Area'].values[0]

            if( area_max == area_min ):
                use_sto = sto_min
                fld_sto = totalsto - use_sto
                break

            if( area_min <= fld_area ):
                use_sto = sto_min + (sto_max - sto_min) * (fld_area - area_min) / (area_max - area_min)  ## linearly interporlate
                use_sto = use_sto * totalsto / regeom['Storage'].values[-1]   ## adjustment to fit GranD original data
                fld_sto = totalsto - use_sto

                if( use_sto > totalsto ):
                    use_sto = totalsto
                    fld_sto = 0

    if sto_max == 0:
        print(damid, 'ERR: sto_max == 0')
        area_max = regeom['Area'].values[-1]
        use_sto = np.mean(regeom.query('Area == @area_max')['Storage'])
        use_sto = use_sto * totalsto / regeom['Storage'].values[-1]
        fld_sto = totalsto - use_sto
        print(fld_sto, totalsto)
        exit()

#    print(use_sto)
#    print('total capacity', totalsto, 'flood storage', fld_sto)


    if fld_sto == 0:
        print('error!')
        print(fld_area, rg['Area'].values[0])
        exit()

    if fld_sto < 0:
        fld_sto = 0.0

#    print('fld_sto:', fld_sto, 'total_sto', totalsto)
    df_i=pd.DataFrame(data=[[damid, damname, fld_area, fld_sto, totalsto]], columns=cols)
    df_new = pd.concat([df_new,df_i], axis=0)


print('------')
print('save results [CSV file]')
print(df_new)
df_new.to_csv(output_file)
print(output_file)
print('##################################')

sys.exit()
