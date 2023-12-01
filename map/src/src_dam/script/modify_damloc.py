#%%

## to check and modify dam (x,y) by
# 1. compare drainage area with the actual value
# 2. if the error of drainage area is more than 100% of the actual value, 
#    search nearest grids for more accurate drainage area
# 3. replace dam (x,y) with that grid

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sys

## initial settings ===============================================
## allocation parameters
tag  =sys.argv[1]               # projhect name
minuparea = float(sys.argv[2])  # minimum drainage area to output

# [grandid,lon,lat,x,y,upreal,upreal_cama]
inputfile  = './'+tag+'/damloc_tmp.txt'
outputfile = './'+tag+'/damloc_modified.csv'

# drainage area from cama map dir
mapdir = './inp/map/'
upareafile = str(mapdir)+'/uparea.bin' 

# minimum uparea error to allow
minerror = 0.1

#===============================================================

## get map nx, ny --------------------

f = open(mapdir+'/params.txt', 'r')
data = f.readline()
nx = int(data.strip().split(' ')[0])
data = f.readline()
ny = int(data.strip().split(' ')[0])
f.close()

print('########################################')
print('NX=', nx, 'NY=', ny)
print('')


## read files--------------------------------------------

dam_data = pd.read_csv(inputfile, sep='\s+', header=0)

uparea_cama = np.fromfile(upareafile, 'float32').reshape(ny,nx)
uparea_cama = uparea_cama/(1e+6)

dam_data_m = dam_data.copy()
#dam_data_m = dam_data_m.query('uparea_cama > 0')  
#dam_data_m = dam_data_m.query('upreal > @minuparea')
dam_data_m['uparea_cama'] = 0.000000

upcama_m = []
x_m, y_m = [], []

#----------------------------------------------------------

for index, row in dam_data.iterrows():
    print('') 
    print('-------------------------------------------------')
    print(row)

    ix, iy = int(row['ix'])-1, int(row['iy'])-1
    iup_real = row['upreal']
    print(' ')
    print(index, 'x=', ix,' y=', iy,' uparea_real=',iup_real)

    if iup_real < 0:
        x_m.append(ix+1)
        y_m.append(iy+1)
        upcama_m.append(uparea_cama[iy, ix])
        continue

    ## step1. comparison of drainage area -------------------------
    error = uparea_cama[iy,ix] - iup_real 
    error = abs(error)             #absolute value!!!
    if error >= iup_real*minerror :
        print("error >= uparea_real*minerror ; modify dam location")
        print("uparea_real=", iup_real, " uparea_cama=", uparea_cama[iy,ix], " error=", error)
        
        ## step2. searching -------------------------------------------
        ix1,iy1 = ix-1,iy-1
        ix2,iy2 = ix  ,iy-1
        ix3,iy3 = ix+1,iy-1
        ix4,iy4 = ix-1,iy
        ix5,iy5 = ix+1,iy
        ix6,iy6 = ix-1,iy+1
        ix7,iy7 = ix  ,iy+1
        ix8,iy8 = ix+1,iy+1
        ix_list = [[ix1,iy1], [ix2,iy2], [ix3,iy3], [ix4,iy4], [ix5,iy5], [ix6,iy6], [ix7,iy7], [ix8,iy8]]

        ix_m, iy_m = ix, iy
        error_m = error
        for i in range(8):
            [ix_tmp, iy_tmp] = ix_list[i]
            print(ix_tmp, iy_tmp)
            ix_tmp=max( [ix_tmp,0   ] )
            ix_tmp=min( [ix_tmp,nx-1] )
            iy_tmp=max( [iy_tmp,0   ] )
            iy_tmp=min( [iy_tmp,ny-1] )

            error_tmp = abs(uparea_cama[iy_tmp,ix_tmp] - iup_real)
            if error_tmp >= error_m:    #if there is no better grid????
                continue
            else: #error_tmp<error_m
                ix_m, iy_m = ix_tmp, iy_tmp
                error_m = error_tmp
            print("modified location:", ix_m, iy_m, 'up_real=', uparea_cama[iy_m,ix_m], 'error=', error_m)

        if error_m>=iup_real*minerror:
            print('')
            print("still have error>=uparea_real*0.1!!!!!!")
            error_m = error
            ix_m, iy_m = ix, iy


            for j_x in range(-2, 3, 1):
                for j_y in range(-2, 3, 1):
                    ix_tmp = ix + j_x
                    iy_tmp = iy + j_y
                    ix_tmp=max( [ix_tmp,0   ] )
                    ix_tmp=min( [ix_tmp,nx-1] )
                    iy_tmp=max( [iy_tmp,0   ] )
                    iy_tmp=min( [iy_tmp,ny-1] )

                    error_tmp = abs(uparea_cama[iy_tmp,ix_tmp] - iup_real)
                    if error_tmp >= error_m:    #if there is no better grid????
                        continue
                    else: #error_tmp<error_m
                        ix_m, iy_m = ix_tmp, iy_tmp
                        error_m = error_tmp
                        print("modified location:", ix_m, iy_m, 'up_cama=', uparea_cama[iy_m,ix_m], 'error=', error_m)
        
        print("final modified location:", ix_m+1, iy_m+1, 'up_cama=' ,uparea_cama[iy_m,ix_m], 'error=', error_m)

    else: #error<iup_real
        ix_m, iy_m = ix, iy


    ## step3. replacing -----------------------------------------------

    x_m.append(ix_m+1)
    y_m.append(iy_m+1)
    upcama_m.append(uparea_cama[iy_m, ix_m])


dam_data_m['uparea_cama'] = upcama_m
dam_data_m['ix'] = x_m
dam_data_m['iy'] = y_m

## output -------------------------------------------------------

print(dam_data_m)

dam_data_m.to_csv(outputfile, index=False) 
print('') 
print("file outputted to:", outputfile)


sys.exit(0)




# %%
