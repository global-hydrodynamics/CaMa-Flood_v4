# to calculate 100yr discharge at dam grids using natsim data
import os
import numpy as np
import pandas as pd
import sys

print(os.path.basename(__file__))

#### initial setting -------------------------------------

syear=int(sys.argv[1])
eyear=int(sys.argv[2])
tag  =sys.argv[3]

pyear = 100   # return period
maxdays = 1

#dam_file = './'+tag+'/damloc_modified.csv'
dam_file = './inp/damlist.txt'
#=====================================================
def PlottingPosition(n):
    ii=np.arange(n)+1
    pp=(ii-alpha)/(n+1-2*alpha)
    return pp

def gum(xx,pp,pyear):
    def func_gum(xx):
        n=len(xx)
        b0=np.sum(xx)/n
        j=np.arange(0,n)
        b1=np.sum(j*xx)/n/(n-1)
        lam1=b0
        lam2=2*b1-b0
        aa=lam2/np.log(2)
        cc=lam1-0.5772*aa
        return aa,cc
    def est_gum(aa,cc,pp):
        return cc-aa*np.log(-np.log(pp))

    aa,cc=func_gum(xx)
    # estimation
    ye=est_gum(aa,cc,pp)
    rr=np.corrcoef(xx, ye)[0][1]
    # probable value
    prob=1.0-1.0/pyear
    yp=est_gum(aa,cc,prob)
    res=np.array([aa,cc,rr])
    ss='GUM\na={0:8.3f}\nc={1:8.3f}\nr={2:8.3f}\nn={3:4.0f}'.format(res[0],res[1],res[2],len(xx))
    
    #print(res,ye,yp,ss)
    return res,ye,yp,ss

def main():
    finarray = np.zeros((ndams))
    pps = PlottingPosition(years)
    for dam in range(ndams):
        site_arr = readdata[:, dam]
        if np.max(site_arr) >= 1e+20:
            finarray[dam] = np.nan
            continue

        if np.max(site_arr) == np.min(site_arr):
            finarray[dam] = np.nan
            continue

        site_arr = np.where(site_arr<0, 0, site_arr)
        site_arr = np.sort(site_arr)
        res, ye, yp, ss = gum(site_arr, pps, pyear)
        #print(str(pyear)+'yr discharge=', yp)

        if yp > 0:
            finarray[dam] = yp
        else:
            finarray[dam] = np.nan

        # print('damID:', dam+1, ", 100yr discharge:", "{:.1f}".format(yp))

    finarray.astype('float32').tofile(outputpath)

    print('Output Plain Binary Files')
    print('-- 100yr-discharge [ndams]', outputpath)
    print('###########################################')
    print(' ')

#===========================================================

if __name__ == '__main__':

    df = pd.read_csv(dam_file, sep='\s+', header=0, skipinitialspace=True)
    ndams = len(df)

    years = eyear - syear + 1
    years = years * maxdays

    alpha = 0.0  #weibull

    readdatapath = './'+tag+'/tmp_p01_AnnualMax.bin'
    print('read annual max files: ', readdatapath)
    readdata = np.fromfile(str(readdatapath), 'float32').reshape(years, ndams)

    outputpath = './'+tag+'/tmp_p02_'+str(pyear)+'year.bin'

    main()

    exit()
