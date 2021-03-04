'''
Simulated flood extent comparison with observed flood extent values.
Some sample data is prepared in ./obs directory.
./obs/fwe/floodarea_{mapname}.bin - sample flood water area
Sample flood water extent is in monthly scale
'''
import numpy as np
import matplotlib.pyplot as plt
import datetime
from matplotlib.colors import LogNorm
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.cm as cm
import sys
import os
import calendar
from multiprocessing import Pool
from multiprocessing import Process
from multiprocessing import sharedctypes
from numpy import ma
import re
import math
import netCDF4 as nc

#========================================
#====  functions for making figures  ====
#========================================
def filter_nan(s,o):
    """
    this functions removed the data  from simulated and observed data
    where ever the observed data contains nan
    """
    data = np.array([s.flatten(),o.flatten()])
    data = np.transpose(data)
    data = data[~np.isnan(data).any(1)]

    return data[:,0],data[:,1]
#========================================
def correlation(s,o):
    """
    correlation coefficient
    input:
        s: simulated
        o: observed
    output:
        correlation: correlation coefficient
    """
    o=ma.masked_where(o<=0.0,o).filled(0.0)
    s=ma.masked_where(o<=0.0,s).filled(0.0)
    o=np.compress(o>0.0,o)
    s=np.compress(o>0.0,s)
    s,o = filter_nan(s,o)
    if s.size == 0:
        corr = np.NaN
    else:
        corr = np.corrcoef(o, s)[0,1]
        
    return corr
#========================================
def obs_data(west,east,south,north,syear=2000,smon=1,eyear=2001,emon=12,obs_dir="./obs",res=0.25):
    # read the sample observation data
    st_dt=datetime.date(syear,smon,1)
    start_dt=datetime.date(syear,smon,1)
    last_dt=datetime.date(eyear,emon,31)
    #-----
    start=0
    last=(last_dt-start_dt).days + 1
    #---
    if res==0.25:
        nx,ny=1440,720
    #---
    npix=int((north0-north)*(1.0/res))
    spix=int((north0-south)*(1.0/res))
    wpix=int((west-west0)*(1.0/res))
    epix=int((east-west0)*(1.0/res))
    #---
    fname =obs_dir+"/floodarea_glb_15min.bin"
    print ( "-- reading observatio file: ", fname)
    fldare = np.fromfile(fname,np.float32).reshape(-1,ny,nx)
    obs=[]
    time=[]
    for day in np.arange(start,last):
        day2=int(day)
        target_dt=start_dt+datetime.timedelta(days=day2)
        yyyy='%04d'%(target_dt.year)
        mm='%02d'%(target_dt.month)
        dd='%02d'%(target_dt.day)
        if int(dd)==15:
            target_mon = (target_dt.year - st_dt.year) * 12 + target_dt.month - st_dt.month
            obs.append(np.sum(fldare[target_mon,npix:spix,wpix:epix]))
            time.append(day)
    return obs 
#
#========================================
##### MAIN calculations ####
# - set start & end dates from arguments
# - set map dimension, read discharge simulation files
# - read data in the given region [west, south, east, north]
# - plot data and save figure
# - write a text file with simulated and observed data
#========================================

indir ="out"         # folder where Simulated discharge
syear,smonth,sdate=int(sys.argv[1]),int(sys.argv[2]),int(sys.argv[3])
eyear,emonth,edate=int(sys.argv[4]),int(sys.argv[5]),int(sys.argv[6])
west,east,south,north=float(sys.argv[7]),float(sys.argv[8]),float(sys.argv[9]),float(sys.argv[10])
output = sys.argv[11]

print ("\n\n@@@@@ flood_extent_validation.py", syear,smonth,sdate, eyear,emonth,edate, west,east,south,north, output )

#========================================
fname="./map/params.txt"
with open(fname,"r") as f:
    lines=f.readlines()
#-------
nx   =int  ( lines[0].split()[0] )
ny   =int  ( lines[1].split()[0] )
gsize=float( lines[3].split()[0] )
west0=float( lines[4].split()[0] )
east0=float( lines[5].split()[0] )
south0=float( lines[6].split()[0] )
north0=float( lines[7].split()[0] )
#----
start_dt=datetime.date(syear,smonth,sdate)
end_dt=datetime.date(eyear,emonth,edate)
size=60

start=0
last=(end_dt-start_dt).days + 1
Ndays=int(last)
Nmons=(end_dt.year - start_dt.year) * 12 + end_dt.month - start_dt.month + 1

print ( '\n#[1] map dim (nx,ny,gsize):', nx, ny, gsize, 'time series N=', Nmons )

#====================

if west < west0 or east > east0 or south < south0 or north > north0:
    print ('\n******** flood extent large than map dimension ********')
    west,east,south,north = west0,east0,south0,north0

print ('\nCalculate the flood extent for: ', west,east,south,north)
#========================
npix=int((north0-north)*(1.0/gsize))
spix=int((north0-south)*(1.0/gsize))
wpix=int((west-west0)*(1.0/gsize))
epix=int((east-west0)*(1.0/gsize))
#
#========================
### read simulation files
#========================

# multiprocessing array
sim=np.ctypeslib.as_ctypes(np.zeros([Nmons],np.float32))
shared_array_sim  = sharedctypes.RawArray(sim._type_, sim)

# for parallel calcualtion
inputlist=[]
inpn=0
for year in np.arange(syear,eyear+1):
    yyyy='%04d' % (year)
    inputlist.append([yyyy,indir])
    #print ( inputlist(inpn))
    inpn=inpn+1

#==============================
#=== function for read data ===
#==============================
def read_data(inputlist):
    yyyy  = inputlist[0]
    indir = inputlist[1]
    #--
    tmp_sim  = np.ctypeslib.as_array(shared_array_sim)

    # year, mon, day
    year=int(yyyy)
    
    if calendar.isleap(year):
        dt=366
    else:
        dt=365

    # simulated flood water area
    if output == "bin":
        fname=indir+"/fldare"+yyyy+".bin"
        simfile=np.fromfile(fname,np.float32).reshape([dt,ny,nx])
    else:
        fname=indir+"/o_fldare"+yyyy+".nc"
        with nc.Dataset(fname,"r") as cdf:
            simfile=cdf.variables["fldare"][:]
    print ("-- reading simulation file:", fname )
    #-------------
    nowyear_st=datetime.date(year,1,1)
    for mon in np.arange(1,12+1):
        mondays=calendar.monthrange(year,mon)[1]
        target_dt=datetime.date(year,mon,1)
        date=(target_dt.year - start_dt.year) * 12 + target_dt.month - start_dt.month
        st=(target_dt-nowyear_st).days
        et=st+mondays
        if et >= Ndays:
            et=None
        tmp_sim[date]=np.sum(np.mean(simfile[st:et,npix:spix,wpix:epix]*1e-6,axis=0))

#--read data parallel--
#para_flag=1
para_flag=0
#--
print ( "\n#[2] Reading simulation file " )
if para_flag==1:
    p=Pool(4)
    res = list(p.map(read_data, inputlist))
    sim = np.ctypeslib.as_array(shared_array_sim)
    p.terminate()
else:
    # list(p.map(read_data, inputlist))   ####  This will also work
    for inpi in np.arange(inpn):
        read_data(inputlist[inpi])
        sim = np.ctypeslib.as_array(shared_array_sim)

#=====================================
#=== function for saving data file ===
#=====================================
def write_text(obs,sim,west, east, north, south):
    CCval=correlation(sim,obs)
    fname="./txt/fwe/flood_water_extent.txt"
    with open(fname,"w") as f:
        print ("-- write comparison result text:", fname )
        f.write("# Validation Data : Flood Water Extent\n")
        f.write("# CaMa-Flood version 4.0.0\n")
        f.write("#============================================================\n")
        f.write("# Dimension:\n")
        f.write("# West : %5.2f\n"%(west))
        f.write("# South: %5.2f\n"%(south))
        f.write("# East : %5.2f\n"%(east))
        f.write("# North: %5.2f\n"%(north))
        f.write("#============================================================\n")
        f.write("#\n")
        f.write("# MEAN MONTHLY FLOOD WATER EXTENT (FWE)\n")
        f.write("# Unit : km2\n")
        f.write("#\n")
        f.write("#\n")
        f.write("#============================================================\n")
        f.write("# Statistics \n")
        f.write("#\tCorr. Coeff   : %3.2f\n"%(CCval))
        f.write("#\n")
        f.write("#\n")
        f.write("YYYY-MM      Observed     Simulated\n")
        f.write("#============================================================\n")
        date=0
        for year in np.arange(syear,eyear):
            for mon in np.arange(1,12+1):
                line = '%04d-%02d%14.4f%14.4f\n'%(year,mon,obs[date],sim[date])
                print (line)
                f.write(line)
                date = date + 1
    return 0
#==================================
#=== function for making figure ===
#==================================
def make_fig():
    print ( "\n make_fig:" )
    plt.close()
    labels=["Observed","Simulated"]
    fig, ax1 = plt.subplots()
    org = np.array( obs_data(west,east,south,north) )

    print ("-- make figure for domain:", west, east, south, north )

    time = np.arange(0,Nmons)

    # draw observations
    lines=[ax1.plot(time,org,label=labels[0],color="#34495e",linewidth=3.0,zorder=101)[0]] #,marker = "o",markevery=swt[point]) markersize=1,

    # draw simulations
    lines.append(ax1.plot(time,sim,label=labels[1],color="#ff8021",linewidth=3.0,alpha=1,zorder=106)[0])

    # # Make the y-axis label, ticks and tick labels match the line color.
    ax1.set_ylabel('$Monthly$ $Averaged$\n$Flood$ $Water$ $Extent$ $(km^2)$', color='k')
    ax1.set_xlim(xmin=0,xmax=Nmons)
    ax1.tick_params('y', colors='k')

    if eyear-syear > 5:
        dtt=5
        dt=int(math.ceil(((eyear-syear)+2)/5.0))
    else:
        dtt=1
        dt=(eyear-syear)+2

    xxlist=np.linspace(0,Nmons,dt,endpoint=True)
    xxlab=np.arange(syear,eyear+2,dtt)
    ax1.set_xticks(xxlist)
    ax1.set_xticklabels(xxlab,fontsize=10)

    # scentific notaion
    ax1.ticklabel_format(style="sci",axis="y",scilimits=(0,0))
    ax1.yaxis.major.formatter._useMathText=True 

    # Correlation Coefficent
    CORR1=correlation(sim,org)
    cocff="Corr. Coeff: %4.2f"%(CORR1)
    #
    ax1.text(0.02,0.98,cocff,ha="left",va="center",transform=ax1.transAxes,fontsize=10)

    plt.legend(lines,labels,ncol=1,loc='upper right')
    
    print ('-- save: '+'flood_water_extent'+".png", west, east, north, south)
    plt.savefig("./fig/fwe/flood_water_extent.png",dpi=500)

    print ('save: '+'flood_water_extent'+".txt", west, east, north, south)
    write_text(org,sim,west, east, north, south)
    return 0

#============================
###   -- make figures --
#============================
print ( "\n#[3] making figures" )
make_fig()

print ( "@@@@@ end: flood extent_validation.py \n\n" )

