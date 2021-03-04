'''
Simulated water surface elevation (wse) comparison with observed wse values.
Some sample data is prepared in ./obs directory.
Pre-processing of correspoinding to observation location coordinates (x,y) of CaMa-Flood map are needed.
./obs/wse/wse_list_{mapname}.txt - list of sample virtual stations
./obs/wse/{name}.txt - sample wse observatons
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
def RMSE(s,o):
    """
    Root Mean Squre Error
    input:
        s: simulated
        o: observed
    output:
        RMSE: Root Mean Squre Error
    """
    o=ma.masked_where(o<=0.0,o).filled(0.0)
    s=ma.masked_where(o<=0.0,s).filled(0.0)
    s,o = filter_nan(s,o)
    return np.sqrt(np.mean((s-o)**2))
#========================================    
def NS(s,o):
    """
    Nash Sutcliffe efficiency coefficient
    input:
        s: simulated
        o: observed
    output:
        ns: Nash Sutcliffe efficient coefficient
    """
    o=ma.masked_where(o<=0.0,o).filled(0.0)
    s=ma.masked_where(o<=0.0,s).filled(0.0)
    o=np.compress(o>0.0,o)
    s=np.compress(o>0.0,s) 
    s,o = filter_nan(s,o)
    return 1 - sum((s-o)**2)/(sum((o-np.mean(o))**2)+1e-20)
#========================================
def obs_data(station,syear=2000,smon=1,sday=1,eyear=2001,emon=12,eday=31,obs_dir="./obs"):
    # read the sample observation data
    start=datetime.date(syear,smon,sday)
    end=datetime.date(eyear,emon,eday)
    
    # read wse observation
    fname=obs_dir+"/"+station+".txt"
    print ( "-- reading observatio file: ", fname)

    with open(fname,"r") as f:
        lines=f.readlines()

    # read Water Surface Elevation
    #--
    head=20
    #--
    time=[] # time in days
    data=[] # WSE in [m]
    for line in lines[head::]:
        if line[0][0] == "#":
            continue
#        line = list(filter(None,re.split(" ",line)))    ####### this works as well 
#        date = line[0]
#        date = re.split("-",date)
        line2 = line.split()
        date = line2[0].split('-')
        yyyy = int(date[0])
        mm   = int(date[1])
        dd   = int(date[2])
        wse  = float(line2[1])
        now  = datetime.date(yyyy,mm,dd)
        if now < start and now > end:
            continue
        data.append(wse)
        lag  = int((now-start).days)
        time.append(lag)
    return time, data
#
#========================================
##### MAIN calculations ####
# - set start & end dates and geoid info from arguments
# - set map dimention, read surface elevation simulation files
# - read station data
# - plot each station data and save figure
# - write a text file with simulated and observed data
#========================================
indir ="out"         # folder where Simulated discharge
syear,smonth,sdate=int(sys.argv[1]),int(sys.argv[2]),int(sys.argv[3])
eyear,emonth,edate=int(sys.argv[4]),int(sys.argv[5]),int(sys.argv[6])
egm=sys.argv[7]      # provide EGM of observations, CaMa-Flood wse is given in EGM96
output = sys.argv[8] # output file type bin/netCDF

print ("\n\n@@@@@ wse_validation.py", syear,smonth,sdate, eyear,emonth,edate, egm, output )

#========================================
fname="./map/params.txt"
with open(fname,"r") as f:
    lines=f.readlines()
#-------
nx   =int  ( lines[0].split()[0] )
ny   =int  ( lines[1].split()[0] )
gsize=float( lines[3].split()[0] )
#nx     = int(filter(None, re.split(" ",lines[0]))[0])
#ny     = int(filter(None, re.split(" ",lines[1]))[0])
#gsize  = float(filter(None, re.split(" ",lines[3]))[0])
#----
start_dt=datetime.date(syear,smonth,sdate)
end_dt=datetime.date(eyear,emonth,edate)
size=60

start=0
last=(end_dt-start_dt).days + 1
N=int(last)

print ( '\n #[1] map dim (nx,ny,gsize):', nx, ny, gsize, 'time seriez N=', N )

#====================
# read SWE station list
pnames=[]
x1list=[]
y1list=[]
rivers=[]
legm08=[]
legm96=[]
#----------------------------
fname="./list.txt"
with open(fname,"r") as f:
    lines=f.readlines()
#----------------------------
for line in lines[1::]:
#    line = filter(None, re.split(" ",line))
    line2 = line.split()
    rivers.append(line2[0].strip())
    pnames.append(line2[1].strip())
    x1list.append(int(line2[2]))
    y1list.append(int(line2[3]))
    legm08.append(float(line2[4]))
    legm08.append(float(line2[5]))
pnum=len(pnames)

print ( '-- read station list', fname, 'station num pnum=', pnum )

#========================
### read simulation files
#========================
org=[]
opn=[]
sim=[]

# multiprocessing array
sim=np.ctypeslib.as_ctypes(np.zeros([N,pnum],np.float32))
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
    yyyy = inputlist[0]
    odir = inputlist[1]
    print (yyyy)
    #--
    tmp_sim  = np.ctypeslib.as_array(shared_array_sim)

    # year, mon, day
    year=int(yyyy)
    
    if calendar.isleap(year):
        dt=366
    else:
        dt=365

    # timimgs
    target_dt=datetime.date(year,1,1)
    st=(target_dt-start_dt).days
    et=st+dt
    if et >= N:
        et=None

    # simulated water surface elevation
    if output == "bin":
        fname=indir+"/sfcelv"+yyyy+".bin"
        simfile=np.fromfile(fname,np.float32).reshape([dt,ny,nx])
    else:
        fname=indir+"/o_sfcelv"+yyyy+".nc"
        with nc.Dataset(fname,"r") as cdf:
            simfile=cdf.variables["sfcelv"][:]
    print ("-- reading simulation file:", fname )
    #-------------
    for point in np.arange(pnum):
        ix1,iy1=x1list[point],y1list[point]
        tmp_sim[st:et,point]=simfile[:,iy1-1,ix1-1]

#--read data parallel--
# para_flag=1
para_flag=0
#--
print ( "\n #[2] Reading simulation file " )
if para_flag==1:
    p   = Pool(4)
    res = list(p.map(read_data, inputlist))      ##### this will also work
    sim = np.ctypeslib.as_array(shared_array_sim)
    p.terminate()
else:
#    res = list(map(read_data, inputlist))             ##### this will also work
    for inpi in np.arange(inpn):
        read_data(inputlist[inpi])
        sim = np.ctypeslib.as_array(shared_array_sim)

#=====================================
#=== function for saving data file ===
#=====================================
def write_text(time,obs,sim,river,pname):
    RMSEval=RMSE(sim[time],obs)
    fname="./txt/wse/"+river+"-"+pname+".txt"
    with open(fname,"w") as f:
        print ("-- write comparison result text:", fname )
        f.write("# Validation Data : Water Surface Elevation\n")
        f.write("# CaMa-Flood version 4.0.0\n")
        f.write("#============================================================\n")
        f.write("# River: %12s RIVER\n"%(river))
        f.write("# Station: %15s\n"%(pname))
        f.write("#============================================================\n")
        f.write("#\n")
        f.write("# MEAN DAILY WATER SURFACE ELEVATION (WSE)\n")
        f.write("# Unit : m\n")
        f.write("#\n")
        f.write("#\n")
        f.write("#============================================================\n")
        f.write("# Statistics \n")
        f.write("#\tRMSE : %3.2f\n"%(RMSEval))
        f.write("#\n")
        f.write("#\n")
        f.write("YYYY-MM-DD      Observed     Simulated\n")
        f.write("#============================================================\n")
        i=0
        for date in np.arange(start,last):
            date2=int(date)
            time2=time[i]
            target_dt=start_dt+datetime.timedelta(days=date2)
            year=target_dt.year
            mon=target_dt.month
            day=target_dt.day
            sim_val=sim[date2]
            if date2==time2:
                obs_val=obs[i]
                i=i+1
                i=min(i,len(time)-1)
            else:
                obs_val=-9999.0
            line = '%04d-%02d-%02d%14.4f%14.4f\n'%(year,mon,day,obs_val,sim_val)
            print (line)
            f.write(line)
    return 0
#==================================
#=== function for making figure ===
#==================================
def make_fig(point):
    print ( "\n make_fig:", point, pnames[point] )
    plt.close()
    labels=["Observed","Simulated"]
    fig, ax1 = plt.subplots()
    time,org=obs_data(pnames[point],syear=syear,eyear=eyear)
    if egm=="EGM96":
        org=np.array(org)+np.array(legm08[point])-np.array(legm96[point])
    else:
        org=np.array(org) 

    print ("-- make figure for:", point, rivers[point], pnames[point] )

    lines=[ax1.plot(time,org,label=labels[0],marker="o",color="#34495e",fillstyle="none",linewidth=0.0,zorder=101)[0]] #,marker = "o",markevery=swt[point])
    
    # draw simulations
    lines.append(ax1.plot(np.arange(start,last),sim[:,point],label=labels[1],color="#ff8021",linewidth=3.0,alpha=1,zorder=106)[0])

    # Make the y-axis label, ticks and tick labels match the line color.
    ax1.set_ylabel('$WSE (m)$', color='k')
    ax1.set_xlim(xmin=0,xmax=last+1)
    ax1.tick_params('y', colors='k')

    if eyear-syear > 5:
        dtt=5
        dt=int(math.ceil(((eyear-syear)+2)/5.0))
    else:
        dtt=1
        dt=(eyear-syear)+2

    xxlist=np.linspace(0,N,dt,endpoint=True)
    xxlab=np.arange(syear,eyear+2,dtt)
    ax1.set_xticks(xxlist)
    ax1.set_xticklabels(xxlab,fontsize=10)

    # Root Mean Square Error
    RE1=RMSE(sim[time,point],org)
    Rmse1="RMSE: %4.2f"%(RE1)
    #
    ax1.text(0.02,0.98,Rmse1,ha="left",va="center",transform=ax1.transAxes,fontsize=10)

    plt.legend(lines,labels,ncol=1,loc='upper right') #, bbox_to_anchor=(1.0, 1.0),transform=ax1.transAxes)
    
    print ('-- save: '+rivers[point]+"-"+pnames[point]+".png", rivers[point] , pnames[point])
    plt.savefig("./fig/wse/"+rivers[point]+"-"+pnames[point]+".png",dpi=500)

    print ('save: '+rivers[point]+"-"+pnames[point]+".txt", rivers[point] , pnames[point])
    write_text(time,org,sim[:,point],rivers[point],pnames[point])
    return 0

#============================
### --make figures parallel--
#============================
print ( "\n #[3] making figures" )
# para_flag=1
para_flag=0
#--
if para_flag==1:
    p=Pool(1)
    list(p.map(make_fig,np.arange(pnum)))
    p.terminate()
else:
    # list(p.map(make_fig,np.arange(pnum))) ### this will also works
    for inum in np.arange(pnum):
        make_fig(inum)

print ( "@@@@@ end: wse_validation.py \n\n" )

