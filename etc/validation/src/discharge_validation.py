'''
Simulated discharge comparison with observed discharge values.
Some sample data is prepared in ./obs directory.
Pre-processing of correspoinding to observation location coordinates (x,y) of CaMa-Flood map are needed.
./obs/discharge/discharge_list_{mapname}.txt - list of sample discahrge locations
./obs/discharge/{name}.txt - sample discharge observatons
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
def NS(s,o):
    """
    Nash Sutcliffe efficiency coefficient
    input:
        s: simulated
        o: observed
    output:
        ns: Nash Sutcliffe efficient coefficient
    """
    #s,o = filter_nan(s,o)
    o=ma.masked_where(o<=0.0,o).filled(0.0)
    s=ma.masked_where(o<=0.0,s).filled(0.0)
    o=np.compress(o>0.0,o)
    s=np.compress(o>0.0,s)
    s,o = filter_nan(s,o)
    return 1 - sum((s-o)**2)/(sum((o-np.mean(o))**2)+1e-20)
#========================================
def NSlog(s,o):
    """
    Nash Sutcliffe efficiency coefficient (log-scale)
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
    s = np.log(s)
    o = np.log(o)
    return 1 - sum((s-o)**2)/(sum((o-np.mean(o))**2)+1e-20)
#========================================
def KGE(s,o):
    """
	Kling Gupta Efficiency (Kling et al., 2012, http://dx.doi.org/10.1016/j.jhydrol.2012.01.011)
	input:
        s: simulated
        o: observed
    output:
        KGE: Kling Gupta Efficiency
    """
    o=ma.masked_where(o<=0.0,o).filled(0.0)
    s=ma.masked_where(o<=0.0,s).filled(0.0)
    o=np.compress(o>0.0,o)
    s=np.compress(o>0.0,s)
    s,o = filter_nan(s,o)
    B = np.mean(s) / np.mean(o)
    y = (np.std(s) / np.mean(s)) / (np.std(o) / np.mean(o))
    r = np.corrcoef(o, s)[0,1]
    return 1 - np.sqrt((r - 1) ** 2 + (B - 1) ** 2 + (y - 1) ** 2)
#========================================
def obs_data(station,syear=2000,smon=1,sday=1,eyear=2001,emon=12,eday=31,obs_dir="./obs"):
    # read the sample observation data
    start_dt=datetime.date(syear,smon,sday)
    last_dt=datetime.date(eyear,emon,eday)
    #-----
    start=0
    last=(last_dt-start_dt).days + 1

    # read discharge
    fname =obs_dir+"/"+station+".txt"
    print ( "-- reading observatio file: ", fname)
    head = 19 #header lines
    if not os.path.exists(fname):
        print ("no file", fname)
        return np.ones([last],np.float32)*-9999.0
    else:
        with open(fname,"r") as f:
            lines = f.readlines()
        #------
        dis = {}
        for line in lines[head::]:
            # line     = list(filter(None, re.split(" ",line)))      ########  this will also works
            # yyyymmdd = list(filter(None, re.split("-",line[0])))
            line2 = line.split()
            yyyymmdd = line2[0].split('-')
#            line     = filter(None, re.split(" ",line))
#            yyyymmdd = filter(None, re.split("-",line[0]))
            yyyy     = '%04d'%(int(yyyymmdd[0]))
            mm       = '%02d'%(int(yyyymmdd[1]))
            dd       = '%02d'%(int(yyyymmdd[2]))
            #---
            if start_dt <= datetime.date(int(yyyy),int(mm),int(dd)) and \
                last_dt >= datetime.date(int(yyyy),int(mm),int(dd)):
                dis[yyyy+mm+dd]=float(line2[1])
            elif last_dt  < datetime.date(yyyy,mm,dd):
                break
        #---
        start=0
        last=(last_dt-start_dt).days + 1
        Q=[]
        for day in np.arange(start,last):
            # day2=int(day)
            target_dt=start_dt+datetime.timedelta(days=int(day))
            yyyy='%04d'%(target_dt.year)
            mm='%02d'%(target_dt.month)
            dd='%02d'%(target_dt.day)
            if (yyyy+mm+dd) in dis.keys():
                Q.append(dis[yyyy+mm+dd])
            else:
                Q.append(-9999.0)
    return np.array(Q)
#
#========================================
##### MAIN calculations ####
# - set start & end dates from arguments
# - set map dimension, read discharge simulation files
# - read station data
# - plot each station data and save figure
# - write a text file with simulated and observed data
#========================================

indir ="out"         # folder where Simulated discharge
syear,smonth,sdate=int(sys.argv[1]),int(sys.argv[2]),int(sys.argv[3])
eyear,emonth,edate=int(sys.argv[4]),int(sys.argv[5]),int(sys.argv[6])
output = sys.argv[7]

print ("\n\n@@@@@ discharge_validation.py", syear,smonth,sdate, eyear,emonth,edate, output )

#========================================
fname="./map/params.txt"
with open(fname,"r") as f:
    lines=f.readlines()
#-------
nx   =int  ( lines[0].split()[0] )
ny   =int  ( lines[1].split()[0] )
gsize=float( lines[3].split()[0] )

#nx     = int(   list(filter(None, re.split(" ",lines[0]))),[0] )   ##### tis will also works
#ny     = int(   list(filter(None, re.split(" ",lines[1]))),[0] )
#gsize  = float( list(filter(None, re.split(" ",lines[3]))),[0] )
#----
start_dt=datetime.date(syear,smonth,sdate)
end_dt=datetime.date(eyear,emonth,edate)
size=60

start=0
last=(end_dt-start_dt).days + 1
N=int(last)

print ( '\n #[1] map dim (nx,ny,gsize):', nx, ny, gsize, 'time series N=', N )

#====================
# read discharge list
pnames=[]
x1list=[]
y1list=[]
x2list=[]
y2list=[]
rivers=[]
#--
fname="./list.txt"
with open(fname,"r") as f:
    lines=f.readlines()
for line in lines[1::]:
    line2 = line.split()
    rivers.append(line2[0].strip())
    pnames.append(line2[1].strip())
    x1list.append(int(line2[2]))
    y1list.append(int(line2[3]))
    x2list.append(int(line2[4]))
    y2list.append(int(line2[5]))

pnum=len(pnames)

print ( '-- read station list', fname, 'station num pnum=', pnum )
#
#========================
### read simulation files
#========================

# multiprocessing array
sim=np.ctypeslib.as_ctypes(np.zeros([N,pnum],np.float32))
shared_array_sim  = sharedctypes.RawArray(sim._type_, sim)

# for parallel calcualtion
inputlist=[]
inpn=0
for year in np.arange(syear,eyear+1):
    yyyy='%04d' % (year)
    inputlist.append([yyyy,indir])
#    print ( inputlist[inpn] )
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

    # timimgs
    target_dt=datetime.date(year,1,1)
    st=(target_dt-start_dt).days
    et=st+dt
    if et >= N:
        et=None

    # simulated discharge
    if output == "bin":
        fname=indir+"/outflw"+yyyy+".bin"
        simfile=np.fromfile(fname,np.float32).reshape([dt,ny,nx])
    else:
        fname=indir+"/o_outflw"+yyyy+".nc"
        print ( fname )
        with nc.Dataset(fname,"r") as cdf:
            simfile=cdf.variables["outflw"][:]
    print ("-- reading simulation file:", fname )
    #-------------
    for point in np.arange(pnum):
        ix1,iy1,ix2,iy2=x1list[point],y1list[point],x2list[point],y2list[point]
        if ix2 == -9999 or iy2 == -9999:
            tmp_sim[st:et,point]=simfile[:,iy1-1,ix1-1]
        else:
            tmp_sim[st:et,point]=simfile[:,iy1-1,ix1-1]+simfile[:,iy2-1,ix2-1]

#--read data parallel--
# para_flag=1
para_flag=0
#--
print ( "\n #[2] Reading simulation file " )
if para_flag==1:
    p=Pool(4)
    res = list(p.map(read_data, inputlist))
    sim = np.ctypeslib.as_array(shared_array_sim)
    p.terminate()
else:
#    res = map(read_data, inputlist)
    for inpi in np.arange(inpn):
        res = read_data(inputlist[inpi])
        sim = np.ctypeslib.as_array(shared_array_sim)

#=====================================
#=== function for saving data file ===
#=====================================
def write_text(obs,sim,river,pname):
    NSval=NS(sim,obs)
    NSLval=NSlog(sim,obs)
    KGEval=KGE(sim,obs)
    fname="./txt/discharge/"+river+"-"+pname+".txt"
    with open(fname,"w") as f:
        print ("-- write comparison result text:", fname )
        f.write("# Validation Data : Discharge\n")
        f.write("# CaMa-Flood version 4.0.0\n")
        f.write("#============================================================\n")
        f.write("# River: %12s RIVER\n"%(river))
        f.write("# Station: %15s\n"%(pname))
        f.write("#============================================================\n")
        f.write("#\n")
        f.write("# MEAN DAILY DISCHARGE (Q)\n")
        f.write("# Unit : m3/s\n")
        f.write("#\n")
        f.write("#\n")
        f.write("#============================================================\n")
        f.write("# Statistics \n")
        f.write("#\tNS   : %3.2f\n"%(NSval))
        f.write("#\tNSlog: %3.2f\n"%(NSLval))
        f.write("#\tKGE  : %3.2f\n"%(KGEval))
        f.write("#\n")
        f.write("#\n")
        f.write("YYYY-MM-DD      Observed     Simulated\n")
        f.write("#============================================================\n")
        for date in np.arange(start,last):
            date2=int(date)
            target_dt=start_dt+datetime.timedelta(days=date2)
            year=target_dt.year
            mon=target_dt.month
            day=target_dt.day
            line = '%04d-%02d-%02d%14.4f%14.4f\n'%(year,mon,day,obs[date],sim[date])
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
    org=obs_data(pnames[point],syear=syear,eyear=eyear)
    org=np.array(org)

    print ("-- make figure for:", point, rivers[point], pnames[point] )

    # draw observations
    lines=[ax1.plot(np.arange(start,last),ma.masked_less(org,0.0),label=labels[0],color="#34495e",linewidth=3.0,zorder=101)[0]] #,marker = "o",markevery=swt[point])
    
    # draw simulations
    lines.append(ax1.plot(np.arange(start,last),sim[:,point],label=labels[1],color="#ff8021",linewidth=3.0,alpha=1,zorder=106)[0])

    # Make the y-axis label, ticks and tick labels match the line color.
    ax1.set_ylabel('discharge (m$^3$/s)', color='k')
    ax1.set_xlim(xmin=0,xmax=last+1)
    ax1.tick_params('y', colors='k')

    # scentific notaion
    ax1.ticklabel_format(style="sci",axis="y",scilimits=(0,0))
    ax1.yaxis.major.formatter._useMathText=True 

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

    # Nash-Sutcliffe calcuation
    NS1=NS(sim[:,point],org)
    Nash1="NS      : %4.2f"%(NS1)
    #
    ax1.text(0.02,0.98,Nash1,ha="left",va="center",transform=ax1.transAxes,fontsize=10)

    # Nash-Sutcliffe calcuation (log-scale)
    NS2=NSlog(sim[:,point],org)
    Nash2="NS(log): %4.2f"%(NS2)
    #
    ax1.text(0.02,0.93,Nash2,ha="left",va="center",transform=ax1.transAxes,fontsize=10)
 
    # Kling-Gupta efficency
    KGE1=KGE(sim[:,point],org)
    Kgeh="KGE     : %4.2f"%(KGE1)
    #
    ax1.text(0.02,0.88,Kgeh,ha="left",va="center",transform=ax1.transAxes,fontsize=10)
 
    plt.legend(lines,labels,ncol=1,loc='upper right')

    print ('-- save: '+rivers[point]+"-"+pnames[point]+".png", rivers[point] , pnames[point])
    plt.savefig("./fig/discharge/"+rivers[point]+"-"+pnames[point]+".png",dpi=500)

    print ('save: '+rivers[point]+"-"+pnames[point]+".txt", rivers[point] , pnames[point])
    write_text(org,sim[:,point],rivers[point],pnames[point])
    return 0

#============================
### --make figures parallel--
#============================
print ( "\n #[3] making figures" )
# para_flag=1
para_flag=0
#--
if para_flag==1:
    p=Pool(4)
    list(p.map(make_fig,np.arange(pnum)))     ##### this will also works
    p.terminate()
else:
#    list(map(make_fig,set(np.arange(pnum)))) ##### this will also works 
#    map(make_fig,np.arange(pnum))
    for inum in np.arange(pnum):
        make_fig(inum)

print ( "@@@@@ end: discharge_validation.py \n\n" )

