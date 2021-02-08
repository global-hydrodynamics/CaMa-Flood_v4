#! /usr/python
# -*- coding: utf-8 -*-
'''
Simulated water surface elevation (wse) comparison with observed wse values.
Some sample data is prepared in ./obs directory.
Pre-processing of correspoinding to observation location coordinates (x,y) of CaMa-Flood map are needed.
./obs/wse/wse_list.txt - list of sample discahrge locations
./obs/wse/{name}.txt - sample discharge observatons
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
#========================================
#========================================
def mk_dir(sdir):
  try:
    os.makedirs(sdir)
  except:
    pass
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
    return 1 - sum((s-o)**2)/(sum((o-np.mean(o))**2)+1e-20)
#========================================
def obs_data(station,syear=2000,smon=1,sday=1,eyear=2001,emon=12,eday=31,CaMa_dir="../../../"):
    # read the sample observation data
    start_dt=datetime.date(syear,smon,sday)
    last_dt=datetime.date(eyear,emon,eday)
    #-----
    start=0
    last=(last_dt-start_dt).days + 1

    # read discharge
    fname =CaMa_dir+"obs/discharge/"+station+".txt"
    head = 21 #header lines
    if not os.path.exists(fname):
        print "no file", fname
        return np.ones([last],np.float32)*-9999.0
    else:
        f = open(fname,"r")
        lines = f.readlines()
        f.close()
        dis = {}
        for line in lines[head::]:
            line     = filter(None, re.split(" ",line))
            yyyymmdd = filter(None, re.split("-",line[0]))
            yyyy     = int(yyyymmdd[0])
            mm       = int(yyyymmdd[1])
            dd       = int(yyyymmdd[2])
            #---
            if start_dt < datetime.date(yyyy,mm,dd) and last_dt > datetime.date(yyyy,mm,dd):
                dis[yyyy,mm,dd]=float(line[1])
            elif last_dt  < datetime.date(yyyy,mm,dd):
                break
        #---
        start=0
        last=(last_dt-start_dt).days + 1
        Q=[]
        for day in np.arange(start,last):
            target_dt=start_dt+datetime.timedelta(days=day)
            if (target_dt.year,target_dt.month,target_dt.day) in dis.keys():
                Q.append(dis[target_dt.year,target_dt.month,target_dt.day])
            else:
                Q.append(-9900.0)
    return np.array(Q)
#========================================
indir = "../../../out/test1-glb_15min" # folder where Simulated discharge
CaMa_dir="../../../" # CaMa folder
mapname="glb_15min" # map name [e.g. glb_15min,glb_06min, etc.]
fname=CaMa_dir+"map/"+mapname+"/params.txt"
f=open(fname,"r")
lines=f.readlines()
f.close()
#-------
nx     = int(filter(None, re.split(" ",lines[0]))[0])
ny     = int(filter(None, re.split(" ",lines[1]))[0])
gsize  = float(filter(None, re.split(" ",lines[3]))[0])
#----
syear,smonth,sdate=2000,1,1
eyear,emonth,edate=2001,12,31
start_dt=datetime.date(syear,smonth,sdate)
end_dt=datetime.date(eyear,emonth,edate)
size=60

start=0
last=(end_dt-start_dt).days + 1
N=int(last)
print N
#====================
pnames=[]
x1list=[]
y1list=[]
x2list=[]
y2list=[]
rivers=[]
#--
fname=CaMa_dir+"/obs/wse/wse_list.txt"
with open(fname,"r") as f:
    lines=f.readlines()
    for line in lines[1::]:
        line = filter(None, re.split(" ",line))
        rivers.append(line[0].strip())
        pnames.append(line[1].strip())
        x1list.append(int(line[2]))
        y1list.append(int(line[3]))
        x2list.append(int(line[4]))
        y2list.append(int(line[5]))

pnum=len(pnames)
#========================
org=[]
opn=[]
sim=[]

# multiprocessing array
sim=np.ctypeslib.as_ctypes(np.zeros([N,pnum],np.float32))
shared_array_sim  = sharedctypes.RawArray(sim._type_, sim)

# for parallel calcualtion
inputlist=[]
for year in np.arange(syear,eyear+1):
    yyyy='%04d' % (year)
    inputlist.append([yyyy,indir])

#========================
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

    # simulated discharge
    fname=indir+"/sfcelv"+yyyy+".bin"
    simfile=np.fromfile(fname,np.float32).reshape([dt,ny,nx])
    #-------------
    for point in np.arange(pnum):
        ix1,iy1,ix2,iy2=x1list[point],y1list[point],x2list[point],y2list[point]
        if ix2 == -9999 or iy2 == -9999:
            tmp_sim[st:et,point]=simfile[:,iy1-1,ix1-1]
        else:
            tmp_sim[st:et,point]=simfile[:,iy1-1,ix1-1]+simfile[:,iy2-1,ix2-1]
#--------
p   = Pool(1)
res = p.map(read_data, inputlist)
sim = np.ctypeslib.as_array(shared_array_sim)
p.terminate()

def make_fig(point):
    plt.close()
    labels=["Observed","Simulated"]
    fig, ax1 = plt.subplots()
    org=obs_data(pnames[point],syear=syear,eyear=eyear)
    org=np.array(org)
    #print org
    lines=[ax1.plot(np.arange(start,last),ma.masked_less(org,0.0),label=labels[0],color="black",linewidth=1.5,zorder=101)[0]] #,marker = "o",markevery=swt[point])
    
    # draw simulations
    lines.append(ax1.plot(np.arange(start,last),sim[:,point],label=labels[1],color="blue",linewidth=1.0,alpha=1,zorder=106)[0])

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
    xxlab=np.arange(syear,eyear+1,dtt)
    ax1.set_xticks(xxlist)
    ax1.set_xticklabels(xxlab,fontsize=10)

    # Nash-Sutcllf calcuation
    NS1=NS(sim[:,point],org)
    #print point,NS1,NS2
    Nash1="NS: %4.2f"%(NS1)
    #
    ax1.text(0.02,0.95,Nash1,ha="left",va="center",transform=ax1.transAxes,fontsize=10)

    plt.legend(lines,labels,ncol=1,loc='upper right') #, bbox_to_anchor=(1.0, 1.0),transform=ax1.transAxes)
    
    print 'save',rivers[point] , pnames[point]
    plt.savefig("../fig/discharge/"+rivers[point]+"-"+pnames[point]+".png",dpi=500)
    return 0

# make folders for figures
mk_dir("../fig")
mk_dir("../fig/discharge")

# para_flag=1
para_flag=0
#--
if para_flag==1:
    p=Pool(1)
    p.map(make_fig,np.arange(pnum))
    p.terminate()
else:
    map(make_fig,np.arange(pnum))