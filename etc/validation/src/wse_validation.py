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
#====  functions for making figures  ====
#========================================
def RMSE(s,o):
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
    #s,o = filter_nan(s,o)
    o=ma.masked_where(o<=0.0,o).filled(0.0)
    s=ma.masked_where(o<=0.0,s).filled(0.0)
    o=np.compress(o>0.0,o)
    s=np.compress(o>0.0,s) 
    return 1 - sum((s-o)**2)/(sum((o-np.mean(o))**2)+1e-20)
#========================================
def obs_data(station,syear=2000,smon=1,sday=1,eyear=2001,emon=12,eday=31,obs_dir="./obs/wse"):
    # read the sample observation data
    start=datetime.date(syear,smon,sday)
    end=datetime.date(eyear,emon,eday)
    
    # read wse observation
    fname=obs_dir+"/"+station+".txt"

    with open(fname,"r") as f:
        lines=f.readlines()

    #--
    head=20
    #--
    time=[] # time in days
    data=[] # WSE in [m]
    for line in lines[head::]:
        if line[0][0] == "#":
            continue
        line = filter(None,re.split(" ",line))
        date = line[0]
        date = re.split("-",date)
        yyyy = int(date[0])
        mm   = int(date[1])
        dd   = int(date[2])
        wse  = float(line[1])
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
#========================================
indir ="out"         # folder where Simulated discharge
syear,smonth,sdate=int(sys.argv[1]),int(sys.argv[2]),int(sys.argv[3])
eyear,emonth,edate=int(sys.argv[4]),int(sys.argv[5]),int(sys.argv[6])
egm=sys.argv[7]      # provide EGM of observations, CaMa-Flood wse is given in EGM96

print ("@@@@@ wse_validation.py", syear,smonth,sdate, eyear,emonth,edate, egm )

#========================================
fname="./map/params.txt"
with open(fname,"r") as f:
    lines=f.readlines()
#-------
nx     = int(filter(None, re.split(" ",lines[0]))[0])
ny     = int(filter(None, re.split(" ",lines[1]))[0])
gsize  = float(filter(None, re.split(" ",lines[3]))[0])
#----
start_dt=datetime.date(syear,smonth,sdate)
end_dt=datetime.date(eyear,emonth,edate)
size=60

start=0
last=(end_dt-start_dt).days + 1
N=int(last)

print ( '' )
print ( '# map dim (nx,ny,gsize):', nx, ny, gsize, 'time seriez N=', N )

#====================
# read SWE station list
pnames=[]
x1list=[]
y1list=[]
rivers=[]
legm08=[]
legm96=[]
#----------------------------
fname="./obs/wse/wse_list.txt"
with open(fname,"r") as f:
    lines=f.readlines()
#----------------------------
for line in lines[1::]:
    line = filter(None, re.split(" ",line))
    rivers.append(line[0].strip())
    pnames.append(line[1].strip())
    x1list.append(int(line[2]))
    y1list.append(int(line[3]))
    legm08.append(float(line[4]))
    legm08.append(float(line[5]))
pnum=len(pnames)

print ( '- read station list', fname, 'station num pnum=', pnum )

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
for year in np.arange(syear,eyear+1):
    yyyy='%04d' % (year)
    inputlist.append([yyyy,indir])

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

    # simulated discharge
    fname=indir+"/sfcelv"+yyyy+".bin"
    simfile=np.fromfile(fname,np.float32).reshape([dt,ny,nx])
    print ("-- reading simulation file:", fname )
    #-------------
    for point in np.arange(pnum):
        ix1,iy1=x1list[point],y1list[point]
        tmp_sim[st:et,point]=simfile[:,iy1-1,ix1-1]

#--read data parallel--
# para_flag=1
para_flag=0
#--
if para_flag==1:
    p   = Pool(4)
    res = p.map(read_data, inputlist)
    sim = np.ctypeslib.as_array(shared_array_sim)
    p.terminate()
else:
    res = map(read_data, inputlist)
    sim = np.ctypeslib.as_array(shared_array_sim)


#==================================
#=== function for making figure ===
#==================================
def make_fig(point):
    plt.close()
    labels=["Observed","Simulated"]
    fig, ax1 = plt.subplots()
    time,org=obs_data(pnames[point],syear=syear,eyear=eyear)
    if egm=="EGM96":
        org=np.array(org)+np.array(legm08[point])-np.array(legm96[point])
    else:
       org=np.array(org) 

    print ("reading observation file:", "./obs/wse/", pnames[point] )

    lines=[ax1.plot(time,org,label="obs",marker="o",color="black",fillstyle="none",linewidth=0.0,zorder=101)[0]] #,marker = "o",markevery=swt[point])
    
    # draw simulations
    lines.append(ax1.plot(np.arange(start,last),sim[:,point],label=labels[1],color="blue",linewidth=1.0,alpha=1,zorder=106)[0])

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
    ax1.text(0.02,0.95,Rmse1,ha="left",va="center",transform=ax1.transAxes,fontsize=10)

    plt.legend(lines,labels,ncol=1,loc='upper right') #, bbox_to_anchor=(1.0, 1.0),transform=ax1.transAxes)
    
    print ('save:', rivers[point]+"-"+pnames[point]+".png", rivers[point] , pnames[point])
    plt.savefig("./fig/wse/"+rivers[point]+"-"+pnames[point]+".png",dpi=500)

    print ( "" )
    return 0

#========================
### --make figures parallel--
#========================
print ( "" )
print ( "# making figures" )
#para_flag=1
# para_flag=1
para_flag=0
#--
if para_flag==1:
    p=Pool(1)
    p.map(make_fig,np.arange(pnum))
    p.terminate()
else:
    map(make_fig,np.arange(pnum))

