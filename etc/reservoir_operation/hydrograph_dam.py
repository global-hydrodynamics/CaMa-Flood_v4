# visualize inflow/outflw to reservoirs
from datetime import datetime
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as tick
import pandas as pd
import matplotlib.dates as mdates
from matplotlib import colors
from matplotlib import gridspec
import sys
from dateutil.relativedelta import relativedelta
import math

#### initial setting ################################################################

SYEAR=sys.argv[1]
SMON=sys.argv[2]
SDAY=sys.argv[3]
EYEAR=sys.argv[4]
EMON=sys.argv[5]
EDAY=sys.argv[6]
DAMIDLIST=sys.argv[7]
DT=sys.argv[8]

# VAR = ['damout', 'daminf', 'damsto', 'natout', 'natinf', 'obsout', 'obsinf', 'obssto']
VAR = ['damout', 'daminf', 'damsto', 'natout', 'obsout', 'obsinf', 'obssto']
ID_list = list(DAMIDLIST.split(','))

# plot data
sdate_fig = datetime(int(SYEAR), int(SMON), int(SDAY), 0)
edate_fig = datetime(int(EYEAR), int(EMON), int(EDAY), 0)
figdir = './fig/'

#### simulation setting -------------------------------

dt = int(DT)

## Observation data
dtobs = 24*60*60
obsdir="./obs_dam/"
damfile = "./damlist.csv"

#####################################################################

def ceil(src,range):
    return int(math.ceil(src/float(range))*range)
#print ceil(4.5,1e+0)

def floor(src,range):
    return int(math.floor(src/float(range))*range)

def read_binary(dirr, var, x_arr, y_arr):

    for year in range(sdate_fig.year, edate_fig.year+1,1):
        path_new = dirr+'/'+var+str(year)+'.bin'
        print(path_new)
        data_yr = np.fromfile(path_new, 'float32').reshape(-1,ny,nx)
        data_yr = data_yr[:,y_arr,x_arr]
        if year == sdate_fig.year:
            data_all = data_yr
        else:
            data_all = np.append(data_all, data_yr, axis=0)
        del data_yr

    return data_all


def read_camaout(x_arr, y_arr):

    if 'damout' in VAR:
        damout = read_binary(damdir, "outflw", x_arr, y_arr)
    else:
        damout = np.array([])
    
    if "daminf" in VAR:
        daminf = read_binary(damdir, 'daminf', x_arr, y_arr)
    else:
        daminf = np.array([])
    
    if "damsto" in VAR:
        damsto = read_binary(damdir, 'damsto', x_arr, y_arr)
    else:
        damsto = np.array([])

    if "natout" in VAR:
        natout = read_binary(natdir, 'outflw', x_arr, y_arr)
    else:
        natout = np.array([])

    if "natinf" in VAR:
        natinf = read_binary(natdir, 'daminf', x_arr, y_arr)
    else:
        natinf = np.array([])


    return damout, daminf, damsto, natout, natinf


def get_dam_info(grandid):

    grandid = np.int(grandid)
    df = damloc.query('GRAND_ID == @grandid')
    if len(df) == 0:
        print('NOT global GRanD reservoir!')
        return 0
    
    index = df.index.values[0]
    df = df.iloc[0]
    name = df['DamName']
    ix, iy = df['DamIX']-1, df['DamIY']-1
    Qf = df['Qf']
    consto = df['ConVol_mcm']
    fldsto = df['FldVol_mcm']
    emesto = consto + fldsto * 0.8
    print(name)
    print('consto:', '{:.1f}'.format(consto), 'emesto:', '{:.1f}'.format(emesto), 'Qn:', '{:.1f}'.format(df['Qn']), 'Qf:', '{:.1f}'.format(df['Qf']))

    return index, name, ix, iy, Qf, consto, fldsto, emesto


def read_dam_obs(grandid, sdate_f, edate_f):

    obsfile = str(obsdir) + '/' + str(grandid) + '.csv'
    obs = pd.read_csv(obsfile)
   
    obs['date'] = pd.to_datetime(obs['date'])
    obs = obs.set_index('date')
    
    return obs


def slice_dam_obs(obs, sdate_f, edate_f):

    ## check observation period
    obs_start, obs_end = obs.index[0], obs.index[-1]
    print('obs_start:', obs_start, 'obs_end:', obs_end)
    print('sdate_f:', sdate_f, 'edate_f:', edate_f)

    ## slice
    if obs_start > sdate_f or obs_end < edate_f:
        print('obs is unavailable btw', sdate_f,  edate_f)
        s_index = max([obs_start, sdate_f])
        e_index = min([obs_end, edate_f])
        obs = obs[s_index:e_index]
    else:
        obs = obs[sdate_f:edate_f]

    ## interpolate invalid values
    obs_dis = obs[['inflow(m3/s)', 'release(m3/s)', 'storage(MCM)']]
    obs_dis = obs_dis.where(obs_dis>=0)
    obs_dis = obs_dis.interpolate(limit_direction='both')

    ## from dataframe to np array
    if 'obsout' in VAR:
        out = obs_dis['release(m3/s)'].values
    else:
        out = np.array([])

    if 'obsinf' in VAR:
        inf = obs['inflow(m3/s)'].values
    else:
        inf = np.array([])

    if 'obssto' in VAR:
        sto = obs['storage(MCM)'].values
    else:
        sto = np.array([])

    return out, inf, sto


def slice_camaout(o_arr, index, sdate_f, edate_f):
    
    if len(o_arr) == 0:
        return np.array([])

    o_slice = o_arr[:,index]

    s_delta = sdate_f - datetime(sdate_fig.year, 1, 1, 0)
    s_idx = s_delta.total_seconds() // dt
    e_delta = edate_f - datetime(sdate_fig.year, 1, 1, 0)
    e_idx = e_delta.total_seconds() // dt
    s_idx, e_idx = int(s_idx), int(e_idx)
    o_slice = o_slice[s_idx:e_idx+1]
    
    return o_slice
   

def draw_dams(grandid, sdate_fig, edate_fig, obs_l, sim_l, name, Qf, consto, emesto):

    o_obs, i_obs, s_obs = obs_l
    o_dam, i_dam, s_dam, o_nat, i_nat = sim_l
        
    ### plot object
    # plt.rcParams["font.size"]=18
    plt.rcParams["font.size"]=22
    if "damsto" in VAR or "obssto" in VAR:
        fig, (host2, host) = plt.subplots(2,1,gridspec_kw={'height_ratios':[1,3]}, figsize=(10,8), sharex=True)
    else:
        fig, host = plt.subplots(figsize=(10,8))
    fig.subplots_adjust(left=0.15, right=0.95, bottom=0.25, top=0.9, hspace=0.14)

    ### x axis
    tsim=[sdate_fig + relativedelta(seconds=dt*i) for i in range(max(len(o_dam), len(i_dam), len(s_dam), len(o_nat)))]
    tobs=[sdate_fig + relativedelta(seconds=dtobs*i) for i in range(max(len(o_obs), len(i_obs), len(s_obs)))]

    host.set_xlim(sdate_fig,edate_fig)
    intv = (edate_fig - sdate_fig).days//6
    days = mdates.DayLocator(interval=max(intv,1))
    host.xaxis.set_major_locator(days)
    host.xaxis.set_major_formatter(mdates.DateFormatter('%m/%d'))
    host.set_xlabel('date')
    
    ### y axis
    discharge = [i_obs, i_dam, o_obs, o_dam, o_nat, i_nat]
    outmax = [np.max(arr, initial=0) for arr in discharge]
    outmax = np.max(outmax)*1.1
    outmax = ceil(outmax, 1e+1)

    host.set_ylim(0, outmax)
    host.set_ylabel('discharge $(m^3/s)$', fontsize=22)
    ytick_int = max(ceil(outmax/5, 1e+1), 1)
    host.set_yticks(np.arange(0,outmax+10,ytick_int))

    if "damsto" in VAR or "obssto" in VAR:
        if "obssto" in VAR:
            storage = [s_obs, s_dam, emesto, consto]
        else:
            storage = [s_dam, emesto, consto]
        stomax = [np.max(arr, initial=0) for arr in storage]
        stomax = np.max(stomax)*1.1
        stomax = ceil(stomax, 1e+1)
        stomin = [np.min(arr, initial=1e+10) for arr in storage]
        stomin = np.min(stomin)*0.5
        stomin = floor(stomin,1e+1)
        
        host2.xaxis.set_major_locator(days)
        host2.xaxis.set_major_formatter(mdates.DateFormatter('%m/%d'))
        host.xaxis.set_major_formatter(mdates.DateFormatter('%m/%d'))
        host2.set_ylabel('storage\n($10^6$ $m^3$)', fontsize=22)
        host2.set_ylim(stomin,stomax)
        ytick_int = ceil((stomax-stomin)/3, 1e+1)
        host2.set_yticks(np.arange(stomin,stomax+10,ytick_int))


    ### plot

    lines = []

    if "damsto" in VAR or "obssto" in VAR:
        host2.plot(tsim, [consto*0.5]*len(tsim), linewidth=0.8, linestyle='dashed', color='dimgrey') 
        host2.plot(tsim, [consto]*len(tsim), linewidth=0.8, linestyle='dashed', color='dimgrey') 
        host2.plot(tsim, [emesto]*len(tsim), linewidth=0.8, linestyle='dashed', color='dimgrey') 

        if "obssto" in VAR and len(s_obs) != 0:
            if np.max(s_obs) != 0:
                pso, = host2.plot(tobs, s_obs, linewidth=1.5, label='storage(obs)', color='black')
                lines.append(pso)
        if "damsto" in VAR:
            psd, = host2.plot(tsim, s_dam, linewidth=1.5, label='storage(dam)', color='#c93a40')
            lines.append(psd)

    # host.plot(tsim, [Qf]*len(tsim), linewidth=0.8, linestyle='dashed', color='dimgrey') 
    #cycler('color', ['#2cbdfe', '#f3a0f2', '#47dbcd', '#f5b14c', '#9d2ec5', '#a0c238', '#f2cf01', '#c93a40', '#9460a0', '#d16b16'])
    if "obsinf" in VAR and len(i_obs) != 0:
        if np.max(i_obs) != 0:
            pio, = host.plot(tobs,i_obs, '--', linewidth=1.0, label='inflow(obs)', color='black', zorder=3)
            lines.append(pio)
    if "daminf" in VAR:
        pid, = host.plot(tsim,i_dam, '--', linewidth=2.0, label='inflow(dam)', color='#f5b14c', zorder=2)
        lines.append(pid)
    if "natout" in VAR:
        pno, = host.plot(tsim,o_nat, linewidth=2.0, label='outflow(nat)', color='#47dbcd', zorder=1)
        lines.append(pno)
    if "natinf" in VAR:
        pni, = host.plot(tsim,i_nat, '--', linewidth=3.0, label='inflow(nat)', color='#9460a0', zorder=0)
        lines.append(pni)
    if "obsout" in VAR and len(o_obs) != 0:
        if np.max(o_obs) != 0:
            poo, = host.plot(tobs,o_obs, linewidth=2.0, label='outflow(obs)', color='black', zorder=5)
            lines.append(poo)
    if "damout" in VAR:
        pod, = host.plot(tsim,o_dam, linewidth=1.5, label='outflow(dam)', color='#c93a40', zorder=4)
        lines.append(pod)
    
    ### legend
    host.legend(lines, [l.get_label() for l in lines], bbox_to_anchor=(0.5,-0.14), ncol=3, loc='upper center', fontsize=22, frameon=False)

    title = str(name)+'  '+ str(sdate_fig.year)+'~'+str(edate_fig.year)   ####
    plt.suptitle(str(title))    
    figpath=str(figdir)+'/hydro_'+str(grandid)+'_'+str(name)+'_'+str(sdate_fig.year)+'_'+str(edate_fig.year)+'.png'

    plt.savefig(str(figpath), dpi=200, bbox_inches='tight')
    print(figpath)
    #plt.show()
    plt.close()


#----------------------------------------------------------------

def main():
    
    global ID_list

    ID_list = damloc['GRAND_ID'].values.tolist() #sort
    print("ID_list:", ID_list)
    print(' ')
    
    ## read simulation output -------------------------
    x_arr = damloc.query('GRAND_ID == @ID_list')['DamIX'].values.astype('int32') -1
    y_arr = damloc.query('GRAND_ID == @ID_list')['DamIY'].values.astype('int32') -1
    damout_all, daminf_all, damsto_all, natout_all, natinf_all = read_camaout(x_arr, y_arr)

    ## dam loop ----------------------------------------
    for index, ID in enumerate(ID_list):
        print('')
        print("damID:", ID)
        
        try:
            index_int, name_str, ix_int, iy_int, Qf_flt, consto_flt, fldsto_flt, emesto_flt = get_dam_info(ID)
        except TypeError:
            continue

        ## read obs
        global sdate_fig, edate_fig
        if "obsinf" in VAR or "obsout" in VAR or "obssto" in VAR:
            try:
                obs_df = read_dam_obs(ID, sdate_fig, edate_fig)
            except FileNotFoundError:
                obs = [[0,0], [0,0], [0,0]]
            else:
                if len(obs_df) != 0:
                    ## slice obs
                    obsout, obsinf, obssto = slice_dam_obs(obs_df, sdate_fig, edate_fig)
                    obs = [obsout, obsinf, obssto]
                else:
                    obs = [[0,0], [0,0], [0,0]]
        else:
            obs = [[0,0], [0,0], [0,0]]

        ## get simulated data at dam site and slice
        damout = slice_camaout(damout_all, index, sdate_fig, edate_fig)
        daminf = slice_camaout(daminf_all, index, sdate_fig, edate_fig)
        damsto = slice_camaout(damsto_all, index, sdate_fig, edate_fig)
        damsto = damsto / 1e+6
        natout = slice_camaout(natout_all, index, sdate_fig, edate_fig)
        natinf = slice_camaout(natinf_all, index, sdate_fig, edate_fig)
        sim = [damout, daminf, damsto, natout, natinf]

        ## visualize
        draw_dams(ID, sdate_fig, edate_fig, obs, sim, name_str, Qf_flt, consto_flt, emesto_flt)



#===============================================================

if __name__ == '__main__':

    print(VAR)

    fname="./map/params.txt"
    with open(fname,"r") as f:
        lines=f.readlines()
    nx = int( lines[0].split()[0] )
    ny = int( lines[1].split()[0] )
    print('nx:', nx, " ny:", ny)

    ## simulation output --------------------------------------
    damdir = "damsim"
    natdir = "natsim"
    # print('damdir:', damdir)
    # print('natdir:', natdir)
    print('figdir:', figdir)
    
    ## read dam loc ---------------------------------------
    damloc = pd.read_csv(damfile, skiprows=1)
    damloc = damloc.query('GRAND_ID == @ID_list')
    N = len(damloc)
    print('number of dams:', N)

    ## make directory ------------------------
    if not os.path.isdir(figdir):
        os.makedirs(figdir)
        print('mkdir'+str(figdir)+' \n')
    
    main()
    sys.exit()

# %%