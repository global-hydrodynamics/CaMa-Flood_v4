# CaMa-Flood Guideline

This document is prepared to give an example how CaMa-Flood is prepared and how we can analyze the results with given demo scripts. This is especially for the new users of CaMa-Flood from github. In this sample running, we drive CaMa-Flood by E2O_ecmwf runoff (global, 0.25deg, 1980-2014). 

## 1. CaMa-Flood setup (scripts and data)

The full package of CaMa-Flood scripts can be accessed and downloadable from website (http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/index.html). It can be cloned from github for developers, while the necessary data should be prepared additionally because of the size limit of github repository.  Internal users at U-Tokyo can copy the data from server. 

### 1.1 Download from website

Users can download CaMa-Flood model package (with source code, basic map data, and sample input data from http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/index.html. A general description can also be found in this website.

```bash
wget --user=USER --password=PASS http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/distribute/CaMa-Flood_v400_20210119.tar.gz
tar -xvf CaMa-Flood_v400_20210119.tar.gz
mv CaMa-Flood_v400_20210119 CaMa-flood_v4
```

Registration is needed to download the package (or only topography map and later the sample runoff). If you have not registered as a CaMa-Flood user, Please fill the Google Form (https://forms.gle/bhq1qWqybeAk157v9) to get an access (username and password) to CaMa-Flood, or please contact to the developer (yamadai [at] iis.u-tokyo.ac.jp) if Google Form does not work well for you.

If users download the package, then please **skip "1.2 Download from Github".** 

### 1.2 Download from Github 

We have launched CaMa-Flood github project for collaboration with all users (https://github.com/global-hydrodynamics/CaMa-Flood_v4/tree/release_v4.00). Two ways are provided to download the package from github. The ReadMe file describes the notices about which version users and developers can select. Then choose the right branch and download it whith git clone or directly download zip. 

```bash
git clone https://github.com/global-hydrodynamics/CaMa-Flood_v4.git
```

or

```bash
wget https://github.com/global-hydrodynamics/CaMa-Flood_v4/archive/release_v4.00.zip
unzip release_4.00.zip 
mv CaMa-Flood_v4-release_v4.00 CaMa-Flood_v4
```

<font color="#FFFF">For internal users</font> 

```bash
cp -r /home/yamadai/work/CaMa_v400/cmf_v400_src CaMa-Flood_v4
```

#### 1.2.1 Prepare the topography data

If users only downloaded the scripts (from github or copy cmd_v400_src), users have to prepare the topography map

```bash
cd CaMa-Flood_v4/map
```

Download global topography map from 

```bash
wget --user=USER --password=PASS http://hydro.iis.u-tokyo.ac.jp/~yamadai/CaMa-Flood_v4/distribute/map_v400/glb_15min.tar.gz
tar -xvf glb_15min.tar.gz
```

<font color="#FFFF">For internal users</font> 

```bash
cp /home/yamadai/public_html/CaMa-Flood_v4/distribute/map_v400/glb_15min.tar.gz .
tar -xvf glb_15min.tar.gz
```

### 1.3 Prepare the runoff input 

CaMa-Flood is driven by runoff. We have processed Earth2Observe (E2O) runoff products for use in CaMa-Flood (0.25deg, daily, 1980-2014, WRR2 version, 7 Land models). Users can create a directory to download the privided runoff, or link to where your own runoff is stored. 

```bash
cd ..		# CaMa-Flood_v4
mkdir -p inp 
cd inp 
```

For external users

```bash
wget --user=USER --password=PASS http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/distribute/inp/E2O_ecmwf.tar		# About 10GB for each runoff
tar -xvf E2O_ecmwf.tar
```

<font color="#FFFF">For internal users</font> 

```bash
cp /home/yamadai/public_html/CaMa-Flood_v4/distribute/inp/E2O_ecmwf.tar .
tar -xvf E2O_ecmwf.tar
```

## 2. Run CaMa-Flood

In this section, users have to adjust settings according to your computation envrionment and proceed the necessary steps before running CaMa-Flood. The details including how users can deal with many specific settings can be found in the official manual 

```bash
/doc/Manual_CaMa-Flood_v400.docx
```

Here we will give an example to run with E2O_ecmwf for 1980-2014 at a global scale with 0.25 degree spatial resolution. 

### 2.1 Computation Environment

You can check and switch the initial settings according to your operating system (Linux or MacOS). In general only the location of NetCDF library needs to be revised. 

```bash
cd ../adm
ls -l *
```

<font color="#FFFF">For internal users</font> 

```bash
rm -f Mkinclude
ln -s Mkinclude_linux_ifort Mkinclude
```

After finished settings, you can compile CaMa-Flood 

```bash
cd ../gosh
./compile.sh yes 
```

You will see 

```bash
Compilation OK! Executable created: ***/CaMa-Flood_v4-release_v4.00/gosh/../src/MAIN_cmf
```

If error exists, first check if the computation setup is correct. 

### 2.2 Prepare the river parameters

Users can specify the river parameters according to a sample runoff (long-term mean) as CaMa-Flood assume the river depth and river width are emperical function with the mean discharge (calculated with the mean runoff, please check the manual for details). 

```bash
cd ../map
```

For external users,

```bash
wget --user=USER --password=PASS http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/distribute/climatology_runoff.tar.gz
tar -xvf climatology_runoff.tar.gz
mv climatology_runoff data
```

<font color="#FFFF">For internal users</font> 

```bash
cp -r /home/yamadai/work/CaMa_v400/cmf_v400_data/dat_map/data .
```

Then generate the river parameters. 

```bash
cd glb_15min
cp -r ../src/src_param .
cd src_param
./s01-channel_params.sh
```

#### 2.3 Input matrix for runoff

Then we need to prepare/revise the input matrix which corresponds to the input runoff by revising /s02-generate_inpmat.sh. In general, users need to redefine the spatial resolution, extent (west, east, south, north) and the latitude direction (NtoS or StoN). Then excute it

```bash
./s02-generate_inpmat.sh
```

### 2.4 Prepare the gosh file

```bash
cd ../../../gosh
```

#### 2.4.1 Server settings

CaMa-Flood can be run at servers/clusters for better parallelization or at local PC. If users run CaMa-Flood in servers/clusters, the settings related to the server/cluster should be revised accordingly. 

```bash
vim sample-glb_15min.sh
```

```bash
#*** PBS setting when needed
#PBS -q E20
#PBS -l select=1:ncpus=20:mem=10gb
#PBS -j oe
#PBS -m ea
#PBS -V
```

If so, change the $BASE to your actual CaMa-Flood directory 

```bash
#BASE=`pwd`/..
BASE="CaMa-Flood_directory" # sample: BASE="/home/User/CaMa-Flood_v4-release_v4.00/"
```

If needed, change the settings for dynamic library

```bash
#*** 0b. Set dynamic library if needed
export IFORTLIB="/opt/intel/lib:/opt/intel/mkl/lib"
export HDF5LIB="/opt/local/hdf5-1.10.5/lib"
export DYLD_LIBRARY_PATH="${HDF5LIB}:${IFORTLIB}:${DYLD_LIBRARY_PATH}"
```

#### 2.4.2 Other settings

Users are recommended to specify your experiment name 

```bash
#*** 1a. Experiment directory setting
EXP="e2o_ecmwf-glb_15min"                       # experiment name (output directory name)
```

Users can select the starting and ending years for the run. 

```bash
#*** 1c. simulation time
YSTA=1980          # start year ( from YSTA / Jan  1st _ 00:00)
YEND=2014          # end   year (until YEND / Dec 31st _ 24:00)
SPINUP=0           # [0]: zero-storage start, [1]: from restart file
NSP=3              # spinup repeat time
```

In general, CaMa-Flood will successfully run with the prepare settings. If not, please check first if the runoff inputs have been put in a correct place.

```bash 
CROFDIR="${BASE}/inp/E2O_ecmwf/" 
```

There are two basic output format in CaMa-Flood (i.e., .bin - binary file, .nc - netcdf file). Users can specify the output format as prefer. 

```bash
LOUTCDF=".FALSE."                           # .TRUE. netCDF output, .FALSE. plain binary output
```

Users can also change the variable list to be outputed. The meaning for each variable can be found in the full manual. 

```bash
CVARSOUT="rivout,rivsto,rivdph,rivvel,fldout,fldsto,flddph,fldfrc,fldare,sfcelv,outflw,storge,pthflw,pthout,maxsto,maxflw,maxdph" # list output variable (comma separated)
```

#### 2.4.3 Start running

With previous modification, CaMa-Flood can be run in three ways. If users submit to server/cluster. This way is strongly recommended because of the parallelization and the large size of CaMa-Flood output. 

```bash
qsub sample-glb_15min.sh
```

or start running in the background (secondary choice).

```bash
nohup ./sample-glb_15min.sh &
```

Users also can start running it simply (It is the last choice, but it is very easy to check if the settings are set correctly).

```bash
./sample-glb_15min.sh
```

#### 2.4.4 Check outputs

There should be an new folder named as the experiment name in the output directory. 

```bash
cd ../out/e2o_ecmwf-glb_15min
ls -l *
```

There should be a number of files corresponding to the $CVARSOUT users specified in the bash file. The size of a single file for the sample run (global, 15min) is around 1.5GB for a single year. Thus, please be aware of the storage especially users are running CaMa-Flood at local PCs. 

## 3. Analysis with the outputs

The CaMa-Flood package provides a few standard analysis tools so that users can check and do simply analysis as needed. The sample tools contain 

1. Simply checking the outputs by mapping plots
2. Downscale the flood water depth to a higher spatial resolution 
3. Calculate the water frequency (flood duration)
4. Estimate the flood hazard map (flood frequency analysis)
5. Runoff presettings

### 3.1 Simply checking the outputs by mapping

With this demo script, users can easily plot the map for river discharge (outflw) to check if CaMa-Flood is run in a proper way. 

```bash
cd ../../etc/result_mapping/
./s01-mapping_plot.sh
```

Users have to adjust in s01-mapping_plot.sh if their output is in netcdf. Plot settings can be modified in plot.py or plot_nc.py

### 3.2 Downscale the flood water depth to a higher spatial resolution 

The global simulation is generally conducted at 15 arcmin (~25km) or 6 arcmin (~10km) which are not sufficient to describe the details at local scale. Therefore, CaMa-Flood provide demo script to downscale the result at coarse spatial resolution to high spatial resolution (e.g., 1min, 15sec, 3sec). Although, users can easily define the extent to be downscaled, we suggest the extent is smaller than 20deg by 20deg (if your target is 3sec) to avoid exceeding the integer limit. 

```bash
cd ../downscale_flddph
cd src
make
cd ..
./s01-downscale.sh 
```

### 3.3 Flood duration 

In certain cases, users want to know the water reoccurrence (how many days with water in a certain period) at specific points. Users can either downscale all daily values to higher resolution and then sum up the days with water. Or users can first calculate the flood duration at a lower spatial resolution and then downscale the flood duration to high spatial resolution. The latter will significantly save computation cost and computation time. 

Flood duration is an estimation of how many days in a year the water level is higher than a threshold. The demo script defines increment of INTDPH=0.05m from 0m to MAXDPH=20m. Users can modify the two variables as per the study area. The calculation is done year by year, users can average the flood duration for all years later. The downscale process is integrated in this step, thus the settings are similar to the previous subsection. 

```bash
cd ../flood_duration
./s01-flood_duration.sh 
```

### 3.4 Flood hazard map

Flood hazard map is a map of water depth of flood at a certain return period (e.g., 1-in-100 year return period). With current model simulation about 35 years, users have to extrapolate the results to obtain the flood water depth for historical-level floods. CaMa-Flood thus provide demo scrpits to do so. 

```bash
cd ../n-year_flood_depth/script
```

In the controlling script main.sh, we provide two ways for downscaling, either with flood water depth or with river water storage. Users can also define the simulation names, the study area and extent, the fitting functions to be used for regression, the return periods. Two steps are separated as the first step to calculate the parameters for the fitting function and the second step to calculate the flood water depth at different return period for specific region. Conducting the demo is easy by

```bash
./main.sh
```

Results can be checked 

```bash
cd ../results/
ls -l *
```

### 3.5 Runoff presettings

CaMa-Flood uses a sample runoff to calculate the parameters of river bathymetry (i.e., river depth and river width). The sample runoff in provided while users can generate the runoff from personal runoff dataset. 

```bash
cd ../../runoff_preset
```

However, we will not go deep in this demo because it is better to keep model parameters consistent among different users. However, users who are especially working on the river bathymetry should look at this and take care of procedures of river parameter settings in /map/glb_15min/src_param/s01-channel_params.sh. 

## 4. Conclusion

With the current guideline, users will be able to run the CaMa-Flood and conduct simple analysis on the CaMa-Flood results. We are looking forward if users are willing to develop other general demo scripts which can be shared with all CaMa-Flood users. Meanwhile, we are looking forward to all users' feedback and issues reporting on the model itself and the analysis tools. Please feel free to conduct us if there are any. 

Dai Yamazaki: yamadai [at] rainbow.iis.u-tokyo.ac.jp

Xudong Zhou: x.zhou [at] rainbow.iis.u-tokyo.ac.jp

