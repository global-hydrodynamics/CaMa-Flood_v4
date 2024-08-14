# Sample workflow to activate reservoir operation scheme.

2021.03.25,  (v4.01) written by Risa Hanazaki & Dai Yamazaki
2023.12.1,   (v4.13 update) updated documenatation for v4.13 and later
2024.05.01,  (v4.20 update) More detailed documentation on Dam Allocation and Reservoir Operation is available in doc/Manual_ReservoirOperation_v420.docx. 


Note: 
This is the sample workflow to activate the reservoir operation scheme using the minimum sample data included in the CaMa-Flood package.

Here, we add reservoir operation to the "test1" sample simulation. (i.e. global 15 arcmin simulation for year 2000-2001).

We highly recommend you to do "long-term naturalized simulations" (>30 years) in order to more precisely estimate flood control discharge (30% of 100yr flood), when you use reservoir operation scheme for your reserch.

## (1)  Prepare CaMa-Flood map parameters and execute naturalized simulations.

### map parameters
Please go to map/glb_15min/ directory.

Copy map/src/src_param/ to map/glb_15min/, then compile Fortran90 codes in map/glb_15min/src_pararm/ directory and excecute scripts (s01,s02,s03,s04).

The map parameters (river channel width and depth, input matrix, MPI region map, River gauge and dam allocation) are preapred.

### execute naturalized simulation
Please go to gosh/ directory, and edit and execute test1-glb_15min.sh.
The naturalized simulation results are saved in out/test1-glb_15min/ directory.

## (2) Prepare reservoir parameters.

### Download reservoir data, and estimate parameter.
Please go to map/glb_15min/ directory.

Copy map/src/src_dam/ to map/glb_15min/

Edit s00-link.sh script, to specify the location of data used for reservoir operation.
If needed, please download the GRSAD and ReGeom data form the URL suggested in S00-link.sh.
Then, execute s00-link.sh script.

Next, please execute s01-calc_damparam.sh script.
The dam parameter file (dam_param.csv) is to be saved in map/glb_15min/ directory.

### Format of dam parameter file

dam_param.csv
 ix, iy: grid index in CaMa-Flood river map (Fortran ix,iy)
 upreal: reported drainage area [km2]. (if not known, -999)
 uparea_cama: drainage area in CaMa river map [km2]
 totalsto_mcm: total storage capacity [million cubic meter]

## (3) Execute simulation with reservoir.

Go to gosh/ directory, and edit test6-reservoir_glb_15min.sh script.
Execute test6-reservoir_glb_15min.sh.
The simulation results are to be saved in out/test6-reservoir/ directory.
