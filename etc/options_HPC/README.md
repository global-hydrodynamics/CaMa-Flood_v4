# Options for High Performance Computing

Brief introductions for CaMa-Flood HPC uses. 

1. MPI parallelization
2. Vector-processor code
3. Bit-idential simulation

Below is a tentative idea.

## MPI parallelization

A sinple MPI parallelization is implemented from v4.03

When MPI is used, global land domain devided into several parts and calculated independently as different MPI processes. In order to maximize the efficiency, grids in a same river basin are allocated to the same MPI process, so that between-MPInode communications can be minimized.

MPI parallelization in CaMa-Flood v4.03 is designed for:

- MPI regionalization is defined by a pre-processed map (mpireg.bin).
- Global map and global runoff forcing is used as input (no option for regionally distributed map and runoff input)
- Results of MPI nodes are gathered to the master node, and output is saved as a global map.
- Time step ΔT is shared with all MPI nodes.

### (1) MPI region map preparation.

Basially, grids in one river basin are assigned to a same MPI region to reduce commnication cost. As **some river basins are inter-connected by bifurcation channel**, river basin ID should be updated, and these inter-connected basins should be merged when MPI regionalization data is prepared. 

Codes for MPI regionalization (considering inter-basin connectivity) are prepared in map/src_param/ directory.

#### s03-bifucation_mpi.sh
Shell script to prepare MPI regionalization map. Please edit the script if you want to generate MPI regionalization map with a setting different from the defaults in this script.

#### set_bifparam.F90
Code to set river bifurcation parameters. (Note: this code is also used in s01-channel_params.sh).
Bifurcation channel parameters (bifprm.txt) is generated, by modifying the original bifurcation parameter file (bifori.txt)

#### set_bif_basin.F90
Code to analyze bifucation channel parameter data bifprm.txt), and integrate river basins inter-connected by bifurcation channels.

<Output files>
- bifbsn.bin = basin ID file, ypdated by considering inter-basin bifurcation channel connectivity
- bifcol.bin = basin color file for visualization, corresponding to bifbsn.bin
- bifmod.bin = supplemental map. 1= Basins absorbed by merging. 2= Basins expanded by merging
- bpoint.bin = point of inter-basin connectivity. 3= River bifurcation connectivity, 2=overland bifurcation connectivity, 1=not connected in bifbsn.bin

<Note / Options>
In order to avoid the generation of very big integrated basins, some inter-basin bifurcation channels are excluded when merging basins.
In a default setting, inter-basinchannels are excluded when the size (grid number) of the two basins to be merged is larger than a threshold (6% of global land grids).
As a result, **reccomended MPI regions are up to 16 in default setting**.

If you want to increase the MPI regions, please use 'MaxMPI' option ( % ./set_bif_basin MaxMPI).
Then, the threshold is "3% of global land grids", and **reccomended MPI regions increased to 30**.

#### set_mpi_region.F90
Allocate grids to MPI regions, following the updated basin ID file bifbsn.bin. MPI region file (mpireg.bin) is generated.
Grids are allocated in order to equalize the grid numbers in each MPI region.

Please use the MPI region number (MPI_NP) as an argument (e.g. ./set_mpi_region 16).

In the sample s03.sh script, MPI region files for MPI_NP=4,8,16 are generated.


### (2) MPI functions in CaMa-Flood main codes.

#### cmf_ctrl_mpi_mod.F90
All subroutines including MPI functions are included in the module file: cmf_ctrl_mpi_mod.F90.
It contains initialization, finalize, time step control, and data gathering for output.

#### MPI region calculation in the main code
MPI process number is stored as REGIONTHIS, while number of MPI process is sroted as REGIONALL.

CaMa-Flood converts river network data (nextxy.bin) from 2D map to 1D vector in initialization. Using MPI region file (mpireg.bin), only grids corresponding to REGINTHIS are converted to 1D vector in each node.

### (3) Sample script:
mpi_test.sh, mpi_netcfd_test.sh, and 1min_challenge.sh are prepared.
Please specify MPI region file (CMPIREG) in the shell script. Other settings are same as the default setting.

Please use MPI related configulations for your server/system.



## Vector-processor Optimization

The subroutine to calculate flood stage (CMF_CALC_FLDSTG in cmf_calc_fldstg_mod.F90 ) needs a special care when using in a vector-processor machine (such as Earth Simulator).

This subroutine contains a loop to decide the current flood stage, which is difficult to parallelize for vector-processors.

Thus, another subroutine with optimization for vector-processor is prepared (CMF_OPT_FLDSTG_ES in cmf_calc_fldstg_mod.F90 ).
This code can be activated by speficing option: "LSTG_ES = .TRUE"

NOTE: The subroutine (CMF_OPT_FLDSTG_ES) is significantly slower when used for scaler-processor machine. 

## Bit-idential simulation

If you need to get bit-identical results (i.e. exactly same value, even considering rounding error in double precision), please specify activate DATM=-DNoAtom option in MkInclude
When this option is turned on, OMP_ATOMIC parallelization is avoided (no parallelization for loops with OMP_ATOMIC function).

Note: OMP_ATMIC used to be controlled by LBITSAFE option, but it was switched to #ifdef DNoAtom in order to avoid complex way of coding
