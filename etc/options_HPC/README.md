# Options for High Performance Computing

This guide covers MPI partitioning, vector-processor calculations, and floating-point reproducibility. For compiler and library setup, see the [build guide](../../adm/README.md); for run configurations, see the [sample execution guide](../../gosh/README.md).

1. MPI parallelization
2. Vector-processor code
3. Floating-point reproducibility

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

Reusable MPI map-preparation tools are in [`map/src/src_param/`](../../map/src/src_param/README.md). Copy the component into the target map directory, build it, and run the workflow from the copied `src_param/` directory. A GitHub clone requires separate map data; a distribution package may include a matching map and prepared MPI partitions.

#### `s03-mpi_map_setup.sh`
Shell script to prepare MPI regionalization map. Please edit the script if you want to generate MPI regionalization map with a setting different from the defaults in this script.

#### set_bifparam.F90
Code to set river bifurcation parameters. (Note: this code is also used in s01-channel_params.sh).
Bifurcation channel parameters (bifprm.txt) is generated, by modifying the original bifurcation parameter file (bifori.txt)

#### `set_bif_basin_mpi.F90`
The MPI workflow uses this program to analyze `bifprm.txt` and group basins connected by bifurcation channels. A separate `set_bif_basin.F90` provides basin-analysis utilities; use the program selected by the MPI workflow for partition preparation.

Outputs are written to the parent map directory:

- `bifbsn_mpi.bin`: basin IDs updated for inter-basin bifurcation connectivity; read by `set_mpi_region`.
- `bifcol_mpi.bin`: visualization colors for the updated basins.
- `bifmod_mpi.bin`: indicators for basins absorbed or expanded during merging.
- `bpoint_mpi.bin`: inter-basin connection markers.
- `bifprm_mpi.txt`: bifurcation parameters retained for MPI partition preparation.

<Note / Options>
In order to avoid the generation of very big integrated basins, some inter-basin bifurcation channels are excluded when merging basins.
In a default setting, inter-basinchannels are excluded when the size (grid number) of the two basins to be merged is larger than a threshold (6% of global land grids).
The sample workflow comments suggest up to 16 partitions with the default setting; the useful partition count depends on the selected river map and its basin connectivity.

If you want to increase the MPI regions, run `./set_bif_basin_mpi MaxMPI`, or set `TAG='MaxMPI'` in `s03-mpi_map_setup.sh`.
This changes the merging threshold to 3% of the active land cells. The sample comments suggest up to 30 partitions in this mode. Some inter-basin connections can be neglected; inspect this trade-off for the intended map and simulation.

#### set_mpi_region.F90
Allocate grids to MPI regions using `bifbsn_mpi.bin`. For a requested process count `<N>`, the program writes `mpireg-<N>.bin` and the corresponding `bifprm-<N>.txt` in the parent map directory.
Grids are allocated in order to equalize the grid numbers in each MPI region.

Please use the MPI region number (MPI_NP) as an argument (e.g. ./set_mpi_region 16).

The sample `s03-mpi_map_setup.sh` generates `mpireg-4.bin`, `mpireg-8.bin`, and `mpireg-16.bin`, then copies `mpireg-16.bin` to `mpireg.bin`. Set `CMPIREG` and the MPI launch process count to match the selected partition file.


### (2) MPI functions in CaMa-Flood main codes.

#### cmf_ctrl_mpi_mod.F90
All subroutines including MPI functions are included in the module file: cmf_ctrl_mpi_mod.F90.
It contains initialization, finalize, time step control, and data gathering for output.

#### MPI region calculation in the main code
MPI process number is stored as REGIONTHIS, while number of MPI process is sroted as REGIONALL.

CaMa-Flood converts river network data (nextxy.bin) from 2D map to 1D vector in initialization. Using MPI region file (mpireg.bin), only grids corresponding to REGINTHIS are converted to 1D vector in each node.

### (3) Sample script:
[`mpi_test.sh`](mpi_test.sh), [`mpi_netcdf_test.sh`](mpi_netcdf_test.sh), and [`1min_challenge.sh`](1min_challenge.sh) are example scripts in this directory. Review their compiler/library paths, launch commands, map paths, and forcing requirements for your system before running them. The main [`gosh/test5_mpi.sh`](../../gosh/test5_mpi.sh) is another MPI execution example.
Please specify MPI region file (CMPIREG) in the shell script. Other settings are same as the default setting.

Please use MPI related configulations for your server/system.



## Vector-processor Optimization

The subroutine to calculate flood stage (CMF_CALC_FLDSTG in cmf_calc_fldstg_mod.F90 ) needs a special care when using in a vector-processor machine (such as Earth Simulator).

This subroutine contains a loop to decide the current flood stage, which is difficult to parallelize for vector-processors.

Thus, another subroutine with optimization for vector-processor is prepared (CMF_OPT_FLDSTG_ES in cmf_calc_fldstg_mod.F90 ).
This code can be activated by speficing option: "LSTG_ES = .TRUE"

NOTE: The subroutine (CMF_OPT_FLDSTG_ES) is significantly slower when used for scaler-processor machine. 

## Floating-point reproducibility

To disable the guarded OpenMP atomic-update paths, set the following in the active `adm/Mkinclude`, then clean and rebuild:

```make
DATM=-DNoAtom_CMF
```

The source tests the preprocessor macro `NoAtom_CMF`; `DATM` is the Make variable that passes the definition into `CFLAGS`. The historical `LBITSAFE` runtime switch is not the control for these paths.

Disabling these atomic updates addresses one source of floating-point ordering differences. It does not establish bitwise identity across compilers, optimization flags, precision modes, thread/process counts, or other reduction orders. Assess reproducibility with an identified baseline, input data, and build/run configuration; see the [numerical and parallel behavior notes](../../src/README.md#numerical-and-parallel-behavior).
