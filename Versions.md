# Update Note: CaMa-Flood v4

## Versions (2022.03.31)
Current official release on webpage is v4.01
Latest GitHub branch (master)  is v4.05

## Update in v4.00 (compared to v3.6)
- Major Version Update
- New code structure for more flexible model use
- Topography data updated to **MERIT DEM / MERIT Hydro**

## Update in v4.01 (2021.03.31)
### Importants changes for all users
- **GitHub** code management started.
- **bugfix** in map/src/src_param (src_region) /**generate_impmat.F90**.
- **LBITSAFE for bit-identical simulation was removed**. DNoAtom should be speficied in Mkinclude for bit-identical simulations by avoiding OMP Atomic calculation.
- Sample script for result visualization and validation: etc/validation
### Changes related to some developpers 
- ILS additional code integrated (for use in U-Tokyo Integrated Land Simulator)
- Reservoir module (test version, no support): CTRL_DAMOUT_MOD, etc/reservoir_operation/  

## Update in v4.02 (2021.07.20)
### Changes related to some developpers 
- Runoff nearest neighbour interporlation reactivated.
- Flood stage calculation for vector processor (Earth Simulator) 

## Update in v4.03 (2021.07.28)
### Importants changes for all users
- bug fix on water storage budget (calc_stonxt)
### Changes related to some developpers 
- Downstream dynamic sea level boundary scheme, bug fix & improvement (etc/sealev_boundary)

## Update in v4.04 (2021.12.24)
### Changes related to some developpers 
- MPI parallelization (etc/options_HPC)
- Improve initialization efficiency

## Update in v4.05 (2021.3.14)
### Changes related to some developpers 
- Levee scheme implemented (etc/levee_test)
- Water balance log for flood stage calculation

## Update in v4.06 (2021.4.8)
### Changes related to some developpers 
- Computationally efficient levee scheme

## Update in v4.06 (2021.4.8)
### Changes related to some developpers 
- Computationally efficient levee scheme
- This version for internal development, so not released.

## Update in v4.07 (2021.10.20)
### Changes related to some developpers 
- Single precision mode activated. Many physics codes are modified for Single Precision mode efficiency.
- (Some codes for EFMWF/IFS system was updated)
