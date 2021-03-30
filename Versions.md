# Update Note: CaMa-Flood v4

## Versions (2021.03.31)
Current official release is v4.01
Latest development branch (master)  is v4.01
Latest working     branch (develop) is develop401

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
