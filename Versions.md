# Update Note: CaMa-Flood v4

## Versions (2021.02.25)
Current official release is v4.00
Latest development branch (master)  is v4.00
Latest working     branch (develop) is develop400

## Update in v4.00 (compared to v3.6)
- Major Version Update
- New code structure for more flexible use
- Topography data updated to MERIT DEM/Hydro

## Update in v4.01 (2021.03.30)
- GitHub code management started.
- bugfix in map/src/src_param (src_region) /generate_impmat.F90 
- ILS additional code integrated
- Reservoir module (in development): CTRL_DAMOUT_MOD, etc/reservoir_operation/  
- LBITSAFE for bit-identical simulation was removed. DNoAtom should be speficied in Mkinclude for bit-identical simulations by avoiding OMP Atomic calculation.
- Sample script for result visualization and validation: etc/validation 
