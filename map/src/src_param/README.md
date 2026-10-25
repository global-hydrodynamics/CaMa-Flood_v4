# Map parameter and input-matrix tools

`src_param/` contains Fortran utilities and sample shell workflows for preparing and inspecting products associated with a particular CaMa-Flood map. In the repository, `map/src/src_param/` is a reusable source template. Copy it into the target map directory (for example, `map/glb_15min/src_param/`) before running map-specific workflows. The scripts use relative paths and commonly expect to be launched from the map root or its copied `src_param/`; check each script's `cd` and paths before running it.

The supplied shell scripts are examples with package-specific filenames and settings. Confirm that all input datasets, dimensions, grid orientation, and products match the selected map before use. Building these tools does not provide the external datasets they consume.

## Build

The component `Makefile` builds the listed utilities using the active configuration included from `adm/Mkinclude`. Build from the map-local copy so executable outputs and inputs are associated with the intended map. The list of targets in the Makefile is the source of truth for the utilities available in this checkout.

## Workflow groups

| Workflow or tool family | What it does | Main implementation and outputs |
| --- | --- | --- |
| Channel parameters (`s01-channel_params.sh`) | Uses runoff climatology to estimate annual mean discharge, derives channel width/depth, fuses width with GWD-LR, prepares bifurcation parameters, permanent-water area, and basin products. | `calc_outclm.F90`, `calc_rivwth.F90`, `set_gwdlr.F90`, `set_bifparam.F90`, `calc_prmwat.F90`, and basin utilities. Typical products include `outclm.bin`, `rivwth.bin`, `rivhgt.bin`, `rivwth_gwdlr.bin`, `bifprm.txt`, and `prmwat.bin`; exact outputs depend on workflow and inputs. |
| Forcing input matrix (`s02-generate_inpmat.sh`) | Creates a map-specific lookup matrix from a regular latitude/longitude forcing grid to map catchments. | `generate_inpmat.F90`; writes the configured `inpmat` binary and dimension metadata. Its sample implementation assumes a linear Cartesian input grid. |
| MPI and bifurcation setup (`s03-mpi_map_setup.sh`) | Sorts/prepares bifurcation data and creates MPI basin/region products. | `set_bifparam.F90`, `set_bif_basin.F90`, `set_bif_basin_mpi.F90`, and `set_mpi_region.F90`; the script selects and copies an MPI region file as `mpireg.bin`. |
| Gauge and dam allocation (`t01-alloc_flow_gauge.sh`, `t02-alloc_dams.sh`, `t03-alloc_level_gauge.sh`) | Matches example external flow-gauge, reservoir, or level-gauge lists to a map network. | `allocate_flow_gauge.F90`, `allocate_dam.F90`, and `allocate_level_gauge.F90`; scripts move generated allocation tables to the map root. Input list schemas and map compatibility must be checked. |
| Inspection and diagnostics | Queries river cells or produces basin and visualization products. | `get_rivinfo.F90`, `get_upbasin.F90`, `get_subbasin.F90`, `get_vecmap.F90`, and `wrte_ctl_map.F90`. Read each program's usage output or source before relying on argument/output details. |

The script names describe sample workflows, not a requirement to run every workflow in sequence. For example, MPI partitions, gauge allocations, and dam allocations are only needed for configurations that use them.

## Input-matrix paths

The `s02-generate_inpmat.sh` sample sets input-grid spacing, bounds, latitude ordering, and output names in the script. The Fortran implementation is for a regular linear grid; a curvilinear or otherwise different coordinate system may need a different implementation. An input matrix belongs to a particular map and forcing-grid definition, so dimensions, bounds, orientation, and file format must match the forcing used by the model.

For the separate workflow that composes an existing CaMa input matrix with a remapping operator for native-grid forcing, see [`native_inpmat/README.md`](native_inpmat/README.md). That workflow has its own assumptions and validation steps; it is not interchangeable with the direct matrix-generation sample without checking the relevant grid definitions.

## Map-local files and external inputs

The sample scripts refer to inputs such as runoff climatology, interpolation weights, gauge/reservoir lists, and high-resolution map layers. Some names and paths are hard-coded examples. Edit them for the target map and verify source-data terms. `s01-channel_params.sh` uses a `diminfo` file and runoff input to calculate climatological discharge; `calc_prmwat` selects available high-resolution data by tag. `s03-mpi_map_setup.sh` includes an optional `MaxMPI` mode whose comments note that some inter-basin connections may be neglected, so inspect that trade-off before selecting it.

Do not combine generated files from different map resolutions or extents based on matching filenames alone. Keep dimensions, grid definitions, and map metadata consistent with the model configuration. The model-side map readers and feature requirements are described in the [map overview](../../README.md) and [model source guide](../../../src/README.md).
