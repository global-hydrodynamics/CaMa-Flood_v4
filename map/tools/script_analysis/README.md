# Map analysis and diagnostic scripts

`script_analysis/` contains standalone checks and diagnostics that operate on prepared maps. The analysis tools are separate from map generation so they can inspect partitions or input maps produced by other workflows.

## MPI bifurcation connectivity

[`a01-check_mpi_bifurcation.sh`](a01-check_mpi_bifurcation.sh) analyzes any MPI region raster against the map's standard `bifprm.txt`. It does not run s04 or change map data. Build the shared executable with `make -C map/tools/code analysis`, copy the wrapper into `<map>/script_analysis/`, and run from any working directory; relative input/output paths are interpreted at the map root.

```sh
./script_analysis/a01-check_mpi_bifurcation.sh mpireg-16.bin
./script_analysis/a01-check_mpi_bifurcation.sh mpireg-8.bin --csv dropped_bifurcations.csv
```

The binary reader expects one direct-access int32 `NX x NY` record in the host-compatible CaMa-Flood map layout. The diagnostic reads `params.txt`, `nextxy.bin`, `bifprm.txt`, and the selected region map. It reproduces the endpoint membership logic from `src/cmf_ctrl_maps_mod.F90:CALC_1D_SEQ`: in-region river cells are traversed in upstream order, and downstream traversal can add an out-of-region downstream cell because that step does not re-check the region label. This is an effective downstream vector extension; the inspected code does not establish a general ghost-cell exchange system. `READ_BIFPARAM` counts a pathway when both endpoints have positive vector indices on the same rank.

The report separates:

- **Retained:** both endpoints receive positive vector indices on at least one rank. They may have different assigned region IDs if the downstream vector extension makes them coexist on one rank.
- **Dropped:** both endpoints have valid vector indices on their assigned ranks, but no single rank has both endpoints.
- **Invalid/unassigned:** an endpoint is outside the map, not a modeled river/outlet cell, or receives no vector index on its assigned rank.

It reports totals, percentages, channel (`width(1)>0`) versus floodplain-only (`width(1)<=0`) subtotals, and the count retained across differing assigned region IDs by downstream vector extension. Optional CSV output contains only dropped pathways with endpoint coordinates, region IDs, elevation, geographic coordinates, path type, and first-layer width. Without `--csv`, the tool writes no persistent diagnostic file.

The dropped fraction describes connections omitted by the selected domain partition. It is not an estimate of discharge, storage, or inundation error, and it does not independently validate the scientific quality of the map or simulation.

The s04 map-generation script invokes this tool for its selected `DEFAULT_MPI` map and prints the summary in the run log. The same standalone command can inspect any compatible externally generated map. See the [parameter workflow guide](../script_params/README.md#mpi-map-preparation) for basin merging, threshold rationale, generated files, and regression evidence.


## MPI region coloring

`analysis_mpi_region_color` is an independent visualization tool. From a map root, pass an existing numbered region map; it reads `params.txt` for dimensions and global-wrap geometry, then writes the matching color raster:

```sh
../tools/code/analysis_mpi_region_color mpireg-8.bin
```

This creates `bsncol_mpi-8.bin`, a single int32 `NX x NY` record with `-9999` outside assigned regions. It colors region IDs using the legacy size-prioritized, adjacency-aware palette logic (large regions first; then small regions, with singleton regions assigned color 1). The palette and tie/order behavior follow `set_color` in `map/src/src_param/set_bif_basin_mpi.F90`; the utility applies that logic to the supplied MPI region raster. It does not alter the region map and is not called by s04. Invoke it manually for any existing partition when visualization is useful. `bifcol_mpi.bin`, produced during s04 basin aggregation, is a separate visualization of MPI-aggregated basin IDs.

## River-cell and basin analysis

The shared `analysis_get_rivinfo`, `analysis_get_upbasin`, and `analysis_get_subbasin` executables are wrapped by the scripts below. Build them first with `make -C map/tools/code analysis`. Copy a template into a map-local `script_analysis/`; scripts resolve map paths relative to their own location and write outputs in the documented map or `analysis/` directory.

- [`a01-river_info.sh`](a01-river_info.sh) queries configured grid indices (`IX`, `IY`) or geographic coordinates and can save the textual result.
- [`a02-upstream_basin.sh`](a02-upstream_basin.sh) extracts the upstream mask for configured `IX`, `IY` to a configurable analysis filename. It does not overwrite standard `upbasin.bin`.
- [`a03-subbasin.sh`](a03-subbasin.sh) runs the same segmentation algorithm as `script_params/s06-subbasin.sh` with a configurable km2 threshold, writing threshold-named files under `analysis/` rather than standard products.

Grid indices are one-based CaMa-Flood map indices. `get_upbasin` marks selected upstream river cells as 1, other mapped river cells as 0, and cells outside the network as -9999. `get_subbasin` applies an upstream-area-increase boundary rule; the threshold is not a simple minimum or maximum basin-area filter. See [standard parameter workflows](../script_params/README.md#river-network-analysis) for the GRDC-based `s07` example and standard product meanings. Optional analysis outputs are not required for model execution. Automatic CTL generation requires registered filenames and does not infer metadata for arbitrary names.
