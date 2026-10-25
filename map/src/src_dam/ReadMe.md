# Sample dam and reservoir parameter workflow

`src_dam/` contains scripts and Python utilities for preparing a map-specific dam parameter table used by the reservoir-operation option. The repository directory `map/src/src_dam/` is a reusable template: copy it into the target map directory (for example, `map/glb_15min/src_dam/`) and use the copied working directory. The scripts rely on relative paths and sample settings, so inspect and adapt them for the selected map, dam allocation, naturalized simulation, and ancillary data.

This README describes the supplied example workflow. For the model's reservoir-operation configuration and parameter interpretation, use the [Reservoir Operation manual](../../../doc/Manual_ReservoirOperation_v420.docx). The map overview also describes [map-specific products](../../README.md).

## Workflow overview

The example has two preparation stages. First, run CaMa-Flood without reservoir operation using the target map and forcing, producing plain-binary `outflwYYYY.bin` files in the location expected by the scripts. Then prepare reservoir parameters from those simulation outputs and dam/ancillary datasets. The naturalized run must use the same map and compatible forcing configuration as the intended reservoir simulation.

### 1. Link inputs with `s00-link.sh`

Edit the paths in `s00-link.sh` before running it. The current sample settings point to `map/glb_15min`, `GRanD_river.txt`, `out/test1-glb_15min`, and sample GRSAD/ReGeom directories. The script creates `inp/` links named `map`, `damlist.txt`, `natsim`, `GRSAD`, and `ReGeom`. These are examples, not portable defaults. Confirm that the dam list's map allocation, naturalized output files, and external dataset files exist and correspond to each other.

GRSAD and ReGeom are external datasets with their own availability and usage terms; follow those terms for the exact data you use. The sample link script contains project/download references.

### 2. Calculate parameters with `s01-calc_damparam.sh`

Review `SYEAR`, `EYEAR`, `DT`, `MINUPAREA`, and `TAG` in the script. The checked-in sample years 2000–2001 are explicitly described there as test data; the script comments recommend a longer naturalized record when estimating a 100-year discharge. The resulting estimate depends on the available record and method; do not treat the sample settings as a scientific recommendation for all applications.

The shell workflow runs these stages:

1. `p01_get_annualmax_mean.py` reads the specified yearly `outflw` files and dam grid cells from the allocation table; writes annual maxima and mean discharge arrays.
2. `p02_get_100yrDischarge.py` fits a Gumbel distribution to annual maxima to estimate the 100-year discharge array.
3. `p03_est_fldsto_surfacearea.py` combines GRSAD surface-area time series with ReGeom reservoir geometry and dam capacity to estimate flood-control storage parameters. Missing or unusable ancillary records receive sentinel values in the intermediate table.
4. `p04_complete_damcsv.py` combines discharge, storage, and dam-list records; applies the minimum drainage-area filter, handles multiple dams mapped to one grid, and writes the merged table.
5. The shell script prepends a count line (`NDAMS,NDAMS`) and saves the result as `../dam_param.csv`.

The intermediate outputs are placed under the script's `TAG` directory; the final file is in the map root above the copied `src_dam/`. Python dependencies used by the scripts include NumPy, pandas, and SciPy; confirm compatible versions in the environment used for the workflow.

## Result and compatibility

The generated CSV contains the columns selected by `p04_complete_damcsv.py`, including dam identity and location, CaMa drainage area, grid indices, flood-control and conservation storage, total capacity, and the derived discharge fields. The file also has the leading count line written by the shell wrapper. Keep that line and column order in mind when inspecting or consuming the file; the model/manual interface is authoritative for the exact format expected by the configured reservoir-operation version.

Review the completed table before a simulation, especially allocation indices, missing-data handling, repeated grid locations, and storage/discharge values. If edits are necessary, preserve a copy of the generated source and record the changes.

## Additional utilities

The `script/` directory also contains `modify_damloc.py` for checking or adjusting dam locations against map drainage area, and `est_fldsto_totalsto.py` for applying a fixed flood-storage fraction to an existing table. These are separate utilities, not automatically invoked by `s01-calc_damparam.sh`; inspect their inputs and outputs before using them. The main sample workflow performs its own merge and fallback handling in `p04_complete_damcsv.py`.

The source files in this directory are designed to be copied into a map-specific working directory. Changes to the repository template do not automatically update copies already placed in map packages.
