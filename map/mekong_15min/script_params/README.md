# Map parameterization and preparation workflows

`script_params/` contains reusable map-local recipes for parameterization and related map preparation. Copy a template into a target map's `script_params/` directory, configure it there, and invoke shared executables from `map/tools/code/`. Build shared programs from the repository root with `make -C map/tools/code param`. The legacy implementation under `map/src/` remains available and unchanged.

## Planned workflow organization

The maintainer-approved organization follows the preferred map-preparation sequence. Numbering communicates a useful order for the standard workflow; it does not make every step mandatory or imply that every map uses every tool.

| Script | Purpose | Status |
| --- | --- | --- |
| `s01-channel_params.sh` | Annual mean discharge, power-law channel width/depth/roughness, and MERIT Hydro width merging. | Implemented |
| `s02-bifurcation.sh` | Standard non-MPI bifurcation parameters and basin-connectivity products. | Implemented |
| `s03-generate_inpmat.sh` | User-configured runoff-grid mapping matrix. | Implemented |
| `s04-mpi_map_setup.sh` | MPI map preprocessing. | Planned; not implemented here |
| `s05-permanent_water.sh` | Permanent-water parameter generation. | Planned; not implemented here |
| `t01-alloc_flow_gauge.sh`, `t02-alloc_dams.sh`, `t03-alloc_level_gauge.sh` | Gauge and dam location allocation. | Planned; not implemented here |

Independent diagnostics belong in `script_analysis/`. No `script_allocate/` directory is part of the approved design.

## Channel parameter workflow

[`s01-channel_params.sh`](s01-channel_params.sh) performs only these three operations:

1. `param_calc_outclm` computes annual mean discharge from the configured runoff climatology.
2. `param_calc_rivwth` derives power-law channel width, depth, roughness, and infinite-depth products.
3. `param_set_merit_width` combines the power-law width with the MERIT Hydro width layer.

It does not run bifurcation, MPI, or permanent-water processing. Configure the script's runoff path and coefficients before use. Its defaults retain the legacy standard recipe: binary runoff in mm/s, `inpmat` interpolation, `diminfo_test-1deg.txt`, and the height/width coefficients shown in the script. The dimension file references the standard matrix `inpmat_test-1deg.bin`; the workflow reads that matrix and does not regenerate it.

The expected annual-mean runoff input is `../data/ELSE_GPCC_clm-1981-2010.one`, relative to a map root in the data-rich distribution package. Its inspected CTL describes a 360 x 180, one-record, little-endian grid and runoff in mm/s. The CTL variable comment says "daily runoff climatology," despite the single record and the channel workflow's annual-mean use; treat this as an existing metadata inconsistency. The package guide identifies the standard climatology as ELSE_GPCC (Kim et al., 2009). The source repository does not contain this dataset, and the available package documentation does not state redistribution terms; do not copy it into GitHub until permission and attribution are confirmed. A source checkout without the package data must provide a compatible runoff dataset and matching dimension/matrix metadata.

Build the shared executables, copy the script into the target map's `script_params/`, edit its configuration, then run it from any working directory. The script sets the map root as CWD, checks required programs and inputs, and reports a failing stage. It writes or replaces `outclm.bin`, `rivwth.bin`, `rivhgt.bin`, `rivman.bin`, `rivhgt_inf.bin`, and `rivwth_merit.bin` in the map root. Back up existing products if they must be retained. The legacy Fortran argument readers use 128-character formatted fields for `DIMINFO` and `CROFBIN`; the wrapper rejects longer configured values, and the defaults use short map-root-relative paths.

The standard channel workflow uses annual mean runoff. The historical `map/src/src_param/calc_outclmday.F90` remains legacy-only: it reflects the older maximum-30-day climatology approach associated with Yamazaki et al. (2011). The maintainer reports that later evaluation favored annual mean runoff. This migration does not revalidate the historical method scientifically.

`rivwth.bin` is the power-law width and `rivwth_merit.bin` is the separately named merged width product; the latter is not simply a copy of `width.bin`. The merge algorithm and thresholds are preserved from the legacy tool. The current core map reader selects `rivwth.bin` and does not automatically choose `rivwth_merit.bin`; this workflow produces the alternate product but does not change model-reader selection. See [`map_variables.csv`](../../map_variables.csv) and the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for product metadata and migration evidence.

## Standard bifurcation workflow

[`s02-bifurcation.sh`](s02-bifurcation.sh) generates ordinary bifurcation parameters and basin-connectivity products after the channel-depth map is available. It uses `rivhgt.bin` from `s01`, plus map-root `nextxy.bin`, `basin.bin`, `bifori.txt`, `params.txt`, and the configured `DIMINFO` (default `diminfo_test-1deg.txt`). The first two dimensions in `DIMINFO` must match `params.txt`. `BIFLAYER` defaults to the legacy value `5`; the documented maximum is 10.

The workflow runs `param_set_bifparam` from the map root, then runs `param_set_bif_basin` from the map-local `script_params/` directory because its input/output paths use `../`. By default it writes or replaces `bifprm.txt`, `bifbsn.bin`, `bpoint.bin`, `bifmod.bin`, and `bifcol.bin`. The depth/width inspection raster is optional: run `s02-bifurcation.sh check_map` to also write or replace `check_bif_dphwth.bin`. That file has two float32 records (`bifdph` depth, then `bifwth` width), is only for checking, and is not a model input. Without `check_map`, the shared v5 program does not create it. Older `bifdph.bin` files from the v4 workflow are left untouched. `bifprm.txt` is read by the model only when bifurcation processing is enabled.

This wrapper retains standard non-MPI behavior and the unsorted order written by `param_set_bifparam`. It does not sort `bifprm.txt`, run `set_bif_basin_mpi` or `set_mpi_region`, generate MPI variants, or apply `MaxMPI` modifications. Those operations belong to the future `s04-mpi_map_setup.sh`. The v4 source files and workflows under `map/src/` are unchanged. The migration regression compared the legacy and v5 outputs on temporary copies of the tracked Mekong inputs; this demonstrates compatibility for that case, not independent scientific validation.

## General-purpose runoff input matrix

[`s03-generate_inpmat.sh`](s03-generate_inpmat.sh) invokes `param_generate_inpmat` to create a matrix for a user-selected regular runoff grid. Its retained example uses 0.25-degree cells, longitude bounds -180 to 180 degrees, latitude bounds -60 to 90 degrees, and north-to-south row order. Edit these values to match the runoff data; they describe that input grid, not the CaMa-Flood map grid.

The target map must provide `params.txt`, a selected high-resolution directory with `location.txt`, and each area's `catmxy.bin`; `grdare.bin` is optional. The runoff values themselves are not read while constructing the matrix. The wrapper writes the selected dimension metadata and matrix into the map root and may replace files with those configured names.

This configurable matrix is distinct from the standard `inpmat_test-1deg.bin` and `diminfo_test-1deg.txt` produced by `region_default_inpmat` and distributed with prepared maps for the standard annual-runoff channel workflow. The configurable `inpmat` is expected to be needed by a future sample simulation, but the sample runoff/map dataset and GitHub-versus-package data selection remain open. See the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for the tested Mekong comparison and limitations.

The Python dam-parameter estimation workflow under legacy `map/src/src_dam/` is distinct from the future Fortran dam-location allocation recipe listed above.
