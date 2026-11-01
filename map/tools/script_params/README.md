# Map parameterization and preparation workflows

`script_params/` contains reusable map-local recipes for parameterization and related map preparation. Copy a template into a target map's `script_params/` directory, configure it there, and invoke shared executables from `map/tools/code/`. Build shared programs from the repository root with `make -C map/tools/code param`. `map/src/` is preserved as the historical v4 tree; v5 migrations belong under `map/tools/` and do not rewrite those legacy workflows. Legacy callers retain their `diminfo_` names; the shared v5 workflows use the `ROFFTAG`-derived pair.

## Planned workflow organization

The maintainer-approved organization follows the preferred map-preparation sequence. Numbering communicates a useful order for the standard workflow; it does not make every step mandatory or imply that every map uses every tool.

| Script | Purpose | Status |
| --- | --- | --- |
| `s01-channel_params.sh` | Annual mean discharge, power-law channel width/depth/roughness, and MERIT Hydro width merging. | Implemented |
| `s02-bifurcation.sh` | Standard non-MPI bifurcation parameters and basin-connectivity products. | Implemented |
| `s03-generate_inpmat.sh` | User-configured runoff-grid mapping matrix. | Implemented |
| `s04-mpi_map_setup.sh` | MPI basin aggregation, region assignment, and default-partition connectivity report. | Implemented |
| `s05-permanent_water.sh` | Permanent-water parameter generation. | Planned; not implemented here |
| `s06-write_ctl.sh` | GrADS CTL generation for existing registered base-map and high-resolution binaries. | Implemented |
| `t01-alloc_flow_gauge.sh`, `t02-alloc_dams.sh`, `t03-alloc_level_gauge.sh` | Gauge and dam location allocation. | Planned; not implemented here |

Independent diagnostics belong in `script_analysis/`. No `script_allocate/` directory is part of the approved design.

## Channel parameter workflow

[`s01-channel_params.sh`](s01-channel_params.sh) performs only these three operations:

1. `param_calc_outclm` computes annual mean discharge from the configured runoff climatology.
2. `param_calc_rivwth` derives power-law channel width, depth, roughness, and infinite-depth products.
3. `param_set_merit_width` combines the power-law width with the MERIT Hydro width layer.

It does not run bifurcation, MPI, or permanent-water processing. Configure the script's runoff path and coefficients before use. Its defaults retain the legacy standard recipe: binary runoff in mm/s, `inpmat` interpolation, `ROFFTAG=test-1deg`, and the height/width coefficients shown in the script. It derives `inpdim_test-1deg.txt` and `inpmat_test-1deg.bin`, verifies that record 7 of the metadata names the paired matrix, then reads both without regenerating them. `ROFFTAG` identifies runoff/input-grid configuration and is distinct from `HIRES_TAG`, which selects high-resolution map data during map generation.

The expected annual-mean runoff input is `../data/ELSE_GPCC_clm-1981-2010.one`, relative to a map root in the data-rich distribution package. Its inspected CTL describes a 360 x 180, one-record, little-endian grid and runoff in mm/s. The CTL variable comment says "daily runoff climatology," despite the single record and the channel workflow's annual-mean use; treat this as an existing metadata inconsistency. The package guide identifies the standard climatology as ELSE_GPCC (Kim et al., 2009). The source repository does not contain this dataset, and the available package documentation does not state redistribution terms; do not copy it into GitHub until permission and attribution are confirmed. A source checkout without the package data must provide a compatible runoff dataset and matching dimension/matrix metadata.

Build the shared executables, copy the script into the target map's `script_params/`, edit its configuration, then run it from any working directory. The script sets the map root as CWD, checks required programs and inputs, and reports a failing stage. It writes or replaces `outclm.bin`, `rivwth.bin`, `rivhgt.bin`, `rivman.bin`, `rivhgt_inf.bin`, and `rivwth_merit.bin` in the map root. Back up existing products if they must be retained. The legacy Fortran argument readers use 128-character formatted fields for the input descriptor and runoff path; the wrapper rejects longer configured values, and the default descriptor path is short and map-root-relative.

The standard channel workflow uses annual mean runoff. The historical `map/src/src_param/calc_outclmday.F90` remains legacy-only: it reflects the older maximum-30-day climatology approach associated with Yamazaki et al. (2011). The maintainer reports that later evaluation favored annual mean runoff. This migration does not revalidate the historical method scientifically.

`rivwth.bin` is the power-law width and `rivwth_merit.bin` is the separately named merged width product; the latter is not simply a copy of `width.bin`. The merge algorithm and thresholds are preserved from the legacy tool. The current core map reader selects `rivwth.bin` and does not automatically choose `rivwth_merit.bin`; this workflow produces the alternate product but does not change model-reader selection. See [`map_variables.csv`](../../map_variables.csv) and the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for product metadata and migration evidence.

## Standard bifurcation workflow

[`s02-bifurcation.sh`](s02-bifurcation.sh) is intended to follow `s01-channel_params.sh`, which produces the required channel-depth map `rivhgt.bin`. It generates ordinary bifurcation parameters and basin-connectivity products using map-root `nextxy.bin`, `basin.bin`, `bifori.txt`, `params.txt`, and `inpdim_<ROFFTAG>.txt` (default `inpdim_test-1deg.txt`). The first two dimensions in the metadata must match `params.txt`, and record 7 must name the paired `inpmat_<ROFFTAG>.bin`. `BIFLAYER` defaults to the legacy value `5`; the wrapper accepts 1–10. If `bifori.txt` contains fewer layers than requested, the Fortran program warns and clamps processing to the available count.

The workflow runs `param_set_bifparam` from the map root, then runs `param_set_bif_basin` from the map-local `script_params/` directory because its input/output paths use `../`. By default it writes or replaces `bifprm.txt`, `bifbsn.bin`, and `bifcol.bin`. `bifcol.bin` remains unconditional because it is routinely used to visualize the merged basin map. The display palette has colors 2–10 and falls back to color 10 if no candidate is free among neighboring basins; adjacent regions can consequently share a color, without changing simulation products. With `s02-bifurcation.sh check_map`, `param_set_bifparam` additionally writes `check_bif_dphwth.bin` with two float32 records (`bifdph` depth, then `bifwth` width), and `param_set_bif_basin` writes `check_bif_bsnmod.bin` with two int32 records (`bifmod`, then `bpoint`). These are inspection-only products, not model inputs. Without `check_map`, neither diagnostic is created or overwritten; a pre-existing check file can therefore reflect an earlier run. The v5 program no longer creates the legacy separate `bifmod.bin` and `bpoint.bin`; pre-existing files are left untouched. Older v4 `bifdph.bin` files are also left untouched. `bifprm.txt` is read by the model only when bifurcation processing is enabled.

This wrapper retains standard non-MPI behavior and the unsorted order written by `param_set_bifparam`. The v5 MPI workflow is separate and described below. Legacy Fortran calculations and `gosh/` workflows are unchanged. Regression tests compare standard outputs and the consolidated diagnostic records with the legacy products on temporary Mekong copies; this demonstrates compatibility for that case, not independent scientific validation.

## MPI map preparation

[`s04-mpi_map_setup.sh`](s04-mpi_map_setup.sh) reads the ordinary `bifprm.txt` produced by s02 and `basin.bin`; it never rewrites, reorders, or regenerates `bifprm.txt`. Build the shared programs with `make -C map/tools/code param analysis`, copy this script into the map's `script_params/`, and configure `MPIMODE`, `MPI_COUNTS`, and `DEFAULT_MPI` there. The defaults are `normal`, `4 8 16`, and `16`. The default count must appear in the requested list. The wrapper warns before replacing map outputs, creates all requested `mpireg-N.bin` maps, and copies the selected default to `mpireg.bin`.

MPI basin integration is distinct from standard s02 basin connectivity. It applies the legacy size thresholds: 6% of valid river cells in normal mode, 3% in `MaxMPI`, and the 0.1% small-basin exception. It first processes channel bifurcations (`wth(1)>0`), then all remaining overland/floodplain paths in reverse order, retaining the legacy repeated merge and size checks. Basin groups are assigned in ascending ID order to the currently least-loaded region; equal loads select the first region ID. The region map uses one int32 `NX x NY` record with `-9999` outside assigned river basins.

The legacy workflow sorted the bifurcation table by columns 15, 16, and 6. Those basin-ID columns move when the number of width layers changes. The v5 aggregation tool parses the standard row and sorts by the values `ibsn`, `jbsn`, and `elv`, with the source text row as a deterministic equal-key fallback matching the legacy sort's whole-line tie behavior. This order is part of the order-sensitive basin-merge algorithm. For the Mekong v4.30 sample, normal and `MaxMPI` basin groups and 4/8/16 maps match the legacy implementation byte-for-byte.

Only `mpireg-N.bin` and `mpireg.bin` are persistent products. `.bifbsn_mpi.tmp` is an intermediate basin-group raster removed by the wrapper, including on failure. The v5 workflow does not write legacy `bifprm_mpi.txt`, `bifprm-N.txt`, `bifmod_mpi.bin`, `bpoint_mpi.bin`, or `bifcol_mpi.bin`; the historical `map/src/` tools remain unchanged. After generation, s04 runs the independent analysis tool for `mpireg-$DEFAULT_MPI.bin`. See [`script_analysis/README.md`](../script_analysis/README.md) for the model-side retention criterion and optional CSV export.

Run the wrapper from any directory after copying it into the target map:

```sh
./script_params/s04-mpi_map_setup.sh
```

It requires `params.txt`, `basin.bin`, and the standard `bifprm.txt`; no package-only global map is required. The executable stages use the map-local `script_params/` working directory because their binary and metadata paths are relative. A successful map-generation run does not establish scientific validity of the partition or of a subsequent simulation.

## GrADS CTL generation

Copy [`s06-write_ctl.sh`](s06-write_ctl.sh) into a map-local `script_params/` directory and run it there. Like the reusable [`script_region/s02-write_ctl.sh`](../script_region/s02-write_ctl.sh), it calls the shared `generate_ctl.py` and scans only `.bin` files that already exist directly in the map root and in immediate child directories containing `location.txt`. The generator creates CTLs only for files registered in `map_variables.csv` whose dimensions and binary sizes validate against `params.txt` or `location.txt`.

The wrapper supports `--dry-run` and `--no-overwrite`; existing CTLs are replaced by default. It does not create, modify, or infer binary map data. See the [map-tools guide](../README.md#grads-ctl-files) for registry, missing-value, and format details.

## General-purpose runoff input matrix

[`s03-generate_inpmat.sh`](s03-generate_inpmat.sh) invokes `param_generate_inpmat` to create a matrix for a user-selected regular runoff grid. Its retained example uses 0.25-degree cells, longitude bounds -180 to 180 degrees, latitude bounds -60 to 90 degrees, and north-to-south row order. Edit these values to match the runoff data; they describe that input grid, not the CaMa-Flood map grid.

The target map must provide `params.txt`, a selected high-resolution directory with `location.txt`, and each area's `catmxy.bin`; `grdare.bin` is optional. The runoff values themselves are not read while constructing the matrix. Set `ROFFTAG` to identify the runoff grid; the wrapper derives `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin`. Set `HIRES_TAG` separately when the default high-resolution map selection is unsuitable. The metadata's seventh line is checked against the derived matrix name.

This configurable matrix is distinct from the standard `inpmat_test-1deg.bin` and `inpdim_test-1deg.txt` produced by `region_default_inpmat` and distributed with prepared v5 maps for the standard annual-runoff channel workflow. The configurable `inpmat` is expected to be needed by a future sample simulation, but the sample runoff/map dataset and GitHub-versus-package data selection remain open. Existing v4 maps may still carry `diminfo_<ROFFTAG>.txt`; those names are retained for legacy workflows. See the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for migration evidence and limitations.

The Python dam-parameter estimation workflow under legacy `map/src/src_dam/` is distinct from the future Fortran dam-location allocation recipe listed above.
