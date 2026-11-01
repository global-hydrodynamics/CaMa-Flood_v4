# Map parameterization and preparation workflows

`script_params/` contains reusable map-local recipes for parameterization and related map preparation. The shell layer holds configuration, selects working directories, sequences commands, and reports progress; compiled programs in `map/tools/code/` perform scientific calculations, validate inputs, and report detailed errors. Build the required executables beforehand with the shared Makefile (for example, `make -C map/tools/code param analysis`); processing scripts do not compile or check them. Scripts s01-s04 use self-contained ASCII `[STEP]` and `[SUCCESS]` banners plus a prominent `ERROR` banner on failure, using POSIX exit traps; no shared message helper is required. Some migrated programs still have incomplete file-open/read diagnostics, a code-layer follow-up. See the [shell-script convention](../../../AGENTS.md#shell-script-design-principles).

```sh
cd "$MAP_DIR"
"$EXEC_OUTCLM" "$TYPE" "$INTERP" "$INPDIM" "$CROFBIN"
"$EXEC_RIVWTH" "$TYPE" "$INPDIM" "$HC" "$HP" "$HO" "$HMIN" \
    "$WC" "$WP" "$WO" "$WMIN"
"$EXEC_MERIT" "$INPDIM"
```

`map/src/` is preserved as the historical v4 tree; v5 migrations belong under `map/tools/` and do not rewrite those legacy workflows. Legacy callers retain their `diminfo_` names; the shared v5 workflows use the `ROFFTAG`-derived pair.

## Planned workflow organization

The maintainer-approved organization follows the preferred map-preparation sequence. Numbering communicates a useful order for the standard workflow; it does not make every step mandatory or imply that every map uses every tool.

| Script | Purpose | Status |
| --- | --- | --- |
| `s01-channel_params.sh` | Annual mean discharge, power-law channel width/depth/roughness, and MERIT Hydro width merging. | Implemented |
| `s02-bifurcation.sh` | Standard non-MPI bifurcation parameters and basin-connectivity products. | Implemented |
| `s03-generate_inpmat.sh` | User-configured runoff-grid mapping matrix. | Implemented |
| `s04-mpi_map_setup.sh` | MPI basin aggregation and coloring, region assignment, and default-N connectivity report; optional pre-partition basin diagnostic with `check_map`. | Implemented |
| `s05-permanent_water.sh` | Permanent-water parameter generation. | Planned; not implemented here |
| `s99-write_ctl.sh` | Final optional step: generate GrADS CTLs for existing registered base-map and high-resolution binaries. | Implemented |
| `t01-alloc_flow_gauge.sh`, `t02-alloc_dams.sh`, `t03-alloc_level_gauge.sh` | Gauge and dam location allocation. | Planned; not implemented here |

Independent diagnostics belong in `script_analysis/`. No `script_allocate/` directory is part of the approved design.

## Channel parameter workflow

[`s01-channel_params.sh`](s01-channel_params.sh) performs only these three operations:

1. `param_channel_calc_outclm` computes annual mean discharge from the configured runoff climatology.
2. `param_channel_calc_rivwth` derives power-law channel width, depth, roughness, and infinite-depth products.
3. `param_channel_set_merit_width` combines the power-law width with the MERIT Hydro width layer.

It does not run bifurcation, MPI, or permanent-water processing. Configure the script's runoff path and coefficients before use. Its defaults retain the legacy standard recipe: binary runoff in mm/s, `inpmat` interpolation, `ROFFTAG=test-1deg`, and the height/width coefficients shown in the script. It reads the paired `inpdim_test-1deg.txt` and `inpmat_test-1deg.bin` without regenerating them. `ROFFTAG` identifies runoff/input-grid configuration and is distinct from `HIRES_TAG`, which selects high-resolution map data during map generation.

The expected annual-mean runoff input is `../data/ELSE_GPCC_clm-1981-2010.one`, relative to a map root in the data-rich distribution package. Its inspected CTL describes a 360 x 180, one-record, little-endian grid and runoff in mm/s. The CTL variable comment says "daily runoff climatology," despite the single record and the channel workflow's annual-mean use; treat this as an existing metadata inconsistency. The package guide identifies the standard climatology as ELSE_GPCC (Kim et al., 2009). The source repository does not contain this dataset, and the available package documentation does not state redistribution terms; do not copy it into GitHub until permission and attribution are confirmed. A source checkout without the package data must provide a compatible runoff dataset and matching dimension/matrix metadata.

Build the required shared executables beforehand, copy the script into the target map's `script_params/`, edit its configuration, then run it from any working directory. The script selects the map root as CWD and calls the programs in order. The programs read the configured inputs and perform the calculations. The workflow writes or replaces `outclm.bin`, `rivwth.bin`, `rivhgt.bin`, `rivman.bin`, `rivhgt_inf.bin`, and `rivwth_merit.bin` in the map root. Back up existing products if they must be retained.

The standard channel workflow uses annual mean runoff. The historical `map/src/src_param/calc_outclmday.F90` remains legacy-only: it reflects the older maximum-30-day climatology approach associated with Yamazaki et al. (2011). The maintainer reports that later evaluation favored annual mean runoff. This migration does not revalidate the historical method scientifically.

`rivwth.bin` is the power-law width and `rivwth_merit.bin` is the separately named merged width product; the latter is not simply a copy of `width.bin`. The merge algorithm and thresholds are preserved from the legacy tool. The current core map reader selects `rivwth.bin` and does not automatically choose `rivwth_merit.bin`; this workflow produces the alternate product but does not change model-reader selection. See [`map_variables.csv`](../../map_variables.csv) and the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for product metadata and migration evidence.

## Standard bifurcation workflow

The compiled `param_bif_set_params` validates `BIFLAYER` as an integer from 1 through 10; invalid values are rejected there rather than prechecked in the shell wrapper.

[`s02-bifurcation.sh`](s02-bifurcation.sh) is intended to follow `s01-channel_params.sh`, which produces the required channel-depth map `rivhgt.bin`. It generates ordinary bifurcation parameters and basin-connectivity products using map-root `nextxy.bin`, `basin.bin`, `bifori.txt`, `params.txt`, and `inpdim_<ROFFTAG>.txt` (default `inpdim_test-1deg.txt`). `BIFLAYER` defaults to the legacy value `5`. If `bifori.txt` contains fewer layers than requested, the Fortran program warns and clamps processing to the available count.

The workflow runs `param_bif_set_params` from the map root, then runs `param_bif_set_basin` from the map-local `script_params/` directory because its input/output paths use `../`. By default it writes or replaces `bifprm.txt`, `bifbsn.bin`, and `bifcol.bin`. `bifcol.bin` remains unconditional because it is routinely used to visualize the merged basin map. The display palette has colors 2–10 and falls back to color 10 if no candidate is free among neighboring basins; adjacent regions can consequently share a color, without changing simulation products. With `s02-bifurcation.sh check_map`, `param_bif_set_params` additionally writes `check_bif_dphwth.bin` with two float32 records (`bifdph` depth, then `bifwth` width), and `param_bif_set_basin` writes `check_bif_bsnmod.bin` with two int32 records (`bifmod`, then `bpoint`). These are inspection-only products, not model inputs. Without `check_map`, neither diagnostic is created or overwritten; a pre-existing check file can therefore reflect an earlier run. The v5 program no longer creates the legacy separate `bifmod.bin` and `bpoint.bin`; pre-existing files are left untouched. Older v4 `bifdph.bin` files are also left untouched. `bifprm.txt` is read by the model only when bifurcation processing is enabled.

This wrapper retains standard non-MPI behavior and the unsorted order written by `param_bif_set_params`. The v5 MPI workflow is separate and described below. Legacy Fortran calculations and `gosh/` workflows are unchanged. Regression tests compare standard outputs and the consolidated diagnostic records with the legacy products on temporary Mekong copies; this demonstrates compatibility for that case, not independent scientific validation.

## MPI map preparation

[`s04-mpi_map_setup.sh`](s04-mpi_map_setup.sh) reads the ordinary `bifprm.txt` produced by s02 and `basin.bin`; it never rewrites, reorders, or regenerates `bifprm.txt`. Build the required shared programs beforehand with `make -C map/tools/code param analysis`, copy this script into the map's `script_params/`, and configure `MPIMODE`, `MPI_COUNTS`, and `DEFAULT_MPI` there. The defaults are `normal`, `4 8 16`, and `16`; choose `DEFAULT_MPI` from the requested counts. Basin aggregation always writes `bifcol_mpi.bin` from the aggregated basin IDs, before any MPI regions are partitioned. This display product is independent of process count N, but may differ by `MPIMODE`. The script then creates only the requested `mpireg-N.bin` maps and runs `analysis_check_mpi_bifurcation` on `mpireg-$DEFAULT_MPI.bin`; it creates no unnumbered `mpireg.bin` alias. The distinct `analysis_mpi_region_color` utility can be run manually, for example `../tools/code/analysis_mpi_region_color mpireg-8.bin` from a map root, to produce optional `bsncol_mpi-8.bin` for visualization of that computational region map. s04 does not run this utility or create `bsncol_mpi-N.bin`. Pass `check_map` to also write `check_bif_bsnmod_mpi.bin` during basin aggregation.

MPI basin integration is distinct from standard s02 basin connectivity. It applies the legacy size thresholds: 6% of valid river cells in normal mode, 3% in `MaxMPI`, and the 0.1% small-basin exception. It first processes channel bifurcations (`wth(1)>0`), then all remaining overland/floodplain paths in reverse order, retaining the legacy repeated merge and size checks. Basin groups are assigned in ascending ID order to the currently least-loaded region; equal loads select the first region ID. The region map uses one int32 `NX x NY` record with `-9999` outside assigned river basins. The optional `check_bif_bsnmod_mpi.bin` diagnostic is produced before region assignment and has two direct-access int32 `NX x NY` records: `bifmod_mpi` (record 1: 0 unchanged, 1 absorbed, 2 expanded; -9999 not applicable) and `bpoint_mpi` (record 2: 1 connection remains between aggregated basins, 2 merged floodplain path, 3 merged river-channel path; -9999 not applicable). It compares original `basin.bin` with the aggregated basin result; its classifications may vary between `normal` and `MaxMPI`, but not with the requested MPI partition count. It is not required for model execution.

The legacy workflow sorted the bifurcation table by columns 15, 16, and 6. Those basin-ID columns move when the number of width layers changes. The v5 aggregation tool parses the standard row and sorts by the values `ibsn`, `jbsn`, and `elv`, with the source text row as a deterministic equal-key fallback matching the legacy sort's whole-line tie behavior. This order is part of the order-sensitive basin-merge algorithm. For the Mekong v4.30 sample, normal and `MaxMPI` basin groups and 4/8/16 maps match the legacy implementation byte-for-byte.

The persistent s04 products are `bifcol_mpi.bin`, generated from the MPI-aggregated basin IDs, and `mpireg-N.bin` for every requested partition count. `bifcol_mpi.bin` is visualization-only, independent of N, and may vary with `MPIMODE`; `check_bif_bsnmod_mpi.bin` is created only when explicitly requested. The separate optional `analysis_mpi_region_color` utility colors MPI region IDs into `bsncol_mpi-N.bin` when run manually; this product depends on N and is not produced by s04. The color raster does not encode rank assignment. `bifbsn_mpi.tmp` is a visible intermediate basin-group raster removed by the wrapper, including on failure. The v5 workflow does not write the legacy MPI products `bifprm_mpi.txt`, `bifprm-N.txt`, `bifmod_mpi.bin`, or `bpoint_mpi.bin`, or the standard non-MPI `bifcol.bin`. The historical `map/src/` tools remain unchanged. The combined MPI basin diagnostic describes connectivity changes before MPI region assignment. The separate `analysis_check_mpi_bifurcation` report examines retained or excluded pathways for a selected `mpireg-N.bin` and therefore depends on the chosen partition. See [`script_analysis/README.md`](../script_analysis/README.md) for the model-side retention criterion and optional CSV export.

Run the wrapper from any directory after copying it into the target map:

```sh
./script_params/s04-mpi_map_setup.sh
./script_params/s04-mpi_map_setup.sh check_map
```

The programs read standard map files `params.txt`, `basin.bin`, `bifprm.txt`, and `nextxy.bin`; no package-only global map is required. Basin aggregation and region assignment run from map-local `script_params/`, while the connectivity analysis runs from the map root. A successful map-generation run does not establish scientific validity of the partition or of a subsequent simulation.

## GrADS CTL generation

`s99-write_ctl.sh` is numbered last because it describes products after map preparation; it does not create or modify map binaries.

Copy [`s99-write_ctl.sh`](s99-write_ctl.sh) into a map-local `script_params/` directory and run it there. Like the reusable [`script_region/s02-write_ctl.sh`](../script_region/s02-write_ctl.sh), it calls the shared `generate_ctl.py` and scans only `.bin` files that already exist directly in the map root and in immediate child directories containing `location.txt`. The generator creates CTLs only for files registered in `map_variables.csv` whose dimensions and binary sizes validate against `params.txt` or `location.txt`.

The wrapper supports `--dry-run` and `--no-overwrite`; existing CTLs are replaced by default. It does not create, modify, or infer binary map data. For the optional MPI basin diagnostic, it creates `check_bif_bsnmod_mpi.ctl` only when `check_bif_bsnmod_mpi.bin` exists; the registry defines its two variables in record order. See the [map-tools guide](../README.md#grads-ctl-files) for registry, missing-value, and format details.

## General-purpose runoff input matrix

[`s03-generate_inpmat.sh`](s03-generate_inpmat.sh) invokes `param_inpmat_generate` to create a matrix for a user-selected regular runoff grid. Its retained example uses 0.25-degree cells, longitude bounds -180 to 180 degrees, latitude bounds -60 to 90 degrees, and north-to-south row order. Edit these values to match the runoff data; they describe that input grid, not the CaMa-Flood map grid.

The target map must provide `params.txt`, a selected high-resolution directory with `location.txt`, and each area's `catmxy.bin`; `grdare.bin` is optional. The runoff values themselves are not read while constructing the matrix. Set `ROFFTAG` to identify the runoff grid; the wrapper derives `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin`. Set `HIRES_TAG` separately when the default high-resolution map selection is unsuitable.

This configurable matrix is distinct from the standard `inpmat_test-1deg.bin` and `inpdim_test-1deg.txt` produced by `region_default_inpmat` and distributed with prepared v5 maps for the standard annual-runoff channel workflow. The configurable `inpmat` is expected to be needed by a future sample simulation, but the sample runoff/map dataset and GitHub-versus-package data selection remain open. Existing v4 maps may still carry `diminfo_<ROFFTAG>.txt`; those names are retained for legacy workflows. See the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for migration evidence and limitations.

The Python dam-parameter estimation workflow under legacy `map/src/src_dam/` is distinct from the future Fortran dam-location allocation recipe listed above.
