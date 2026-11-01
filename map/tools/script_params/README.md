# Map parameterization and preparation workflows

`script_params/` contains reusable map-local recipes for parameterization and related map preparation. The shell layer holds configuration, selects working directories, sequences commands, and reports progress; compiled programs in `map/tools/code/` perform scientific calculations, validate inputs, and report detailed errors. Build the required executables beforehand with the shared Makefile (for example, `make -C map/tools/code param analysis`); processing scripts do not compile or check them. The s01-s07 parameter scripts use self-contained ASCII `[STEP]` and `[SUCCESS]` banners plus a prominent `ERROR` banner on failure, using POSIX exit traps; no shared message helper is required. Some migrated programs still have incomplete file-open/read diagnostics, a code-layer follow-up. See the [shell-script convention](../../../AGENTS.md#shell-script-design-principles).

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
| `s05-permanent_water.sh` | Aggregate high-resolution permanent-water area by unit catchment. | Implemented |
| `s06-subbasin.sh` | Generate standard optional subbasin IDs and display colors using the configured upstream-area-increase threshold. | Implemented |
| `s07-upbasin.sh` | Use the largest valid allocated GRDC station for river lookup and upstream-mask generation. | Implemented |
| `s99-write_ctl.sh` | Final optional step: generate GrADS CTLs for existing registered base-map and high-resolution binaries. | Implemented |
| `t01-alloc_flow_gauge.sh`, `t02-alloc_dams.sh`, `t03-alloc_level_gauge.sh` | Flow-gauge, reservoir, and water-level-gauge mapping from MERIT Hydro to CaMa-Flood catchments. | Implemented |

Independent diagnostics belong in `script_analysis/`. No `script_allocate/` directory is part of the approved design.

## Channel parameter workflow

[`s01-channel_params.sh`](s01-channel_params.sh) performs only these three operations:

1. `param_channel_calc_outclm` computes annual mean discharge from the configured runoff climatology.
2. `param_channel_calc_rivwth` derives power-law channel width, depth, roughness, and infinite-depth products.
3. `param_channel_set_merit_width` combines the power-law width with the MERIT Hydro width layer.

It does not run bifurcation, MPI, or permanent-water processing. Configure the script's runoff path and coefficients before use. Its defaults retain the legacy standard recipe: binary runoff in mm/s, `inpmat` interpolation, `ROFFTAG=test-1deg`, and the height/width coefficients shown in the script. It reads the paired `inpdim_test-1deg.txt` and `inpmat_test-1deg.bin` without regenerating them. `ROFFTAG` identifies runoff/input-grid configuration and is distinct from `HIRES_TAG`, which selects high-resolution map data during map generation.

The expected annual-mean runoff input is `../data/ELSE_GPCC_clm-1981-2010.one`, relative to the map root. This 360 x 180, one-record little-endian binary is now included in the Git repository; its CTL specifies runoff in mm/s. The CTL variable comment says "daily runoff climatology," despite the single record and the channel workflow's annual-mean use; treat this as an existing metadata inconsistency. The package guide identifies ELSE_GPCC and cites Kim et al. (2009). Its inclusion in this repository was directed by the maintainer; upstream terms for other ELSE_GPCC products or derivatives have not been established.

Build the required shared executables beforehand, copy the script into the target map's `script_params/`, edit its configuration, then run it from any working directory. The script selects the map root as CWD and calls the programs in order. The programs read the configured inputs and perform the calculations. The workflow writes or replaces `outclm.bin`, `rivwth.bin`, `rivhgt.bin`, `rivman.bin`, `rivhgt_inf.bin`, and `rivwth_merit.bin` in the map root. Back up existing products if they must be retained.

The standard channel workflow uses annual mean runoff. The historical `map/src/src_param/calc_outclmday.F90` remains legacy-only: it reflects the older maximum-30-day climatology approach associated with Yamazaki et al. (2011). The maintainer reports that later evaluation favored annual mean runoff. This migration does not revalidate the historical method scientifically.

`rivwth.bin` is the power-law width and `rivwth_merit.bin` is the separately named merged width product; the latter is not simply a copy of `width.bin`. The merge algorithm and thresholds are preserved from the legacy tool. The current core map reader selects `rivwth.bin` and does not automatically choose `rivwth_merit.bin`; this workflow produces the alternate product but does not change model-reader selection. See [`map_variables.csv`](../../map_variables.csv) and the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for product metadata and migration evidence.

## Permanent-water area

[`s05-permanent_water.sh`](s05-permanent_water.sh) runs `param_water_calc_prmwat` to create `prmwat.bin` from a base map and selected high-resolution map. Set `HIRES_TAG` near the top of the copied script; it defaults explicitly to `1min` and is not inferred from available directories. Build the shared executable beforehand with `make -C map/tools/code param`. The script can be run from any current working directory and does not run the preceding parameterization steps.

The source layout must use the non-tiled `location.txt` format. Inputs are `params.txt`, `nextxy.bin`, and `ctmare.bin` in the map root, plus `<HIRES_TAG>/location.txt` and per-area `catmxy`, `rivwth`, and `flddif` binaries; `grdare.bin` is optional. The output is one float32 `NX x NY` record in m2. Non-network cells are `-9999`; network cells with no selected water pixels are zero. For valid catchment assignments, selected pixels satisfy `(rivwth > 0 OR rivwth == -1) AND flddif <= 5 m`; `rivwth == -1` denotes water surface outside the channel centerline. Their pixel areas are summed and capped by `ctmare`. The legacy `flddif <= 5` condition also accepts `flddif=-9999`; the migrated program warns and preserves that behavior rather than silently adding a new scientific filter.

The elevation cutoff limits `prmwat` to permanent water within the flood area CaMa-Flood can reproduce. A permanent lake isolated high on a hill is outside the model's expected inundation domain, so its area must not be subtracted as pre-existing simulated floodwater. The maintainer described the intended criterion as `flddif < 5 m`. The legacy v4 and migrated v5 source use `flddif <= 5 m`, which includes pixels exactly at 5 m; this boundary difference is documented but has not been changed. The numerical derivation of the 5 m value remains unverified.

`prmwat.bin` is an aggregated estimate of permanent-water surface area per unit catchment for post-processing, such as excluding pre-existing water from newly inundated area. It is not a pixel mask and is not read by the current simulation core. If `grdare.bin` is absent, the legacy scaled `rgetarea` fallback is used; this computes an area from a rectangle ten times larger in both directions and divides by 100 to reduce round-off fluctuations at fine resolution. The method is preserved, but its accuracy has not been independently quantified. See [`map_metadata_spec.md`](../../map_metadata_spec.md#permanent-water-area) and [`map_variables.csv`](../../map_variables.csv) for product metadata.

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

The Python dam-parameter estimation workflow under legacy `map/src/src_dam/` is separate from the Fortran dam-location allocation workflows listed below.

## Allocation workflows

The three optional wrappers use the shared `allocate_*` executables in `map/tools/code/`. Build them beforehand with `make -C map/tools/code allocate`. Copy the wrappers into a map-local `script_params/` directory, configure the input path and `HIRES_TAG`, then run from any directory. Each allocation program intentionally requires exactly one high-resolution river-network area; the shared reader reports an error for multi-area layouts. This preserves the single-network topology/catchment matching contract, not a universal limit on map resolution. `HIRES_TAG` is explicit and does not fall back between `1min` and `15sec`.

The v4.30 distribution package includes a single-area global `glb_15min/1min` layer and a single-area Japan `tej_01min/15sec` layer, which the maintainer identifies as the intended CaMa-Flood allocation inputs. These are distinct from the global 30-arcsec MERIT Hydro and Japan 5-arcsec J-FlwDir maps used for the first-stage allocation described by the AllocRiverGauge manual. See the [area-handling rationale and validation plan](../../../docs/map_data_tools_plan.md#high-resolution-area-handling-contract).

The input tables are expected to have already been matched and reviewed on the high-resolution MERIT Hydro network using [AllocRiverGauge](../../../doc/Manual_AllocateRiverGauges.docx). CaMa-Flood performs the second-stage mapping from those allocated coordinates and drainage areas to its own unit catchments. This consistency rationale does not establish that each individual station or dam is correct. Inspect the outputs before using them in analysis.

| Wrapper | Default input | Required map files | Outputs in the map root |
| --- | --- | --- | --- |
| [`t01-alloc_flow_gauge.sh`](t01-alloc_flow_gauge.sh) | `../../data/GRDC_allocated.csv` | `params.txt`, `uparea.bin`, `nextxy.bin`; `<HIRES_TAG>/location.txt`, `.uparea.bin`, `.catmxy.bin`, `.downxy.bin` | `GRDC_alloc.txt` |
| [`t02-alloc_dams.sh`](t02-alloc_dams.sh) | `../../data/GRanD_allocated.csv` | Flow-gauge files plus `ctmare.bin` | `GRanD_river.txt`, `GRanD_small.txt`, `GRanD_error.txt` |
| [`t03-alloc_level_gauge.sh`](t03-alloc_level_gauge.sh) | `../../data/MLIT_LevelGauge_allocated.csv` | Flow-gauge files plus `elevtn.bin` and `<HIRES_TAG>/<HIRES_TAG>.elevtn.bin` | `MLIT_levelgauge.txt` |

The Fortran readers skip one header row and read fields positionally; they do not implement general CSV quoting. Expected leading columns are `ID,lat_alloc,lon_alloc,area_alloc` for gauges and `ID,lat_alloc,lon_alloc,area_alloc,DamName,RiverName,CAP_MCM,YEAR` for GRanD. Coordinates are degrees and allocated drainage areas are km2. GRanD capacity is MCM and the year is passed through as an integer.

Output tables are whitespace-delimited fixed-width text, not CSV. They retain the legacy schemas and meanings:

- **Flow gauges (`GRDC_alloc.txt`):** `ID`, `lat`, `lon`, `area_Input`, `area_CaMa`, `error`, `diff`, `Type`, `ix1`, `iy1`, `ix2`, `iy2`, `area1`, `area2`. Coordinates are degrees; areas and `diff` are km2; `error` is the signed dimensionless relative area difference; indices are one-based CaMa-Flood grid indices. `area_CaMa` is the selected aggregate upstream area; `area1`/`area2` are the primary and optional secondary substitute areas. `Type` is the number of upstream substitute catchments (0–2). Unallocated rows use `Type=-9`, indices `-999`, and CaMa area/error/difference/substitute-area fields `-999`. Out-of-domain rows are omitted.
- **Dams (`GRanD_river.txt`, `GRanD_small.txt`, `GRanD_error.txt`):** `ID`, `lat`, `lon`, `area_CaMa`, `area_Input`, `error`, `diff`, `ix`, `iy`, `cap_mcm`, `year`, `damname`, `rivname`. Coordinates are degrees, areas and `diff` are km2, `error` is dimensionless, `cap_mcm` is MCM, and indices are one-based. `area_CaMa` is the selected upstream area; `diff=area_CaMa-area_Input`. The `small` class uses the legacy catchment-area criterion; it is not a verified physical mainstem/tributary class. Its sentinel is `-888` (`error=-8`). The error file uses `-999` for unmapped area/index/error fields. Out-of-domain rows and rows with negative capacity are skipped, matching v4.
- **Level gauges (`MLIT_levelgauge.txt`):** `ID`, `lat`, `lon`, `area`, `Type`, `ix1`, `iy1`, `ix2`, `iy2`, `elv_outlet`, `elv_gauge`, `elv_upst`, `dst_outlet`, `dst_upst`, `upa_outlet`. Coordinates are degrees, `area` and `upa_outlet` are km2, elevations are metres, distances along the river network are km, and indices are one-based. `ix1/iy1` identify the assigned catchment; `ix2/iy2` identify its selected upstream reference catchment where applicable. The elevation fields are CaMa outlet, high-resolution gauge, and upstream catchment-outlet elevations; `dst_outlet` and `dst_upst` are the corresponding along-network distances. `Type=1/2/3` denotes the legacy mainstem/tributary/small-stream path classes; `Type=-9` is unallocated. Missing index, elevation, distance, and outlet-area values use `-999` in unmatched rows; upstream-reference fields are also `-999` for Type 3.

For all three products, row order, formatting precision, classifications, and sentinels preserve legacy behavior. The dam input capacity and year columns pass through into the corresponding output fields.

Run the migration regression with:

```sh
python3 -B -m unittest map.tools.tests.test_allocation_regression -v
```

It compiles v4 and v5 programs in temporary directories, compares all six output products to the committed legacy snapshots, checks classifications and high-resolution selection, rejects a multi-area layout, and verifies that the Mekong inputs were not modified. Regression demonstrates parity for this sample/compiler, not independent scientific validation. Threshold rationale remains open; see the [allocation migration plan](../../../docs/map_allocation_migration_plan.md#8-knowledge-gaps-and-maintainer-questions).

## High-resolution areas in aggregation workflows

The general parameter input-matrix generator (`s03-generate_inpmat.sh`) and permanent-water aggregator (`s05-permanent_water.sh`) read all rows in the selected `HIRES_TAG/location.txt` and accumulate across areas. This multi-area behavior is distinct from the intentional single-area allocation contract above. Source inspection confirms the loops; the Mekong sample and current focused fixtures do not validate full-package multi-area behavior. The [map-tools plan](../../../docs/map_data_tools_plan.md#high-resolution-area-handling-contract) gives the maintainer-operated large-data integration procedure.

## River-network analysis

The shell layer sets locations and parameters; `analysis_river_point_info`, `analysis_upbasin`, `analysis_subbasin`, and other programs in `map/tools/code/` read and validate map data and perform the network calculations. Build them beforehand with `make -C map/tools/code analysis`. The standard s06/s07 workflows invoke the shared executables directly and do not depend on the optional copied binaries in `script_analysis/`.

[`s06-subbasin.sh`](s06-subbasin.sh) creates or replaces optional `subbsn.bin` and `subcol.bin` products in the map root. Its configurable example is `AREA_THRESHOLD_KM2=50000`; this is not a calibrated universal threshold. Positive subbasin IDs identify segments, zero marks modeled network cells left below the threshold, and -9999 marks cells outside the modeled river network. In the preserved segmentation rule, a boundary is cut when the current upstream area exceeds the threshold and the valid downstream cell's upstream area exceeds the current value by more than the threshold. This is not a minimum catchment-area filter. The threshold is in km2. The Mekong sample uses a separate 30,000 km2 setting and renames both outputs to `subbsn-30000.bin` and `subcol-30000.bin` so the sample filenames record their threshold.

[`s07-upbasin.sh`](s07-upbasin.sh) requires map-local `GRDC_alloc.txt` (normally produced by `t01-alloc_flow_gauge.sh`). It filters unallocated/invalid records, chooses the largest positive `area_CaMa` (first file row wins a tie), and uses that row's primary `ix1,iy1` cell. It writes `grdc_station_selection.txt`, `grdc_river_info.txt`, and `upbasin-<GRDC_ID>.bin`; repeated execution overwrites the selected station's same-named mask and reports. The Mekong sample is `upbasin-2569002.bin`. The primary-cell upstream mask is not guaranteed to represent the complete aggregate drainage area of a gauge mapped to multiple catchments, so do not treat its cell count/area as equal to `area_CaMa` or observed drainage area.

These are independent map-preparation recipes; neither requires the other. They create optional diagnostics, not simulation inputs. Afterward, `s99-write_ctl.sh` can create CTLs for registered products. See [`script_analysis/README.md`](../script_analysis/README.md) for arbitrary-cell analysis that keeps exploratory outputs separate from standard names. User-selected filenames need an additional registry entry to use automatic CTL generation.
