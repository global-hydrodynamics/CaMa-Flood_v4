# Map data/tools modernization stage

## Current boundary and sequence

This project is **in progress**; the end date and final stage commit are not yet known.

- **Project registration/start:** `a91cec8fa73d74fc96966214bbe02f2de1fa643a`, `2026-10-27T11:01:42+09:00`, parent is the documentation merge `11d3fcd`.
- **Current audit snapshot:** local commit `39dc3df7c092bdbd1825cd9e3063d0f669dbe871`, `2026-10-28T15:47:43+09:00`. At task start this commit was one commit ahead of the remote branch; it is not a completion marker.
- **Coordination:** open [Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122); no PR/merge boundary exists yet. The Issue body was last updated before later Oct. 28 commits and does not represent all current progress.
- The branch contains the stage work and the first cross-project research framework. Research logging itself is not owned by this project; it is included here only because this is the current branch where the maintainer authorized these updates.

The visible commit sequence is `a91cec8` (register project), `04b15ba` (status clarification), `7e699d7` (map inventory), `6989ab6` (Python setup), `51d56a5` and `33a3b16` (Mekong sample preparation and complete tracking), `3db9873` (research framework), and `39dc3df` (cross-project research scope clarification). Commit authors are recorded as Dai Yamazaki; Git author metadata does not establish which parts were written by an AI agent.

## Objectives and implementation visible in Git

The project plan aims to improve portability and maintenance of map metadata and map-processing utilities while retaining existing binary and Fortran workflows. Changes through this snapshot include:

- `docs/map_data_inventory.md`, `map/map_variables.csv`, `map/map_metadata_spec.md`, and updated map guidance describing formats, source/reader relationships, and a proposed metadata basis.
- `map/src/map_data_tools/` with category-11 GrADS CTL generation and binary-to-NetCDF tools plus synthetic tests and a Fortran reader-contract smoke test. The implementation is explicitly limited; it does not yet generate all module-level `CRIVCLINC`/`CRIVPARNC` inputs or cover optional schemes.
- Root `requirements.txt` and `s00-python_setup.sh` for a local Python environment.
- Git-tracked development/test `map/mekong_15min/`, including map products, metadata, CTLs, bifurcation/input-matrix files, and `1min/` layers; a GitHub Actions workflow checks the committed sample.
- An initial cross-project `research/` framework. Later maintainer instruction explicitly separated research logging from this map project.

The Git diff from `origin/master` to the current snapshot covers 204 changed paths, with 5,845 insertions and 24 deletions; it includes the binary sample data. The diff also contains tracked Python `__pycache__` bytecode under `map/src/map_data_tools/`. This is a verified repository artifact and a maintenance issue, not corrected by this history-reconstruction task.

## Validation evidence from this audit

These are **newly rerun checks on the current source snapshot**, not historical test results from the original commits. Source reference was `39dc3df`; the worktree had only research-record edits during validation.

- `python3 -B map/tests/validate_mekong_sample.py`: 5 tests passed, checking dimensions, binary sizes/counts, network indices, CTL targets, local-path independence, and high-resolution layers.
- `python3 -B -m unittest discover -s map/src/map_data_tools/tests -p 'test_*.py' -v`: 6 tests passed. This included synthetic endian/shape and CTL cases, binary-to-NetCDF checks, and the Fortran NetCDF reader-contract smoke test.
- Environment observed: Darwin 23.3.0 arm64; Python 3.13.3; GNU Fortran 14.2.0; NetCDF 4.6.2. See [`../baseline/environment_inventory.md`](../baseline/environment_inventory.md).
- No full CaMa-Flood simulation was run. Passing sample and converter tests do not establish numerical model equivalence or optional-scheme NetCDF compatibility.

The sample-validation workflow exists in `.github/workflows/map-sample-validation.yml`; GitHub check/status queries for the inspected historical/current refs returned no checks. The workflow's presence is not evidence that it ran successfully on those commits.

## Knowledge gaps, interventions, and next boundaries

Maintainer-confirmed map provenance and unavailable generation workflows are recorded in [`KG-0002`](../records/knowledge_gaps/KG-0002.md) and [`KG-0003`](../records/knowledge_gaps/KG-0003.md), with technical details left in the map inventory/specification. The stage history contains no persistent, timestamped ChatGPT source from which to reconstruct exact alternatives or interventions. No human-intervention event is asserted from commit messages alone; the maintainer can later provide reviewed summaries using the queue document.

The stage remains incomplete. The next work includes the ECMWF-equivalent module-level NetCDF generation contract/code, optional-scheme map support decisions, broader validation, and the project's accepted completion boundary. Do not create `research-stage-map-tools` until the maintainer confirms that boundary.

## Phase B1: parallel v5 regional tools (2026-10-30)

The maintainer authorized a parallel `map/tools/` framework on the existing project branch. The implementation adds independent copies of the five regional workflow Fortran programs, a map-local Makefile and `s01-regional_map.sh` template, and user manuals. The source copies match the current `map/src/src_region/` files; the new workflow omits CTL generation for Phase C. The v4 source and its CTL workflow were not edited. The parameter workflow remains for Phase B2.

GNU Fortran 14.2 compiled all five programs from a temporary repository-shaped copy; `combine_hires` emitted existing integer conversion warnings. Shell syntax, relative paths, and source-copy identity passed. The maintainer has not run the regional workflow, so outputs and scientific behavior remain unverified. Intel `ifort` could not be validated in this environment: the active GNU include supplies incompatible options and the local compiler could not discover the macOS SDK. See [`VR-0005`](../records/validation_runs/VR-0005.md).

Two constraints remain open. First, `doc/make_package.sh` recursively copies the source tree, so it would include `map/tools/`; the phase instruction also prohibits package-script edits. Second, during validation, 56 tracked files under `map/mekong_15min/` remained absent on disk. An untracked map-local `script_region/` with files matching the template was present and preserved. The cause of the missing tracked files is unknown; no restoration or regeneration was performed. Neither finding changes the scope of the Phase B1 implementation.


### Phase B1 build and documentation refinement (2026-10-30)

The maintainer selected one shared `map/tools/code/Makefile` and `region_` / `param_` source and executable prefixes. An intermediate implementation placed executables in each map-local script directory; that placement was superseded by the final decision below. The Mekong overview describes its directory as a development/test sample and no longer relies on a map-local README. Earlier build revisions are recorded in [`VR-0006`](../records/validation_runs/VR-0006.md) and [`VR-0007`](../records/validation_runs/VR-0007.md).

The final architecture decision is to build shared executables and compiler intermediates directly in `map/tools/code/`. All map datasets use those executables. Build explicitly with `make -C map/tools/code region` or `params`; map-local scripts do not compile, and they call the shared executables by relative path while preserving their own working directory for the programs' relative data paths. The details and checks are recorded in [`VR-0008`](../records/validation_runs/VR-0008.md).


### Phase C: generalized v5 CTL generation (2026-10-30)

The v5 Python generator in `map/tools/code/generate_ctl.py` scans one selected directory at a time, uses `map_variables.csv` for variable, record, and missing-value metadata, and selects grid geometry from `params.txt` or the selected high-resolution directory's `location.txt`. It follows the legacy `yrev little_endian` CTL convention and checks exact file sizes. No geometry was added to the common CSV, and no schema extension was required for currently supported record layouts. The Mekong sample was inspected read-only; the generator was not run against it. Eight synthetic tests passed. The existing v4 map-data-tools suite currently errors because the maintainer-edited category 11 includes `nextxy_noedge.bin`, while the legacy parser expects exactly six core files; v4 code and the CSV were preserved. See [`VR-0009`](../records/validation_runs/VR-0009.md).

The maintainer then manually ran `s02-write_ctl.sh` on Mekong and reported 18 base CTLs and 12 high-resolution CTLs written, with the initial input-matrix/width failures and `lsmask` skip. The input matrix uses the shared runoff-variant suffix in `inpmat_<TAG>.bin` and `diminfo_<TAG>.txt`; the current Fortran workflow writes `diminfo_test-1deg.txt`. The generator now reads that descriptor for input-matrix dimensions and records, the registry declares `-9999` for `lsmask`, unknown dimensions are skipped, and the wrapper overwrites existing CTLs by default. Ten synthetic tests pass after these changes. A read-only audit confirmed that the actual descriptor's `80x120` map and `INPN=4` match the 460800-byte binary. The real Mekong wrapper has not been rerun after the fixes, so the final real-map output set remains unverified. See [`VR-0010`](../records/validation_runs/VR-0010.md).

The maintainer clarified that `lonlat.bin` stores unit-catchment outlet coordinates. Its inventory entry is now represented by separate `outlon` and `outlat` rows with record-specific descriptions and units. The generator resolves both names from the registry; 12 synthetic tests pass. The current committed Mekong `lonlat.ctl` uses the corrected `outlon` / `outlat` labels. VR-0011 itself did not run the wrapper; the current CTL file state is a separate repository observation. See [`VR-0011`](../records/validation_runs/VR-0011.md).


### Regionalization documentation audit (2026-10-30)

A read-only audit compared the v5 regional scripts and Fortran outputs with the tracked Mekong sample and `map_variables.csv`. All 33 tracked Mekong binary files (base and high-resolution) match registry entries. The shared registry's provisional `workflow_stage` field was normalized: the unsupported value `default / src_region` was cleared because provenance belongs in `generation_or_processing`; the one confirmed `src_region` annotation remains. Documentation now distinguishes v4 and v5 workflows, identifies `nextxy_noedge.bin` as a parent-map output, and explains the processing stages and the separate sample-runoff matrix. Several high-resolution layer semantics remain explicitly provisional.

The 12 synthetic CTL tests, shell syntax checks, relative Markdown-link scan, and research record validator passed. The legacy `map/tests/validate_mekong_sample.py` was also tried read-only and failed because it expects the fuller package/runtime inventory (including `script_params` products and metadata) that is absent from this regionalization-stage sample. It was not changed. No map generation or binary modification was performed. See [`VR-0012`](../records/validation_runs/VR-0012.md).

### High-resolution layer metadata clarification (2026-10-30)

The maintainer supplied scientific definitions for `catmzz`, `catmz100`, `visual`, and `flwdir`, and authorized inspection of the private FLOW `src_conv` producer source. The inventory describes the 10-band and 100-level floodplain-profile ranks, including the distance perturbation used to distinguish equal elevations; FLOW's visualization classes include code 7 for small streams; and its D8 direction code 8 is NW. The initial sentinel discrepancy recorded in [`VR-0013`](../records/validation_runs/VR-0013.md) was resolved by tracing both `catmxy` and profile-layer code: `catmxy` uses -9999 for sea and -999 for unassigned unit catchment, while raw `catmzz`/`catmz100` initialize to -99 for sea and -9 for unassigned catchments. FLOW `merge_hires` collapses both non-catchment cases to -9; v5 `region_combine_hires` copies available profile values unchanged and uses -9 when an input layer is unavailable. The Mekong sample confirms this transformation: every sampled `catmxy=-9999` and `catmxy=-999` pixel has profile value -9. FLOW source maps direction code 8 to NW; its visualization classes include 7 for small streams, also present in the sample. This read-only follow-up changed only registry/docs and research records; see [`VR-0014`](../records/validation_runs/VR-0014.md).


### High-resolution parameter-layer definitions (2026-10-30)

The maintainer confirmed `flddif` as relative height above the unit-catchment reference river pixel (m), `hand` as Height Above Nearest Drainage relative to a river with upstream area >=0.5 km2 (m), high-resolution `uparea` as contributing area (km2), and high-resolution `grdare` as land-only area per grid cell (km2), aggregated from the underlying 3-second pixels. Source inspection found `flddif` calculated from local elevation minus the unit-catchment reference elevation; `hand` and high-resolution `uparea` are carried from supplied layers. FLOW's `calc_lndare` / `calc_grdare` code documents km2, sums only cells whose source flow-direction layer is not the sea/undefined code, and writes -9999 for cells with no land. The v5 `generate_inpmat` multiplies positive `grdare` values by 1e6 to convert km2 to m2. This is consistent with the Mekong 1-minute sample's positive values (median about 3.10 km2, maximum about 3.41 km2) and the geometry-derived cell-area range. The base-resolution `uparea` remains m2. Definitions and area units were added to the shared registry and regional workflow manual; no generation programs ran. See [`VR-0015`](../records/validation_runs/VR-0015.md).

Source inspection established high-resolution `downxy` as two signed 16-bit relative offsets on the high-resolution grid (`downx`, then `downy`), added to the current 1-based pixel indices to find the downstream pixel. FLOW adjusts x offsets across the global longitude seam. It assigns `-999` to river mouths and `-1000` to inland terminations; `-9999` is the initial value left where no offset is assigned, but the exact pixel classes are not established. CaMa-Flood parameter readers stop following links when `downx <= -900`, and the v5 regional combiner preserves the offsets unchanged. The maintainer clarified that low-resolution `downxy` uses `-9999` for sea, while high-resolution land pixels without an assigned unit catchment may use a different code. The registry, metadata specification, and regional READMEs distinguish this low/high-resolution behavior and retain the unresolved high-resolution encoding. No map processing or scientific validation was run; see [`VR-0016`](../records/validation_runs/VR-0016.md) and the subsequent maintainer clarification in [`VR-0017`](../records/validation_runs/VR-0017.md).

### CTL missing-value metadata and testing boundaries (2026-10-30)

The maintainer clarified that the map-local scripts in `map/mekong_15min/` prepare or refresh the official GitHub sample from global source datasets in the separate CaMa-Flood package. They are not tests, and missing package data is not a skipped test result. The test workflow now has three distinct concerns: sample preparation, automated code/data checks using synthetic fixtures and the committed Mekong binaries, and independent scientific validation. CI does not execute regionalization or claim scientific correctness from generated files.

The common registry replaced free-text `missing_value_convention` with numeric `missing_value` plus human-readable `special_values`. Of 81 rows, 41 have numeric values recorded and 40 are blank/non-applicable; 26 binary product entries remain blank because their stored sentinel is not verified. The v5 generator now uses those numeric declarations, warns and counts type-specific visualization fallbacks for blank entries, rejects invalid or conflicting values, and maps signed int8 sentinel `-9` to GrADS unsigned-byte `undef 247` without changing binary files. GrADS' variable-format documentation describes unsigned one-byte input, signed two-byte input, and four-byte integers; negative non-sentinel int8 classes remain displayed as unsigned values. The legacy v4 generator was left unchanged; its shared CSV reader was updated to accept the new columns.

The prior package-oriented Mekong validator failed because it required absent model/runtime package products. It was replaced by checks for the actual tracked sample and a non-destructive CTL-wrapper integration test in a temporary copy. Seventeen synthetic generator tests, four Mekong file checks, all 33 temporary CTL outputs (21 base, 12 high-resolution), registry validation, shell syntax checks, and the five-program GNU Fortran build passed. Zero sample files were skipped, failed, or used a fallback. The tests did not invoke the Mekong sample-preparation scripts, run a simulation, or validate scientific network behavior. See [`VR-0018`](../records/validation_runs/VR-0018.md).
