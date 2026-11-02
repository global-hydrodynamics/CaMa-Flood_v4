# CaMa-Flood Map Data and Tools Modernization

**Status (2026-11-03):** The first v5 map-tools implementation tranche was merged into `master` through [PR #125](https://github.com/global-hydrodynamics/CaMa-Flood_v4/pull/125) (`bf82017`). Issue [#122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122) remains open and paused between stages; the broader modernization is not complete. This document preserves the initial planning rationale and candidate architecture. Future work, including model-facing NetCDF contracts, optional-scheme coverage, and additional validation, still requires its own approved scope.

- **Persistent working branch:** `feature/map-data-tools` (based on `master`).
- **Owner:** Dai Yamazaki (maintainer); Codex supports tasks assigned by the owner.
- **Approval recorded:** The initial authorization covered planning; subsequent bounded map-tools implementation was separately assigned and merged in PR #125. This plan does not authorize additional implementation.
- **Development policy:** Follow the repository's [AGENTS.md](../AGENTS.md). Project coordination and current progress are summarized in [docs/dev_status.md](dev_status.md). GitHub Issue: [#122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122). Issues are recommended for substantial development but optional; when none is used, record approved scope, assignments, and checks in this plan and the project status.

## Purpose

Make CaMa-Flood map datasets and map-processing tools easier to understand, validate, convert, and maintain while preserving compatibility with existing maps, Fortran utilities, and model workflows.

The repository uses GrADS-style binary map products, and source code contains some NetCDF map-input paths. The completeness and behavior of those paths, particularly for optional model schemes, remain to be audited. This plan keeps the requested future goals open for incremental architecture decisions rather than prescribing a format or API in advance.

## Initial evidence

A read-only comparison of the local v4.30 distribution package and Git source tree found:

- The package contains data-bearing global, regional, and specialized map directories with binary files, CTL metadata, text metadata, and high-resolution subdirectories.
- The Git repository contains map-processing source and documentation but not those packaged map datasets; binary map assets are excluded from Git.
- The inspected package has no NetCDF files under `map/`. NetCDF forcing samples elsewhere in the package are not map datasets.
- Some map CTLs use generic title/variable labels, and some binary files have no same-name CTL. The intended metadata contract and which files should receive CTLs need variable-by-variable review.

These observations describe the inspected package layout. They do not establish that every dataset, variable, or optional feature uses the same representation.

## Goals under consideration

1. Improve metadata for existing GrADS binary maps and evaluate automatic CTL generation.
2. Support conversion between existing binary maps and NetCDF where the data model and compatibility requirements permit.
3. Support both single-variable NetCDF products for map processing and combined, module-level NetCDF inputs for the model, if the audit confirms both are useful and feasible.
4. Gradually modernize map-processing utilities; evaluate shared I/O routines, including Python options, without requiring a wholesale rewrite.
5. Preserve legacy datasets, Fortran tools, model interfaces, layouts, and operational workflows unless a separately approved compatibility change says otherwise.
6. Audit NetCDF map compatibility for optional schemes and their additional parameters.

## Proposed work plan

Each phase is a planning checkpoint. Before changing code or formats, define and obtain maintainer approval for its implementation scope and acceptance checks. Record them in a project Issue when one is used, or in this plan and the project status otherwise.

### Phase 1 — Source and dataset inventory

- Catalog representative map variables and datasets: dimensions, layers, numeric kind, byte order, row/column order, missing values, and current CTL fields.
- Trace map writers, binary/CTL generation, model map readers, common I/O modules, and optional-scheme-specific map readers.
- Identify current NetCDF paths and precisely which files, variables, and build/runtime conditions they support.
- Record discrepancies between the source tools, packaged data, and model expectations. Do not copy packaged datasets into Git.

**Checkpoint:** A source-linked inventory distinguishes verified behavior from assumptions and identifies compatibility-sensitive interfaces.

### Phase 2 — Architecture decisions

- Decide the metadata fields and naming conventions needed for binary maps and generated CTLs.
- Decide how individual-variable NetCDF products relate to combined module-level model inputs.
- Decide which I/O implementations and languages are appropriate for each tool, including dependency and deployment costs.
- Define how legacy binary files and existing Fortran workflows remain usable.

**Checkpoint:** A concise decision record resolves the questions needed for a small first implementation; unresolved issues remain explicit.

### Phase 3 — Small map-tool increments

- Implement only the first approved tool or I/O capability, such as metadata inspection or CTL generation, after its variable coverage and behavior are specified.
- Add conversion or shared-I/O capabilities in separate reviewable tasks after the first format contracts are agreed.
- Keep existing workflows available during transition and avoid adding large map assets to the source repository.

**Checkpoint:** Each tool has documented inputs, outputs, metadata behavior, dependencies, and failure handling, with tests using approved small fixtures.

### Phase 4 — Model and optional-scheme compatibility

- Integrate or extend model map input paths only where the source audit shows a concrete gap and the scope is approved.
- Check standard model inputs and each in-scope optional scheme independently; do not infer feature coverage from core NetCDF support.
- Preserve established binary and NetCDF contracts and document any intentionally unsupported combinations.

**Checkpoint:** Supported paths and configurations are demonstrated by targeted checks; unsupported paths are stated clearly.

### Phase 5 — Validation and maintenance

- Validate metadata and shapes before conversion or model use.
- Test representative binary-to-NetCDF-to-binary round trips and model reads against agreed values, kinds, byte order, orientation, layers, and missing-value rules.
- Record the exactness or tolerance criteria for each conversion and model comparison before interpreting results.
- Update component documentation when an approved tool or supported format is delivered.

**Checkpoint:** Reproducible checks cover the approved scope and legacy compatibility; no claim extends beyond the tested formats and features.

## Candidate areas for Phase 1 review

These are investigation targets, not final ownership assignments:

- `map/src/src_param/`, `map/src/src_region/`, and `map/src/src_dam/` for map preparation and map-local outputs.
- `src/cmf_ctrl_maps_mod.F90` and shared `src/common/` or `src/mod/` I/O and grid utilities for model map loading.
- Optional feature controllers and their map inputs, selected from the source audit rather than assumed in advance.
- Existing map documentation and build/runtime configuration needed to describe supported workflows.

## Decisions to resolve with the maintainer

- Which map variables and datasets form the initial supported scope?
- Which metadata fields are authoritative, and what should generated CTLs contain for multi-layer and non-float variables?
- What is the intended role and layout of single-variable versus combined module-level NetCDF files?
- Which NetCDF conventions, dimensions, coordinate names, compression choices, and byte/value preservation guarantees are required?
- Which existing tools should be wrapped, shared, or gradually replaced, and what external Python/NetCDF dependencies are acceptable?
- Which optional schemes and package workflows must be included in each compatibility check?
- What small, redistributable test fixtures can be used, and what exact acceptance criteria apply to conversions and model reads?

## Planning boundary

The initial planning phase created this project overview and its branch-local status registration; it did not itself change source code, map data, scripts, numerical behavior, or public interfaces. Later, separately scoped map-tool implementation was merged through PR #125. The phases, candidate areas, and questions in this document preserve the original proposal and identify topics for future review; they are not all active assignments. Before any further implementation, obtain approval for its specific scope and checks, then update coordination in Issue #122 and [`docs/dev_status.md`](dev_status.md) under [`AGENTS.md`](../AGENTS.md).

## High-resolution area handling contract

**Current v5 design contract, verified from source unless explicitly marked maintainer context.** A high-resolution `location.txt` may describe one or several areas. The number of areas accepted depends on the operation; it is not a property of the map format that every tool must impose identically.

| Workflow | Current high-resolution area contract | Source evidence | Status |
| --- | --- | --- | --- |
| Flow-gauge allocation | Exactly one area; v5 reports an error for any other count. | `map/tools/code/mod_allocation_map_io.F90:read_allocation_hires`; regression in `map/tools/tests/test_allocation_regression.py`. | Intentional design contract, not a missing feature for the current migration. |
| Dam allocation | Exactly one area. | Shared reader `read_allocation_hires`, used by `allocate_dam`. | Same allocation contract. |
| Level-gauge allocation | Exactly one area. | Shared reader `read_allocation_hires`, used by `allocate_level_gauge`. | Same allocation contract. |
| Parameter input-matrix generation | Multiple areas, processed sequentially and accumulated into one output matrix. | `map/tools/code/param_inpmat_generate.F90`; legacy `map/src/src_param/generate_inpmat.F90`. | Implemented; large multi-area integration validation is pending. |
| Regional default input matrix | Multiple areas, processed sequentially and accumulated into one output matrix. | `map/tools/code/region_default_inpmat.F90`; legacy `map/src/src_region/generate_inpmat.F90`. | Implemented; large multi-area integration validation is pending. |
| Permanent-water aggregation | Multiple areas, processed sequentially into base-map catchments. | `map/tools/code/param_water_calc_prmwat.F90`; legacy `map/src/src_param/calc_prmwat.F90`. | Implemented; large multi-area integration validation is pending. |
| Regional high-resolution map combination | Reads the listed areas and crops/combines available source layers. | `map/tools/code/region_combine_hires.F90`. | Implemented by source inspection; package-scale integration validation is pending. |

### Allocation rationale and resolution context

The allocation programs map station or reservoir locations from an already reviewed high-resolution river network to CaMa-Flood unit catchments. The [AllocRiverGauge v1.2 manual](../doc/Manual_AllocateRiverGauges.docx) documents a first-stage allocation and QA/QC on high-resolution networks before CaMa-Flood's second-stage mapping; it describes the first-stage global MERIT Hydro network at 30 arcsec and Japan J-FlwDir at 5 arcsec. Those first-stage grids are distinct from the high-resolution layers packaged with CaMa-Flood maps; the maintainer further explained that this preparation addresses location and drainage-area discrepancies and is intended to make the two network representations correspond. Allocation therefore needs river topology and catchment correspondence, including downstream-path traversal for level gauges. It does not require the finest floodplain inundation-topography grid merely because that grid has a finer nominal resolution.

The v5 single-area restriction preserves the reader/traversal contract used by the migrated allocation programs. This is a deliberate design, not evidence that a particular resolution is adequate for every basin or application. The v4.30 distribution package provides one global `1min` area in `map/glb_15min/1min/location.txt` (21600 x 10800) and one Japan `15sec` area in `map/tej_01min/15sec/location.txt` (6000 x 5280; bounds 123–148 E, 24–46 N). The Japan directory contains 22 files (11 binary layers, 10 CTLs, and `location.txt`); its binary data total about 1.11 GB. Both `location.txt` files declare one area. These are CaMa-Flood high-resolution map layers, and the maintainer identifies them as the intended global and Japan allocation inputs.

Keep these distributed CaMa-Flood layers distinct from the separate high-resolution maps used for the first-stage allocation documented by AllocRiverGauge: global MERIT Hydro at 30 arcsec and Japan J-FlwDir at 5 arcsec. Those are allocation-specific input maps, not the global `1min` and Japan `15sec` layers in the CaMa-Flood v4.30 package. The package structure and the manual describe separate workflow inputs; no derivation relationship between the differently resolved grids is asserted here. Multi-area allocation is not a required task for the initial v5 migration; reconsider it only for a separately approved use case.

### Remaining verification items

- The allocation-map resolution distinction is documented from the AllocRiverGauge manual, package inspection, and maintainer clarification. This does not establish that any particular station or dam allocation is scientifically correct.
- Run the documented multi-area integration procedure against a distribution dataset whose `location.txt` declares multiple areas, if such data are available. This is pending validation, not a missing implementation or a claim of scientific correctness.

### Implemented capability, automated checks, and large-data validation

These are separate evidence levels. The source loops establish implemented multi-area processing; they do not by themselves prove package-scale correctness. The committed Mekong sample has one high-resolution area, so routine sample tests do not exercise multi-area aggregation. Allocation regression explicitly verifies that multi-area allocation is rejected. Existing `param_water_calc_prmwat` tests use a small single-area fixture. The high-resolution CTL test's multi-area `location.txt` fixture checks descriptor generation only, not map-processing behavior.

A maintainer-operated large-data integration check remains pending. Use an isolated writable copy of a distribution dataset whose high-resolution `location.txt` actually declares multiple areas, if such a dataset is available; do not assume the inspected global `1min` or Japan `15sec` data is multi-area, and do not run against or overwrite the packaged source or tracked Mekong sample. Record the exact package version, map tag, environment, command lines, and outputs after execution.

1. Identify the available high-resolution map tag with more than one declared area, if present, and check every listed area identifier, dimensions, cell size, and required layer file against the package inventory.
2. Build the already implemented tools with the repository's shared Makefile. Configure copied map-local wrappers to select `HIRES_TAG=15sec` where applicable.
3. In the isolated map copy, run the parameter input-matrix workflow (`script_params/s03-generate_inpmat.sh`) and permanent-water workflow (`script_params/s05-permanent_water.sh`). If validating regional combination, configure and run the reusable `script_region/s01-regional_map.sh` from its documented map-local working directory against the isolated global source.
4. Confirm that every declared area is processed exactly once, no area is silently skipped, expected products are created, and dimensions, byte sizes, record counts, missing values, and finite/range checks match the output contracts. Review all warnings and per-area summaries.
5. Where inputs and settings are compatible, compare v5 output against the corresponding preserved v4 program. Record exact or tolerance-based comparisons and any differences; do not treat parity alone as scientific validation.
6. Record elapsed time, peak memory if available, compiler/runtime settings, and any dataset-dependent limitation.

No package-scale multi-area run is claimed by this document. Successful file generation or parity checks establish software behavior for the tested data only, not scientific validity of the regional river network or aggregated products.
