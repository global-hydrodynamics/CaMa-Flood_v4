# CaMa-Flood Map Data and Tools Modernization

**Status:** Planning proposal. The detailed implementation scope and architecture are not yet approved.

- **Persistent working branch:** `feature/map-data-tools` (based on `master`).
- **Owner:** Dai Yamazaki (maintainer); Codex supports tasks assigned by the owner.
- **Approval recorded:** The maintainer authorized this project branch and planning task in the Codex conversation. This document prepares the project plan; it does not authorize implementation changes.
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

This planning phase created the project overview and a branch-local status registration. It did not change source code, map data, scripts, numerical behavior, or public interfaces, and it does not authorize implementation. Before implementation begins, the maintainer should review this proposal, approve the initial scope and checks, and reconcile the project row into the master development-status registry using [AGENTS.md](../AGENTS.md). A GitHub Issue may be created for substantial work, but is optional; if none is used, keep coordination details in the project status and technical specifications here or in the appropriate component documentation.
