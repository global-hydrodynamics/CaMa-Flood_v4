# Mekong regional map preparation

This directory is the map-specific working area for rebuilding the `mekong_15min` regional map from the linked `glb_15min` parent map. The current development input is the workstation's v400 global map. The target bounds are 90–110°E and 5–35°N. This workflow is for development and regeneration; the repository distribution is intended to contain the resulting regional map data without the parent-map link. Package users can follow the general regionalization instructions in [`map/src/src_region/README.md`](../../src/src_region/README.md) to create regional maps from data they have obtained.

The status labels below distinguish behavior read from source (**code-derived**) from steps observed on this workstation (**observed**). A command is not considered verified until its outputs have been inspected.

## Current workstation state

- **Observed:** The nine regional-processing executables were present in this directory at 09:47 JST on 2026-10-29. They were produced after `make clean`; they are local build products and are ignored by Git.
- **Observed:** `s01-regional_map.sh` was run far enough to create `region_info.txt`. Its current five lines select `../../glb_15min/`, west 90.0, east 110.0, south 5.0, and north 35.0.
- **Code-derived:** The current script exits immediately after writing `region_info.txt`. It does not yet launch `cut_domain` or later stages.
- **Observed:** The previous `dim_change.txt` has been removed. No regional `params.txt` or map outputs have been confirmed yet.

## Files and roles

- Parent-map inputs: `../../glb_15min/`, resolved from this working directory. The current link points to a v400 data package on an external volume. Keep this source unchanged.
- Reusable implementation: the `*.F90` entries here link to the maintained programs in `map/src/src_region/`.
- Local workflow and configuration: `Makefile`, `s01-regional_map.sh`, the CTL scripts, `varname.txt`, and the generated `region_info.txt`.
- Generated regional products: written mainly to the parent directory `map/mekong_15min/`; intermediate files such as `dim_change.txt` remain in `src_region/` for later steps.
- Local executables: built in this directory and ignored by Git.

## Workflow, one step at a time

Do not run the complete `s01-regional_map.sh` workflow during manual verification. It can execute multiple data-producing stages. The current script intentionally stops after writing the region configuration so each program can be checked separately.

### Step 1: write and inspect the region configuration

The first part of `s01-regional_map.sh` writes `region_info.txt` as five lines: source map path, west, east, south, and north bounds. The source path is relative to `src_region/`.

```sh
cd /path/to/CaMa-Flood_v4/map/mekong_15min/src_region
./s01-regional_map.sh
cat region_info.txt
```

**Observed on this workstation:** the file was created with the selected v400 parent and 90–110°E, 5–35°N bounds. The output values have been inspected; the command's console output and exit status were not retained. No regional map generation is part of this step.

### Step 2: extract the base region — awaiting manual execution

`cut_domain` reads `region_info.txt`, the parent `params.txt`, and these parent binaries: `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, and `width.bin` (**code-derived**). It crops them to the requested domain and adjusts the downstream coordinates.

From this directory, after reviewing the command:

```sh
./cut_domain
```

Expected outputs from source inspection (**not yet experimentally verified**) include `../params.txt`; regional `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, and `lsmask.bin`; plus `nextxy_noedge.bin` and a newly generated `dim_change.txt` in this directory. For 0.25° source cells and the selected bounds, the expected base grid is 80 × 120 with 10 floodplain layers. Record the actual command output, exit status, file sizes, and dimensions before proceeding to `cut_bifway`.

### Later stages — code-derived outline, not yet run in this rebuild

1. `cut_bifway` uses `dim_change.txt`, `nextxy_noedge.bin`, and the parent `bifori.txt` to write the regional `bifori.txt`.
2. `set_map` reads the regional parameters and base network to create `rivseq.bin`, `uparea_grid.bin`, `upgrid.bin`, `nxtdst_grid.bin`, `rivlen_grid.bin`, `basin.bin`, and `bsncol.bin`.
3. `combine_hires <tag>` processes high-resolution layers only when the matching parent `<tag>/location.txt` exists. It writes regional high-resolution data under `../<tag>/`. The current parent has a `1min/` source; actual required layers and outputs must be confirmed from execution.
4. The regional `generate_inpmat` program uses the selected high-resolution catchment mapping to create `../inpmat_test-1deg.bin` and `../diminfo_test-1deg.txt` for the source's hard-coded regular 1° runoff grid. Check the source before using it for another forcing grid.
5. `s02-wrte_ctl_map.sh` and `s03-wrte_ctl_hires.sh <tag>` create optional GrADS control files. These describe binaries for visualization; they do not create or validate the map data.

These steps must be run and reviewed separately. Do not proceed to the next step until the maintainer has reviewed the preceding output.

## Open technical points

- This checkout's `map/glb_15min` link resolves to v400 data; it is the selected development input for this rebuild. Do not replace or edit it during regionalization.
- The regional `generate_inpmat` implementation has fixed assumptions about a regular 1° grid and output names. Whether that input grid is appropriate for the intended packaged example should be confirmed before running it.
- The full channel-parameter workflow is not yet included in the verified sequence. Its sample script refers to runoff climatology and other inputs whose availability and suitability for this region must be checked before that stage is planned.
- A GitHub package cannot rely on the developer's absolute parent-map link. At the distribution boundary, the regional map outputs need to be present and package paths must not depend on this workstation.

## General preparation guidance

For a new region, first obtain a compatible parent river map and any high-resolution source layers, then copy the reusable `map/src/src_region/` workflow into a map-local working directory. Set the source path and region bounds, build with the repository's selected Fortran configuration, and inspect each generated product before continuing. The common component guide explains the program interfaces and general file roles; this README records the Mekong-specific configuration and workstation observations.
