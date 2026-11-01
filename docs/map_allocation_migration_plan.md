# CaMa-Flood v5 Allocation Tools Migration Plan

**Phase:** B — legacy characterization, v5 migration, and regression
**Status:** Phase A assessment and Phase B0–B6 migration, documentation, research reconciliation, and regression are complete and pushed to `feature/map-data-tools` (`ef9fec9`). Independent scientific validation remains outside this migration.
**Project:** map data/tools modernization, [Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122)
**Branch:** `feature/map-data-tools`. Phase A originally inspected 93d5a36; Phase B started at 05258b288e1045e5d6c056ae30216a13f5e84793, confirmed equal to `origin/feature/map-data-tools` after fetch.

This document separates **verified source/package observations**, **maintainer-confirmed clarifications**, **migration proposals**, and **unresolved questions**. The maintainer has confirmed redistribution permission for the named processed GRDC/GRanD and MLIT tables and separately directed Git tracking of every GRDC, GRanD, and MLIT list CSV currently present in `map/data/`, under its existing filename. The maintainer clarifies that `GRanD_alloc+Mekong.csv` extends the GRanD list with several recently constructed Mekong dams added by additional research at Yamazaki Laboratory, and confirms it is suitable for tracking. The specific added rows, source evidence, and investigation date are not documented here. The instruction does not cover non-list source workbooks or unrestricted source databases, or change legacy algorithms, outputs, or interfaces.

## 1. Executive summary

The three legacy programs are optional map-preparation utilities that place external flow gauges, dams/reservoirs, and water-level gauges on a CaMa-Flood map. For GRDC and GRanD, their `*_allocated.csv` inputs already represent a first-stage allocation to high-resolution MERIT Hydro; these CaMa-Flood programs perform a second-stage mapping from MERIT Hydro to CaMa-Flood unit catchments. They share substantial map loading, upstream-cell indexing, and high-resolution outlet tracing code, but their candidate acceptance rules, classifications, and outputs have different meanings.

The standard Mekong sample includes the base maps and one-area 1min layers required by all three readers. The five allocation-list CSVs are distributed separately under `map/data/` and are now Git-tracked. Before the November 2026 tracking update, the broad `/map/data/*` rule excluded them; retain that as historical context, not a current CI limitation. The legacy v4 `t03-alloc_level_gauge.sh` still names absent `MLIT_allocated.csv`; Phase B uses the maintainer-approved canonical `map/data/MLIT_LevelGauge_allocated.csv` for the new v5 wrapper and leaves the v4 script unchanged.

**Implemented:** three separate `allocate_*` executables now reside in `map/tools/code/`, with shared input readers in `mod_allocation_map_io.F90` and wrappers under `map/tools/script_params/`. Scientific selection and classification routines remain in separate programs. Regression tests compare v5 output with saved v4 behavior; successful allocation and legacy-output equality are not scientific validation.

## 2. Scope and inspection state

Inspected the three programs and wrappers in map/src/src_param/, its README and Makefile, the current shared map/tools/code/Makefile, map/tools/script_params/ documentation and workflows, map/map_variables.csv, the current map/mekong_15min/ tree, local map/data/, and the separately supplied read-only v4.30 package reference.

At Phase A, the three legacy sources were 460, 485, and 527 lines, and `map/tools/tests/` had no allocation-specific tests. At Phase B start, `git fetch origin feature/map-data-tools` succeeded; local HEAD and the remote branch were both `05258b2`. Issue #122 is open, and the allocation-related open PR search returned no matches. The documentation and research changes from the immediately preceding maintainer clarification are preserved in the worktree.

## 3. Legacy workflow inventory (v4 baseline)

The v4 shell templates are intended for map-local `src_param/`; v5 wrappers are copied to map-local `script_params/`. Both execute from that directory to preserve the established relative map paths. V5 wrappers call shared executables in `map/tools/code/` and move text products to the map root.

| Workflow | Arguments and inputs | Required map products | Outputs and behavior | Entry points |
| --- | --- | --- | --- | --- |
| Flow gauge | `allocate_flow_gauge GaugeListFile [multi/single]`; reads first four fields as ID, latitude, longitude, and upstream area. Omitted or invalid mode falls back to multi. | `params.txt`, `uparea.bin`, `nextxy.bin`; 1min/location.txt or fallback 15sec/location.txt, plus selected high-resolution `.uparea.bin`, `.catmxy.bin`, `.downxy.bin`. | Writes `gauge_alloc.txt`; v4 wrapper moves it to `GRDC_alloc.txt`. Fixed-width text includes selected map-area comparison, status/count, up to two map-cell indices and areas. Out-of-domain rows are skipped; unmatched/ineligible rows use negative sentinels. | [allocate_flow_gauge.F90](../map/src/src_param/allocate_flow_gauge.F90#L61), [t01-alloc_flow_gauge.sh](../map/src/src_param/t01-alloc_flow_gauge.sh#L1) |
| Dam/reservoir | `allocate_dam DamListFile`; reads first eight fields as ID, latitude, longitude, drainage area, dam name, river name, capacity, and year. | Flow-gauge base/high-resolution inputs plus `ctmare.bin`; no high-resolution elevation is read. | Opens `dam_alloc_river.txt`, `dam_alloc_small.txt`, and `dam_alloc_error.txt`. The v4 wrapper moves only river/small files to `GRanD_river.txt` and `GRanD_small.txt`; the error product stays in the script working directory. Out-of-domain rows and negative capacities are skipped; area-based sub-grid and failure categories are separate. | [allocate_dam.F90](../map/src/src_param/allocate_dam.F90#L58), [t02-alloc_dams.sh](../map/src/src_param/t02-alloc_dams.sh#L1) |
| Water-level gauge | `allocate_level_gauge GaugeListFile`; reads first four fields as ID, latitude, longitude, and upstream area. | Flow-gauge base/high-resolution inputs plus `elevtn.bin` and `<HIRES>/<HIRES>.elevtn.bin`. | Writes `gauge_alloc.txt`; v4 wrapper moves it to `MLIT_levelgauge.txt`. Output includes classification, map indices, outlet/gauge/upstream elevations, river-path distances, and map upstream area. Out-of-domain rows are skipped; unmatched rows use sentinels. | [allocate_level_gauge.F90](../map/src/src_param/allocate_level_gauge.F90#L61), [t03-alloc_level_gauge.sh](../map/src/src_param/t03-alloc_level_gauge.sh#L1) |

The v4 programs implicitly try `1min` then `15sec`. The v4.30 distribution package includes global `glb_15min/1min` and Japan `tej_01min/15sec` as single-area CaMa-Flood map layers. The AllocRiverGauge manual's global 30-arcsec and Japan 5-arcsec maps are separate allocation-specific first-stage inputs. V5 makes `HIRES_TAG` explicit and requires exactly one area in `location.txt`. This is an intentional allocation contract: the migrated programs match locations to a selected high-resolution river network, not to the finest inundation-topography surface. The rationale, package evidence, and contrast with multi-area aggregation workflows are documented in the [high-resolution area handling contract](map_data_tools_plan.md#high-resolution-area-handling-contract). Multi-area allocation is not a required follow-up for the initial v5 migration.

### V5 invocation and outputs

The new programs run from map-local `script_params/` and read map files from `../`. The wrappers resolve shared executables centrally and default to the canonical CSV files under the sibling `map/data/` directory. Each wrapper is configurable before use.

| Workflow | V5 command-line arguments | Required map files | Wrapper and persistent outputs |
| --- | --- | --- | --- |
| Flow gauge | `allocate_flow_gauge GaugeListFile multi\|single HIRES_TAG` | `params.txt`, `uparea.bin`, `nextxy.bin`, plus `<HIRES_TAG>/location.txt`, `.uparea.bin`, `.catmxy.bin`, `.downxy.bin` | `t01-alloc_flow_gauge.sh` writes `GRDC_alloc.txt` |
| Dam/reservoir | `allocate_dam DamListFile HIRES_TAG` | Flow-gauge inputs plus `ctmare.bin` | `t02-alloc_dams.sh` writes `GRanD_river.txt`, `GRanD_small.txt`, and `GRanD_error.txt` |
| Water-level gauge | `allocate_level_gauge GaugeListFile HIRES_TAG` | Flow-gauge inputs plus `elevtn.bin` and `<HIRES_TAG>/<HIRES_TAG>.elevtn.bin` | `t03-alloc_level_gauge.sh` writes `MLIT_levelgauge.txt` |

The wrappers move every program output into the map root. In particular, the dam error table is retained as `GRanD_error.txt`, unlike the legacy v4 wrapper, which left it in its working directory.

All three outputs are whitespace-delimited, fixed-width text tables with a one-line header; they are not CSV files. The coordinate columns copy the input longitude/latitude in degrees. The source converts coarse drainage area from m² to km² before comparison, while candidate high-resolution drainage area is compared without conversion, so the supplied area is expected to be km². Confirm and document the precise source-product area units before generalizing the input contract.

| Program/output | Data columns in order (header spelling) | Units and interpretation |
| --- | --- | --- |
| Flow: gauge_alloc.txt, moved by t01 to map-root GRDC_alloc.txt | ID, lat, lon, area_Input, area_CaMa, error, diff, Type, ix1, iy1, ix2, iy2, area1, area2 | Input coordinates: degrees; input/map areas and area difference: km²; error: dimensionless relative difference; Type is snum, the number of upstream substitute coarse cells (0, 1, or 2); indices are 1-based coarse-map coordinates. A failed allocation writes Type=-9 and -999 sentinels in result fields. Out-of-domain input rows are omitted, not emitted as failures. |
| Dam: dam_alloc_river.txt, dam_alloc_small.txt, dam_alloc_error.txt | ID, lat, lon, area_CaMa, area_Input, error, diff, ix, iy, cap_mcm, year, damname, rivname | Coordinates: degrees; areas and difference: km²; error: dimensionless relative difference; indices: 1-based coarse-map coordinates; capacity is passed through in the source field named CAP_MCM (the local conversion does not calculate capacity). The small file uses area_CaMa=-888 and error=-8 for a sub-grid classification. The error file uses area_CaMa=-999 and -999 result sentinels. Out-of-domain rows and negative capacities are omitted. |
| Level: gauge_alloc.txt, moved by t03 to map-root MLIT_levelgauge.txt | ID, lat, lon, area, Type, ix1, iy1, ix2, iy2, elv_outlet, elv_gauge, elv_upst, dst_outlet, dst_upst, upa_outlet | Coordinates: degrees; input and upstream areas: expected km²; elevations: m per source comments/data contract; distances: km. Type values 1=first/largest-area upstream path (commented mainstem), 2=later upstream path (commented tributary), 3=no tested upstream path reaches gauge (small stream); -9 means no allocation. Indices are 1-based coarse-map coordinates; unmatched result fields use -999. The elevation fields are absolute elevations, not computed differences. Out-of-domain rows are omitted. |

The header and data types are source-verified, but legacy headers do not fully define all units or sentinel semantics. The mappings above distinguish explicit source comments from units inferred from the area conversion and require maintainer review before becoming a stable v5 public data contract. In particular, do not silently reinterpret the flow Type field or dam small/error markers.

### 3.1 Flow-gauge algorithm

1. Read coarse-map drainage area and two-record nextxy; convert positive coarse uparea values from m² to km². Read high-resolution drainage area, two-record catmxy and downxy, and derive pixel-center coordinates from location.txt.
2. Build up to eight immediate upstream coarse cells for each downstream cell, ordered by descending upstream area. Identify each coarse catchment's high-resolution outlet by flow-direction offsets or a change in catmxy catchment ID.
3. Convert the gauge coordinate to a high-resolution pixel. Search a 7x7 window (dx,dy=-3..3). Candidates must have high-resolution upstream area greater than 5% of the supplied area. Score by an area-ratio error with a distance adjustment of 0.02 per pixel; over- and under-estimation are transformed symmetrically, and the under-estimation score is capped at 1000. Strictly smaller scores replace the current best, so exact ties keep the earlier loop candidate.
4. Use catmxy to obtain the coarse catchment. If the matched coarse outlet area differs from the reported area by more than 5%, trace the up to eight coarse upstream candidates through high-resolution downxy; reject candidates under 10% of reported gauge area; greedily retain candidates that improve the absolute summed-area error. Stop after two upstream cells, after one in single mode, or once error is below 10%.

Here single and multi control the number of *upstream alternatives* that may be selected (one versus at most two); snum is the count emitted in the Type output column. The primary coarse cell is initially the matched catchment. If no acceptable upstream alternative is found, an area error over 100% (selected area more than twice the input area) is treated as unmatched. These semantics follow assignments and branches at [allocate_flow_gauge.F90 lines 258–429](../map/src/src_param/allocate_flow_gauge.F90#L258).

### 3.2 Dam/reservoir algorithm

The high-resolution 7x7 area-matching search is structurally similar to the flow-gauge search, including its 5% minimum candidate area, 0.02-per-pixel location penalty, symmetric area score, and 1000 score cap. It then maps the selected high-resolution pixel to the coarse catchment.

The small/sub-grid decision is based on area, not an explicit network mainstem label: if input reservoir area is below 30% of the matched coarse ctmare, it is assigned marker -888 to the small output. Otherwise, when coarse drainage-area error exceeds 20%, the program searches the same bounded upstream-catchment list and high-resolution path for a single upstream candidate with improved area agreement, discarding candidates whose upstream area is below 10% of reservoir input area and stopping when error is below 10%. If the final area is still more than twice the input area, it is also marked -888. Marker -999 is used for the error output path. Capacity and year are copied to output; capacity does not participate in spatial matching.

The v4 t02 comment calls `GRanD_small.txt` dams “not on the mainstem,” but the source criterion is area-based: the allocated drainage area is below 30% of catchment area, or the later >100% relative-area fallback applies. V5 documentation describes the code criterion, not a verified physical mainstem class. The legacy source also contains an empty repeated `if (area0 < ctmare*0.3)` block; it has no effect and remains untouched.

### 3.3 Water-level-gauge algorithm

The high-resolution area search and coarse-cell mapping follow the same general method, but the output is for river-network position and height context. After mapping, the program reads coarse and high-resolution elevations. It traces downxy from the selected pixel to the coarse catchment outlet, accumulating distance in km. elv_outlet, elv_gauge, and elv_upst are elevations (the source does not subtract them into elevation differences despite one nearby comment).

It classifies a gauge as 1 when the path is reached from the first/largest-area upstream coarse candidate, 2 for a later candidate, and 3 when none of the up to eight upstream paths contains the matched high-resolution gauge pixel. dst_outlet is distance from the matched pixel to the coarse-cell outlet; dst_upst is distance from a candidate upstream outlet to the matched point along the traced high-resolution path. iXX0/iYY0 and jXX0/jYY0 are coarse-map grid indices; for type 3, the upstream indices/elevation/distance are sentinel values. The distance function uses an Earth-centered chord and converts it to an arc length in km; the exact radius/ellipsoid approximation is source-coded in rgetlen.

### 3.4 Cross-workflow facts and discrepancies

- Shared facts: all three assume a regular lat/lon coarse map described by params.txt, one selected high-resolution area, high-resolution drainage area plus catchment IDs and downstream offsets, and the first four station/reservoir input fields in the same positional order.
- Scientific differences: flow gauges can return up to two upstream alternatives; dams use a separate catchment-area threshold and at most one upstream substitute; level gauges classify stream position and emit elevation/path-distance context.
- Input names: the historical v4 `t03-alloc_level_gauge.sh` requests `../../data/MLIT_allocated.csv`, which is absent. The maintainer has approved the canonical `map/data/MLIT_LevelGauge_allocated.csv` as the v5 workflow input. The v4 wrapper remains unchanged.
- The v4 t02 comment calls its dam list a “Gauge List File”; the v5 wrapper names the input by its dam role and moves the error file to the map root as `GRanD_error.txt`.
- Input parsing is Fortran list-directed rather than a CSV parser. It skips exactly one header line and reads fields positionally. Quoted commas, blank/malformed fields, header variants, invalid numeric values, and names with whitespace need explicit compatibility tests before promising general CSV support.

## 4. External allocation data inventory and provenance

Sizes and counts below are direct observations from the local checkout/package on 2026-11-01. Record counts are non-header CSV rows. Package files are read-only; nothing was copied or edited. The package source workbooks are candidates for provenance, but a complete preparation pipeline was not reconstructed.

| Dataset/file | Checkout / package availability | Size; rows | Observed schema/use | Provenance and redistribution status |
| --- | --- | --- | --- | --- |
| GRDC_allocated.csv | Present locally and in inspected package; tracked under this name. | 1,297,061 bytes (1.237 MiB); 7,564 | Header begins ID,lat_alloc,lon_alloc,area_alloc,...; t01 consumes the first four fields. Remaining table includes original and modified coordinates/areas and station metadata. | AllocRiverGauge v1.2 manual (package copy v1.1) identifies this as the finalized MERIT Hydro allocation table for its Step 2 (CaMa-Flood) workflow. It documents repeated automatic allocation, manual/visual QAQC, correction or exclusion of suspicious attributes, and a final allocation using MERIT Hydro-based locations and upstream area. Retain source attribution. |
| GRanD_allocated.csv | Present locally and in inspected package; tracked under this name. | 679,568 bytes (0.648 MiB); 7,320 | ID,lat_alloc,lon_alloc,area_alloc,DamName,RiverName,CAP_MCM,YEAR,...; t02 consumes the first eight fields. CAP_MCM is the capacity label used by the file/program; its unit should be formalized from the exact source product before new metadata claims. | AllocRiverGauge v1.2 manual (package copy v1.1) identifies this as the finalized MERIT Hydro allocation table for its Step 2 (CaMa-Flood) workflow. It documents manual attribute correction, repeated allocation/QAQC, and final allocated location/upstream-area fields. Retain GRanD/GeoDAR attribution. |
| GRanD_alloc+Mekong.csv | Present locally and in package; tracked under this name. | 816,871 bytes (0.779 MiB); 7,325 | Header has MERIT allocation fields (lat_MERIT, lon_MERIT, area_MERIT) plus original coordinates/area and capacity. The file combines the GRanD list with several recently constructed Mekong dams added by additional research at Yamazaki Laboratory, according to the maintainer. | Maintainer-confirmed provenance summary; suitable for tracking. The individual added rows, their supporting sources, and the research date are not specified in this assessment. |
| MLIT_allocated.csv | Absent locally and in inspected package. | N/A | Historical v4 t03 path only; not used by v5. | The v5 wrapper uses the maintainer-approved canonical `MLIT_LevelGauge_allocated.csv`; the legacy v4 reference remains unchanged. |
| MLIT_LevelGauge_allocated.csv | Present in `map/data/` and the inspected package; tracked under this name. | 617,687 bytes (0.589 MiB); 7,539 | Starts ID,lat_alloc,lon_alloc,area_alloc,lat_ori,lon_ori,...; first four fields are positionally compatible with the level-gauge reader. Japanese station names occur later in the file. | The maintainer confirms this is a processed/curated table and redistribution is permitted. This exact `map/data/` filename is canonical. The legacy `t03-alloc_level_gauge.sh` references absent `MLIT_allocated.csv`; the v5 wrapper uses the canonical name; the historical v4 reference remains unchanged. Retain MLIT source attribution; consult [MLIT content terms](https://www1.river.go.jp/WDBrules_20230915.pdf) and [official use notes](https://www1.river.go.jp/caution.html) for underlying data context. |
| MLIT_FlowGauge_alloc_final.csv | Present in `map/data/` and the inspected package; tracked under this name. | 176,689 bytes (0.169 MiB); 2,059 | Has allocated/original positions and area; not consumed by t01–t03 as configured. | The maintainer confirms this is a processed/curated table and redistribution is permitted. This exact `map/data/` filename is canonical. Retain MLIT source attribution; consult [MLIT content terms](https://www1.river.go.jp/WDBrules_20230915.pdf) and [official use notes](https://www1.river.go.jp/caution.html) for underlying data context. |
| Candidate source workbooks | GRDC_MERIT_v400.xlsx (above), GRanD_v13_MERITv400.xlsx (above), and HydroWeb_MERITv400.xlsx (478,355 bytes) are in package data. | See sizes | The three allocation scripts consume CSV lists, not these workbooks directly. | Their presence does not alone establish that a workbook is the sole source or that its processing is reproducible. |

Historical Phase A observation (before the maintainer-approved tracking change): `/map/data/*` ignored the allocation lists. Current state verified at Phase B start: all five CSVs listed above appear in `git ls-files`, none is ignored, and `.gitignore` contains narrow path exceptions. `.gitattributes` treats those exact CSVs as text for repository line-ending normalization. The CSV content and names were not changed.

### 4.1 Two-stage allocation and evidence

The existing [AllocRiverGauges manual in this repository](../doc/Manual_AllocateRiverGauges.docx) is v1.1 (2024-04-16); the later AllocRiverGauge tool-repository v1.2 manual (`doc/Manual_AllocateRiverGauges.docx`, updated 2024-11-22) was also inspected during Phase A. Both explicitly define two steps: allocate station/reservoir records to a high-resolution river map (global MERIT Hydro or Japan J-FlwDir), then re-allocate those records to a coarse CaMa-Flood river map. It describes Step 1 as semi-automated: compare reported location and upstream area with the high-resolution network, inspect suspicious results, correct attributes or exclude erroneous records using expert review, and retain QA/QC records. For GRDC, it describes multiple allocation rounds, review of location/area differences, replacing reported location and area with MERIT Hydro-based values for the final allocation, and producing `GRDC_allocated.csv` for Step 2. For GRanD, it describes manual attribute correction, repeated allocation/QAQC, and `GRanD_allocated.csv` as the final Step 1 table, with allocated coordinates and area based on MERIT Hydro. This is the manual's documented methodology; it does not establish that each row is independently accurate.

In short, the allocation provenance is:

1. External station/reservoir metadata.
2. AllocRiverGauge first-stage allocation and review on MERIT Hydro, producing corrected high-resolution locations and drainage areas.
3. CaMa-Flood second-stage mapping from those MERIT Hydro results to CaMa-Flood unit catchments.

The maintainer additionally confirms that the processed GRDC and GRanD tables addressed errors in officially reported drainage areas and discrepancies between MERIT Hydro and real-world drainage areas during first-stage allocation. Because CaMa-Flood river networks are derived from MERIT Hydro, the maintainer expects the pre-allocated locations and drainage areas to be internally consistent when mapped to CaMa-Flood unit catchments. This is the design rationale for using these tables as second-stage inputs, not proof that every individual allocation is accurate. No correction rules beyond those in the manual are inferred here.

The CaMa-Flood programs do not repeat the AllocRiverGauge review process. They map the supplied MERIT Hydro-based records to coarse CaMa-Flood unit catchments using their own candidate-selection and classification logic. Successful output generation or parity with legacy output is not scientific validation.

### 4.2 Mekong sample suitability

map/mekong_15min/params.txt defines NX=80, NY=120, 0.25-degree cells, and a bounding rectangle 90–110°E, 5–35°N. 1min/location.txt declares a single 1min area over the same rectangle with 1200 x 1800 pixels. The tracked sample contains base params.txt, uparea.bin, nextxy.bin, ctmare.bin, and elevtn.bin, and high-resolution 1min.uparea.bin, 1min.catmxy.bin, 1min.downxy.bin, and 1min.elevtn.bin. The inspected binary sizes agree with those dimensions and element widths: base float32 grids are 38,400 bytes, two-record nextxy.bin is 76,800 bytes, and each one-record high-resolution float32 or two-record int16 layer is 8,640,000 bytes.

Using the *rectangular map bounds only*, the current GRDC_allocated.csv contains 175 rows and GRanD_allocated.csv contains 373 rows whose allocated coordinates fall in the box. These are candidates for local smoke/regression experiments, not proof that a record intersects the active Mekong river cells or is scientifically suitable. The MLIT level and flow tables have zero coordinates in the box. The map is therefore structurally capable of running all three algorithms, but cannot provide a realistic MLIT case. All five GRDC/GRanD/MLIT list CSVs in `map/data/` are tracking targets under their existing names. Non-list workbooks and source products are outside this instruction.

## 5. Git tracking and reproducibility recommendation

| Dataset | Recommendation | Reason |
| --- | --- | --- |
| `GRDC_allocated.csv` and `GRanD_allocated.csv` | **Track complete CSVs in Git** | The maintainer instructs tracking list CSVs from `map/data/`. Retain the full tables and source attribution; do not create subsets. |
| `GRanD_alloc+Mekong.csv` | **Track complete CSV in Git** | It contains the GRanD list plus several recently constructed Mekong dams added through additional Yamazaki Laboratory research. The maintainer confirms it is suitable for tracking; retain the existing filename. |
| `MLIT_LevelGauge_allocated.csv` and `MLIT_FlowGauge_alloc_final.csv` | **Track both complete CSVs in Git** | The maintainer confirms these curated tables may be redistributed and instructs tracking them under the existing names. Retain MLIT source attribution. |
| Other source products and workbooks | **Do not add under this list-tracking instruction** | The tracking scope is the GRDC/GRanD/MLIT list CSVs in `map/data/`; source workbooks and databases remain separate products. |
| Synthetic test records | **C — commit small, fabricated fixtures** | Enables deterministic automated tests without redistributing third-party station or reservoir records. Mark all coordinates, IDs, names, and drainage areas as synthetic. |

Track all five list CSVs currently present in `map/data/`—`GRDC_allocated.csv`, `GRanD_allocated.csv`, `GRanD_alloc+Mekong.csv`, `MLIT_LevelGauge_allocated.csv`, and `MLIT_FlowGauge_alloc_final.csv`—under their existing filenames. The maintainer's instruction applies to these map/data list files; it does not include source workbooks or unrestricted source databases. The maintainer clarified that `GRanD_alloc+Mekong.csv` adds several recently constructed Mekong dams identified through additional Yamazaki Laboratory research. Exact added-row provenance is not documented here. The Mekong base map can be used for tests that operate only on its already tracked map files; tests should not overwrite the committed sample. Keep synthetic records as routine CI fixtures unless a realistic-data integration test is separately justified.

## 6. V5 implementation and sharing boundary

The approved organization is implemented as follows:

- The historical implementations remain unchanged under `map/src/src_param/`.
- Migrated sources and `allocate_*` executables reside in `map/tools/code/`.
- Map-local templates are `t01-alloc_flow_gauge.sh`, `t02-alloc_dams.sh`, and `t03-alloc_level_gauge.sh` in `map/tools/script_params/`; no `script_allocate/` exists.
- Build the three programs with `make -C map/tools/code allocate`; the shared Makefile compiles `mod_allocation_map_io.F90` once and links it with each allocation program.
- Wrappers expose metadata-file paths and `HIRES_TAG`, while the programs retain map-local relative paths and the scientific decisions.

**Shared non-scientific I/O:** `mod_allocation_map_io.F90` centralizes `params.txt`, base-map and high-resolution binary reads, and one-area `location.txt` validation. Coordinate search, upstream candidate ordering, and downstream path traversal remain in the three separate programs, preserving their scientific logic and operation order. The three selection algorithms should remain separately readable. In particular, do not unify flow multi/single selection, dam area-to-catchment classification, or level-gauge outlet/upstream classification merely because the low-level map arrays are alike. Any module source placed in map/tools/code/ follows the local mod_ filename convention; that convention does not apply to legacy map/src/.

The v4 tools accepted one selected area and automatically chose `1min` or `15sec`. The v5 programs require explicit `HIRES_TAG`, parse its `location.txt`, and fail when it declares more than one area. Supporting tiled high-resolution mosaics should be a separate decision and validation increment, because lookups across area boundaries can change selected pixels and path traversal.

## 7. Regression, structural, and scientific validation strategy

`map/tools/tests/test_allocation_regression.py` compiles the preserved v4 sources and v5 programs in a temporary directory, reconstructs the documented Mekong input selections, and compares output rows to the committed `legacy_gfortran_14_2` snapshots. It covers flow single/multi, all dam output categories, level types 1/2/3, explicit `HIRES_TAG` handling, rejection of multi-area layouts, and unchanged hashes for committed map inputs. Numeric comparisons use at most half of the printed least-significant decimal place; IDs, classifications, sentinels, headers, and row order compare exactly. The fixture includes synthetic out-of-domain records and a negative-capacity dam. Additional threshold-boundary and malformed-input tests remain future work.

1. **Completed regression:** the committed Mekong sample and tracked GRDC/GRanD inputs drive tests in temporary directories; level-gauge classes use the labeled synthetic fixture because MLIT lists have no records in this domain. Tests compare the saved baseline and v4 runtime outputs before comparing v5.
2. **Still useful structural tests:** small synthetic maps, malformed and zero-area inputs, deterministic score ties, dateline cases, and threshold boundaries may be added in future work. Do not change existing scientific rules to make such tests pass.
3. **Scientific validation:** independently compare selected gauges/dams and level-gauge placement with reviewed station metadata, river topology, and domain-specific evidence. Regression and output generation do not establish scientific correctness.

Phase A had no allocation tests. B1 captured the v4 baselines; current regression checks pass for all six output products under GNU Fortran 14.2.0. This demonstrates compatibility for the tested Mekong records, not independent scientific validation.

## 8. Knowledge gaps and maintainer questions

Resolved decisions and remaining knowledge gaps are distinguished below. No blocking architecture question remains for the implemented migration; unresolved scientific rationale is retained for future maintainer review.

| Priority / status | Trigger and source evidence | Question and why it matters | Work that can proceed |
| --- | --- | --- | --- |
| **Resolved data tracking** | Maintainer instructions and current Git index. | All five named GRDC/GRanD/MLIT list CSVs are tracked; source workbooks and source databases are outside this policy. | No allocation migration work remains for this decision. |
| **Resolved architectural scope** | Phase B approval and the subsequent rationale review. | V5 allocation uses an explicit `HIRES_TAG` and intentionally supports exactly one high-resolution river-network area; matrix and permanent-water aggregation have separate multi-area contracts. Multi-area allocation is not a required follow-up without a new use case. | Implemented and regression-tested; see the [area-handling contract](map_data_tools_plan.md#high-resolution-area-handling-contract) for package-scale verification limits. |
| **Important scientific question** | Flow lines 278–318/361–409; dam lines 282–324/351–445; level lines 284–329/349–410. | What was the rationale for the 7x7 search, 5% candidate cutoff, 0.02 pixel-distance penalty, score cap 1000, top-eight upstream list, 10%/20%/30% cutoffs, and “more than twice” fallback? Preserve them for strict parity unless a later approved task changes them. | Mechanical migration and regression can proceed if thresholds are treated as frozen behavior. |
| **Resolved data/interface decision** | Phase B task approval and canonical `map/data/` inventory. | V5 t03 uses `map/data/MLIT_LevelGauge_allocated.csv`; the historical v4 wrapper continues to name `MLIT_allocated.csv` unchanged. | Implemented in the v5 wrapper. |
| **Documented legacy semantics** | Output headers and classification branches in all three source programs. | Flow `Type` is the count of upstream substitutes; level-gauge `Type` is the legacy path class; dam `small` is the source area criterion, not independently verified mainstem status. | V5 output names and values are preserved; docs describe these source meanings. No additional maintainer decision is needed to document observed behavior. |
| **Documentation / provenance question** | The maintainer clarified that `GRanD_alloc+Mekong.csv` extends the GRanD list with several recent Mekong dams identified through additional Yamazaki Laboratory research. | If row-level provenance is needed for future citation or maintenance, which records were added, what sources support them, and when was the review performed? | Tracking and the file's broad role are settled. Existing allocation migration work can proceed while row-level provenance is recovered if needed. |


The current Phase A research gap register links these questions to source and records; see research/records/knowledge_gaps/ and the matching rows in research/knowledge_gaps.csv.

## 9. Phase B milestone record

1. **B0 — complete:** reconciled current CSV tracking, AllocRiverGauge manual evidence, maintainer clarifications, explicit `HIRES_TAG`/single-area scope, and canonical MLIT input.
2. **B1 — complete:** compiled unmodified v4 sources with GNU Fortran 14.2.0 and saved six output snapshots with input selection and source hashes in `map/tools/tests/data/allocation/legacy_gfortran_14_2/`.
3. **B2 — complete:** implemented direct-record readers and single-area `location.txt` validation in `mod_allocation_map_io.F90`.
4. **B3–B5 — complete:** migrated the flow-gauge, dam, and level-gauge programs separately; regression tests match all six v4 reference outputs.
5. **B6 — complete:** integrated the three wrappers into the shared build and script architecture, documented their input/output contracts and limits, reconciled Phase A records, ran the full map-tools and repository data/research validators, and reviewed the diff. The results are recorded in `VR-0064` and `VR-0065`. The migration was committed as `ef9fec9` and pushed to `feature/map-data-tools`; Issue #122 was updated. Independent scientific validation remains outside this migration.

Every milestone should re-run the knowledge-gap checklist. Any proposed numerical or interface change requires explicit maintainer approval under AGENTS.md.
