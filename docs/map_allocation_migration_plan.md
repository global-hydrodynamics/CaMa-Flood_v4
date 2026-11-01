# CaMa-Flood v5 Allocation Tools Migration Plan

**Phase:** A — legacy inventory, data assessment, and scientific knowledge-gap review  
**Status:** Assessment proposal for maintainer review; no allocation tool has been migrated by this document.  
**Project:** map data/tools modernization, [Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122)  
**Branch observed:** feature/map-data-tools at 93d5a36 (prmwat); worktree was clean at inspection.

This document separates **verified source/package observations**, **migration proposals**, and **unresolved questions**. It does not authorize a change to the legacy algorithms, outputs, data policy, or interfaces.

## 1. Executive summary

The three legacy programs are optional map-preparation utilities that place external flow gauges, dams/reservoirs, and water-level gauges on a CaMa-Flood map. Each first searches a high-resolution flow network near a supplied coordinate and drainage area, then maps that high-resolution pixel to a coarse CaMa-Flood catchment. They share substantial map loading, upstream-cell indexing, and high-resolution outlet tracing code, but their candidate acceptance rules, classifications, and outputs have different meanings.

The standard Mekong sample includes the base maps and its one-area 1min layers required by all three readers. It does not include the external allocation CSVs. The allocation datasets present in the local v4.30 package are excluded from Git by /map/data/*; the expected MLIT_allocated.csv is absent both in the repository data directory and in the inspected package, while a differently named MLIT level-gauge table is present.

**Recommendation:** migrate the three programs as separate allocate_* executables under map/tools/code/, and put their optional wrappers in the already approved map/tools/script_params/ family. Initially retain three distinct scientific selection routines. Share only neutral map I/O, coordinate, and validated lookup helpers after baseline behavior is captured. Use synthetic records for CI. Keep GRDC and GRanD source-derived records external until redistribution rights for these particular processed tables are established. Do not infer scientific validation from successful allocation or legacy-output equality.

## 2. Scope and inspection state

Inspected the three programs and wrappers in map/src/src_param/, its README and Makefile, the current shared map/tools/code/Makefile, map/tools/script_params/ documentation and workflows, map/map_variables.csv, the current map/mekong_15min/ tree, local map/data/, and the separately supplied read-only v4.30 package reference.

The three source programs are respectively 460, 485, and 527 lines. map/tools/tests/ has no allocation-specific tests. Git reports the current branch tracking origin/feature/map-data-tools at 93d5a36, with no local changes. A git fetch origin feature/map-data-tools attempt could not update .git/FETCH_HEAD in this environment, so the displayed remote-tracking ref is not a fresh fetch performed during this review. GitHub Issue #122 is open; the GitHub search found no open allocation-related Issue or PR.

## 3. Legacy workflow inventory

The shell templates are intended to be copied into a map-local src_param/ directory and run there. This working directory is part of the current interface: the programs open base maps as ../<file>, high-resolution layers as ../<HIRES>/<HIRES>.<layer>.bin, and create output text files in the current directory. The wrappers call ./allocate_* and then move selected outputs to the map root. In v5, shared executable paths can change, but running from a map-local workflow directory can preserve these relative data paths.

| Workflow | Arguments and inputs | Required map products | Outputs and behavior | Entry points |
| --- | --- | --- | --- | --- |
| Flow gauge | allocate_flow_gauge GaugeListFile [multi/single]; reads first four fields as ID, latitude, longitude, and upstream area. Omitted or invalid mode falls back to multi. | params.txt, uparea.bin, nextxy.bin; 1min/location.txt or fallback 15sec/location.txt, plus <HIRES>.uparea.bin, .catmxy.bin, .downxy.bin. | Writes gauge_alloc.txt; wrapper moves it to GRDC_alloc.txt. Fixed-width text columns include input location/area, selected map-area comparison, status/count, up to two map-cell indices and their areas. Out-of-domain rows are skipped; unmatched/ineligible rows use negative sentinels. | [allocate_flow_gauge.F90](../map/src/src_param/allocate_flow_gauge.F90#L61), [t01-alloc_flow_gauge.sh](../map/src/src_param/t01-alloc_flow_gauge.sh#L1) |
| Dam/reservoir | allocate_dam DamListFile; reads first eight fields as ID, latitude, longitude, drainage area, dam name, river name, capacity in MCM, and year. | Flow-gauge base/high-resolution inputs plus ctmare.bin; no high-resolution elevation is read. | Opens dam_alloc_river.txt, dam_alloc_small.txt, and dam_alloc_error.txt. Wrapper moves only the first two to GRanD_river.txt and GRanD_small.txt; the error output remains in the script working directory. Out-of-domain rows and negative capacities are skipped; failures and size-based sub-grid classifications are separated. | [allocate_dam.F90](../map/src/src_param/allocate_dam.F90#L58), [t02-alloc_dams.sh](../map/src/src_param/t02-alloc_dams.sh#L1) |
| Water-level gauge | allocate_level_gauge GaugeListFile; reads first four fields as ID, latitude, longitude, and upstream area. | Flow-gauge base/high-resolution inputs plus elevtn.bin and <HIRES>.elevtn.bin. | Writes gauge_alloc.txt; wrapper moves it to MLIT_levelgauge.txt. Columns include classification, map indices, outlet/gauge/upstream elevations, distances along the high-resolution flow path, and map upstream area. Out-of-domain rows are skipped; unmatched rows use sentinels. | [allocate_level_gauge.F90](../map/src/src_param/allocate_level_gauge.F90#L61), [t03-alloc_level_gauge.sh](../map/src/src_param/t03-alloc_level_gauge.sh#L1) |

The shell wrappers do not pass a high-resolution-map selector. Each program tries 1min when location.txt declares one area, then falls back to 15sec; it reads one set of layer files and does not iterate tiled areas. This is a verified current limitation, not an endorsed general map contract.

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

The t02 comment calls GRanD_small.txt dams “not on the mainstem,” but the verified code criterion is the 30%-of-catchment-area rule (plus the later >100% relative-area fallback). Do not carry the shell wording forward as a verified scientific classification. The source also has an empty repeated if(area0 < ctmare*0.3) block at lines 362–365; it has no effect and should be recorded for a later code review, not removed during Phase A.

### 3.3 Water-level-gauge algorithm

The high-resolution area search and coarse-cell mapping follow the same general method, but the output is for river-network position and height context. After mapping, the program reads coarse and high-resolution elevations. It traces downxy from the selected pixel to the coarse catchment outlet, accumulating distance in km. elv_outlet, elv_gauge, and elv_upst are elevations (the source does not subtract them into elevation differences despite one nearby comment).

It classifies a gauge as 1 when the path is reached from the first/largest-area upstream coarse candidate, 2 for a later candidate, and 3 when none of the up to eight upstream paths contains the matched high-resolution gauge pixel. dst_outlet is distance from the matched pixel to the coarse-cell outlet; dst_upst is distance from a candidate upstream outlet to the matched point along the traced high-resolution path. iXX0/iYY0 and jXX0/jYY0 are coarse-map grid indices; for type 3, the upstream indices/elevation/distance are sentinel values. The distance function uses an Earth-centered chord and converts it to an arc length in km; the exact radius/ellipsoid approximation is source-coded in rgetlen.

### 3.4 Cross-workflow facts and discrepancies

- Shared facts: all three assume a regular lat/lon coarse map described by params.txt, one selected high-resolution area, high-resolution drainage area plus catchment IDs and downstream offsets, and the first four station/reservoir input fields in the same positional order.
- Scientific differences: flow gauges can return up to two upstream alternatives; dams use a separate catchment-area threshold and at most one upstream substitute; level gauges classify stream position and emit elevation/path-distance context.
- Input names: t03 requests ../../data/MLIT_allocated.csv, which is absent. The inspected package instead contains MLIT_LevelGauge_allocated.csv; its first four columns have the positional fields expected by the program, but its intended equivalence has not been confirmed. Do not silently change the path during migration.
- t02 calls its dam list “Gauge List File” in a shell comment, and the error file is not moved by the wrapper. Both are documentation/workflow details to resolve while implementing wrappers.
- Input parsing is Fortran list-directed rather than a CSV parser. It skips exactly one header line and reads fields positionally. Quoted commas, blank/malformed fields, header variants, invalid numeric values, and names with whitespace need explicit compatibility tests before promising general CSV support.

## 4. External allocation data inventory

Sizes and counts below are direct observations from the local checkout/package on 2026-11-01. Record counts are non-header CSV rows. Package files are read-only; nothing was copied or edited. The package source workbooks are candidates for provenance, but a complete preparation pipeline was not reconstructed.

| Dataset/file | Checkout / package availability | Size; rows | Observed schema/use | Provenance and redistribution status |
| --- | --- | --- | --- | --- |
| GRDC_allocated.csv | Present locally and in inspected package; not tracked. | 1,297,061 bytes (1.237 MiB); 7,564 | Header begins ID,lat_alloc,lon_alloc,area_alloc,...; t01 consumes the first four fields. Remaining table includes original and modified coordinates/areas and station metadata. | Package also has GRDC_MERIT_v400.xlsx (12,834,632 bytes), but a complete reproducible conversion/curation path was not found. The GRDC portal prohibits redistribution of downloaded data to third parties/public internet and commercial use of original data; whether this specific processed station table is covered or separately licensed is unresolved. Keep real records external pending confirmation. [GRDC data portal terms](https://grdc.bafg.de/data/data_portal/). |
| GRanD_allocated.csv | Present locally and in inspected package; not tracked. | 679,568 bytes (0.648 MiB); 7,320 | ID,lat_alloc,lon_alloc,area_alloc,DamName,RiverName,CAP_MCM,YEAR,...; t02 consumes the first eight fields. CAP_MCM is the capacity label used by the file/program; its unit should be formalized from the exact source product before new metadata claims. | Package also has GRanD_v13_MERITv400.xlsx (1,808,954 bytes). The record count matches the 7,320-record GRanD v1.3 product, but allocation and correction steps are undocumented here. Original GRanD terms restrict commercial use and redistribution without permission; do not publish the full file or a geographic subset without rights clarification. [GRanD database overview](https://www.globaldamwatch.org/database), [GRanD technical documentation](https://water-future.org/wp-content/uploads/2017/11/gwsp_GRanD_Technical_Documentation_v1_1.pdf). |
| GRanD_alloc+Mekong.csv | Present locally and in package; not referenced by t02. | 816,871 bytes (0.779 MiB); 7,325 | Header has MERIT allocation fields (lat_MERIT, lon_MERIT, area_MERIT) plus original coordinates/area and capacity. Despite the filename, it contains the same global-scale record count; “Mekong” in the filename is not evidence it is a regional subset. | Preparation relation to GRanD_allocated.csv is unresolved. Same GRanD rights caution applies. |
| MLIT_allocated.csv | Absent locally and in inspected package. | N/A | Referenced by t03, not available for inspection. | No exact file provenance, schema, size, or license can be asserted. |
| MLIT_LevelGauge_allocated.csv | Present locally and in package; not tracked. | 617,687 bytes (0.589 MiB); 7,539 | Starts ID,lat_alloc,lon_alloc,area_alloc,lat_ori,lon_ori,...; first four fields are positionally compatible with the level-gauge reader. Japanese station names occur later in the file. | Official MLIT hydrology database terms allow reuse and processing with source attribution but warn that some content may carry third-party rights. This local derived table has no evident collection date or generation procedure, so exact rights/attribution should be confirmed before redistribution. [MLIT content terms (PDF)](https://www1.river.go.jp/WDBrules_20230915.pdf), [official use notes](https://www1.river.go.jp/caution.html). |
| MLIT_FlowGauge_alloc_final.csv | Present locally and in package; no matching allocation shell wrapper in the inspected three workflows. | 176,689 bytes (0.169 MiB); 2,059 | Has allocated/original positions and area; not consumed by t01–t03 as configured. | Same need to identify the source table, processing date, and attribution. |
| Candidate source workbooks | GRDC_MERIT_v400.xlsx (above), GRanD_v13_MERITv400.xlsx (above), and HydroWeb_MERITv400.xlsx (478,355 bytes) are in package data. | See sizes | The three allocation scripts consume CSV lists, not these workbooks directly. | Their presence does not alone establish that a workbook is the sole source or that its processing is reproducible. |

map/data/* is ignored by root .gitignore; git ls-files map/data listed only the GPCC runoff sample files, not allocation data. git check-ignore -v confirms the four inspected allocation CSVs are ignored by line 16. No allocation data was staged or tracked as part of this task.

### 4.1 Mekong sample suitability

map/mekong_15min/params.txt defines NX=80, NY=120, 0.25-degree cells, and a bounding rectangle 90–110°E, 5–35°N. 1min/location.txt declares a single 1min area over the same rectangle with 1200 x 1800 pixels. The tracked sample contains base params.txt, uparea.bin, nextxy.bin, ctmare.bin, and elevtn.bin, and high-resolution 1min.uparea.bin, 1min.catmxy.bin, 1min.downxy.bin, and 1min.elevtn.bin. The inspected binary sizes agree with those dimensions and element widths: base float32 grids are 38,400 bytes, two-record nextxy.bin is 76,800 bytes, and each one-record high-resolution float32 or two-record int16 layer is 8,640,000 bytes.

Using the *rectangular map bounds only*, the current GRDC_allocated.csv contains 175 rows and GRanD_allocated.csv contains 373 rows whose allocated coordinates fall in the box. These are candidates for local smoke/regression experiments, not proof that a record intersects the active Mekong river cells or is scientifically suitable. The MLIT level and flow tables have zero coordinates in the box. The map is therefore structurally capable of running all three algorithms, but cannot provide a realistic MLIT case; all real-data tests depend on external data and usage rights.

## 5. Git tracking and reproducibility recommendation

| Dataset class | Recommendation | Reason |
| --- | --- | --- |
| GRDC-derived station records | **D — external pending exact redistribution permission** | Small enough for Git technically, but official GRDC terms prohibit redistribution of downloaded data; derived station tables have no documented permission/provenance. Use fabricated synthetic fixtures in CI. |
| GRanD-derived dam records | **D — external pending exact redistribution permission** | The legacy table appears to cover GRanD v1.3 globally. A regional subset remains derivative data and does not avoid original restrictions. |
| MLIT-derived station tables | **C for CI; external until provenance/attribution is recorded** | Official reuse terms appear permissive with attribution and third-party-right caveats, but this specific transformed file lacks processing/source metadata. Mekong has no MLIT stations in its bounds. |
| Synthetic test records | **C — commit small, fabricated fixtures** | Enables deterministic automated tests without redistributing third-party station or reservoir records. Mark all coordinates, IDs, names, and drainage areas as synthetic. |

Do not use Git LFS or commit a real geographic subset at this stage. If the maintainer later confirms redistribution rights, revisit full-versus-subset storage, provide provenance/citations and licensing notices, and consider repository-history growth. The Mekong base map can be used for tests that operate only on its already tracked map files; tests should not overwrite the committed sample.

## 6. Proposed v5 organization and sharing boundary

**Proposal, consistent with approved project architecture:**

- Keep the historical implementations unchanged under map/src/src_param/.
- Place migrated sources and executables in map/tools/code/ with names allocate_flow_gauge.F90, allocate_dam.F90, and allocate_level_gauge.F90 and corresponding allocate_* executable names.
- Put map-local wrapper templates in map/tools/script_params/; do not add script_allocate/.
- Extend the shared Makefile with an explicit allocation target/family. Today it auto-discovers param_*.F90 but not allocate_*.F90; the target/build contract must be specified before adding these programs.
- Expose one clear data-directory configuration in wrappers, but keep scientific settings and algorithm branches in Fortran. Preserve map-local working-directory behavior unless the maintainer approves a different path interface.

**Potentially shareable, after baseline tests:** common params.txt and map-field loading; high-resolution location.txt parsing and field loading; conversion of geographic coordinates to pixel indices; upstream coarse-cell candidate indexing; tracing through relative high-resolution downstream offsets; and structured error/report helpers. The three selection algorithms should remain separately readable. In particular, do not unify flow multi/single selection, dam area-to-catchment classification, or level-gauge outlet/upstream classification merely because the low-level map arrays are alike. Any module source placed in map/tools/code/ follows the local mod_ filename convention; that convention does not apply to legacy map/src/.

The current tools accept only one high-resolution area and hard-code the 1min/15sec fallback. Proposal: preserve the one-area behavior for the first parity migration and fail clearly for unsupported multi-area input, while making high-resolution selection visible/configurable in the new wrapper. Supporting tiled high-resolution mosaics should be a separate decision and validation increment, because lookups across area boundaries can change selected pixels and path traversal.

## 7. Regression, structural, and scientific validation strategy

1. **Structural and I/O tests:** synthetic small map arrays and fabricated input records; check required fields, dimensions, out-of-domain behavior, malformed/zero-area rows, sentinel values, output columns/order, and no mutation of inputs. Include CSV edge cases because the current reader is list-directed, not a general CSV parser.
2. **Legacy regression:** execute legacy and v5 tools against disposable copies of the same map and allowed inputs. Compare allocation status, selected coarse indices, area/error fields, classifications, distances/elevations, and output record order. Define floating-point comparison rules for computed values; do not assume whole-file byte identity across compilers or platforms. Cover single and multi, deterministic score ties, unmatched candidates, and dateline logic where relevant.
3. **Mekong integration:** use its tracked base and one-area high-resolution layers with synthetic stations/reservoirs placed at known active catchments. It can cover map I/O and path traversal for all three programs. Do not use repository-ignored global GRDC/GRanD/MLIT files in routine GitHub CI.
4. **Classification edge cases:** dams just above/below 0.3 * ctmare, candidate area residuals around 20%, >2x fallback, and the 10% upstream cutoff; level type 1/2/3, outlet path length and sentinels; flow mode limits and 5%/10% thresholds. These tests freeze legacy behavior, not endorse threshold rationale.
5. **Scientific validation:** separately compare selected gauges/dams and level-gauge placement with independently reviewed station metadata, river topology, and domain-specific evidence. A passing regression or a generated output does not establish scientific correctness.

No allocation tests were run in Phase A; this is a proposed sequence, not an implementation validation report.

## 8. Knowledge gaps and maintainer questions

These are recorded as unresolved rather than silently interpreted. **Blocking** means the dependent implementation decision should wait; unrelated work can proceed.

| Priority / status | Trigger and source evidence | Question and why it matters | Work that can proceed |
| --- | --- | --- | --- |
| **Blocking before realistic-data distribution** | GRDC portal terms prohibit public redistribution; GRanD source terms restrict redistribution; transformed local files lack embedded permissions/provenance. See data table above. | Are the exact *_allocated.csv derivative tables approved for public GitHub/package redistribution, under what attribution, and from which source snapshots? A subset is still derivative. | Implement algorithms and synthetic tests; keep realistic inputs external. |
| **Blocking before selecting high-resolution API** | All three programs read one location.txt area, choose 1min/15sec implicitly, and do not traverse multiple areas. | Should first v5 parity scope explicitly support only one high-resolution area with user selection, or must it support tiled high-resolution regions from the start? This affects APIs, I/O modules, and substantial test coverage. | Inventory and synthetic tests for the existing one-area path. |
| **Important scientific question** | Flow lines 278–318/361–409; dam lines 282–324/351–445; level lines 284–329/349–410. | What was the rationale for the 7x7 search, 5% candidate cutoff, 0.02 pixel-distance penalty, score cap 1000, top-eight upstream list, 10%/20%/30% cutoffs, and “more than twice” fallback? Preserve them for strict parity unless a later approved task changes them. | Mechanical migration and regression can proceed if thresholds are treated as frozen behavior. |
| **Important data/interface question** | t03 points to absent MLIT_allocated.csv; package has MLIT_LevelGauge_allocated.csv with the first four positional fields expected. | Is MLIT_LevelGauge_allocated.csv the intended t03 input, and what source/processing date and attribution should accompany it? | Build level-gauge reader/wrapper around an explicit synthetic fixture; do not silently relink the real table. |
| **Important semantics question** | Flow output Type column is populated with number of upstream substitutes (snum), while level-gauge Type has mainstem/tributary/small-stream classes. Dam wrapper describes small as “not on mainstem,” but code uses area thresholds. | Confirm preferred names/definitions for output classification fields, and whether dam output should be documented by its area-based legacy criterion rather than mainstem terminology. | Preserve source output values and names during first port; correct new documentation based on source behavior. |
| **Documentation / provenance question** | CSVs have allocated/original fields and multiple candidate workbooks, but no repository preparation script or manifest was found. | Which workbook/version and manual edits produced each allocated table? Is GRanD_alloc+Mekong.csv an old alternate allocation or an independently prepared product? | Record observed schema and keep all datasets external. |

The current Phase A research gap register links these questions to source and records; see research/records/knowledge_gaps/ and the matching rows in research/knowledge_gaps.csv.

## 9. Proposed Phase B sequence

1. **B0 — decision checkpoint:** settle the high-resolution area support/API boundary, confirm the intended MLIT level input, and define external data handling. Agree on baseline compiler/options and output comparison rules. Do not change any threshold.
2. **B1 — synthetic characterization:** make tiny synthetic map/input fixtures and tests for the three existing behaviors, including malformed/out-of-domain records and threshold boundaries. If legacy tools cannot consume the synthetic fixture without assumptions, document that first; do not change legacy code to force it.
3. **B2 — shared neutral map input layer:** port map readers, coordinate geometry, and high-resolution path helpers into map/tools/code/ only after the fixtures can verify each piece. Keep map-local relative paths or explicitly approve an alternate interface.
4. **B3 — flow gauge:** migrate as a separate allocate_flow_gauge program; cover single and multi; compare against legacy on synthetic cases and any permitted local reference data.
5. **B4 — dam allocation:** port its area-based sub-grid and upstream rules independently; clarify outputs and error-file handling; do not reinterpret “mainstem.”
6. **B5 — level gauges:** port elevation/path-distance reporting and type classification; confirm the MLIT input table before using it.
7. **B6 — docs and reproducibility:** document each wrapper's input schema, map prerequisites, output fields/units/sentinels, data provenance, and manual review expectations. Add only redistributable fixtures to CI. Conduct independent scientific validation as a separately scoped activity.

Every milestone should re-run the knowledge-gap checklist. Any proposed numerical or interface change requires explicit maintainer approval under AGENTS.md.
