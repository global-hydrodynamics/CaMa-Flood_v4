# Common Map Metadata Specification

## Purpose and authority

`variable_table/map_variables.csv` is the shared, machine-readable definition of CaMa-Flood map variables and binary products. It is intended to describe variables across map resolutions and regional domains, not to define one dataset's dimensions.

This document defines the CSV columns, records the design choices, and explains how the shared variable definitions relate to metadata files distributed with a map. The CSV is the primary source for variable short names, proposed long names, units, data types, symbolic dimensions, binary record positions, missing conventions, distribution roles, and model-reader/NetCDF variable mappings. Do not copy its full variable table into another README or manual.

Dataset-specific dimensions, bounds, and spacing remain in each map dataset's metadata. Input-grid/runoff settings remain in the paired input-grid descriptor and input matrix: v5 uses `inpdim_<ROFFTAG>.txt`; unconverted v4 package maps may retain `diminfo_<ROFFTAG>.txt`. Neither dataset-specific dimensions nor runoff-grid settings belong in the common variable definition.

## CSV format and row model

The CSV uses UTF-8, a header row, and one row per logical variable or traceable data product. Multi-variable files such as `nextxy.bin` have one row per variable, with a repeated filename and a per-row record index. When the same variable is available in multiple files, record one row per file. Non-raster tables and source-generated outputs may also be recorded, with their real layout stated rather than forced into `NX,NY`.

Dimensions use symbolic names and Fortran array order where verified, such as `NX,NY`, `NX,NY,NLFP`, or `NX_HIRES,NY_HIRES`. Use `N/A` for non-spatial products and `unknown` when the shape is unresolved. `record_index` and `record_count` summarize positions and counts; `record_layout` adds a concise file-level structure where useful. Text metadata rows do not need a verbose record description when the format is documented elsewhere and no tool consumes it. Blank cells mean “not specified in this inventory,” not zero, dimensionless, or absent. Evidence paths are repository-relative; citations may be URLs.

The column order is intentional. Fields before `usage` are the concise, human-facing inventory; `record_index` and `record_count` appear before `record_layout` for quick scanning. `usage` and the columns to its right are supporting workflow, provenance, and verification notes, and may contain more detail. CSV consumers use column names, so the display order does not define processing order.

For high-resolution GrADS CTL output, the generator removes a terminal `_hires` from the registry `name` when writing the variable declaration. The suffix can distinguish high-resolution products in the shared registry, while the shorter GrADS variable name is easier to use in plotting commands. The registry value itself is unchanged.

### Categories and sorting

`category_id` is the first column and numeric sorting key. IDs are organized to follow the map-product workflow where practical: 10–19 cover products distributed by default or produced during regionalization, and 900–999 cover high-resolution products. IDs from 20 onward are being organized as the parameter and auxiliary workflows are inventoried. The scheme remains provisional; IDs group products and are not intrinsic properties of map variables. The table below records current assignments in `map_variables.csv`; it is not a reservation of unassigned IDs.

`sub_id` is a separate functional-role label. It does not encode processing order and must not be combined with `category_id` (for example, as `21A`). The currently approved values are:

| Value | Meaning | Classification evidence |
| --- | --- | --- |
| `A` | Model input: data read during CaMa-Flood execution, including supported alternative inputs that can be selected or configured. | Trace the model read/configuration path; being a default input is not required. |
| `B` | Supporting data: map-preparation intermediates, diagnostics, and analysis data not directly read during model execution. | Trace the preprocessing, diagnostic, or analysis use. |
| `C` | Opt-in output: data emitted only when a user explicitly requests an optional output or diagnostic. | Verify the output is gated by an option, argument, or equivalent explicit request; this label does not itself imply runtime use. |

Assign `A`, `B`, or `C` only when evidence supports the role. Do not classify from a filename or producer alone. If repository evidence does not establish the role, leave `sub_id` blank and report the gap. A product used by preprocessing is not automatically a model input, and an opt-in output is not necessarily runtime input. Preserve existing maintainer classifications; when inspected code appears to conflict, record the evidence and request maintainer review rather than silently changing the value. Adding another role requires maintainer approval.

| ID | Group / current category labels | Meaning |
| ---: | --- | --- |
| 10 | `default_metadata` | Default map metadata used to describe map geometry. |
| 11 | `default_rivmap` | Core river-network and topography map products. |
| 12 | `default_rivmap_grid` | Derived products on the base map grid. |
| 13 | `default_rivmap_region` | Products created or updated during regional map preparation. |
| 14 | `default_bifurcation` | Bifurcation products in the default preparation workflow. |
| 15 | `default_inpmap` | Default map-to-input-grid products. |
| 16 | `default_rivmap_supp` | Supplementary products associated with default map preparation. |
| 20 | Unassigned | No current product uses this ID. |
| 21 | `param_cross-section` | Channel-discharge and cross-section parameter products, including outputs of the `s01` workflow and alternate width products. |
| 22 | `bifurcation` | Standard non-MPI bifurcation parameters and basin-connectivity products. |
| 23 | `input_matrix` | User-configured input-matrix products and paired configuration. |
| 24 | `MPI` / `bifurcation` | MPI-specific basin aggregation and region products. `bifcol_mpi.bin` colors aggregated basin IDs independent of process count; optional `bsncol_mpi-N.bin` colors N computational regions. |
| 25 | `river_map_supplement` | Permanent-water area aggregated for post-processing. |
| 26 | `river_map_supplement` | Optional river-network analysis products, including generic tool outputs and the named Mekong examples `subbsn-30000.bin`, `subcol-30000.bin`, and `upbasin-2569002.bin`. |
| 29 | Unassigned in the current registry | No current row uses this ID. |
| 31 | `dam_reservoir` / `gauge_alloc` | Allocation workflow text products, including dam, flow-gauge, and level-gauge outputs. |
| 41 | `inflow_scheme` | Specialized inflow-boundary map variants. |
| 52 | Unassigned | No current product uses this ID. |
| 62–63 | `river_map_supplement` | Supporting or specialized map products. |
| 81 | Unassigned in the current registry | No current row uses this ID. |
| 99 | `netCDF parameter` | NetCDF parameter product records. |
| 900 | `hires_metadata` | High-resolution area metadata. |
| 901 | `hires_data` | High-resolution map layers. |

The CSV columns begin with `category_id`, `sub_id`, and `workflow_stage`, in that order. `workflow_stage` is a sample/workflow status annotation, not an intrinsic property of a map variable or a statement of runtime necessity. The maintainer updates this column manually for review. Codex and other AI agents must preserve its values and must not infer or change `default` from file presence; report suggested status changes to the maintainer instead. Keep all rows for a multi-variable file consistent when the maintainer updates them. This field is not source provenance: record global defaults and regional derivation in `generation_or_processing`.

Sort rows by ascending numeric `category_id`, then `binary_filename` (case-insensitive), then numeric `record_index` with blank indices last. For the human-facing columns, prefer concise labels and structures; put extended provenance and verification notes in the fields from `usage` onward. Preserve all evidence and verification-status fields during recategorization. Use the functional descriptions and evidence to select a group; do not classify from a filename alone. Treat `workflow_stage` as a temporary sample/workflow-status annotation; never use it to define the product or general workflow.

### Columns

| Column | Meaning |
| --- | --- |
| `category_id` | Provisional numeric grouping/sort key from the table above; it is the first CSV column and may be revised as the inventory develops. |
| `sub_id` | Functional role independent of `category_id`: `A` for model-execution inputs, including configurable alternatives; `B` for preparation intermediates, diagnostics, and analysis data; `C` for outputs emitted only on explicit request; blank when evidence is insufficient. `C` describes output selection, not runtime requirement. Verify actual read/write paths and preserve maintainer assignments pending review of conflicts. |
| `workflow_stage` | Provisional sample/workflow status immediately after `sub_id`. `default` means the matching product file is present in the current Mekong sample; it does not mean the product is required by the standard simulation or distributed in every map. Blank means no status is confirmed. Keep all rows for a multi-variable file consistent. |
| `category` | Descriptive product subgroup within the broad functional group identified by `category_id`. |
| `product_kind` | One of five data-product classes: `binary_map` for one 2-D map variable; `binary_mult` for a binary file containing multiple variable arrays or a 3-D/multilayer array; `text_meta` for descriptive metadata/configuration text; `text_table` for tabular text such as station records; or `netcdf` for a named NetCDF variable. NetCDF dimensions use symbolic names and follow CDL order. Repeat the file-level kind on every variable row for that file. The `dtype` column records each variable's actual storage type or encoding. |
| `binary_filename` | Primary filename or logical output filename; it may be a pattern such as `bifprm-<N>.txt`. |
| `name` | One variable's short name for GrADS or the corresponding product field. Keep one name per row; represent a multi-variable file with one row per variable and repeat `binary_filename`. Do not put slash-separated names in this column. |
| `long_name` | Concise English label proposed for human and future NetCDF use. Blank when semantics are unresolved. |
| `physical_description` | Concise physical or functional meaning, with only essential assumptions or uncertainty. Put code legends in `special_values`, provenance in `generation_or_processing`, and file structure in `record_layout`; do not repeat those details here. Do not infer meaning from a filename alone. |
| `units` | Verified or proposed unit string. Blank does not imply dimensionless. |
| `dtype` | Data type or encoding, using the compact values present in the registry, such as `int8`, `int16`, `int32`, `float32`, `text`, `CSV`, `NetCDF`, or `unknown`. |
| `dimensions` | Symbolic array shape such as `NX,NY`, `NX,NY,NLFP`, `NX_HIRES,NY_HIRES`, or NetCDF `path` / `layer,path`; use `N/A` for non-spatial products and `unknown` when unresolved. Do not hardcode one map's concrete dimensions here. |
| `record_index` | One-based record or variable-block index for this row. For input matrices, the order is `inpx`, `inpy`, `inpa`; each block contains `INPN` levels. Blank when no position is established or needed. |
| `record_count` | Total records/variable blocks in the file (for example, `2` for `nextxy.bin`, `3` for an input matrix, or `NLFP` for `fldhgt`). |
| `record_layout` | Concise file-level record/block arrangement. The CTL generator parses recognized layered (`fldhgt`) and input-matrix layouts; ordinary per-variable records are ordered by `record_index`. Keep text-metadata layouts brief or blank when no current tool consumes them; document detailed text formats in their component documentation. |
| `missing_value` | One numeric sentinel stored in the original binary variable. Blank means the stored sentinel is not established; it does not mean that the data are known to have no missing value. This field records current binary data, not an automatic NetCDF `_FillValue`/`missing_value` policy. |
| `special_values` | Human-readable meanings for other meaningful codes, such as river mouths, boundary outlets, sinks, and categorical values. Do not repeat the missing sentinel here or treat these codes as missing. |
| `usage` | Short workflow role, such as runtime input, preprocessing input/output, visualization, or analysis. |
| `runtime_requirement` | Whether runtime use is core, feature-conditional, MPI-conditional, preprocessing-only, or unknown. |
| `distribution_status` | Intended distribution/generation role; see controlled values below. |
| `sample_availability` | Package evidence only: semicolon-separated inspected map tags (`glb_15min`, `conus_06min`, `mekong_15min`, `tej_01min`, `data`), `not_found_in_inspected_v4.30_map_package`, or unknown. This is independent of distribution status. |
| `generation_or_processing` | Original provenance, in-repository producer/transformer, or explicit statement that it was not found. Regional extraction is not original global generation. |
| `model_reader` | Verified model read routine and source path; blank where not traced. |
| `netcdf_variable` | Existing model-reader container and variable lookup, if any; not a proposed final NetCDF grouping. |
| `verification_status` | Controlled confidence state (below). |
| `evidence_ref` | Source, manual, package observation, maintainer confirmation, or citation supporting the row. |

### Controlled statuses

- `standard_distributed_core`: core river-network/topography dataset product described as supplied in standard map packages. Check the selected archive; this does not assert that every release or map tag contains every file.
- `standard_distributed_support`: standard supporting product distributed with map datasets but not a core runtime river-network/topography layer. The selected map archive still determines whether a particular product is present.
- `map_specific_user_generated`: channel parameters intended to be generated or checked for a target map. Some packages include prepared examples.
- `optional_or_auxiliary`: ancillary, specialized, derived, analysis, or workflow products; not implied to be standard runtime requirements.
- `source_only_generated`: traceable source output not found in the inspected v4.30 map packages.
- `package_example_or_specialized`: observed package product whose distribution as a general standard is not established.

`verification_status` values in this draft are `verified_source_and_sample`, `verified_source`, `maintainer_confirmed`, `package_observed`, and `provisional_semantics`. `verified_source_and_sample` means the source-derived definition and sample presence have both been checked; it does not mean scientific suitability or correctness has been validated. `maintainer_confirmed` records a maintainer-provided definition that the inspected source only passes through or does not establish independently; it does not imply scientific validation. The `workflow_stage` field separately records the current sample status described above. `provisional_semantics` means product presence or structure may be known while producer, meaning, format, or use remains unresolved. Blank/unknown semantic fields remain intentionally unresolved.

River-network analysis products can use sample-specific filenames to record the parameter that produced them or the selected station identity. The reusable subbasin program and template retain `subbsn.bin` and `subcol.bin`; the Mekong sample wrapper uses `subbsn-30000.bin` and `subcol-30000.bin` to identify its 30,000 km2 example threshold. The station wrapper uses `upbasin-<GRDC_ID>.bin`; its current Mekong sample is `upbasin-2569002.bin`. Each concrete filename intended for automatic CTL generation has its own registry row. Short variable names remain `subbsn`, `subcol`, or `upbasin`; filenames do not change variable identity. These products are optional analyses and are not model runtime inputs.

The fields deliberately separate two questions: `distribution_status` describes the product's role in the workflow, while `sample_availability` records where it was actually observed. Package observations do not prove universal distribution. A source program that writes a file does not establish that a corresponding product is shipped. These distinctions should be preserved when adding entries.

The v5 bifurcation workflow distinguishes regular supporting output from optional diagnostics. `bifcol.bin` is generated on every standard run because it is routinely used to visualize the merged basin map. With `check_map`, `param_bif_set_params` writes `check_bif_dphwth.bin` (float32 records `bifdph`, then `bifwth`), and `param_bif_set_basin` writes `check_bif_bsnmod.bin` (int32 records `bifmod`, then `bpoint`). Each diagnostic file has two `NX × NY` direct-access records and is not a model runtime input. The corresponding registry rows use `sub_id=C` to mark these outputs as opt-in: the programs emit them only when `check_map` is requested. This is distinct from `runtime_requirement`, which records whether a product is consumed during model execution. The v5 basin utility no longer writes separate `bifmod.bin` and `bpoint.bin`; those names belong to the unchanged v4 workflow. The legacy v4 `set_bifparam.F90` continues to write `bifdph.bin`, while v5 does not create or replace it. `biftag.bin` is excluded from the v5 product registry; any package or legacy occurrences remain untouched. The registry uses two variable rows per multi-record diagnostic file, preserving record indexes and order.

The v5 MPI basin-aggregation step writes `bifcol_mpi.bin`, a visualization of the MPI-aggregated basin IDs. It is created by `param_mpi_set_basin` independently of the requested region count N; its colors may vary with `normal` versus `MaxMPI` aggregation mode. The subsequent partition step writes one persistent model input, `mpireg-N.bin`, for each configured count. Each is a single int32 `NX × NY` direct-access record; `-9999` marks cells outside an assigned MPI region. No unnumbered `mpireg.bin` alias is written. The standalone `analysis_mpi_region_color` reads a selected `mpireg-N.bin` and `params.txt`, then writes a one-record int32 `NX × NY` `bsncol_mpi-N.bin` display-color map for its region IDs. Its `-9999` cells are outside assigned regions; values 1–10 are visualization indices, not MPI rank IDs. This optional non-runtime utility is run manually for any existing numbered partition; the s04 wrapper does not invoke it. The v5 serial and MPI preprocessing consumers use `bifprm.csv`; the current simulation reader still uses legacy `bifprm.txt` until a separate model-side migration. The visible `bifbsn_mpi.tmp` is an intermediate basin-group raster removed after partition generation and is not a registry product. The registry distinguishes `bifcol_mpi.bin` (MPI-aggregated basins, independent of N) from `bsncol_mpi-N.bin` (optional visualization of the N-region map). The v5 registry does not catalog v4-only products such as separate `bifbsn_mpi.bin`, `bifmod_mpi.bin`, `bpoint_mpi.bin`, `bifprm_mpi.txt`, or `bifprm-<N>.txt`. The unchanged historical implementations remain under `map/src/`.

With the explicit `check_map` argument, the v5 `param_mpi_set_basin` writes `check_bif_bsnmod_mpi.bin` before assigning individual MPI regions. It contains two int32 `NX × NY` direct-access records: `bifmod_mpi` (record 1) classifies original basin cells as unchanged (`0`), absorbed (`1`), or expanded by merging (`2`); `bpoint_mpi` (record 2) classifies both endpoints of original inter-basin pathways that remain between aggregated basins (`1`), were incorporated as a floodplain path (`2`), or as a river-channel path (`3`). Both records use `-9999` for not-applicable cells. The output is optional, not required for model execution, and is not created or overwritten without `check_map`. Its classifications can change with the `normal` versus `MaxMPI` aggregation mode, but not with the later MPI region count. This pre-partition aggregation diagnostic is distinct from `analysis_check_mpi_bifurcation`, which evaluates pathway connectivity for a selected `mpireg-N.bin`; that report depends on the partition. The standalone analysis wrapper can optionally write a dropped-path CSV.

## Bifurcation parameter tables

The v5 parameter workflow writes `bifprm.csv` from `bifori.txt` after the configured `BIFLAYER` path filter and existing parameter calculations. It preserves the existing retained row order; the standard generator has no separate path sort. `pth_id` is a consecutive one-based row index for that particular map configuration, not a permanent global path ID. Match paths across configurations by `(ix,iy,jx,jy)`. `start_lat/start_lon` retain the pathway-source coordinates; `end_lon/end_lat` come from the unit-catchment outlet-coordinate records 1/2 of `lonlat.bin` at `(jx,jy)`. The `bifori.txt` sample header labels its final coordinate pair `(lon,lat)`, while the existing reader assigns the values as `(lat,lon)` and the generated `bifprm.txt` confirms that legacy interpretation. The v5 conversion preserves that established interpretation; the source producer/header discrepancy remains unresolved. The basin identifiers `ibsn/jbsn` are stored in ascending numeric order, not as directional endpoint labels.

The independent `map/tools/code/convert_bifprm_csv_to_netcdf.py` converts the finalized CSV without filtering, recalculation, or reordering. `bifprm.nc` has static `path` and `layer` dimensions; per-path fields use `path`, while `width` is `(layer,path)`. Numeric variables are int32 or float32 and use shuffle/deflate level 3. The current simulation reader still requires legacy `bifprm.txt`; the new map products are not yet direct simulation inputs. v5 map consumers under `map/tools/` use the CSV. The legacy v4 tools and current model reader remain unchanged pending their separate migrations.

## Maintenance rules

When changing a map reader, writer, producer, or map-related model interface:

1. Search for the product in the source tree and inspect its actual inputs, outputs, types, records, and dimensions.
2. Update this CSV for introduced, changed, or removed products. Keep package distribution, package sample availability, and user-generated/derived status distinct.
3. Mark unresolved semantics or layouts as provisional; do not fill gaps by guessing from filenames.
4. Update the responsible module README for workflows or assumptions a human user needs. Keep detailed variable metadata in the CSV rather than copying the entire inventory into README files.
5. Check `map/README.md` and `docs/map_data_inventory.md` for references that became inaccurate. Run the CSV parser and verify changed links.

The CSV is an evolving source-linked inventory. Category 11 now has the machine-readable record metadata used by the initial CTL and NetCDF tools. Other categories remain an inventory, not a promise that every field is complete enough for conversion. Dataset-specific byte order, row orientation, sentinel values, coordinate interpretation, and file paths belong in a separate dataset config, not the shared variable rows.

## Development/test repository sample

`map/mekong_15min/` is the official GitHub development/test sample. Its regionalization scripts are maintainer-operated sample-preparation tools used to create or update this distributed sample from source data supplied separately in CaMa-Flood distribution packages. They are not automated tests and do not run in GitHub Actions. Sample inclusion does not establish scientific validation or production suitability. The sample includes the 1-minute high-resolution subdirectory and prepared channel, input-matrix, bifurcation, auxiliary, and MPI-preparation products. Its dimensions and geometry are dataset-specific (`NX=80`, `NY=120`, `NLFP=10`, 0.25°, 90–110°E, 5–35°N); they do not define the general map format.

GitHub-only validation checks the tracked sample's geometry, registered binary types/layout sizes, and base/high-resolution inventory with `map/tests/validate_mekong_sample.py`. `map/tools/tests/test_mekong_ctl_integration.py` invokes the CTL wrapper using a temporary copy of sample inputs, so it never regenerates the tracked CTLs. The Python generator has separate synthetic unit tests. GitHub Actions runs these checks without the distribution package or a model simulation. These checks validate the sample files and CTL metadata/layout consistency, not the river network's scientific correctness.

The package-preparation workflow copies the source archive into the user package, so the tracked `map/mekong_15min/` directory is inherited once; it must not be copied into a second package location.

## Common binary conventions

The current model binary readers use unformatted, direct-access records of `4 × NX × NY` bytes. `JPIM` is a signed 4-byte integer and `JPRM` is a 4-byte real; these are the kinds used by the inventoried map fields. `JPRB` is switchable: 4-byte under `SinglePrec_CMF`, otherwise 8-byte. Arrays are shaped `(NX,NY)` in Fortran, so x is the first and fastest-varying index. Multi-variable/profile files place each field or profile layer in its own record.

- `nextxy.bin`: record 1 is `nextx`; record 2 is `nexty`; each is an `NX × NY` signed int32 array.
- Scalar real files: one `NX × NY` float32 record.
- `fldhgt.bin`: records 1 through `NLFP`, one `NX × NY` float32 profile layer per record.
- `nextx` and `nexty` use Fortran-style 1-based grid indices: positive `nextx` values are 1 through `NX`; positive `nexty` values are 1 through `NY`. Preserve those indices in any conversion. The reference package contains `-9` (river mouth), `-10` (inland termination), and `-9999` (ocean/undefined), as described in the v4.20 manual; runtime sequence construction treats negative non-missing x codes as endpoints.
- The inspected package stores `-9999` in real map cells outside the network. Model constants also include `RMIS=1.E20`, which is not automatically substituted for the binary sentinel by the map reader.

Byte order and row orientation are dataset properties, not properties of a variable. The inspected `glb_15min` CTLs specify `little_endian` and `yrev`; the model's `LMAPEND` controls byte conversion and defaults to false. `params.txt` does not encode either setting. Do not assume every regional or future dataset shares the reference package's binary byte order or orientation.

## Current map tools and model reader contract

The current shared map utilities are under `map/tools/`. `map/tools/code/generate_ctl.py` reads registered binary products from this CSV and combines their variable-level metadata with map geometry from `params.txt` or high-resolution `location.txt`. The category-11 core runtime products are `nextxy.bin`, `ctmare.bin`, `elevtn.bin`, `nxtdst.bin`, `rivlen.bin`, and `fldhgt.bin`; category IDs describe workflow order and are not a file-selection API for every utility.

The CaMa-Flood v4 reader contract is verified in `src/cmf_ctrl_maps_mod.F90`:

- `READ_MAP_CDF` reads `nextx`, `nexty`, `lon`, and `lat` from the `CRIVCLINC` file.
- `READ_TOPO_CDF` reads `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and `fldhgt` from that same file.
- Horizontal variables have Fortran array bounds `(NX,NY)`; `fldhgt` is read one layer at a time into `(NX,NY)` for each `1..NLFP`. This describes the model reader interface, not a binary-to-NetCDF converter layout.
- The 1-D `lon` and `lat` coordinate variables are required by `READ_MAP_CDF`; coordinates are cell centers derived from the dataset bounds in `params.txt`.

The current CTL generator follows the legacy `little_endian` and `yrev` convention and reads geometry from the target map directory. The inspected `glb_15min` CTLs and Mekong sample are reference evidence; byte order and row orientation remain dataset properties and must not be generalized without evidence.

The v5 model now supports individual-file Binary/NetCDF inputs in addition to the legacy consolidated NetCDF reader. `map/tools/code/convert_map_binary_to_netcdf.py` converts supported registered raster files, using the existing `dtype`, `dimensions`, `record_index`, `record_count`, and metadata fields. This implementation establishes a practical individual-file convention for the supported v5 map inputs; it does not define a replacement for legacy consolidated containers or a universal conversion contract for every optional map product.

## Confirmed facts and proposed attributes

The model names in the CSV are established variable names. English `long_name` text and some unit strings are proposed conventions, distinct from attributes verified in existing binary maps. The existing CTLs often have generic labels/titles; they are not an authoritative semantic dictionary. No CF `standard_name` is added where no applicable name has been verified.

- The maintainer confirmed that `elevtn` uses the EGM96 vertical datum; this fact is currently stated in its `physical_description` and evidence, because the CSV has no dedicated `vertical_datum` column. Elevation units are metres.
- `rivman` is the river-channel Manning roughness coefficient. The maintainer previously stated its SI dimensional unit as `s m-1/3`; the current inventory task explicitly directs that the CSV `units` cell remain blank. The field is intentionally blank and does not mean dimensionless. That blank is deliberate and does not mean dimensionless; do not auto-fill it without a revised maintainer decision.
- Core network/topography inputs `nextxy`, `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and `fldhgt` are provided in the standard distributed map package.
- Channel parameters `rivwth`, `rivhgt`, and `rivman` are intended to be generated or checked by users for the target map with tools under `map/src/src_param/`, rather than assumed to be universal inputs. The v4.30 `glb_15min` package contains precomputed copies; this does not make them guaranteed standard files for every map.
- The original global maps were developed in Yamazaki Laboratory. The public methodological overview is [Yamazaki et al. (2011)](https://doi.org/10.1029/2010WR009726). Detailed original production procedures are not public. Repository regional extraction programs are not the original global-map generators.

## Metadata and configuration files

These file types have different scopes. Their structures below are verified from the v4.30 reference examples and repository readers/writers; numeric example values in a particular map must not be mistaken for common definitions.

### `params.txt` — map-dataset geometry

The inspected format has one formatted value per record, often followed by a comment:

| Record | Field | Meaning | Units/scope |
| --- | --- | --- | --- |
| 1 | `NX` | Map columns | count; dataset-specific |
| 2 | `NY` | Map rows | count; dataset-specific |
| 3 | `NLFP` | Floodplain profile layers | count; dataset-specific |
| 4 | `GSIZE` | Regular map-grid spacing | degrees in the inspected geographic maps; dataset-specific |
| 5 | `WEST` | Western map edge | degrees in inspected geographic maps |
| 6 | `EAST` | Eastern map edge | degrees in inspected geographic maps |
| 7 | `SOUTH` | Southern map edge | degrees in inspected geographic maps |
| 8 | `NORTH` | Northern map edge | degrees in inspected geographic maps |

This is map-dataset metadata, not a simulation namelist. Programs such as `generate_inpmat.F90`, regional `set_map.F90`, and `cut_domain.F90` read it; `cut_domain` writes a regional version. The standard model map reader does not open `params.txt`; model array dimensions come from its configured dimensions/`diminfo`.

For a regular geographic grid, source code calculates cell-center coordinates as longitude = `WEST + (i - 0.5) × GSIZE` and latitude = `NORTH - (j - 0.5) × GSIZE`. The inspected `glb_15min` file is 1440 × 720, 10 layers, 0.25°, and global, but those values belong only to that reference dataset.

### `mapdim.txt` — external map-generation parameter

The package example contains three records: `NX`, `NY`, and `NLFP`. The maintainer confirmed that external algorithms use this auxiliary parameter file when generating fundamental river-network maps such as `nextxy`. CaMa-Flood simulation code does not read it. Its dimensions overlap with records 1–3 of `params.txt`, but it serves the external generation workflow and does not replace the richer map geometry file.

The external algorithm's source is not included in this repository. The repository therefore has no in-tree reader/writer for this file; the distributed map package is the reference for its observed three-record representation.

### `inpdim_<ROFFTAG>.txt` — v5 runoff/input-grid configuration

v5 map workflows pair `inpdim_<ROFFTAG>.txt` with `inpmat_<ROFFTAG>.bin`. `ROFFTAG` identifies the runoff product or input-grid mapping configuration (for example, `test-glb_1deg` or `test-E2O_15min_nc`). It is independent of `HIRES_TAG`, which selects the high-resolution map directory used to generate a mapping, such as `1min` or `15sec`.

The legacy `diminfo_` prefix remains in existing v4 package data and workflows that have not migrated. The v5 CTL generator accepts both `inpdim_` and legacy `diminfo_` companions. The model's namelist/API field remains named `diminfo_file`; that interface name does not dictate the physical descriptor filename. Existing legacy descriptors are retained for backward compatibility while v5 scripts select `inpdim_<ROFFTAG>.txt`.

### Text descriptor format (legacy `diminfo_*.txt` and v5 `inpdim_*.txt`)

The standard sample format contains eleven records:

| Record | Field | Scope |
| --- | --- | --- |
| 1–3 | `NX`, `NY`, `NLFP` | Destination map dimensions/layers; map-specific |
| 4–5 | `NXIN`, `NYIN` | Runoff/input-grid dimensions; input product and domain specific |
| 6 | `INPN` | Maximum input-grid contributors per CaMa-Flood map cell; mapping-specific |
| 7 | Paired input-matrix filename text | Must name `inpmat_<ROFFTAG>.bin` for a v5 pair; the model parser skips this line and callers supply the actual matrix path |
| 8–11 | west, east, north, south map bounds | Destination map domain, in degrees for the inspected geographic grids |

The core run configuration parser reads the dimensions, input-grid dimensions, and `INPN`; when `LGRIDMAP` is true (the inspected default), it also reads the four map bounds. The input matrix is a separate binary file, with records grouped as `INPN` x-index records, `INPN` y-index records, then `INPN` float32 overlap-area records, each covering `NX × NY`.

The file combines destination-map geometry with runoff/input-grid configuration. It does not by itself specify runoff-grid spacing, source bounds, longitude convention, or row order; the shell workflow/input dataset provides those. `generate_inpmat.F90` reads the destination grid from `params.txt`, calculates `NXIN`/`NYIN` from configured input bounds and spacing, and writes the descriptor plus paired matrix. Thus `params.txt` and the descriptor repeat some map dimensions/bounds but have distinct roles. v5 `param_inpmat_generate` derives both filenames from `ROFFTAG`; `HIRES_TAG` is passed separately to select high-resolution lookup data.

### GrADS `.ctl` files — binary visualization metadata

CTL files are read by GrADS to describe a binary dataset. The CaMa-Flood binary model readers inspected here do not read them.

- Structural interpretation fields include `dset`, `xdef`/`ydef`, `zdef`, `vars` and record order, and storage `options` such as `little_endian` and `yrev`.
- `undef` tells GrADS which value to treat as missing. The generator reads the numeric `missing_value` column. For a blank field it uses a type-specific provisional fallback and reports the affected file; this does not update the registry or confirm that fallback scientifically.
- `title`, variable names, and comments/descriptions are descriptive. They do not establish correct physical meaning or units. The inspected package uses the generic title `Flow Direction` even for scalar variables, and has `rivman.bin` without a same-directory `rivman.ctl`.

The CSV supplies variable names, proposed long names/units, data types, symbolic dimensions, the numeric stored `missing_value`, descriptive `special_values`, and binary record layouts. `special_values` remain in the registry; the generator does not append them to CTL variable descriptions. Dataset geometry is read from `params.txt` for base-map files and from the selected high-resolution directory's `location.txt` for its files. Neither geometry file is duplicated in the CSV.

The legacy `wrte_ctl_map.F90` and `wrte_ctl_hires.F90` writers both emit `options yrev little_endian`; both start `xdef` and `ydef` at the southwest cell center (`west + spacing/2`, `south + spacing/2`). `yrev` accounts for the north-to-south row order of the Fortran direct-access arrays. The v5 generator follows this established convention for current CaMa-Flood workflow products and does not detect other byte orders or row conventions.

For blank `missing_value` entries, the v5 generator uses provisional CTL-only defaults: `float32=-9999`, `int32=-9999`, `int16=-9999`, and `int8=-9`. It prints a warning per affected binary and includes a fallback count in the summary. Explicit numeric values must be finite, integral for integer types, and in range for the declared signed storage type. Logical variables in the same binary must resolve to the same missing value; otherwise CTL generation skips the incompatible file. A default is a practical visualization assumption, not a verified binary convention; fill known values into the registry only after verification.

GrADS' documented `-1,40` non-float formats include 1-byte unsigned chars (`-1,40,1`), 2-byte unsigned (`-1,40,2`), 2-byte signed (`-1,40,2,-1`), and 4-byte integers (`-1,40,4`). No signed 1-byte format is documented in the referenced variable-format table. CaMa-Flood `int8` files are signed, so CTLs use the unsigned-byte reader and convert a signed missing sentinel such as `-9` to its byte value `247` in `undef`. Binary bytes are not changed. This allows GrADS to mask that byte value but means other negative `int8` codes are displayed as unsigned values (for example, `-1` appears as `255`); use a signed-aware reader for their original scientific code values. See [GrADS Variable Formats](https://sources.debian.org/data/main/g/grads/3%3A2.2.1-1/doc/variableformats.html) and [GrADS Data Descriptor File: non-float data](https://sources.debian.org/data/main/g/grads/2%3A2.0.2-3/doc/descriptorfile.html#VARS).

The current `map/tools/code/generate_ctl.py` reads the common CSV, scans only the selected directory, validates supported layouts and binary sizes, and writes CTLs beside supported binaries. For high-resolution files it reads `location.txt`, whose first record is the area count, second is the field-name header, and following rows give `iarea`, `carea`, bounds, `nx`, `ny`, and `csize`; `<AREA>` in a registry path is matched to `carea`. It recognizes the layered `fldhgt` layout, input-matrix blocks, and independent per-variable records. It applies per-type defaults with warnings for blank sentinels and rejects conflicting resolved missing values across logical variables in a file.

## Channel-parameter generation workflow

The repository provides tools for deriving channel geometry for a target map. They are separate from the distributed core river-network maps.

1. **Runoff sample:** the repository tracks `map/data/ELSE_GPCC_clm-1981-2010.one`, a 360 x 180, single-record little-endian binary with runoff in mm/s according to its CTL. The CTL's variable comment calls it "daily runoff climatology," while the file has one record and the standard channel workflow uses it as annual-mean runoff; this is an existing metadata inconsistency. The package manual identifies the standard climatology as ELSE_GPCC (Kim et al., 2009). The maintainer directed that this sample be tracked for reproducible channel-tool testing; upstream terms for other ELSE_GPCC products or derivatives have not been established. A source checkout can run the channel regression with the tracked sample.
2. **Annual mean discharge:** the v5 [`s01-channel_params.sh`](../map/tools/script_params/README.md) runs `param_channel_calc_outclm` with binary runoff and `inpmat` interpolation. The migrated `param_channel_calc_outclm.F90` uses the runoff grid metadata and existing standard input matrix, maps runoff to map catchments, and accumulates flow downstream using `nextxy.bin`; it also reads `ctmare.bin`. It writes/replaces `outclm.bin`, one direct-access float32 record of mean discharge in m³/s. Binary input is used by the supplied wrapper; the source retains optional NetCDF input support when built with the corresponding macro and libraries.
3. **Empirical channel parameters:** the v5 script passes its configurable coefficients and minima to `param_channel_calc_rivwth`. The formulas are `H = max(HMIN, HC × Q̄^HP + HO)` and `W = max(WMIN, WC × Q̄^WP + WO)`, where `Q̄` is mean discharge. The standard wrapper values are `HC=0.1`, `HP=0.50`, `HO=0`, `HMIN=1`, `WC=2.50`, `WP=0.60`, `WO=0`, and `WMIN=5`; they are preserved from the legacy sample workflow, not universal constants or executable defaults. The program writes/replaces power-law `rivhgt.bin`, `rivwth.bin`, `rivman.bin`, and `rivhgt_inf.bin`. It assigns `rivman=0.03` on network cells and `-9999` elsewhere.
4. **MERIT Hydro width merge:** v5 `param_channel_set_merit_width` reads power-law `rivwth.bin`, MERIT Hydro width `width.bin`, and `nextxy.bin`, then writes the separately named `rivwth_merit.bin`. It preserves the legacy width-merging rules and thresholds; it does not replace `rivwth.bin` or `rivhgt.bin`. The v4 `set_gwdlr.F90` remains unchanged and continues to write `rivwth_gwdlr.bin`. The current model reader selects `rivwth.bin` and does not automatically select the v5 alternate output.
5. **Workflow scope and locality:** the v5 `s01-channel_params.sh` runs only these three operations in order: annual mean discharge → empirical channel parameters → MERIT Hydro width merge. It runs from map-root CWD and may overwrite the listed products. Bifurcation, permanent-water, and MPI preparation are assigned to separate workflow scripts. The historical `calc_outclmday.F90` daily/maximum-30-day approach is retained only under `map/src/`; the maintainer reports later evaluation favored annual mean runoff. This migration does not scientifically revalidate either method.

### Permanent-water area

The v5 `script_params/s05-permanent_water.sh` migrates `map/src/src_param/calc_prmwat.F90` to the shared `map/tools/code/param_water_calc_prmwat.F90` program. It reads base-map `params.txt`, `nextxy.bin`, and `ctmare.bin`, then processes the non-tiled high-resolution areas listed in `<HIRES_TAG>/location.txt` using `catmxy`, `rivwth`, and `flddif`; `grdare` is optional. The generated `prmwat.bin` is one float32 `NX x NY` direct-access record in m2. It represents estimated permanent-water surface area aggregated per unit catchment, not a high-resolution mask. Outside-network cells contain `-9999`; valid network cells with no selected pixels contain 0. The present CaMa-Flood simulation core does not read this product; it is intended for post-processing such as excluding permanent water when estimating newly inundated area.

The `flddif` cutoff keeps `prmwat` within terrain where CaMa-Flood can reproduce inundation. A permanent lake isolated high on a hill is outside the modeled flood extent; counting it would incorrectly subtract it as pre-existing simulated floodwater during post-processing. The maintainer described the intended criterion as `flddif < 5 m`. Positive `rivwth` marks channel-centerline width and `rivwth == -1` marks water-surface pixels outside the centerline. The legacy v4 and migrated v5 implementations select valid catchment pixels with `(rivwth > 0 OR rivwth == -1) AND flddif <= 5 m`, so exactly-5 m pixels remain included in the current code. This strict-versus-inclusive boundary is unresolved and the code was not changed. The numerical derivation of the 5 m value also remains undocumented. The legacy comparison accepts `flddif=-9999`; the v5 program warns but preserves that behavior. Selected high-resolution pixel areas (km2) are converted to m2, accumulated by catchment, and capped at the corresponding `ctmare` value. When `grdare` is absent, the legacy `rgetarea` fallback remains: it estimates a pixel's area from a rectangle 10 times larger in both dimensions and divides by 100. The maintainer identified this scaling as a way to reduce round-off fluctuations at very fine resolutions; its numerical accuracy has not been independently quantified. This migration does not claim scientific validation from successful generation or sample regression. See [`script_params/README.md`](../map/tools/script_params/README.md#permanent-water-area) and the `prmwat` row in [`map_variables.csv`](map_variables.csv).

The script also proceeds to bifurcation and ancillary products after channel parameter generation; these are outside this initial variable metadata scope.

### Existing partial NetCDF output

Some NetCDF-writing code already exists in legacy `calc_rivwth.F90` and its migrated v5 copy `param_channel_calc_rivwth.F90` when built and invoked with `type=cdf`. It writes `rivpar.nc` with `lon`, `lat`, `rivwth`, and `rivhgt`, but not `rivman`. It sets NetCDF `missing_value=1.e20` while the arrays contain `-9999` outside the network. This is a partial utility output and is distinct from the module-level `CRIVCLINC`/`CRIVPARNC` files expected by the model reader. **Maintainer decision:** defer adding `rivpar.nc` to `map_variables.csv` until the NetCDF map product design is considered; do not treat this legacy utility output as a finalized map product or infer its future container/layout from the current writer.

The maintainer confirmed that the module-level files were previously generated at ECMWF, are absent from the current distribution, and that code to reproduce this generation workflow is planned. Do not infer a finalized file grouping, dimension order, coordinate convention, or missing-value policy from the current partial writer.

## Future use and limitations

### GrADS CTL generation

The v5 generator is implemented at `map/tools/code/generate_ctl.py`. It combines variable metadata from `map_variables.csv` with dataset-specific geometry read from `params.txt` or `location.txt`. The CSV describes reusable products and record layouts, not sample-map dimensions. It accepts explicit independent-record layouts (including one row per variable with record indices), the documented `NLFP` layered-variable layout, and the regionalization input matrix paired with its dimension description. In a v5 input-matrix filename pair, the shared suffix (for example `_test-glb_1deg`) identifies the runoff-data/configuration variant: `inpmat_<ROFFTAG>.bin` pairs with `inpdim_<ROFFTAG>.txt`. The generator also accepts legacy `diminfo_<ROFFTAG>.txt` descriptors for existing maps. The description supplies `NX`, `NY`, `INPN`, bounds, and the paired binary filename; map dimensions and bounds are checked against `params.txt`. A same-basename `.txt` is also accepted. Unknown dimensions/types and unrecognized record descriptions are skipped; blank missing values use the per-type fallback with a warning; invalid values, conflicting declarations, and binary size/layout mismatches are rejected. It scans one selected directory at a time and does not recurse.

The tool follows the existing legacy CTL convention `yrev little_endian` and cell-center coordinates. The standard regional input matrix has three logical variables (`inpx`, `inpy`, `inpa`) with `INPN` levels, matching the two integer-index blocks and one floating-point overlap-area block written by `region_default_inpmat.F90`; unused slots initialized to zero are represented by `undef 0`. This is the distributed mapping for the standard annual runoff climatology used in channel cross-section parameter estimation. It is distinct from the general-purpose `generate_inpmat.F90` utility for user-selected runoff grids. The generator does not read the runoff dataset itself. The CTL tool does not infer or validate alternative endianness or row order.

### NetCDF map support

The two model input routes are separate. `LMAPCDF=.TRUE.` selects the legacy consolidated NetCDF containers (`CRIVCLINC`, `CRIVPARNC`, `CMEANSLNC`, and `CMPIREGNC`) and preserves their historical reader path. `LMAPCDF=.FALSE.` selects individual map files, where the `.bin` or `.nc` suffix chooses the format independently for each input; mixed formats are allowed. Individual NetCDF support is limited to map variables handled by the legacy Binary path. ECMWF-specific `elevslope` (`LSLOPEMOUTH`) and `mask_slope` (`LSLPMIX`) remain consolidated-NetCDF-only; enabling either with `LMAPCDF=.FALSE.` causes an explicit initialization error. No individual-file contract or new namelist setting is defined for those special inputs; any future support is a separate task.

The `netcdf_variable` column retains its historical meaning: names looked up by the legacy consolidated reader. It has not been repurposed as an individual-file mapping. In individual mode (`LMAPCDF=.FALSE.`), the reader determines the expected variable from the logical input role, independent of basename: ordinary rasters use their logical names, and the two records of `nextxy` use `nextx` and `nexty`. The converter derives these from the registry's `name` and record metadata; for `nextxy`, record 1 maps to `nextx` and record 2 maps to `nexty`.

The current implementation writes NetCDF-4 files with `lon` and `lat` coordinate dimensions, float32 or int32 map variables, and `(lat,lon)` CDL order for 2-D rasters. `fldhgt` uses `(layer,lat,lon)` and preserves the source layer order. These are presented by the NetCDF Fortran interface as `(NX,NY)` and `(NX,NY,NLFP)`. Longitude centers increase from west to east; latitude centers descend from north to south. Stored missing and special index values are retained and corresponding `_FillValue`/`missing_value` attributes are supplied where the registry specifies them. `nextx`/`nexty` remain 1-based integer indices; no index translation is performed. The Mekong conversion and model-input comparison are documented in `docs/model_source_workflow_plan.md`. Optional groundwater delay, mean-sea-level, MPI, and legacy consolidated inputs are not all runtime-tested; the individual-file slope-option rejection is tested, but the legacy consolidated slope routines remain untested and have source-observed NetCDF-handle defects.

## High-resolution source semantics

FLOW source distinguishes `catmxy=-9999` for sea from `catmxy=-999` for a pixel without an assigned unit catchment. Raw profile-grid initialization distinguishes `catmzz` / `catmz100` values -99 for sea and -9 for unassigned catchments. FLOW `merge_hires` sets profile values to -9 whenever `catmxy <= -900`, collapsing both cases. The v5 regional combiner copies available profile-layer values unchanged and uses -9 as the fill value only when an input layer is unavailable. The Mekong sample confirms this merged behavior: both `catmxy=-9999` and `catmxy=-999` pixels have profile value -9. These are different layers and processing stages, not conflicting encodings. `catmzz` contains 10 profile bands (1 nearest the channel, 10 farthest); `catmz100` contains 100 ranks and adds a small downstream-distance term to distinguish equal elevations. FLOW maps `flwdir` 1–8 to N, NE, E, SE, S, SW, W, NW; 0 is a river mouth and -1 an inland sink. Its `visual` classes include 7 for small streams. The maintainer confirms that `flddif` is relative height above the unit-catchment reference river pixel in metres, `hand` is Height Above Nearest Drainage in metres relative to a river with upstream area >=0.5 km2, and high-resolution `uparea` is contributing area in km2. High-resolution `grdare` is land area per grid cell in km2, including only land pixels in coastal cells; FLOW area-generation code labels the result km2 and `generate_inpmat` converts positive values to m2 with a factor of 1e6. The base-resolution `uparea` remains in m2.

High-resolution `downxy` stores two signed 16-bit relative offsets on the high-resolution grid, not absolute destination indices: for source pixel `(ix, iy)` with Fortran-style 1-based indices, the downstream pixel is `(ix + downx, iy + downy)`. FLOW adjusts the x offset to take the shorter periodic route across the global longitude seam. It writes `-999` in both records for a river mouth and `-1000` in both for an inland termination. The arrays are initialized to `-9999`, which remains wherever no downstream offset is assigned. The maintainer confirms that `-9999` means sea in the low-resolution layer, but high-resolution land pixels without an assigned unit catchment may use a different code; the exact high-resolution encoding is not yet confirmed. CaMa-Flood high-resolution parameter tools treat any `downx <= -900` as a stopping condition. The v5 regional combiner copies these records without coordinate adjustment. See the [regional workflow guide](../map/tools/script_region/README.md#high-resolution-downstream-links) and the `downx` / `downy` rows for `<HIRES>/<AREA>.downxy.bin` in [`map_variables.csv`](map_variables.csv).

## Open Questions

1. **Blocking before implementing the ECMWF-equivalent NetCDF generator:** confirm the expected contents of `CRIVCLINC`/`CRIVPARNC`, variable types/dimensions/order, coordinates, and attributes. Proceeding without this contract risks incompatibility. The binary inventory, CSV, and existing reader analysis can proceed independently.
2. **Important for non-legacy binary datasets:** the v5 CTL generator follows the `little_endian` and north-to-south storage convention confirmed in legacy CTL writers. If a future map source uses another byte order or row orientation, the format must be established and represented explicitly before that dataset can be supported; the current generator cannot detect it from file size.
3. **Documentation-level provenance:** the external algorithm that consumes `mapdim.txt` is not present in the repository. Its purpose and role are confirmed, so this does not block the CSV or metadata specification; details can be added if the algorithm becomes available.
4. **Documentation-level high-resolution sentinel:** the maintainer confirms `-9999` means sea in the low-resolution `downxy` layer. The FLOW producer initializes high-resolution offsets to `-9999` where it assigns no downstream offset, but this value's exact high-resolution pixel classes are not established; land pixels without an assigned unit catchment may use a different code. The registry and guide distinguish the two resolutions without guessing the high-resolution code.

## Evidence and source references

- Variable definitions and common product mappings: [`map_variables.csv`](map_variables.csv).
- Map-specific geometry and input-matrix workflow: [`params.txt` readers/writers and general input matrix](../map/src/src_param/generate_inpmat.F90), [input-matrix shell configuration](../map/src/src_param/s02-generate_inpmat.sh), [regional default climatology matrix](../map/tools/code/region_default_inpmat.F90), and [regional extraction](../map/src/src_region/cut_domain.F90).
- Model binary and NetCDF readers: [`cmf_ctrl_maps_mod.F90`](../src/cmf_ctrl_maps_mod.F90#L232); dimension config: [`cmf_ctrl_nmlist_mod.F90`](../src/cmf_ctrl_nmlist_mod.F90#L180).
- Binary kind definitions: [`parkind1.F90`](../src/parkind1.F90#L20), including the precision switch for `JPRB`.
- Channel workflow: [`s01-channel_params.sh`](../map/src/src_param/s01-channel_params.sh), [`calc_outclm.F90`](../map/src/src_param/calc_outclm.F90), [`calc_rivwth.F90`](../map/src/src_param/calc_rivwth.F90), and [`set_gwdlr.F90`](../map/src/src_param/set_gwdlr.F90).
- Historical map-method overview: [Yamazaki et al. (2011)](https://doi.org/10.1029/2010WR009726).
- Package examples, inspected read-only: `glb_15min/params.txt`, `glb_15min/mapdim.txt`, CTLs, `diminfo*.txt`, channel maps, `map/data/ELSE_GPCC_clm-1981-2010.one`, and `map/data/width.bin`.
