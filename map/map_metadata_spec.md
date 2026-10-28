# Common Map Metadata Specification

## Purpose and authority

`map/map_variables.csv` is the shared, machine-readable definition of CaMa-Flood map variables and binary products. It is intended to describe variables across map resolutions and regional domains, not to define one dataset's dimensions.

This document defines the CSV columns, records the design choices, and explains how the shared variable definitions relate to metadata files distributed with a map. The CSV is the primary source for variable short names, proposed long names, units, types, symbolic dimensions, binary records, missing conventions, distribution roles, and model reader/NetCDF name mappings. Do not copy its full variable table into another README or manual.

Dataset-specific dimensions, bounds, and spacing remain in each map dataset's metadata. Input-grid/runoff settings remain in the run's `diminfo` and its paired input matrix. Neither dataset-specific dimensions nor runoff-grid settings belong in the common variable definition.

## CSV format and row model

The CSV uses UTF-8, a header row, and one row per logical variable or traceable data product. Multi-record files such as `nextxy.bin` have one row per logical variable. A single row may link alternative files that implement the same variable, as for `rivwth.bin` and `rivwth_gwdlr.bin`; the latter is width only. Non-raster tables and source-generated outputs may also be recorded, with their real layout stated rather than forced into `NX × NY`.

Dimensions use symbolic names and Fortran array order where verified, such as `NX,NY` or `NX,NY,NLFP`. Blank cells mean “not specified in this inventory,” not zero, dimensionless, or absent. Use `unknown` or `not established` where a value is known to be unresolved. Evidence paths are repository-relative; citations may be URLs.

### Categories and sorting

`category_id` is the first column and numeric sorting key. The descriptive `category` value is retained. Current assignments are:

| ID | Category | Meaning |
| ---: | --- | --- |
| 11 | `river_network` | Core downstream topology, catchment, channel-topography, and floodplain-profile products. This is the initial CTL/NetCDF conversion scope. |
| 12 | `river_parameter` | Main channel width, depth, and roughness parameters, including alternate source products for the same variable. |
| 21 | `input_matrix` | Runoff-grid to map-cell mapping products. |
| 31 | `network_analysis` | Basin, sequence, upstream lookup, and diagnostic analysis products. |
| 32 | `grid_variant` | Alternate or derived grid/network views, including MPI region assignments. |
| 41 | `bifurcation` | Bifurcation maps and parameter tables. |
| 42 | `dam_reservoir` | Dam and reservoir parameter products. |
| 43 | `optional_parameter` | Optional model/preprocessing parameters not in the core network set. |
| 81 | `auxiliary` | Supporting data, intermediates, and other workflow inputs. `outclm.bin` is an intermediate discharge estimate, not a final runtime channel parameter. |
| 99 | `unclassified` | A source-backed/package-observed product whose functional role is not established yet. |

Sort rows by ascending numeric `category_id`, then `binary_filename` (case-insensitive), then numeric `record_index` with blank indices last. Preserve all evidence and verification-status fields during recategorization. Use ID 99 until a role is supported by source or documentation; do not classify from a filename alone.

### Columns

| Column | Meaning |
| --- | --- |
| `category_id` | Numeric functional sort key from the table above; it is the first CSV column. |
| `category` | Descriptive functional group matching `category_id`. |
| `product_kind` | `binary_variable_or_product`, `nonbinary_or_generated_product`, or a brief product-kind value when a source format is known. |
| `binary_filename` | Primary filename or logical output filename; it may be a pattern such as `bifprm-<N>.txt`. |
| `alternative_binary_filename` | Alternate file carrying the same variable/product, when applicable. Blank means no alternative is registered. |
| `name` | Established model variable/product short name; use the source name where available. |
| `long_name` | Concise English label proposed for human and future NetCDF use. Blank when semantics are unresolved. |
| `physical_description` | Meaning, assumptions, or explicit uncertainty. Do not infer from a filename alone. |
| `units` | Verified or proposed unit string. Blank does not imply dimensionless. |
| `dtype` | Verified binary primitive/layout, e.g. `int32`, `float32`, `mixed`, `formatted text`, `NetCDF`, or `unknown`. |
| `dimensions` | Symbolic data shape or a descriptive configuration-dependent value; do not hardcode one map's concrete dimensions here. |
| `record_index` | One-based numeric record for a row representing one field in a multi-variable file. Blank for products whose record cannot be established. |
| `record_count` | Total records in the binary file for the row's product (`1`, `2`, or symbolic `NLFP` when variable by dataset). |
| `record_layout` | Human-readable record indexing/order, or `not established` if unresolved. |
| `missing_value_convention` | Existing binary/file sentinel conventions only. This is not a decision about future NetCDF `_FillValue` or `missing_value`. |
| `usage` | Short workflow role, such as runtime input, preprocessing input/output, visualization, or analysis. |
| `runtime_requirement` | Whether runtime use is core, feature-conditional, MPI-conditional, preprocessing-only, or unknown. |
| `distribution_status` | Intended distribution/generation role; see controlled values below. |
| `sample_availability` | Package evidence only: semicolon-separated inspected map tags (`glb_15min`, `conus_06min`, `mekong_15min`, `tej_01min`, `data`), `not_found_in_inspected_v4.30_map_package`, or unknown. This is independent of distribution status. |
| `generation_or_processing` | Original provenance, in-repository producer/transformer, or explicit statement that it was not found. Regional extraction is not original global generation. |
| `model_reader` | Verified model read routine and source path; blank where not traced. |
| `netcdf_variable` | Existing model-reader container and variable lookup, if any; not a proposed final NetCDF grouping. |
| `vertical_datum` | Datum only where verified; currently EGM96 for `elevtn`. |
| `verification_status` | Controlled confidence state (below). |
| `evidence_ref` | Source, manual, package observation, maintainer confirmation, or citation supporting the row. |

### Controlled statuses

- `standard_distributed_core`: core river-network/topography dataset product described as supplied in standard map packages. Check the selected archive; this does not assert that every release or map tag contains every file.
- `map_specific_user_generated`: channel parameters intended to be generated or checked for a target map. Some packages include prepared examples.
- `optional_or_auxiliary`: ancillary, specialized, derived, analysis, or workflow products; not implied to be standard runtime requirements.
- `source_only_generated`: traceable source output not found in the inspected v4.30 map packages.
- `package_example_or_specialized`: observed package product whose distribution as a general standard is not established.

`verification_status` values in this draft are `verified_source_and_sample`, `verified_source`, `package_observed`, and `provisional_semantics`. The last means package presence is confirmed but producer, meaning, format, or use is not. Blank/unknown semantic fields remain intentionally unresolved.

The fields deliberately separate two questions: `distribution_status` describes the product's role in the workflow, while `sample_availability` records where it was actually observed. Package observations do not prove universal distribution. A source program that writes a file does not establish that a corresponding product is shipped. These distinctions should be preserved when adding entries.

## Maintenance rules

When changing a map reader, writer, producer, or map-related model interface:

1. Search for the product in the source tree and inspect its actual inputs, outputs, types, records, and dimensions.
2. Update this CSV for introduced, changed, or removed products. Keep package distribution, package sample availability, and user-generated/derived status distinct.
3. Mark unresolved semantics or layouts as provisional; do not fill gaps by guessing from filenames.
4. Update the responsible module README for workflows or assumptions a human user needs. Keep detailed variable metadata in the CSV rather than copying the entire inventory into README files.
5. Check `map/README.md` and `docs/map_data_inventory.md` for references that became inaccurate. Run the CSV parser and verify changed links.

The CSV is an evolving source-linked inventory. Category 11 now has the machine-readable record metadata used by the initial CTL and NetCDF tools. Other categories remain an inventory, not a promise that every field is complete enough for conversion. Dataset-specific byte order, row orientation, sentinel values, coordinate interpretation, and file paths belong in a separate dataset config, not the shared variable rows.

## Common binary conventions

The current model binary readers use unformatted, direct-access records of `4 × NX × NY` bytes. `JPIM` is a signed 4-byte integer and `JPRM` is a 4-byte real; these are the kinds used by the inventoried map fields. `JPRB` is switchable: 4-byte under `SinglePrec_CMF`, otherwise 8-byte. Arrays are shaped `(NX,NY)` in Fortran, so x is the first and fastest-varying index. Multi-variable/profile files place each field or profile layer in its own record.

- `nextxy.bin`: record 1 is `nextx`; record 2 is `nexty`; each is an `NX × NY` signed int32 array.
- Scalar real files: one `NX × NY` float32 record.
- `fldhgt.bin`: records 1 through `NLFP`, one `NX × NY` float32 profile layer per record.
- `nextx` and `nexty` use Fortran-style 1-based grid indices: positive `nextx` values are 1 through `NX`; positive `nexty` values are 1 through `NY`. Preserve those indices in any conversion. The reference package contains `-9` (river mouth), `-10` (inland termination), and `-9999` (ocean/undefined), as described in the v4.20 manual; runtime sequence construction treats negative non-missing x codes as endpoints.
- The inspected package stores `-9999` in real map cells outside the network. Model constants also include `RMIS=1.E20`, which is not automatically substituted for the binary sentinel by the map reader.

Byte order and row orientation are dataset properties, not properties of a variable. The inspected `glb_15min` CTLs specify `little_endian` and `yrev`; the model's `LMAPEND` controls byte conversion and defaults to false. `params.txt` does not encode either setting. Do not assume every regional or future dataset shares the reference package's binary byte order or orientation.

## Core conversion contract and dataset configuration

The tools in [`src/map_data_tools/`](src/map_data_tools/README.md) select category ID 11 and require the six core files `nextxy.bin`, `ctmare.bin`, `elevtn.bin`, `nxtdst.bin`, `rivlen.bin`, and `fldhgt.bin`. They read this CSV for names, types, record indices/counts, long names, units, and datum metadata. They validate the binary byte count using `NX`, `NY`, and `NLFP` from the map's `params.txt` before writing output.

The CaMa-Flood v4 reader contract is verified in `src/cmf_ctrl_maps_mod.F90`:

- `READ_MAP_CDF` reads `nextx`, `nexty`, `lon`, and `lat` from the `CRIVCLINC` file.
- `READ_TOPO_CDF` reads `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and `fldhgt` from that same file.
- Horizontal variables have Fortran array bounds `(NX,NY)`; `fldhgt` is read one layer at a time into `(NX,NY)` for each `1..NLFP`.
- The Python NetCDF file uses on-disk dimensions `(lat,lon)` for horizontal variables and `(lfp,lat,lon)` for `fldhgt`. A synthetic test reads these variables using the same NetCDF-Fortran API calls and array extents used by the model. This verifies the file's shape/order contract, not a full model simulation.
- The 1-D `lon` and `lat` coordinate variables are required by `READ_MAP_CDF`; coordinates are cell centers derived from the dataset bounds in `params.txt`.

Dataset-level settings are explicit in `map_metadata.json` (see [`the portable example`](src/map_data_tools/examples/map_metadata.example.json)). `byte_order`, `row_order`, `coordinate_mode`, file paths, and each core binary's missing-value sentinel are not inferred from the filename or hard-coded as universal dataset properties. The tools reject missing or unsupported values. Relative paths in `files` are resolved from the target map directory. The inspected global 15-minute sample uses little-endian binary records, north-to-south rows, regular longitude/latitude cells, and `-9999` sentinels; other maps must be checked and configured individually.

For this first NetCDF output, binary numeric values and 1-based `nextx`/`nexty` indices are preserved exactly. The configured binary sentinel is also written as NetCDF `_FillValue` and `missing_value`; it is not translated to the model's `RMIS=1.E20`. Negative `nextxy` endpoint codes such as `-9` and `-10` remain data values. `elevtn` carries `vertical_datum=EGM96`. No variable `standard_name` is added beyond the verified coordinate names.

Each generated CTL describes one category-11 binary file. The `nextxy.ctl` variable order follows records `nextx`, then `nexty`; `fldhgt.ctl` defines `NLFP` levels. The configured endian option and row order determine `little_endian`/`big_endian` and `yrev`. Generation refuses to replace an existing CTL unless `--overwrite` is given.

The converter emits only the core `CRIVCLINC` map. It does not create `CRIVPARNC` channel parameter inputs; the model still needs those separately when configured. Optional schemes, parameter products, and non-core CSV rows are outside this first conversion implementation.

## Confirmed facts and proposed attributes

The model names in the CSV are established variable names. English `long_name` text and some unit strings are proposed conventions, distinct from attributes verified in existing binary maps. The existing CTLs often have generic labels/titles; they are not an authoritative semantic dictionary. No CF `standard_name` is added where no applicable name has been verified.

- The maintainer confirmed that `elevtn` uses the EGM96 vertical datum. The CSV records this in `vertical_datum`; elevation units are metres.
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

### `diminfo*.txt` — simulation and runoff/input-grid configuration

The standard sample format contains eleven records:

| Record | Field | Scope |
| --- | --- | --- |
| 1–3 | `NX`, `NY`, `NLFP` | Destination map dimensions/layers; map-specific |
| 4–5 | `NXIN`, `NYIN` | Runoff/input-grid dimensions; input product and domain specific |
| 6 | `INPN` | Maximum input-grid contributors per CaMa-Flood map cell; mapping-specific |
| 7 | Paired input-matrix filename text | Descriptive filename written by the generator; model parser skips this line and callers supply the actual matrix path |
| 8–11 | west, east, north, south map bounds | Destination map domain, in degrees for the inspected geographic grids |

The core run configuration parser reads the dimensions, input-grid dimensions, and `INPN`; when `LGRIDMAP` is true (the inspected default), it also reads the four map bounds. The input matrix is a separate binary file, with records grouped as `INPN` x-index records, `INPN` y-index records, then `INPN` float32 overlap-area records, each covering `NX × NY`.

The file combines destination-map geometry with runoff/input-grid configuration. It does not by itself specify runoff-grid spacing, source bounds, longitude convention, or row order; the shell workflow/input dataset provides those. `generate_inpmat.F90` reads the destination grid from `params.txt`, calculates `NXIN`/`NYIN` from configured input bounds and spacing, and writes `diminfo` plus the paired matrix. Thus `params.txt` and `diminfo` repeat some map dimensions/bounds but have distinct roles.

### GrADS `.ctl` files — binary visualization metadata

CTL files are read by GrADS to describe a binary dataset. The CaMa-Flood binary model readers inspected here do not read them.

- Structural interpretation fields include `dset`, `xdef`/`ydef`, `zdef`, `vars` and record order, and storage `options` such as `little_endian` and `yrev`.
- `undef` documents the value GrADS treats as missing.
- `title`, variable names, and comments/descriptions are descriptive. They do not establish correct physical meaning or units. The inspected package uses the generic title `Flow Direction` even for scalar variables, and has `rivman.bin` without a same-directory `rivman.ctl`.

The CSV supplies variable names, proposed long names/units, data types, symbolic dimensions, and binary record layouts. `params.txt` supplies dataset-specific grid size and bounds. Neither currently provides dataset byte order/row orientation or every CTL choice. The current generator requires the dataset config for those settings and checks that the selected binary files match the category-11 record layout.

## Channel-parameter generation workflow

The repository provides tools for deriving channel geometry for a target map. They are separate from the distributed core river-network maps.

1. **Runoff sample:** the packaged workflow expects a long-term runoff climatology such as `map/data/ELSE_GPCC_clm-1981-2010.one`, with runoff in mm/s. This data-bearing `map/data/` directory is present in the inspected v4.30 distribution package, but not in the Git source repository. Users need the package sample or a compatible input dataset.
2. **Annual mean discharge:** `s01-channel_params.sh` runs `calc_outclm` with binary runoff and `inpmat` interpolation. `calc_outclm.F90` uses the runoff grid metadata and input matrix, maps runoff volume to map catchments, and accumulates flow downstream using `nextxy.bin`. It also reads `ctmare.bin`. Its active output is `outclm.bin`, one direct-access float32 record of mean discharge in m³/s. The source supports binary or (when built with NetCDF) NetCDF runoff input.
3. **Empirical channel parameters:** the script passes its coefficients and minima to `calc_rivwth.F90`. The formulas are `H = max(HMIN, HC × Q̄^HP + HO)` and `W = max(WMIN, WC × Q̄^WP + WO)`, where `Q̄` is mean discharge. The current sample script passes `HC=0.1`, `HP=0.50`, `HO=0`, `HMIN=1`, `WC=2.50`, `WP=0.60`, `WO=0`, and `WMIN=5`; these are workflow settings, not universal constants or the executable defaults. The program writes empirical `rivhgt.bin` (channel depth), `rivwth.bin` (channel width), and `rivman.bin`. It currently assigns `rivman=0.03` on network cells and `-9999` elsewhere.
4. **Satellite-width merge:** `set_gwdlr.F90` reads empirical `rivwth.bin`, satellite-derived `width.bin`, and `nextxy.bin`, then writes `rivwth_gwdlr.bin`. It adjusts/clamps the width product using the empirical width and configured limits. It does not generate or contain `rivhgt`.
5. **Ordering and target-map locality:** these steps are ordered: discharge estimate → empirical channel width/depth/roughness → optional GWD-LR merged width. Run them in the working copy associated with the intended map and verify that its dimensions, map files, input matrix, runoff data, and high-resolution width source correspond.

The script also proceeds to bifurcation and ancillary products after channel parameter generation; these are outside this initial variable metadata scope.

### Existing partial NetCDF output

Some NetCDF-writing code already exists in `calc_rivwth.F90` when built and invoked with `type=cdf`. It writes `rivpar.nc` with `lon`, `lat`, `rivwth`, and `rivhgt`, but not `rivman`. It sets NetCDF `missing_value=1.e20` while the arrays contain `-9999` outside the network. This is a partial utility output and is distinct from the module-level `CRIVCLINC`/`CRIVPARNC` files expected by the model reader.

The maintainer confirmed that the module-level files were previously generated at ECMWF, are absent from the current distribution, and that code to reproduce this generation workflow is planned. Do not infer a finalized file grouping, dimension order, coordinate convention, or missing-value policy from the current partial writer.

## Future use and limitations

### GrADS CTL generation

A CTL generator can combine:

- common variable/product metadata from `map_variables.csv`;
- dimensions, regular geographic spacing, and domain bounds from the map's `params.txt`;
- concrete file paths, selected primary/alternative product, and actual `NX`, `NY`, and `NLFP` values;
- dataset byte order and row orientation from verified metadata or an explicit user setting;
- the selected CTL convention for axes, undefined values, record/layer structure, title, and variable descriptions.

Current `params.txt` does not store endianness or row orientation. Existing CTLs may be generic or misleading, so they can evidence storage options but should not be trusted for variable semantics without corroboration.

### NetCDF conversion

The CSV contributes variable names, descriptions, units, types, dimensions, vertical datum, missing sentinels, and current model-reader variable names. Dataset-specific grid coordinates come from map geometry. The tool must preserve 1-based `nextx`/`nexty` indices unless an explicit transformation is specified.

For category 11, the initial converter uses one `CRIVCLINC`-compatible file, disk dimensions `(lat,lon)` and `(lfp,lat,lon)`, 1-D longitude/latitude coordinates, and the dataset-configured sentinel as both `_FillValue` and `missing_value`. These choices are checked against the existing Fortran reader interface and a synthetic smoke test. Other categories, `CRIVPARNC`, optional schemes, and maps requiring non-regular coordinates remain outside the implementation. The `netcdf_variable` column records existing model-reader lookup names only.

## Open Questions

1. **Blocking before implementing the ECMWF-equivalent NetCDF generator:** confirm the expected contents of `CRIVCLINC`/`CRIVPARNC`, variable types/dimensions/order, coordinates, and attributes. Proceeding without this contract risks incompatibility. The binary inventory, CSV, and existing reader analysis can proceed independently.
2. **Important for automatic CTL generation:** how should dataset-specific byte order and row orientation be supplied when the dataset has no reliable CTL? The variable CSV and `params.txt` do not encode these fields. This task documents the gap; future generator design can proceed after choosing a configuration source.
3. **Documentation-level provenance:** the external algorithm that consumes `mapdim.txt` is not present in the repository. Its purpose and role are confirmed, so this does not block the CSV or metadata specification; details can be added if the algorithm becomes available.

## Evidence and source references

- Variable definitions and common product mappings: [`map_variables.csv`](map_variables.csv).
- Map-specific geometry and input-matrix workflow: [`params.txt` readers/writers](src/src_param/generate_inpmat.F90), [input-matrix shell configuration](src/src_param/s02-generate_inpmat.sh), and [regional extraction](src/src_region/cut_domain.F90).
- Model binary and NetCDF readers: [`cmf_ctrl_maps_mod.F90`](../src/cmf_ctrl_maps_mod.F90#L232); dimension config: [`cmf_ctrl_nmlist_mod.F90`](../src/cmf_ctrl_nmlist_mod.F90#L180).
- Binary kind definitions: [`parkind1.F90`](../src/parkind1.F90#L20), including the precision switch for `JPRB`.
- Channel workflow: [`s01-channel_params.sh`](src/src_param/s01-channel_params.sh), [`calc_outclm.F90`](src/src_param/calc_outclm.F90), [`calc_rivwth.F90`](src/src_param/calc_rivwth.F90), and [`set_gwdlr.F90`](src/src_param/set_gwdlr.F90).
- Historical map-method overview: [Yamazaki et al. (2011)](https://doi.org/10.1029/2010WR009726).
- Package examples, inspected read-only: `glb_15min/params.txt`, `glb_15min/mapdim.txt`, CTLs, `diminfo*.txt`, channel maps, `map/data/ELSE_GPCC_clm-1981-2010.one`, and `map/data/width.bin`.
