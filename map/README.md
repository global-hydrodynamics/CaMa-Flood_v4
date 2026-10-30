# River maps and map-preparation tools

The `map/` directory connects CaMa-Flood's river and floodplain representation to the datasets and preprocessing tools used to prepare it. The GitHub source repository supplies map-processing code and documentation, plus the official `mekong_15min` development/test sample. Global source maps such as `glb_15min` are distributed separately in CaMa-Flood data packages. Check the map files included with your package and obtain any additional datasets required by the simulation.

For model setup and example runs, see the [sample execution guide](../gosh/README.md). This page is an overview; tool-specific details remain in the component documentation linked below.

## Development and test sample

`map/mekong_15min/` is the official GitHub development/test sample. The scripts retained in its map-local preparation directory are maintainer-operated tools to create or refresh this distributed sample from source data in the separate CaMa-Flood distribution package. They are not automated tests; the dataset's presence does not establish scientific validation or production suitability.

## What is a CaMa-Flood river map?

CaMa-Flood represents a river network as **unit catchments**: drainage areas associated with cells on a regular longitude/latitude grid. A map records how those cells connect downstream and supplies the geographical and hydraulic information needed to route water and diagnose river and floodplain conditions.

The model reads the downstream network, converts active grid cells into a river sequence for calculation, and combines that topology with catchment area, elevations, channel dimensions, reach lengths, and floodplain topography. These sub-grid parameters describe channel and floodplain properties that a model grid cell cannot represent explicitly. The map and parameters therefore work together; a river-network file alone is not a complete simulation map.

The source includes binary map readers and an optional NetCDF map path. The active map files and formats are selected in the model namelist. Consult the [scientific and user manuals](../doc/CaMa-Flood_Guideline.md) for the scientific formulation and detailed model setup.

## Obtain official map datasets

Use the [official CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) for current package, map, registration, and download information. Map archives are distributed separately from the development repository; access may require registration and a license agreement. The website lists global maps at several resolutions and specialized Japan maps, with their current availability and download instructions.

The official page describes basic map datasets under CC BY 4.0 and notes that some advanced high-resolution data have separate access conditions. Dataset terms can differ. Check the current website and the license or agreement supplied with the exact dataset you use; the source-code license does not automatically apply to map data.

Cloning the GitHub repository provides the legacy v4 source templates under `map/src/` and the developing v5 framework under `map/tools/`; it does not provide the map datasets used by the standard simulations. A distribution package combines code with selected maps and sample inputs. Package contents depend on the release; check its included files and release documentation before downloading additional data or preparing new products.

## Directory organization

| Path | Role | Notes |
| --- | --- | --- |
| `map/tools/` | Shared v5 map-processing implementations, a unified Makefile, and reusable map-local workflow templates. | See the [v5 tools overview](tools/README.md) and [regional workflow guide](tools/script_region/README.md). |
| `map/src/src_param/` | Legacy v4 reusable source template for preparing or inspecting map parameters and forcing-to-map input matrices. | Has a [component README](src/src_param/README.md); copy the component into a target map directory to work with that map. |
| `map/src/src_param/native_inpmat/` | Optional native-grid remapping workflow associated with `src_param`. | Compose a native-grid remapping operator with an existing CaMa input matrix and validate results; see its [README](src/src_param/native_inpmat/README.md). |
| `map/src/src_region/` | Legacy v4 reusable source template for regional map preparation. | See the [v4 regional-map guide](src/src_region/README.md). |
| `map/tools/script_region/` | v5 regionalization template; copied into a target map directory and run with shared tools. | See the [v5 regional workflow guide](tools/script_region/README.md). |
| `map/src/src_dam/` | Reusable source template for preparing dam/reservoir locations and parameters for a target map. | Has an existing [reservoir-preparation README](src/src_dam/ReadMe.md). |
| `map/src/map_data_tools/` | Shared metadata validation plus standalone CTL and NetCDF conversion CLIs for core category-11 maps. | See its [tool guide](src/map_data_tools/README.md); the current converter supports only the core river-network group. |
| `map/src/` | Reusable source templates and standalone map-data tools. | Contains `src_param/`, `src_region/`, `src_dam/`, and `map_data_tools/`; this is separate from the model's numerical `src/`. |
| `map/<map-tag>/` | A map instance: downloaded map data, metadata, derived products, and any map-local processing copies needed for that workflow. | Examples include `glb_15min` and `conus_06min`; these datasets are supplied through distribution packages or separate map downloads. |
| `map/data/` | Optional supporting datasets in some data-rich packages. | Some reservoir and levee workflows use supporting data here, including GRSAD and ReGeom. Availability and terms depend on the dataset and package release. |

### Reusable sources and map-local working copies

The legacy v4 directories `map/src/src_param/`, `map/src/src_region/`, and `map/src/src_dam/` are reusable workflow sources intended to be copied into the relevant map directory when preparing or extending that map. The v5 regional template is `map/tools/script_region/`; its shared programs remain under `map/tools/code/`. For example, the channel-parameter workflow is run from a map-local `src_param/`; the regionalization and reservoir workflows similarly use map-local working directories and relative paths. Copy only the components needed for the task. Some data-rich packages already include selected copies, but not every map directory contains every component.

Keep the distinction clear: `map/src/` holds source templates, while paths such as `map/glb_15min/src_param/` are working copies associated with an individual map. Copying creates a separate working copy; edits to a template do not automatically update copies already placed in map directories. These map tools operate on map data; they are not the model's numerical source tree at repository `src/`.

The v4.30 package-preparation workflow assembles `glb_15min/`, `conus_06min/`, and `tej_01min/` for the standard examples. Other maps require the corresponding downloads or regional preparation. If a map directory is a symbolic link, check that its target is accessible before running the model or preprocessing tools.

`glb_15min/` is the global 15-minute (0.25-degree) river map; `params.txt` records its dimensions and geographical coverage. Higher-resolution supporting files, such as those under `glb_15min/1min/`, are used by selected preprocessing and downscaling workflows. They do not change the base river-map resolution. Packages may also include prepared working copies such as `glb_15min/src_param/`; check which components and supporting layers are included with your map release.

The v4 and v5 map-processing programs use the repository's active build configuration in [`adm/`](../adm/README.md). The v5 shared Makefile builds executables directly under `map/tools/code/`; regional scripts call them from map-local working directories. Map tools are separate from the numerical model executable in `src/`.

### Dataset, tools, and products are different things

| Category | Examples | How to treat it |
| --- | --- | --- |
| Distributed datasets | Global or Japan river-map archives and high-resolution supporting layers. | Obtain from the official project download page and follow the specific dataset terms. |
| Repository tools | Legacy v4 tools under `map/src/` and v5 shared code/templates under `map/tools/`. | These are source code for preparing, inspecting, or transforming data; they are not the datasets themselves. |
| Generated products | Regional network files, channel parameters, runoff input matrices, MPI region maps, and dam parameter tables. | Generate or obtain these for the selected map and configuration; keep their metadata and inputs together. |
| Optional ancillary data | Groundwater-delay, sea-level, reservoir, gauge, and high-resolution information. | Prepare only when a selected feature or workflow requires it; requirements are feature-specific. |

A successful build of the map tools does not supply external source datasets or create a complete ready-to-run map by itself. Conversely, a downloaded map may include precomputed products, so check its archive contents before regenerating parameters.

## Global and regional maps

A **global map** supplies a connected network and parameters over its published global domain at a selected grid spacing; `glb_15min/` is the global 15-minute example used by the standard runs. A **regional map** is derived for a smaller domain, usually by starting from a compatible global map and selecting region bounds. The regional tools adjust the network and related arrays to the regional grid and can combine available higher-resolution map information. Source global maps used for preparation are package data, not GitHub fixtures.

The legacy v4 regionalization template is under `map/src/src_region/`; the reusable v5 template is under `map/tools/script_region/`. These are map-preparation workflows, not automated tests. The v5 template can be copied to another target map directory, configured with a compatible source map and bounds, and run from its map-local `script_region/` directory. `s02-write_ctl.sh` is a separate CTL-generation step. See the [v5 regional workflow guide](tools/script_region/README.md) and [v5 tools overview](tools/README.md) for commands and limitations.

GitHub-only map-tools checks validate metadata, source functionality, and CTL generation using synthetic fixtures and the committed Mekong binary sample. They do not execute regionalization or require the distribution package's global data. See [GitHub-only automated tests](tools/README.md#github-only-automated-tests).

Examples of maps used by sample runs are listed in [`gosh/README.md`](../gosh/README.md). A regional map must remain consistent with its source resolution and extent, downstream connectivity, metadata, and parameter files. Do not substitute files from a different map tag merely because their filenames match.

## Shared map-product registry

[`map_variables.csv`](map_variables.csv) is the source-linked variable and data-product registry for CaMa-Flood maps. It uses symbolic dimensions such as `NX`, `NY`, and `NLFP`, so the definitions apply across map resolutions and regional domains. It is the primary source for variable names, proposed long names and units, data types, record layouts, numeric stored missing values, meaningful special codes, product variants, distribution roles, and current model-reader mappings.

[`map_metadata_spec.md`](map_metadata_spec.md) explains the CSV schema and how it relates to per-map `params.txt`, `mapdim.txt`, run-specific `diminfo*.txt`, and GrADS CTL files. The CSV defines common variable-level metadata; each map's files supply its actual dimensions, bounds, spacing, and dataset-specific binary settings. Runoff-grid configuration belongs to `diminfo` and the paired input matrix.

The channel-parameter entries describe products users generate or verify for a target map. Data-rich packages may include prepared examples; check the actual package contents before generating replacements. The CSV distinguishes empirical `rivwth.bin` from the alternative GWD-LR merged width product `rivwth_gwdlr.bin`.

### Core map metadata and conversion tools

`map/src/map_data_tools/` contains a GrADS CTL generator and a binary-to-NetCDF converter for category 11 only. Both read the CSV and validate category-11 record sizes against the map's `params.txt`. A portable per-map JSON configuration supplies byte order, row order, missing sentinels, coordinate mode, and any non-default file paths; these dataset-specific settings are not inferred automatically.

Install the Python requirements and copy/edit the example config for the selected map before running:

```sh
python3 -m pip install -r map/src/map_data_tools/requirements.txt
cp map/src/map_data_tools/examples/map_metadata.example.json map/glb_15min/map_metadata.json
# Edit dataset_id and verify byte_order, row_order, missing_values, and files.
python3 map/src/map_data_tools/generate_ctl.py --map-dir map/glb_15min --config map/glb_15min/map_metadata.json
python3 map/src/map_data_tools/binary_to_netcdf.py --map-dir map/glb_15min --config map/glb_15min/map_metadata.json
```

The NetCDF output is a `CRIVCLINC`-compatible core river map; it does not contain channel parameters for `CRIVPARNC`. Existing CTLs and output NetCDF files are not overwritten unless `--overwrite` is supplied. See the [tool guide](src/map_data_tools/README.md) and [metadata conversion contract](map_metadata_spec.md#core-conversion-contract-and-dataset-configuration) for supported formats, validation, and limitations.

## Map parameters and preprocessing

`src/src_param/` supports several kinds of preparation:

- Estimating channel width and depth from long-term discharge inputs using the configured empirical relationships.
- Fusing channel-width estimates with Global Width Database for Large Rivers (GWD-LR) information where configured.
- Preparing bifurcation parameters and other river-network lookup data.
- Generating an `inpmat` lookup table that maps an external runoff grid onto the CaMa-Flood unit catchments. One matrix is specific to a map and a forcing-grid definition; it must match the forcing grid and its orientation.
- Allocating gauges, dams, or level stations, and creating basin, sub-basin, or other diagnostic products.

The [parameter-tools README](src/src_param/README.md) and scripts document those workflows. For supported native rectilinear runoff grids, the separate [composed-remapping workflow](src/src_param/native_inpmat/README.md) documents its assumptions, output files, and validation. These two input-matrix paths should not be treated as interchangeable without checking their grid and format assumptions.

### Reservoir and dam information

Reservoir operation uses additional dam locations and parameter data associated with the selected river map. The scripts in `src/src_dam/` link to reservoir datasets and combine them with naturalized simulation statistics to prepare a map-specific `dam_param.csv`. The procedure and fields are described in the [reservoir workflow README](src/src_dam/ReadMe.md) and the [Reservoir Operation manual](../doc/Manual_ReservoirOperation_v420.docx).

The dam table is optional and is needed only when reservoir operation is configured. Its locations and drainage areas must correspond to the target CaMa-Flood map. External reservoir source datasets have their own availability and license conditions.

## Other optional map information

A simulation may need map files beyond the basic network and topography, depending on its runtime options and build. Examples in the model namelist include:

- Bifurcation-channel parameters when bifurcation flow is enabled.
- Groundwater-delay and mean-sea-level parameters when those options are enabled.
- A dam-parameter table for reservoir operation.
- MPI region information for a distributed run.
- Higher-resolution layers used by selected regional, diagnostic, or specialized workflows.

Not every feature uses a file generated under `map/`, and some optional products are prepared under `etc/` or another component directory. Check the option's source, its namelist group, and its dedicated documentation before preparing additional files.

## Typical workflows

### Use an existing map

1. Obtain a map archive from the official project download page and review its terms.
2. Extract or reference the map in the location expected by your namelist and execution script.
3. Check that all required map files, metadata, grid dimensions, and optional products belong to the same map release and resolution.
4. Configure the map paths in the run namelist or sample script, then follow the appropriate [`gosh/` run guide](../gosh/README.md).

### Create a regional map

1. Obtain a compatible source map and any high-resolution layers or sample runoff data the workflow needs.
2. Build the shared v5 regional programs with `make -C map/tools/code region`.
3. Copy `map/tools/script_region/` into the target map directory, configure `SOURCE` and the domain bounds in `s01-regional_map.sh`, then run it from the copied `script_region/` directory.
4. Review the program output and map dimensions/products; run `s02-write_ctl.sh` separately if GrADS descriptors are needed.
5. Prepare simulation-specific channel parameters and runoff input matrices in the separate `script_params/` workflow when available; the regionalization sample matrix is only for channel parameterization from sample runoff.

The [v5 regional workflow guide](tools/script_region/README.md) lists the current source requirements, outputs, and verification limits. The v4 template remains documented under `map/src/src_region/README.md`.

### Prepare additional parameters

1. Identify the exact missing parameter and which model option or forcing workflow needs it.
2. Confirm the target map tag, grid dimensions, resolution, orientation, units, and required source datasets.
3. Use the corresponding `src_param/` or specialized tool, following its own documentation.
4. Verify the generated file belongs with the target map and has the layout expected by the model reader or consuming workflow.

### Prepare optional map information

Check the feature-specific manual or workflow first. For reservoirs, see `src/src_dam/ReadMe.md`; for levees, see [`doc/levee.md`](../doc/levee.md). Optional features can require external datasets, build options, namelist groups, and matching map metadata in addition to an extra file.

## Map file organization and compatibility

Map directories are commonly named by region and grid spacing, for example `glb_15min` or `conus_06min`. The model's namelist explicitly names the map paths; scripts commonly set a base such as `FMAP` and build those paths from it. A directory name alone does not establish that its contents are complete or compatible.

The standard binary map path uses files such as `nextxy.bin` (downstream grid indices), `ctmare.bin` (unit-catchment area), `elevtn.bin`, `nxtdst.bin`, `rivlen.bin`, and `fldhgt.bin`; channel parameters may include `rivwth.bin`, `rivhgt.bin`, and `rivman.bin`. `diminfo` or other map metadata describe dimensions and geographic coverage. Optional files and required names depend on the selected model options. The `NMAP` namelist in `src/cmf_ctrl_maps_mod.F90` shows the paths read by the model, while sample execution scripts show concrete configurations.

Many legacy `.bin` files are Fortran unformatted direct-access records, but the record contents and numeric types vary by file. For example, the `nextxy` reader expects two integer records; other readers expect real-valued parameters. Text metadata and tables may use `.txt` or `.csv`. The code also contains an optional NetCDF map reader, which requires a NetCDF-enabled build and matching NetCDF variables. Do not infer a complete file specification from the extension alone.

When combining or generating map products, keep dimensions, grid spacing, geographic bounds, latitude/longitude ordering, stored missing values and special codes, data types, and units consistent with the target map and reader. An input matrix must additionally match the runoff grid and orientation. Detailed format specifications belong with the relevant tool or manual rather than in this overview.

## Relationship to other directories

| Directory | Relationship to `map/` |
| --- | --- |
| [`src/`](../src/README.md) | Reads the configured network and map parameters and uses them during model initialization and simulation. |
| [`gosh/`](../gosh/README.md) | Provides sample build/run scripts that point to prepared maps and forcing data. |
| [`adm/`](../adm/README.md) | Supplies compiler and build configuration for the map tools and model. |
| [`inp/`](../inp/README.md) | Prepares runoff forcing; map input matrices connect that forcing grid to the selected river map. |
| `etc/` | Contains specialized workflows, some of which consume or prepare additional map-related data. |
| [`doc/`](../doc/CaMa-Flood_Guideline.md) | Holds user guidance and scientific/technical manuals. |

## Related documentation

- [Official CaMa-Flood website: maps, package downloads, and license information](https://global-hydrodynamics.github.io/CaMa-Flood/)
- [CaMa-Flood user guideline](../doc/CaMa-Flood_Guideline.md)
- [Main user manual](../doc/Manual_CaMa-Flood_v420.docx)
- [Reservoir operation manual](../doc/Manual_ReservoirOperation_v420.docx)
- [Parameter-processing tools](src/src_param/README.md)
- [Native-grid input-matrix tools](src/src_param/native_inpmat/README.md)
- [Dam/reservoir parameter workflow](src/src_dam/ReadMe.md)
- [Model source architecture](../src/README.md)
- [Sample simulation workflows](../gosh/README.md)
