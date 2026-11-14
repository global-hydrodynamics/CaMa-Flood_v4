# River maps and map-preparation tools

The `map/` directory connects CaMa-Flood's river and floodplain representation to the datasets and preprocessing tools used to prepare it. The GitHub source repository supplies map-processing code and documentation, plus the official `mekong_15min` development/test sample. Global source maps such as `glb_15min` are distributed separately in CaMa-Flood data packages. Check the map files included with your package and obtain any additional datasets required by the simulation.

Some shared tools and workflow documentation under `map/tools/` were developed during the v5 work that previously took place in this repository. They remain in this checkout, but their documentation describes that development snapshot and may assume its former registry and directory layout. Check that required files are present before using those workflows. Current v5 development is in [global-hydrodynamics/cmf_v5_beta](https://github.com/global-hydrodynamics/cmf_v5_beta); this repository's primary maintenance role is CaMa-Flood v4.

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

Cloning this repository provides the v4 source templates under `map/src/` and the map utilities and workflow files currently retained under `map/tools/`; it does not provide all map datasets used by standard simulations. Some `map/tools/` content was created during v5 development and reflects the former in-repository setup, not the current v5 development tree. A distribution package combines code with selected maps and sample inputs. Package contents depend on the release; check its included files and release documentation before downloading additional data or preparing new products.

## Directory organization

| Path | Role | Notes |
| --- | --- | --- |
| `map/tools/` | Shared map-processing implementations and map-local workflow templates retained in this checkout; some were developed for v5. | See the [tools overview](tools/README.md); verify its referenced inputs against the current checkout. |
| `map/src/src_param/` | Legacy v4 reusable source template for preparing or inspecting map parameters and forcing-to-map input matrices. | Has a [component README](src/src_param/README.md); copy the component into a target map directory to work with that map. |
| `map/src/src_region/` | Legacy v4 reusable source template for regional map preparation. | See the [v4 regional-map guide](src/src_region/README.md). |
| `map/tools/script_region/` | Regionalization template retained from the former in-repository v5 workflow; copied into a target map directory and run with shared tools. | See the [regional workflow guide](tools/script_region/README.md); its v5-era path assumptions may be stale. |
| `map/src/src_dam/` | Legacy v4 reusable source template for preparing dam/reservoir locations and parameters for a target map. | Has an existing [reservoir-preparation README](src/src_dam/ReadMe.md). |
| `map/src/` | v4 map-preparation tools, retained for users of the v4 workflows. | Contains `src_param/`, `src_region/`, and `src_dam/`; current v5 map-tool development is in the separate `cmf_v5_beta` repository. It is separate from the model's numerical `src/`. |
| `map/<map-tag>/` | A map instance: downloaded map data, metadata, derived products, and any map-local processing copies needed for that workflow. | Examples include `glb_15min` and `conus_06min`; these datasets are supplied through distribution packages or separate map downloads. |
| `map/data/` | Tracked reference inputs and supporting data for map-preparation workflows. | Git tracks the curated `GRDC_allocated.csv`, `GRanD_allocated.csv`, `GRanD_alloc+Mekong.csv`, `MLIT_FlowGauge_alloc_final.csv`, and `MLIT_LevelGauge_allocated.csv` tables, plus the ELSE_GPCC runoff climatology used by the sample channel workflow. These processed tables are the approved allocation inputs; this does not imply unrestricted redistribution of source workbooks or databases. Other data such as GRSAD and ReGeom samples may be package-specific; check their availability and terms separately. |

### Reusable sources and map-local working copies

The v4 directories `map/src/src_param/`, `map/src/src_region/`, and `map/src/src_dam/` are reusable workflow sources intended to be copied into the relevant map directory when preparing or extending that map. Some regional and shared tools under `map/tools/` were added during the former v5 development phase; check the notice above and their specific documentation before relying on them. For example, the v4 channel-parameter workflow is run from a map-local `src_param/`; regionalization and reservoir workflows similarly use map-local working directories and relative paths. Copy only the components needed for the task. Some data-rich packages already include selected copies, but not every map directory contains every component.

Keep the distinction clear: `map/src/` holds source templates, while paths such as `map/glb_15min/src_param/` are working copies associated with an individual map. Copying creates a separate working copy; edits to a template do not automatically update copies already placed in map directories. These map tools operate on map data; they are not the model's numerical source tree at repository `src/`.

The v4.30 package-preparation workflow assembles `glb_15min/`, `conus_06min/`, and `tej_01min/` for the standard examples. Other maps require the corresponding downloads or regional preparation. If a map directory is a symbolic link, check that its target is accessible before running the model or preprocessing tools.

`glb_15min/` is the global 15-minute (0.25-degree) river map; `params.txt` records its dimensions and geographical coverage. Higher-resolution supporting files, such as those under `glb_15min/1min/`, are used by selected preprocessing and downscaling workflows. They do not change the base river-map resolution. Packages may also include prepared working copies such as `glb_15min/src_param/`; check which components and supporting layers are included with your map release.

The v4 map-processing programs use the repository's active build configuration in [`adm/`](../adm/README.md). The retained `map/tools/` Makefile and workflows reflect their former v5 development setup; verify their dependencies and active configuration before using them. Map tools are separate from the numerical model executable in `src/`.

### Dataset, tools, and products are different things

| Category | Examples | How to treat it |
| --- | --- | --- |
| Distributed datasets | Global or Japan river-map archives and high-resolution supporting layers. | Obtain from the official project download page and follow the specific dataset terms. |
| Repository tools | v4 tools under `map/src/` and shared code/templates retained under `map/tools/`. | Some `map/tools/` documentation describes the former v5 workflow. These are source code for preparing, inspecting, or transforming data; they are not the datasets themselves. |
| Generated products | Regional network files, channel parameters, runoff input matrices, MPI region maps, and dam parameter tables. | Generate or obtain these for the selected map and configuration; keep their metadata and inputs together. |
| Optional ancillary data | Groundwater-delay, sea-level, reservoir, gauge, and high-resolution information. | Prepare only when a selected feature or workflow requires it; requirements are feature-specific. |

A successful build of the map tools does not supply external source datasets or create a complete ready-to-run map by itself. Conversely, a downloaded map may include precomputed products, so check its archive contents before regenerating parameters.

## Global and regional maps

A **global map** supplies a connected network and parameters over its published global domain at a selected grid spacing; `glb_15min/` is the global 15-minute example used by the standard runs. A **regional map** is derived for a smaller domain, usually by starting from a compatible global map and selecting region bounds. The regional tools adjust the network and related arrays to the regional grid and can combine available higher-resolution map information. Source global maps used for preparation are package data, not GitHub fixtures.

The v4 regionalization template is under `map/src/src_region/`. A separate regionalization template and guide remain under `map/tools/script_region/`; they document the former in-repository v5 workflow and may depend on its archived registry or directory assumptions. Consult the current v5 repository for up-to-date v5 instructions. These are map-preparation workflows, not automated tests.

GitHub-only map-tools checks validate metadata, source functionality, and CTL generation using synthetic fixtures and the committed Mekong binary sample. They do not execute regionalization or require the distribution package's global data. See [GitHub-only automated tests](tools/README.md#github-only-automated-tests).

Examples of maps used by sample runs are listed in [`gosh/README.md`](../gosh/README.md). A regional map must remain consistent with its source resolution and extent, downstream connectivity, metadata, and parameter files. Do not substitute files from a different map tag merely because their filenames match.

## Shared map-product registry

[`v5_dev/variable_table/map_variables.csv`](../v5_dev/variable_table/map_variables.csv) is the archived source-linked variable and data-product registry for map products from the former v5 development workflow. It uses symbolic dimensions such as `NX`, `NY`, and `NLFP`; treat its model-reader mappings and workflow status as historical rather than authoritative for current v4 development.

[`v5_dev/variable_table/map_metadata_spec.md`](../v5_dev/variable_table/map_metadata_spec.md) explains the archived CSV schema and its relation to per-map `params.txt`, `mapdim.txt`, runoff/input-grid descriptors, and GrADS CTL files. Legacy v4 configurations may use `diminfo_*.txt`; check the actual map and workflow documentation before applying archived metadata conventions.

The former v5 bifurcation workflow documentation describes writing named `bifprm.csv` and a static NetCDF-4 counterpart; verify the current v4 reader and map-local instructions before using those products. The archived registry and workflow notes remain available for historical reference. Data-rich packages may include prepared examples; check the actual package contents before generating replacements.

### GrADS CTL generation

The retained CTL generator is [`map/tools/code/generate_ctl.py`](tools/code/generate_ctl.py), invoked by the map-local [`s02-write_ctl.sh`](tools/script_region/s02-write_ctl.sh) wrapper. Its original workflow reads the v5 registry archived under `v5_dev/variable_table/`; verify that its expected registry path and other inputs are available before running it. This section documents the former workflow snapshot, not a guarantee of compatibility with the current v4 checkout.

```sh
cd map/<map-tag>/script_region
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

See the retained [tools guide](tools/README.md#grads-ctl-files) for the former workflow's registry formats and limitations. NetCDF map-reading support in the v4 model is separate from map conversion tooling.

## Map parameters and preprocessing

`src/src_param/` supports several kinds of preparation:

- Estimating channel width and depth from long-term discharge inputs using the configured empirical relationships.
- Fusing channel-width estimates with Global Width Database for Large Rivers (GWD-LR) information where configured.
- Preparing bifurcation parameters and other river-network lookup data.
- Generating an `inpmat` lookup table that maps an external runoff grid onto the CaMa-Flood unit catchments. One matrix is specific to a map and a forcing-grid definition; it must match the forcing grid and its orientation.
- Allocating gauges, dams, or level stations, and creating basin, sub-basin, or other diagnostic products.

The [v4 parameter-tools README](src/src_param/README.md) documents the v4 workflow. The retained [parameter workflow guide](tools/script_params/README.md) describes the former v5 workflow. Check each workflow's grid and format assumptions before reusing an input matrix; naming conventions may differ.

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
2. For the v4 workflow, follow the instructions in `map/src/src_region/README.md`. The retained v5-era workflow under `map/tools/script_region/` is included only as a snapshot; consult the current v5 repository before using it for v5 work.
3. Keep the chosen workflow's source map, domain bounds, and required supporting data consistent.
4. Review the program output and map dimensions/products; run `s02-write_ctl.sh` separately if GrADS descriptors are needed.
5. Prepare simulation-specific channel parameters and runoff input matrices in the separate `script_params/` workflow when available; the regionalization sample matrix is only for channel parameterization from sample runoff.

The [v4 regional workflow guide](src/src_region/README.md) documents the v4 template. The [retained regional workflow guide](tools/script_region/README.md) is historical v5-era material and may not describe the current v5 implementation.

### Prepare additional parameters

1. Identify the exact missing parameter and which model option or forcing workflow needs it.
2. Confirm the target map tag, grid dimensions, resolution, orientation, units, and required source datasets.
3. Use the corresponding `src_param/` or specialized tool, following its own documentation.
4. Verify the generated file belongs with the target map and has the layout expected by the model reader or consuming workflow.

### Prepare optional map information

Check the feature-specific manual or workflow first. For reservoirs, see `src/src_dam/ReadMe.md`; for levees, see [`doc/levee.md`](../doc/levee.md). Optional features can require external datasets, build options, namelist groups, and matching map metadata in addition to an extra file.

## Map file organization and compatibility

Map directories are commonly named by region and grid spacing, for example `glb_15min` or `conus_06min`. The model's namelist explicitly names the map paths; scripts commonly set a base such as `FMAP` and build those paths from it. A directory name alone does not establish that its contents are complete or compatible.

The standard binary map path uses files such as `nextxy.bin` (downstream grid indices), `ctmare.bin` (unit-catchment area), `elevtn.bin`, `nxtdst.bin`, `rivlen.bin`, and `fldhgt.bin`; channel parameters may include `rivwth.bin`, `rivhgt.bin`, and `rivman.bin`. Runoff/input-grid descriptors carry dimensions and map bounds: v5 tools name them `inpdim_<ROFFTAG>.txt`, while existing v4 configurations retain `diminfo_*.txt`. The model's namelist key remains `diminfo_file`. Optional files and required names depend on the selected model options. The `NMAP` namelist in `src/cmf_ctrl_maps_mod.F90` shows the paths read by the model, while sample execution scripts show concrete configurations.

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
- [Dam/reservoir parameter workflow](src/src_dam/ReadMe.md)
- [Model source architecture](../src/README.md)
- [Sample simulation workflows](../gosh/README.md)
