# River maps and map-preparation tools

The `map/` directory connects CaMa-Flood's river and floodplain representation to the datasets and preprocessing tools used to prepare it. This source checkout contains map-processing code and documentation, but it does not contain the downloadable global or regional map datasets. Obtain map data separately before running a simulation.

For model setup and example runs, see the [sample execution guide](../gosh/README.md). This page is an overview; tool-specific details remain in the component documentation linked below.

## What is a CaMa-Flood river map?

CaMa-Flood represents a river network as **unit catchments**: drainage areas associated with cells on a regular longitude/latitude grid. A map records how those cells connect downstream and supplies the geographical and hydraulic information needed to route water and diagnose river and floodplain conditions.

The model reads the downstream network, converts active grid cells into a river sequence for calculation, and combines that topology with catchment area, elevations, channel dimensions, reach lengths, and floodplain topography. These sub-grid parameters describe channel and floodplain properties that a model grid cell cannot represent explicitly. The map and parameters therefore work together; a river-network file alone is not a complete simulation map.

The source includes binary map readers and an optional NetCDF map path. The active map files and formats are selected in the model namelist. Consult the [scientific and user manuals](../doc/CaMa-Flood_Guideline.md) for the scientific formulation and detailed model setup.

## Obtain official map datasets

Use the [official CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) for current package, map, registration, and download information. Map archives are distributed separately from the development repository; access may require registration and a license agreement. The website lists global maps at several resolutions and specialized Japan maps, with their current availability and download instructions.

The official page describes basic map datasets under CC BY 4.0 and notes that some advanced high-resolution data have separate access conditions. Dataset terms can differ. Check the current website and the license or agreement supplied with the exact dataset you use; the source-code license does not automatically apply to map data.

This repository and a downloadable package are not necessarily identical. In the current checkout, `map/` contains source tools but not folders such as `glb_15min/`, `conus_06min/`, or the associated binary map files. A package may include some basic maps and sample inputs; verify the contents of the package rather than assuming it matches this Git checkout.

## Directory organization

| Path | Role | Notes |
| --- | --- | --- |
| `map/src/src_param/` | Reusable source template for preparing or inspecting map parameters and forcing-to-map input matrices. | Has a [component README](src/src_param/README.md); copy the component into a target map directory to work with that map. |
| `map/src/src_param/native_inpmat/` | Optional native-grid remapping workflow associated with `src_param`. | Compose a native-grid remapping operator with an existing CaMa input matrix and validate results; see its [README](src/src_param/native_inpmat/README.md). |
| `map/src/src_region/` | Reusable source template for extracting a regional domain from a global map and preparing regional products. | See the [regional-map preparation guide](src/src_region/README.md). |
| `map/src/src_dam/` | Reusable source template for preparing dam/reservoir locations and parameters for a target map. | Has an existing [reservoir-preparation README](src/src_dam/ReadMe.md). |
| `map/src/` | Source templates for map-local workflows. | Contains `src_param/`, `src_region/`, and `src_dam/`; this is separate from the model's numerical `src/`. |
| `map/<map-tag>/` | A map instance: downloaded map data, metadata, derived products, and any map-local processing copies needed for that workflow. | Examples include `glb_15min` and `conus_06min`; map data directories are not included in this source checkout. |
| `map/data/` | Optional supporting datasets in some data-rich packages. | The inspected package sample includes GRSAD, ReGeom, and levee-related data; availability and terms are package-specific. |

### Reusable sources and map-local working copies

The directories `map/src/src_param/`, `map/src/src_region/`, and `map/src/src_dam/` are reusable workflow sources intended to be copied into the relevant map directory when preparing or extending that map. For example, the channel-parameter workflow is run from a map-local `src_param/`; the regionalization and reservoir workflows similarly use map-local working directories and relative paths. Copy only the components needed for the task. Some data-rich packages already include selected copies, but not every map directory contains every component.

Keep the distinction clear: `map/src/` holds source templates, while paths such as `map/glb_15min/src_param/` are working copies associated with an individual map. Copying creates a separate working copy; edits to a template do not automatically update copies already placed in map directories. These map tools operate on map data; they are not the model's numerical source tree at repository `src/`.

In the inspected data-rich package sample, the map root contains global and regional map-tag directories (including `glb_15min/`, `conus_06min/`, `mekong_15min/`, and `tej_01min/`) plus a `data/` directory with optional supporting datasets. Some other map-tag entries are symbolic links, so a directory entry alone does not prove that its data are present locally.

`glb_15min/` is the global 15-minute map. Its `params.txt` describes a 0.25-degree grid covering the globe, and it contains a nested `1min/` directory with higher-resolution supporting files. That nested support data does not change the base river-map resolution indicated by `glb_15min`. The package sample also contains a map-specific copy at `glb_15min/src_param/` and regional workflow directories under selected regional maps. Contents vary by map and package.

The map-processing Makefiles use the repository's active build configuration in [`adm/`](../adm/README.md). Their programs are separate from the numerical model executable in `src/`.

### Dataset, tools, and products are different things

| Category | Examples | How to treat it |
| --- | --- | --- |
| Distributed datasets | Global or Japan river-map archives and high-resolution supporting layers. | Obtain from the official project download page and follow the specific dataset terms. |
| Repository tools | Fortran and Python programs, shell workflows, configuration, and documentation under `map/src/`. | These are source code for preparing, inspecting, or transforming data; they are not the datasets themselves. |
| Generated products | Regional network files, channel parameters, runoff input matrices, MPI region maps, and dam parameter tables. | Generate or obtain these for the selected map and configuration; keep their metadata and inputs together. |
| Optional ancillary data | Groundwater-delay, sea-level, reservoir, gauge, and high-resolution information. | Prepare only when a selected feature or workflow requires it; requirements are feature-specific. |

A successful build of the map tools does not supply external source datasets or create a complete ready-to-run map by itself. Conversely, a downloaded map may include precomputed products, so check its archive contents before regenerating parameters.

## Global and regional maps

A **global map** supplies a connected network and parameters over its published global domain at a selected grid spacing; `glb_15min/` is the global 15-minute example confirmed by the inspected package. A **regional map** is derived for a smaller domain, usually by starting from a compatible global map and selecting region bounds. The regional tools adjust the network and related arrays to the regional grid and can combine available higher-resolution map information.

The sample regional script, `src/src_region/s01-regional_map.sh`, reads a source map and west/east/south/north bounds from its settings, then calls `cut_domain`, `cut_bifway`, and `set_map`. It may combine high-resolution layers when those source layers are present, generate a regional input matrix, and write map-control metadata. The exact outputs depend on the source map and available high-resolution data; this overview does not claim that the sample has been run.

Examples of maps used by sample runs are listed in [`gosh/README.md`](../gosh/README.md). A regional map must remain consistent with its source resolution and extent, downstream connectivity, metadata, and parameter files. Do not substitute files from a different map tag merely because their filenames match.

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

1. Obtain a compatible global map and any high-resolution layers required by the regional workflow.
2. Copy the relevant `src_region/` workflow from `map/src/` into the target map directory and set the source map and target domain bounds.
3. Run the regional processing tools from their map-local working directory to cut the domain and create its network, associated products, and metadata.
4. Copy or use the relevant `src_param/` tools to prepare channel and forcing input parameters for that regional product, then configure a run using the regional grid.

See `src/src_region/s01-regional_map.sh` for the current orchestration example. It is a starting point to inspect, not a universal command sequence for all map sources or regions.

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

When combining or generating map products, keep dimensions, grid spacing, geographic bounds, latitude/longitude ordering, missing-value conventions, data types, and units consistent with the target map and reader. An input matrix must additionally match the runoff grid and orientation. Detailed format specifications belong with the relevant tool or manual rather than in this overview.

## Relationship to other directories

| Directory | Relationship to `map/` |
| --- | --- |
| [`src/`](../src/README.md) | Reads the configured network and map parameters and uses them during model initialization and simulation. |
| [`gosh/`](../gosh/README.md) | Provides sample build/run scripts that point to prepared maps and forcing data. |
| [`adm/`](../adm/README.md) | Supplies compiler and build configuration for the map tools and model. |
| [`inp/`](../inp/download_ERA5/ReadMe.md) | Prepares runoff forcing; map input matrices connect that forcing grid to the selected river map. |
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
