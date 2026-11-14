# CaMa-Flood v4

## Project Overview

CaMa-Flood (Catchment-based Macro-scale Floodplain) is a river hydrodynamics model for regional, continental, and global river and flood simulations. It routes runoff through a river network and represents river channels and floodplain storage using sub-grid topography. The v4 model computes river discharge and flow velocity using the local inertial equation, and diagnoses water level and inundated area from water storage.

The v4 code includes adaptive time stepping, river bifurcations, reservoir and levee options, and upstream-inflow and sea-level boundary workflows. Optional extensions have their own data and build requirements; consult the corresponding v4 documentation before enabling them.

This repository is maintained for using and maintaining CaMa-Flood v4, accessing its source and documentation, and referring to its development history. The v4 source is under `src/`; existing build and run workflows remain under `adm/` and `gosh/`.

## CaMa-Flood v5 Development

**CaMa-Flood v5 development has moved to the independent repository [global-hydrodynamics/cmf_v5_beta](https://github.com/global-hydrodynamics/cmf_v5_beta).** v5 is under active beta development and is not yet an official stable release. Its goals include improving source maintainability, reproducible experiments, tutorials and visualizations, package creation, community contributions, and AI-assisted development.

Use the new repository for v5 development, feature work, and pull requests. The [`v5_dev/`](v5_dev/README.md) directory in this repository is an archive of the v5 development files that were here before the move; it is not the current v5 work area. Users who need a stable, supported distribution should continue to use the official CaMa-Flood v4 package.

## Download and Getting Started

For a standard v4 installation, use the official [CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) to find the current package and download instructions. Package access requires registration and acceptance of the license agreement. Submit the [registration form](https://forms.gle/bhq1qWqybeAk157v9); download credentials are sent by email, after which the package can be obtained from the website.

Start with the [CaMa-Flood v4.20 manual](doc/Manual_CaMa-Flood_v420.docx) and [user guideline](doc/CaMa-Flood_Guideline.md). The [sample execution guide](gosh/README.md) explains the legacy v4 build and run scripts. A source-code clone does not necessarily include all map and forcing datasets required for a simulation; use the official distribution or obtain required data separately, subject to its terms.

## Repository Structure

| Path | Purpose |
| --- | --- |
| [`src/`](src/) | CaMa-Flood v4 model source. |
| [`adm/`](adm/) | v4 compiler and library build configuration. |
| [`gosh/`](gosh/) | v4 compilation and sample execution scripts. |
| [`map/`](map/) | River-map data, map preparation sources, and related tools. |
| [`inp/`](inp/) | Runoff forcing preparation and input utilities. |
| [`etc/`](etc/) | Optional schemes, specialized workflows, and analysis utilities. |
| [`util/`](util/) | Supporting utilities. |
| [`out/`](out/) | Local model-run output location; contents are generated and may be ignored by Git. |
| [`doc/`](doc/) | User manuals, scientific notes, and package documentation. |
| [`visualization/`](visualization/README.md) | Visualization examples retained from v5 development; not validated for v4. |
| [`v5_dev/`](v5_dev/README.md) | Archived v5 development files from before the repository split; not the current v5 development location. |
| [`archive_src.sh`](archive_src.sh) | Source-archive helper retained with the v4 repository. |
| [`Versions.md`](Versions.md) | Recorded v4 source changes. |

The [`map/README.md`](map/README.md) describes map data and preparation resources. Map and forcing data may have separate download requirements and licenses; check the documentation supplied with each dataset.

## Documentation

| Topic | Documentation |
| --- | --- |
| Model formulation, setup, inputs, and standard execution | [v4.20 manual](doc/Manual_CaMa-Flood_v420.docx); [CaMa-Flood guideline](doc/CaMa-Flood_Guideline.md) |
| Build and sample runs | [gosh sample execution guide](gosh/README.md) |
| River maps and map preparation | [map guide](map/README.md) |
| Reservoir operation | [v4.20 reservoir manual](doc/Manual_ReservoirOperation_v420.docx); [reservoir preparation guide](map/src/src_dam/ReadMe.md) |
| Levee parameters | [levee guide](doc/levee.md); [parameter manual](doc/LeveeParameter.docx) |
| Upstream inflow | [upstream inflow manual](doc/Mannual_UpstreamInflowScheme.docx) |
| HPC, MPI, and OpenMP | [HPC note](doc/Note_HighPerformanceComputing.docx); [example workflows](etc/options_HPC/README.md) |
| Gauge allocation | [gauge allocation manual](doc/Manual_AllocateRiverGauges.docx) |
| Flood-depth downscaling | [workflow guide](etc/downscale_flddph/ReadMe.md); [method note](doc/Note_Downscaling.docx) |
| Validation examples | [validation guide](etc/validation/ReadMe.md) |
| Distribution package preparation | [package guide](doc/HowTo_Make_Package.md) |

The [`visualization/`](visualization/README.md) directory contains reference examples produced during v5 development. Their scripts and data assumptions are not a v4 visualization support guarantee. Consult the v5 repository for the current visualization implementation.

## Development and Maintenance

This repository accepts maintenance and bug-fix work for v4. Preserve existing v4 interfaces, data formats, and supported workflows unless a compatibility change has been explicitly agreed. Read [`AGENTS.md`](AGENTS.md) before making changes, and use the component documentation and current v4 source as the authority for behavior.

New v5 development, features, and pull requests belong in [cmf_v5_beta](https://github.com/global-hydrodynamics/cmf_v5_beta). Historical v5 files in `v5_dev/` are retained for reference only and may depend on their former directory layout.

## Versions and Releases

See [`Versions.md`](Versions.md) for recorded changes. The [official website](https://global-hydrodynamics.github.io/CaMa-Flood/) provides the current downloadable package information. A GitHub branch should not be assumed to match a particular package release; record the source revision and datasets used for reproducibility.

## Citation and License

Please cite the model description and numerical-method papers when using CaMa-Flood:

- Yamazaki, D. (2025). **Advancing global river hydrodynamics simulations by catchment-based macro-scale floodplain modeling approach.** *Geoscience Letters*, 12, 72. [doi:10.1186/s40562-025-00452-z](https://doi.org/10.1186/s40562-025-00452-z).
- Yamazaki, D., Kanae, S., Kim, H., & Oki, T. (2011). A physically based description of floodplain inundation dynamics in a global river routing model. *Water Resources Research*, 47, W04501. [doi:10.1029/2010WR009726](https://doi.org/10.1029/2010WR009726).
- Yamazaki, D., de Almeida, G. A., & Bates, P. D. (2013). Improving computational efficiency in global river models by implementing the local inertial flow equation and a vector-based river network map. *Water Resources Research*, 49(11), 7221–7235. [doi:10.1002/wrcr.20552](https://doi.org/10.1002/wrcr.20552).

When using the river water temperature model, also cite Tokuda, D., Kim, H., Yamazaki, D., & Oki, T. (2019). Development of a global river water temperature model considering fluvial dynamics and seasonal freeze-thaw cycle. *Water Resources Research*, 55(2), 1366–1383. [doi:10.1029/2018WR023083](https://doi.org/10.1029/2018WR023083).

Use the [CaMa-Flood Zenodo archive](https://doi.org/10.5281/zenodo.4609654) to find and cite the specific archived software version used. The [official references](https://global-hydrodynamics.github.io/CaMa-Flood/#references--related-papers) provide additional model, scheme, and dataset publications.

The model source code is distributed under the [Apache License 2.0](LICENSE). Separately distributed river maps, forcing, and other datasets have their own licensing terms. Consult the terms supplied with each dataset and the [official licensing information](https://global-hydrodynamics.github.io/CaMa-Flood/#license-to-use-cama-flood-model); the software license does not grant rights to all accompanying data.

## Contact

For package access and project inquiries, contact Dai Yamazaki at **yamadai [at] iis.u-tokyo.ac.jp**. Community discussion is available through the [CaMa-Flood user group](https://groups.google.com/g/cama-flood-user). Report reproducible v4 bugs through [GitHub Issues](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues), including the source revision, configuration, environment, reproduction steps, and relevant diagnostics.
