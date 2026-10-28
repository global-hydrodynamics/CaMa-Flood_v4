# CaMa-Flood v4

## Overview

CaMa-Flood (Catchment-based Macro-scale Floodplain) is a river hydrodynamics model for regional, continental, and global river and flood simulations. It routes runoff through a river network and represents river channels and floodplain storage using sub-grid topography. The model computes river discharge and flow velocity using the local inertial equation, and diagnoses water level and inundated area from water storage.

The repository includes adaptive time stepping, river bifurcations, reservoir operation, levee representation, upstream inflow and sea-level boundary options, and tools for diagnostic downscaling of flood depth. Optional extensions include water temperature and ice, tracers, and sediment transport. Their maturity, data requirements, and available combinations differ; consult the relevant documentation before enabling them.

## Getting Started

### Downloadable distribution package

**New users who want to run CaMa-Flood should start with the distribution package.** It includes model code, basic river-map datasets, sample runoff forcing, and example experiment configurations.

The repository and downloadable distribution also include the official `map/mekong_15min/` sample, including its complete 1-minute high-resolution layers. See the [map guide](map/README.md) and [sample inventory](map/mekong_15min/README.md).

Visit the [official CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) for downloads and current package information:

1. Complete the [registration and license-agreement form](https://forms.gle/bhq1qWqybeAk157v9).
2. Receive the download password by email.
3. Follow the download-page link on the official website.

Start with the [main manual](doc/Manual_CaMa-Flood_v420.docx) and [user guideline](doc/CaMa-Flood_Guideline.md). These documents contain version-specific examples; use the instructions and datasets appropriate to your package.

### Source code repository

This repository provides source code, build configurations, execution scripts, and preprocessing and analysis tools for development, customization, and source-code access. **Cloning it alone does not supply all the river-map and forcing datasets needed for the standard simulations.** Obtain the required data separately through the official distribution.

Build settings are in [adm/Mkinclude](adm/Mkinclude), with compiler-specific examples in [adm/](adm/). The [compilation script](gosh/compile.sh) and [sample experiments](gosh/) provide execution entry points. Configure paths, libraries, and options for your environment before building or running; the presence of a configuration example does not establish that it has been tested on your system.

### Python utilities

Use Python 3.10 or newer on macOS or Linux. The repository setup creates an isolated `.venv/` and installs the shared dependencies for supported Python utilities; it does not change system Python or an existing Conda environment. Some specialist workflows have separate requirements, noted in their component guides.

```bash
bash s00-python_setup.sh
source .venv/bin/activate
```

The optional ERA5-Land downloader also requires CDS account credentials and dataset terms acceptance. The legacy N-year flood-depth scripts and the GTSM preprocessing notebook use additional dependencies not included in the shared environment. Windows is not supported by this shell setup script.

## Repository Structure

| Path | Responsibility |
| --- | --- |
| [src/](src/) | Fortran numerical model, drivers, shared state, I/O, and optional extensions. |
| [gosh/](gosh/) | Compilation and execution scripts, example experiments, and namelist templates. |
| [adm/](adm/) | Compiler, library, precision, parallelization, and build configuration. |
| [map/](map/) | River-map preprocessing, regionalization, river parameters, input mapping, and gauge/reservoir allocation. |
| [inp/](inp/) | Runoff forcing download and preparation tools. |
| [etc/](etc/) | Optional-scheme workflows, parameter preparation, downscaling, validation, and analysis. |
| [util/](util/) | Supporting utilities used by scripts. |
| [doc/](doc/) | Scientific manuals, user documentation, and package-preparation guidance. |
| [docs/](docs/) | Development coordination information. |
| [AGENTS.md](AGENTS.md) | Authoritative development rules for human developers and AI coding agents. |
| [Versions.md](Versions.md) | Source-code change history. |
| [README.md](README.md) | Introduction and navigation for the repository. |

Within `src/`, `cmf_drv_*` manages initialization, advancement, and finalization; `cmf_ctrl_*` coordinates model components; `cmf_calc_*` implements calculations; and `yos_cmf_*` holds shared configuration, state, map, time, and diagnostic variables. [MAIN_cmf.F90](src/MAIN_cmf.F90) is the stand-alone entry point. The main manual describes the core organization and hydrodynamic calculations.

## Documentation

The existing manuals remain the detailed scientific and workflow references. Their filenames identify applicable versions where available; they do not imply that all later extensions are covered.

| Topic | Documentation |
| --- | --- |
| Model formulation, basic setup, standard input/output, and execution | [Main manual, v4.20 (Word)](doc/Manual_CaMa-Flood_v420.docx); [user guideline](doc/CaMa-Flood_Guideline.md) |
| River parameters and runoff input matrices | [Map-parameter tools](map/src/src_param/README.md) |
| Native-grid runoff remapping | [Composed remapping tools, assumptions, and validation](map/src/src_param/native_inpmat/README.md) |
| Runoff forcing preparation | [ERA5-Land download and conversion](inp/download_ERA5/ReadMe.md) |
| Reservoir operation | [Reservoir manual, v4.20 (Word)](doc/Manual_ReservoirOperation_v420.docx); [parameter-preparation workflow](map/src/src_dam/ReadMe.md) |
| Levee parameters | [Levee guide](doc/levee.md); [Word manual](doc/LeveeParameter.docx) |
| Upstream inflow | [Upstream inflow manual (Word)](doc/Mannual_UpstreamInflowScheme.docx) |
| MPI/OpenMP, precision, and high-performance computing | [HPC notes (Word)](doc/Note_HighPerformanceComputing.docx); [example workflows](etc/options_HPC/README.md) |
| River gauge allocation | [Gauge allocation manual (Word)](doc/Manual_AllocateRiverGauges.docx); [attribute-error supplement (Word)](doc/Supp_GaugeAttributeError.docx) |
| Flood-depth downscaling | [Workflow guide](etc/downscale_flddph/ReadMe.md); [method notes (Word)](doc/Note_Downscaling.docx) |
| Water temperature and ice | [Heatlink stability and diagnostics](src/heatlink/README.md) |
| Comparison with observations | [Validation examples](etc/validation/ReadMe.md) |
| Distribution preparation | [Package-creation guide](doc/HowTo_Make_Package.md) |

Small tests are located under [src/common/test/](src/common/test/), [src/mod/test/](src/mod/test/), [src/io/test/](src/io/test/), [src/phys/test/](src/phys/test/), and [src/heatlink/test/](src/heatlink/test/). Check their Makefiles and runners for dependencies and execution steps: some `test` targets only build executables. Native-grid remapping tests and integration-validation instructions are documented in its guide above.

## Development and Contributions

Read [AGENTS.md](AGENTS.md) for development rules, AI-agent behavior, branch management, and contribution procedures. Consult [docs/dev_status.md](docs/dev_status.md) for active-project coordination and the status-publication workflow defined in `AGENTS.md`.

## Versions and Releases

See [Versions.md](Versions.md) for recorded changes and the [official website](https://global-hydrodynamics.github.io/CaMa-Flood/) for distribution information. Source development and data-rich distribution packages are maintained separately; do not assume that `master` is identical to a particular downloadable package. For reproducible work, record the source commit or tag and the datasets used.

## Citation and License

Please cite the model description and numerical-method papers when using CaMa-Flood:

- Yamazaki, D. (2025). **Advancing global river hydrodynamics simulations by catchment-based macro-scale floodplain modeling approach.** *Geoscience Letters*, 12, 72. [doi:10.1186/s40562-025-00452-z](https://doi.org/10.1186/s40562-025-00452-z).
- Yamazaki, D., Kanae, S., Kim, H., & Oki, T. (2011). A physically based description of floodplain inundation dynamics in a global river routing model. *Water Resources Research*, 47, W04501. [doi:10.1029/2010WR009726](https://doi.org/10.1029/2010WR009726).
- Yamazaki, D., de Almeida, G. A., & Bates, P. D. (2013). Improving computational efficiency in global river models by implementing the local inertial flow equation and a vector-based river network map. *Water Resources Research*, 49(11), 7221–7235. [doi:10.1002/wrcr.20552](https://doi.org/10.1002/wrcr.20552).

When using the river water temperature model, also cite:

- Tokuda, D., Kim, H., Yamazaki, D., & Oki, T. (2019). Development of a global river water temperature model considering fluvial dynamics and seasonal freeze-thaw cycle. *Water Resources Research*, 55(2), 1366–1383. [doi:10.1029/2018WR023083](https://doi.org/10.1029/2018WR023083).

Use the [CaMa-Flood Zenodo archive](https://doi.org/10.5281/zenodo.4609654) to find and cite the specific archived software version used. The [official references](https://global-hydrodynamics.github.io/CaMa-Flood/#references--related-papers) provide additional model, scheme, and dataset publications.

The model source code is distributed under the [Apache License 2.0](LICENSE). Separately distributed river maps, forcing, and other datasets have their own licensing terms. Consult the terms supplied with each dataset and the [official licensing information](https://global-hydrodynamics.github.io/CaMa-Flood/#license-to-use-cama-flood-model); the software license does not grant rights to all accompanying data.

## Contact and Contributions

For package access and project inquiries, contact Dai Yamazaki at **yamadai [at] iis.u-tokyo.ac.jp**. Community discussion is available through the [CaMa-Flood user group](https://groups.google.com/g/cama-flood-user).

Report reproducible bugs or propose source-code improvements through [GitHub Issues](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues). Include the source revision, relevant configuration, environment, reproduction steps, and diagnostic output. Follow [AGENTS.md](AGENTS.md) when undertaking development work.
