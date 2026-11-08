# Tutorial Design — Tutorial01: Get Started

**Purpose:** define the first-run actions, inputs, and acceptance criteria for the CaMa-Flood v5 getting-started tutorial. Shared implementation instructions are in [`../agent_tutorial_development.md`](../agent_tutorial_development.md); the learner-facing content is specified in [`design/manual_design.md`](design/manual_design.md).

## Section 0: Obtain CaMa-Flood

Introduce two ways to obtain CaMa-Flood, and distinguish their purpose:

- **v5 development repository:** [`https://github.com/global-hydrodynamics/CaMa-Flood_v4`](https://github.com/global-hydrodynamics/CaMa-Flood_v4) is the GitHub repository for development contributors. Tutorial01's current instructions and sample workflow are verified against this v5 development checkout.
- **Official distribution package:** the [official CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) describes the general-user package download. Access requires registration and license agreement through a Google Form; the download password is sent by email. Download the archive and extract it into a suitable working directory.

Tutorial01 has not been verified with the official distribution package. Do not imply that its v5 development files, sample inputs, or script are present or compatible there. For this tutorial's workflow, obtain the v5 development repository unless a separately verified package is specified.

Ask learners to clone or download the chosen source, extract it if needed, place it in a suitable location, and identify the resulting CaMa-Flood root directory before Section 1.

## Learning outcomes

Tutorial01 introduces repository navigation and a first model run; it does not teach river hydrodynamics or numerical modeling. Learning outcomes focus on observable **operational skills** and simple file/configuration relationships.

| Stage | Operational skills | Basic relationship to identify | Observable evidence of learning |
| --- | --- | --- | --- |
| Obtain CaMa-Flood | Clone or download a suitable source, extract an archive if needed, and locate its root folder. | Distinguish the v5 development repository from the official user distribution package. | The learner can identify which source they obtained and point to its root folder. |
| Find the repository | Confirm the root with `pwd`, assign `cmf_root`, and locate `src_main/`, `map/`, `inp/`, and setup scripts. | Know that `cmf_root` is a shell variable available in the current session. | The learner can name where the model source and bundled inputs are and reuse the root path in commands. |
| Prepare the environment | Run the setup scripts, recognize check results and prompts, and locate the suggested configuration. | Identify that the suggested root `Mkinclude` is separate from active `config/Mkinclude`; recognize Python setup as optional for this tutorial. | The learner can follow the setup instructions and avoid replacing an active configuration without a backup. |
| Build the model | Run `make all` in `src_main/` and locate `MAIN_cmf`. | Identify that the executable is produced in `src_main/` from model source. | The learner can find the executable after building. |
| Read experiment settings | Read this specification and inspect the standalone GOSH script. | Identify where experiment details are described and where the script settings are written. | The learner can locate a setting in the script after finding it in the experiment information. |
| Locate river-map files | List representative map files and inspect `MAPDIR`, `NEXTXY`, and `RIVWTH`. | Match the script's path and filenames to files in the map directory. | The learner can point out the selected map directory, connectivity file, and width file. |
| Locate runoff files | List daily runoff files and find the relevant dataset and filename settings. | Match the directory and filename parts in the script to actual sample files. | The learner can locate a dated runoff file and the settings that point to its dataset and naming pattern. |
| Locate runoff-mapping files | List the input-dimension descriptor and matrix and find `INPDIM` and `INPMAT`. | Match those settings to the two files in the map directory; identify `LINTERP` as another setting in the same block. | The learner can find each referenced file and its setting in the GOSH script. |
| Run an experiment | Execute the Tutorial01 script from its documented directory and identify `PROJ` and `RUNDIR`. | Match the project name to the output directory shown by the script. | The learner can run the script, recognize its success message, and locate the selected run directory. |
| Locate results | List the project output and inspect the model log. | Identify which generated file is discharge output and which file is the model log. | The learner can locate `outflw2000.bin`, `log_CaMa.txt`, and see evidence that the run completed. |

Starting from the v5 development repository with its bundled Mekong samples and a prepared configuration, the learner completes these stages, runs the 40-day experiment, and locates results under `tutorial/tutorial01-get_started/out/${PROJ}/`. The current tutorial has not been verified against the official distribution package. The starting presence of the executable or output directory is not prescribed.

## Repository layout to introduce

The manual should show a compact tree based on the actual repository, including:

```text
CaMa-Flood_v4/
├── config/Mkinclude                 # Active v5 compiler configuration
├── src_main/                        # v5 Fortran source and MAIN_cmf after build
├── map/mekong_15min/                # Bundled 80 x 120 river map
│   ├── params.txt                   # Map dimensions, grid, and geographic bounds
│   ├── inpdim_test-mek_1deg.txt     # Runoff-grid dimensions and mapping metadata
│   └── inpmat_test-mek_1deg.bin     # Runoff-to-map input matrix
├── inp/test-mek_1deg/runoff/        # Bundled daily Binary runoff
├── gosh_sample/                     # Maintained sample GOSH workflows
└── tutorial/tutorial01-get_started/
    ├── tutorial_design.md
    ├── tutorial01-get_started.sh    # Standalone educational GOSH experiment
    ├── manual_tutorial01-get_started.md
    └── out/tutorial01-get_started/   # Default project results; Git-ignored
```

The repository also contains root setup scripts and shared tools; include them in the explanatory text, not necessarily in this compact tree. `tutorial/design/` is developer-facing material and need not appear in the learner's main tree.

## Starting state and prerequisites

- A newly obtained checkout or qualifying package contains `src_main/`, `config/`, the bundled `map/mekong_15min/` and `inp/test-mek_1deg/` samples, the root `s01-setup_fortran.sh` and `s02-python_setup.sh`, and Tutorial01.
- The current state of `src_main/MAIN_cmf` and Tutorial01's `out/` is not prescribed. `make all` builds or updates the required executable, and the sample script replaces only its known generated files on rerun. Sample map, runoff, and matching input-matrix data are included; no scientific data download or map generation is required.
- GNU Make, GNU Fortran with OpenMP, and NetCDF-Fortran are available. Run `s01-setup_fortran.sh`, review its recommended configuration, and manually copy the staged `Mkinclude` to the existing `config/Mkinclude` location before building.
- Run `s02-python_setup.sh` for Python environment diagnosis/setup. Tutorial01's CTL tools use Python standard-library features; installing the full optional requirements set is not required for the sample run.
- The developer reset utility requires `config/Mkinclude` to exist and must preserve it.

## Experiment inputs and settings

- Map: `map/mekong_15min/`, Binary format, 80 × 120 cells, 0.25-degree grid, and 10 floodplain layers.
- Runoff: `inp/test-mek_1deg/runoff/`, daily Binary `mm/day` files named `Roff____YYYYMMDD.one`; the sample grid is 20 × 30.
- Runoff-to-map files: `map/mekong_15min/inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin`, generated for this map/runoff grid pairing.
- Simulation: 2000-01-01 00:00 to 2000-02-10 00:00 exclusive (40 days), non-MPI, base timestep 3600 seconds, adaptive timestep and bifurcation enabled, reservoir operation disabled, one OpenMP thread.
- Preserve the scientific and I/O settings of `gosh_sample/mek01-default_run.sh`. The tutorial's complete standalone script changes only repository-relative context, output isolation under `tutorial/tutorial01-get_started/out/${PROJ}/`, the project name, and OpenMP thread count to one. The maintained `gosh_sample/` script remains unchanged.
- Define `PROJ="tutorial01-get_started"` using the established experiment-name convention. Construct `RUNDIR` as the Tutorial01 script directory's `out/${PROJ}/`. Generated executable copy, Namelist, logs, model outputs, CTLs, and Restart files all belong there.
- A rerun of the same project may replace only that project's known generated files. Changing `PROJ` creates a separate experiment directory and must preserve the previous project directory. Do not remove the shared `out/` parent or other project subdirectories.
- Output variables: `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc`; daily Binary output and standard end-of-run Binary Restart.

## Required workflow

1. Present the GitHub development repository and official package access routes. State that the Tutorial01 workflow is verified only against the v5 development checkout, not the official package. Ask the learner to obtain the appropriate source, extract an archive if needed, place it in a suitable location, and locate its root folder.
2. Confirm the v5 development repository root with `pwd`, define `cmf_root="$(pwd)"`, and inspect the main directories. The subsequent workflow uses the v5 checkout; package compatibility is not implied.
3. Prepare the Fortran and Python environments using the root setup scripts.
4. Activate `config/Mkinclude` by copying the reviewed staged `Mkinclude` manually; `config/` already exists.
5. From `${cmf_root}`, enter `src_main/` and run `make all` to build or update the current model executable.
6. Locate and list bundled map, runoff, input-dimension, and input-matrix files before relating their names to the GOSH settings.
7. Read `tutorial_design.md`, then inspect the plain-text standalone Tutorial01 GOSH configuration and its experiment settings.
8. From `tutorial/tutorial01-get_started/`, execute `./tutorial01-get_started.sh`; the default project output is `out/tutorial01-get_started/`.
9. Inspect the generated Namelist, logs, CTLs, model outputs, and Restart under the project-specific directory.

Map generation and visualization are not required.

## Acceptance criteria

- Running `make all` from `src_main/` returns zero and leaves an executable `MAIN_cmf`, whether it is newly built or updated.
- Section 0 identifies the GitHub v5 development repository and the official package route, accurately describes the official site's registration/password process, and does not imply Tutorial01 package compatibility.
- The standalone tutorial script defines the default `PROJ`, constructs its `RUNDIR` under `out/${PROJ}/`, does not call or source `gosh_sample/mek01-default_run.sh`, and runs from its documented directory.
- Tutorial01 and the model return zero; `log_CaMa.txt` reaches normal calculation completion.
- Five expected daily Binary output variables and their CTLs, Namelist, logs, executable copy, and end Restart products are created under only `tutorial/tutorial01-get_started/out/tutorial01-get_started/`.
- Each daily output contains 40 records for the 40-day interval. The end time is exclusive; timestamp details follow the model's output convention.
- Bundled map, runoff, and matching input-matrix files remain unchanged.
- In a disposable checkout, the developer reset can remove build products and one explicitly selected Tutorial01 project directory while preserving `config/Mkinclude`, bundled inputs, the `out/` parent, and unrelated project directories.

Successful execution is operational evidence, not scientific validation.
