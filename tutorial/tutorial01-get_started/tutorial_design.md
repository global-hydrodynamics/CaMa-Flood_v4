# Tutorial Design — Tutorial01: Get Started

**Purpose:** define the first-run actions, inputs, and acceptance criteria for the CaMa-Flood v5 getting-started tutorial. Shared implementation instructions are in [`../agent_tutorial_development.md`](../agent_tutorial_development.md); the learner-facing content is specified in [`design/manual_design.md`](design/manual_design.md).

## Section 0: Obtain CaMa-Flood

GitHub is the standard acquisition route for Tutorial01. The intended released workflow uses `master` after Tutorial01 and its required setup scripts have been integrated there.

- **Current integration dependency:** as checked on 2026-11-08, `origin/master` at `19e2c1a` contains the v5 source and Mekong map/runoff samples, but does not contain `tutorial/tutorial01-get_started/`, `s01-setup_fortran.sh`, or `s02-python_setup.sh`. Do not tell learners that the default `master` clone can run Tutorial01 yet.
- **Temporary verified route:** until integration, use the `feature/gosh-refactor` branch, which contains the Tutorial01 files and setup scripts. For learners using Git, show an explicit branch clone command. For learners using GitHub's interface, have them select `feature/gosh-refactor` before choosing **Code → Download ZIP**.
- **After integration:** the unqualified clone command below will use the repository's default `master` branch, which is the intended Tutorial01 route once the missing files are integrated:

  ```bash
  git clone https://github.com/global-hydrodynamics/CaMa-Flood_v4.git
  ```

  Git creates a local directory named `CaMa-Flood_v4` by default. The user can instead download a ZIP from the GitHub **Code** menu, extract it, and place the resulting directory in a suitable working location.
- **Official distribution package:** the [official CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) provides a separate downloadable package and requires registration through a Google Form. The v5 package for this Tutorial01 workflow is not yet available as the supported tutorial distribution. Users who prefer the official package should wait for that release; do not use an older package for this tutorial.

Place the cloned or extracted repository in a suitable working directory; do not assume a fixed machine-specific path. Before Section 1, learners should identify the repository root. Tutorial01's current commands and sample workflow are verified against the development branch, not an official package.

## Learning outcomes

Tutorial01 introduces repository navigation and a first model run; it does not teach river hydrodynamics or numerical modeling. Learning outcomes focus on observable **operational skills** and simple file/configuration relationships.

| Stage | Operational skills | Basic relationship to identify | Observable evidence of learning |
| --- | --- | --- | --- |
| Obtain CaMa-Flood | Clone the temporary `feature/gosh-refactor` branch or select it before downloading a ZIP; after integration, use the default `master` clone. Locate the resulting root folder. | GitHub is the standard Tutorial01 route; the supported official v5 package is a future release. | The learner can obtain the currently usable branch, identify the root, and recognize that `master` becomes the standard route after integration. |
| Find the repository | Confirm the root with `pwd`, assign `cmf_root`, and inspect it with `ls -lF` for directories and executable scripts. | `ls -lF` adds display markers such as `/` for directories and `*` for executables; these markers are not filename characters. | The learner can locate `src_main/`, `map/`, `inp/`, and identify directory and executable markers in the listing. |
| Prepare the environment | Run the setup scripts, recognize check results and prompts, and locate the suggested configuration. | Identify that the suggested root `Mkinclude` is separate from active `config/Mkinclude`; recognize Python setup as optional for this tutorial. | The learner can follow the setup instructions and avoid replacing an active configuration without a backup. |
| Build the model | Run `make all` in `src_main/` and locate `MAIN_cmf`. | Identify that the executable is produced in `src_main/` from model source. | The learner can find the executable after building. |
| Read experiment settings | Read this specification and inspect the standalone GOSH script. | Identify where experiment details are described and where the script settings are written. | The learner can locate a setting in the script after finding it in the experiment information. |
| Locate river-map files | List representative map files and inspect `MAPDIR`, `NEXTXY`, and `RIVWTH`. | Match the script's path and filenames to files in the map directory. | The learner can point out the selected map directory, connectivity file, and width file. |
| Locate runoff files | List daily runoff files and find the relevant dataset and filename settings. | Match the directory and filename parts in the script to actual sample files. | The learner can locate a dated runoff file and the settings that point to its dataset and naming pattern. |
| Locate runoff-mapping files | List the input-dimension descriptor and matrix and find `INPDIM` and `INPMAT`. | Match those settings to the two files in the map directory; identify `LINTERP` as another setting in the same block. | The learner can find each referenced file and its setting in the GOSH script. |
| Run an experiment | Execute the Tutorial01 script from its documented directory and identify `PROJ` and `RUNDIR`. | Match the project name to the output directory shown by the script. | The learner can run the script, recognize its success message, and locate the selected run directory. |
| Locate results | List the project output and inspect the model log. | Identify which generated file is discharge output and which file is the model log. | The learner can locate `outflw2000.bin`, `log_CaMa.txt`, and see evidence that the run completed. |

Starting from the `feature/gosh-refactor` development checkout with its bundled Mekong samples and a prepared configuration, the learner completes these stages, runs the 40-day experiment, and locates results under `tutorial/tutorial01-get_started/out/${PROJ}/`. The checkout is temporary until Tutorial01 and its setup scripts reach `master`. The supported official v5 package is not yet available; the tutorial has not been verified against older distribution packages. The starting presence of the executable or output directory is not prescribed.

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

- The current verified starting point is the `feature/gosh-refactor` checkout, which contains `src_main/`, `config/`, the bundled `map/mekong_15min/` and `inp/test-mek_1deg/` samples, the root `s01-setup_fortran.sh` and `s02-python_setup.sh`, and Tutorial01. `origin/master` does not yet contain Tutorial01 or those two setup scripts. The official v5 package for this workflow is a future distribution; older packages are not a supported substitute.
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

1. Direct learners to GitHub. Until integration, provide `git clone --branch feature/gosh-refactor --single-branch https://github.com/global-hydrodynamics/CaMa-Flood_v4.git` or instructions to select that branch before **Code → Download ZIP**. Mark `git clone https://github.com/global-hydrodynamics/CaMa-Flood_v4.git` as the intended default-`master` method after integration, not a currently runnable Tutorial01 path. Explain that the official v5 package is not yet available and users should wait rather than use an older package. Ask learners to place the obtained checkout in a suitable location.
2. Confirm the current development checkout root with `pwd`, define `cmf_root="$(pwd)"`, and inspect it with `ls -lF`. Explain that `/` marks directories and `*` normally marks executable files; suffixes are display markers, not part of filenames.
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
- Section 0 identifies GitHub as the standard route, gives the temporary branch and post-integration `master` methods, and does not imply that `master` currently contains Tutorial01 or that an older official package is compatible.
- The root-directory demonstration uses `ls -lF`, with observed output showing directory and executable markers and an explanation that suffixes are not part of filenames.
- The standalone tutorial script defines the default `PROJ`, constructs its `RUNDIR` under `out/${PROJ}/`, does not call or source `gosh_sample/mek01-default_run.sh`, and runs from its documented directory.
- Tutorial01 and the model return zero; `log_CaMa.txt` reaches normal calculation completion.
- Five expected daily Binary output variables and their CTLs, Namelist, logs, executable copy, and end Restart products are created under only `tutorial/tutorial01-get_started/out/tutorial01-get_started/`.
- Each daily output contains 40 records for the 40-day interval. The end time is exclusive; timestamp details follow the model's output convention.
- Bundled map, runoff, and matching input-matrix files remain unchanged.
- In a disposable checkout, the developer reset can remove build products and one explicitly selected Tutorial01 project directory while preserving `config/Mkinclude`, bundled inputs, the `out/` parent, and unrelated project directories.

Successful execution is operational evidence, not scientific validation.
