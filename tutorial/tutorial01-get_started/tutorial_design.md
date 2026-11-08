# Tutorial Design — Tutorial01: Get Started

**Purpose:** define the first-run actions, inputs, and acceptance criteria for the CaMa-Flood v5 getting-started tutorial. Shared implementation instructions are in [`../agent_tutorial_development.md`](../agent_tutorial_development.md); the learner-facing content is specified in [`design/manual_design.md`](design/manual_design.md).

## Learning outcomes

Learning outcomes are observable and distinguish **operational skills** (actions a learner can perform) from **conceptual understanding** (relationships a learner can explain).

| Stage | Operational skills | Conceptual understanding | Observable evidence of learning |
| --- | --- | --- | --- |
| Find the repository | Confirm the checkout root with `pwd`, assign `cmf_root`, and locate `src_main/`, `map/`, `inp/`, and setup scripts. | Explain that `cmf_root` stores the repository path only in the current shell session. | The learner can name where model source and bundled inputs are located and use the root variable in later commands. |
| Prepare the environment | Run the setup scripts, recognize check results and prompts, and identify the recommended configuration file. | Explain the difference between the generated root `Mkinclude` and active `config/Mkinclude`; identify Python `.venv` setup as optional for this tutorial's CTL tools. | The learner can decide whether to activate the proposed configuration and avoid replacing an existing active file without a backup. |
| Build the model | Run `make all` in `src_main/` and locate the resulting `MAIN_cmf`. | Explain that `make all` compiles or updates the executable from Fortran source; the GOSH script later runs it with a prepared Namelist. | The learner can find the executable and describe why it is built before the sample run. |
| Read experiment settings | Read `tutorial_design.md` and inspect the standalone GOSH script. | Explain the role of an experiment specification versus the script that applies its settings. | The learner can find the experiment conditions in the specification and the corresponding editable values in the script. |
| Select the river map | List representative map files and inspect `MAPDIR`, `NEXTXY`, and `RIVWTH`. | Explain that `MAPDIR` selects the map directory and the active `NEXTXY` and `RIVWTH` assignments select connectivity and width products within it. | The learner can identify the exact connectivity and width files selected by the sample. |
| Select runoff forcing | Locate daily runoff files and read `ROFFTAG`, frequency, naming, and unit settings. | Explain how the prefix, date, and suffix form a runoff filename; distinguish a 24-hour input interval from the `mm/day` to `m/s` conversion. | The learner can construct a sample daily filename and explain what `IFRQ_INP` and `DROFUNIT` control. |
| Map runoff onto the river grid | Inspect the dimension descriptor and input matrix and identify `INPDIM`, `INPMAT`, and `LINTERP`. | Explain why the coarser runoff grid needs a map-specific input matrix to map forcing onto river-map cells, and that `LINTERP` enables mapping through it. | The learner can name the descriptor/matrix pair and explain the purpose of enabling matrix mapping. |
| Run an experiment | Execute the Tutorial01 script from its documented directory and identify `PROJ` and `RUNDIR`. | Explain how `PROJ` determines an isolated output directory and why another project name keeps experiments separate. | The learner can predict the default output directory and recognize the script's successful completion message and zero exit status. |
| Inspect results | List project outputs and inspect the model log and output CTL. | Explain which file contains discharge, which records model progress, and what the CTL time definition says; distinguish workflow success from scientific validation. | The learner can locate `outflw2000.bin` and `log_CaMa.txt`, find the 40-record CTL time axis, and state the limit of these checks. |

Starting from a v5 checkout or qualifying package with bundled Mekong sample inputs and a prepared configuration, the learner completes these stages, runs a 40-day Mekong simulation, and finds the results in `tutorial/tutorial01-get_started/out/${PROJ}/`. The starting presence of the executable or output directory is not prescribed.

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

1. Obtain CaMa-Flood, confirm the repository root with `pwd`, and define `cmf_root="$(pwd)"` in the same terminal session.
2. Inspect the main directories from `${cmf_root}`.
3. Prepare the Fortran and Python environments using the root setup scripts.
4. Activate `config/Mkinclude` by copying the reviewed staged `Mkinclude` manually; `config/` already exists.
5. From `${cmf_root}`, enter `src_main/` and run `make all` to build or update the current model executable.
6. Locate and list bundled map, runoff, input-dimension, and input-matrix files before explaining their settings and relationships.
7. Read `tutorial_design.md`, then inspect the plain-text standalone Tutorial01 GOSH configuration and its experiment settings.
8. From `tutorial/tutorial01-get_started/`, execute `./tutorial01-get_started.sh`; the default project output is `out/tutorial01-get_started/`.
9. Inspect the generated Namelist, logs, CTLs, model outputs, and Restart under the project-specific directory.

Map generation and visualization are not required.

## Acceptance criteria

- Running `make all` from `src_main/` returns zero and leaves an executable `MAIN_cmf`, whether it is newly built or updated.
- The standalone tutorial script defines the default `PROJ`, constructs its `RUNDIR` under `out/${PROJ}/`, does not call or source `gosh_sample/mek01-default_run.sh`, and runs from its documented directory.
- Tutorial01 and the model return zero; `log_CaMa.txt` reaches normal calculation completion.
- Five expected daily Binary output variables and their CTLs, Namelist, logs, executable copy, and end Restart products are created under only `tutorial/tutorial01-get_started/out/tutorial01-get_started/`.
- Each daily output contains 40 records for the 40-day interval. The end time is exclusive; timestamp details follow the model's output convention.
- Bundled map, runoff, and matching input-matrix files remain unchanged.
- In a disposable checkout, the developer reset can remove build products and one explicitly selected Tutorial01 project directory while preserving `config/Mkinclude`, bundled inputs, the `out/` parent, and unrelated project directories.

Successful execution is operational evidence, not scientific validation.
