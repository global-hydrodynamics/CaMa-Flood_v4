# Tutorial Design — Tutorial01: Get Started

**Purpose:** define the first-run actions, inputs, and acceptance criteria for the CaMa-Flood v5 getting-started tutorial. Shared implementation instructions are in [`../agent_tutorial_development.md`](../agent_tutorial_development.md); the learner-facing content is specified in [`design/manual_design.md`](design/manual_design.md).

## Learner outcome

Starting from a v5 checkout or qualifying package with bundled Mekong sample inputs and a prepared configuration, the learner builds or updates `MAIN_cmf`, runs a 40-day Mekong simulation using a standalone educational GOSH script, and finds the results under this tutorial's `out/` directory. The starting presence of the executable is not prescribed.

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
    └── out/                          # Generated tutorial results; Git-ignored
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
- Preserve the scientific and I/O settings of `gosh_sample/mek01-default_run.sh`. The tutorial's complete standalone script changes only repository-relative context, output isolation to `tutorial/tutorial01-get_started/out/`, and OpenMP thread count to one. The maintained `gosh_sample/` script remains unchanged.
- Output variables: `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc`; daily Binary output and standard end-of-run Binary Restart.

## Required workflow

1. Obtain CaMa-Flood, confirm the repository root with `pwd`, and define `cmf_root="$(pwd)"` in the same terminal session.
2. Inspect the main directories from `${cmf_root}`.
3. Prepare the Fortran and Python environments using the root setup scripts.
4. Activate `config/Mkinclude` by copying the reviewed staged `Mkinclude` manually; `config/` already exists.
5. From `${cmf_root}`, enter `src_main/` and run `make all` to build or update the current model executable.
6. Locate and list bundled map, runoff, input-dimension, and input-matrix files before explaining their settings and relationships.
7. Inspect short, relevant settings excerpts in the standalone Tutorial01 GOSH script.
8. From `tutorial/tutorial01-get_started/`, execute `./tutorial01-get_started.sh`.
9. Inspect the generated Namelist, logs, CTLs, model outputs, and Restart under `out/`.

Map generation and visualization are not required.

## Acceptance criteria

- Running `make all` from `src_main/` returns zero and leaves an executable `MAIN_cmf`, whether it is newly built or updated.
- The standalone tutorial script is executable, does not call or source `gosh_sample/mek01-default_run.sh`, and runs from its documented directory.
- Tutorial01 and the model return zero; `log_CaMa.txt` reaches normal calculation completion.
- Five expected daily Binary output variables and their CTLs, namelist, logs, and end Restart products are created under only `tutorial/tutorial01-get_started/out/`.
- Each daily output contains 40 records for the 40-day interval. The end time is exclusive; timestamp details follow the model's output convention.
- Bundled map, runoff, and matching input-matrix files remain unchanged.
- In a disposable checkout, the developer reset can remove the build products and only this tutorial's `out/`, while preserving `config/Mkinclude` and input data.

Successful execution is operational evidence, not scientific validation.
