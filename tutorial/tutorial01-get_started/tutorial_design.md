# Tutorial Design — Tutorial01 Get Started

**Purpose:** define the first-run actions, inputs, and acceptance criteria for the CaMa-Flood v5 getting-started tutorial. Shared development rules are in [`../tutorial_development_guidelines.md`](../tutorial_development_guidelines.md); the manual's audience and teaching content are in [`manual_design.md`](manual_design.md).

## Learner outcome

The executable workflow takes a CaMa-Flood v5 checkout or qualifying distribution from an uncompiled state to a completed 40-day Mekong sample run, with outputs isolated to Tutorial01.

## Starting state and prerequisites

- The checkout or package contains `src_main/`, `map/mekong_15min/`, `inp/test-mek_1deg/`, `gosh_sample/`, the root setup scripts, and this tutorial.
- The model starts uncompiled: `src_main/MAIN_cmf` and generated build products are absent. Map and runoff samples are already present; no dataset download or regeneration is required.
- GNU Make, GNU Fortran with OpenMP, and NetCDF-Fortran are available for the standard v5 build. The user prepares the Fortran configuration with `s01-setup_fortran.sh` and manually copies the reviewed staging `Mkinclude` to `config/Mkinclude`.
- The user runs `s02-python_setup.sh` to diagnose or prepare the repository Python environment. Tutorial01's CTL generation uses the standard library, so installation of the full optional package set is not a prerequisite for this simulation.
- `config/Mkinclude` exists before the developer reset is used. The reset restores the uncompiled/output-free state but must never remove or modify `config/Mkinclude`.

## Experiment inputs and settings

- Map: `map/mekong_15min/`, Binary format, 80 × 120 cells at 0.25 degrees with 10 floodplain layers.
- Runoff: `inp/test-mek_1deg/runoff/`, daily Binary files named `Roff____YYYYMMDD.one`.
- Runoff grid and map relationship: `map/mekong_15min/inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin`; the runoff grid is 20 × 30.
- Simulation: 2000-01-01 00:00 to 2000-02-10 00:00 (exclusive), 40 days, 3600-second base timestep, non-MPI, adaptive timestep and bifurcation enabled, one OpenMP thread in the tutorial wrapper.
- Reuse `gosh_sample/mek01-default_run.sh` without duplicating its scientific configuration. The tutorial wrapper overrides `RUNDIR` to `tutorial/tutorial01-get_started/out/` and `OMP_NUM_THREADS` to 1.
- Write all run products to the tutorial's `out/`; do not alter the shared Mekong sample's default output location.

## Required workflow

1. Obtain the repository or qualifying package and locate the source, bundled map/runoff inputs, setup scripts, and Tutorial01 files.
2. Prepare and activate the Fortran configuration with `s01-setup_fortran.sh`. Run `s02-python_setup.sh` for Python environment diagnosis/setup; the broader package installation may be unavailable offline and is not required by this tutorial's standard-library CTL step.
3. Use `tutorial01-reset.sh` when a reproducible clean build/output state is needed. It runs the `src_main/` clean target and removes only this tutorial's `out/` directory. It preserves the active configuration and all inputs and unrelated outputs.
4. Build with `make -C src_main all` and run `bash tutorial/tutorial01-get_started/tutorial01-get_started.sh` from the repository root.
5. Inspect the tutorial output directory, Namelist, CTLs, model and wrapper logs, Binary outputs, and end Restart.

## Acceptance criteria

- Before a clean build demonstration, `src_main/MAIN_cmf` is absent; after `make -C src_main all`, it exists and the build returns zero.
- The tutorial wrapper and model return zero; `log_CaMa.txt` reaches normal calculation completion.
- Expected output files are created under only `tutorial/tutorial01-get_started/out/`: five daily Binary variables (`outflw`, `storge`, `sfcelv`, `flddph`, `fldfrc`) with 40 records, their CTLs, Namelist, logs, and end Restart products.
- The bundled Mekong map, runoff files, and matching input matrix are available without download or regeneration.
- The reset script can restore the uncompiled/output-free state and leaves `config/Mkinclude` unchanged.

The checked run produced five 80 × 120 × 40 float32 outputs and 40 daily records. These checks establish operation and file consistency, not scientific correctness or observational agreement. Exact sizes and timing vary by environment.
