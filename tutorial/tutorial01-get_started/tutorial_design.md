# Tutorial Design — Tutorial01 Get Started

**Purpose:** define what a first-time user executes and accomplishes in the CaMa-Flood v5 getting-started tutorial. Shared implementation conventions are in [`../tutorial_development_guidelines.md`](../tutorial_development_guidelines.md); manual content is in [`manual_design.md`](manual_design.md).

## Objective and learning outcomes

The learner starts from the repository checkout, prepares the supported build configuration, compiles the v5 model, runs a short Mekong example using bundled map and runoff inputs, and checks the generated files and model log.

After completing the tutorial, a learner should be able to:

- Locate v5 source code, the Mekong map, bundled runoff sample, and tutorial scripts.
- Explain the distinction between a model executable, river map, runoff forcing, and runoff-to-map input matrix.
- Prepare `config/Mkinclude` using the repository setup workflow and compile `src_main/MAIN_cmf`.
- Run the 40-day sample and identify its outputs, CTLs, log, and Restart.
- Distinguish a technical run check from scientific validation.

## Prerequisites and initial conditions

- Use a complete checkout containing `src_main/`, `map/mekong_15min/`, `inp/test-mek_1deg/`, `gosh_sample/`, `s01-setup_fortran.sh`, and `s02-python_setup.sh`.
- Have GNU Make, GNU Fortran, OpenMP support, and NetCDF-Fortran available for the standard v5 configuration. The setup script diagnoses these prerequisites; installation is environment-specific.
- Python 3.10 or later is required by the repository's Python setup/readiness check. Tutorial01's output CTL utilities use the standard library; the full repository Python package set is not needed for this run.
- `config/Mkinclude` is created by activating the reviewed root staging `Mkinclude`. The user copies it manually.
- The tutorial reset script is for developers or documentation authors preparing a clean build/output state; new users do not need to run it.

## Inputs

- Map: `map/mekong_15min/`, Binary format, 80 × 120 cells, 0.25 degrees, 10 floodplain layers.
- Runoff: `inp/test-mek_1deg/runoff/`, daily Binary records with the `Roff____YYYYMMDD.one` naming pattern.
- Runoff grid and mapping: `map/mekong_15min/inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin`; the runoff grid is 20 × 30.
- The required 2000-01-01 through 2000-02-09 daily forcing and matching map products are bundled. No dataset download or regeneration is required.

## Workflow and experiment settings

1. Inspect the repository, map metadata, input-matrix descriptor, and sample input files.
2. Run `bash s01-setup_fortran.sh`, review the recommended configuration, and copy `Mkinclude` to `config/Mkinclude`.
3. Optionally run `bash s02-python_setup.sh`; this diagnoses/creates the repository-local Python environment used by broader workflows.
4. For a clean build demonstration, use the tutorial reset procedure to remove existing `src_main/` build products and this tutorial's `out/`, while preserving `config/Mkinclude`; then compile with `make -C src_main all` from the repository root.
5. Run `bash tutorial/tutorial01-get_started/tutorial01-get_started.sh` from the repository root.
6. Inspect the tutorial's `out/` directory, generated `input_cmf.nam`, variable CTLs, `log_CaMa.txt`, runtime log, Binary outputs, and end Restart.

The wrapper reuses `gosh_sample/mek01-default_run.sh` and overrides only `RUNDIR` and `OMP_NUM_THREADS`. It writes outputs to `tutorial/tutorial01-get_started/out/` and uses one OpenMP thread. The shared sample's default remains four threads and `out/mek01-default_run/`.

The simulation is non-MPI, uses adaptive timestep and bifurcation, uses a 3600-second base timestep, starts 2000-01-01 00:00 and ends 2000-02-10 00:00 (exclusive), and writes daily Binary `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc`. It begins without a Restart and writes a Binary Restart at the end.

## Expected outputs and success criteria

The tutorial output directory contains the copied model executable, `input_cmf.nam`, `log_CaMa.txt`, `run.stdout.log`, `run_time.log`, five 40-record Binary model-output files and their CTLs, and the end Restart plus any enabled-scheme companion file. Exact sizes and timestamps are environment-specific.

The workflow succeeds when the build returns zero, the tutorial wrapper/model returns zero, the model log reaches its normal calculation end, and expected output files are produced. The current tested run produced five 80 × 120 × 40 float32 Binary fields and 40 daily records. These checks demonstrate operation and file consistency, not scientific correctness or observational agreement.

For manual evidence, retain representative terminal output and file listings from before and after compilation and simulation. Clearly label captured output and shorten long logs only where omissions are identified; the model log remains the authoritative source for model progress.

## Reset and preservation contract

`tutorial01-reset.sh` may clean generated products under `src_main/` using the reviewed Makefile clean target and remove only this tutorial's `out/` directory. It requires an existing `config/Mkinclude`, confirms before cleanup unless invoked with `--yes`, and verifies that the config checksum did not change. It must not remove or alter map/runoff data, other experiment outputs, Python environments, or configuration files.

## Execution record

The current manual is based on a successful disposable-checkout build and run on macOS Apple Silicon on 2026-11-08. The tested setup found GNU Make 3.81, GNU Fortran 14.2.0, OpenMP, NetCDF-Fortran 4.6.2, and MPI Fortran. Full `requirements.txt` installation through `s02-python_setup.sh` was blocked by package-index DNS access; this did not block Tutorial01's standard-library CTL generation. See the manual-authoring assessment and VR-0125 for evidence and limitations.
