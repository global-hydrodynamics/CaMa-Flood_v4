# Tutorial01 — Get started with CaMa-Flood v5

This tutorial takes a new user from a checkout of the v5 development repository to a short Mekong simulation. It uses the repository's bundled map and runoff sample, builds `src_main/MAIN_cmf`, and writes generated files only to this tutorial's `out/` directory.

The instructions and output examples below were checked on macOS Apple Silicon against source commit `648499305606f381e745e0231614dbcdf9f8322d` from `feature/gosh-refactor` on 2026-11-08. For the canonical-path rerun, the Tutorial01 and shared tutorial files from the current worktree were copied into a disposable checkout. This task's reorganization is not yet committed. This is not a claim that a separate downloadable v5 release package has been assembled or tested. Paths, timestamps, compiler versions, and file sizes vary by machine.

## 1. Introduction

### Purpose

CaMa-Flood routes runoff over a river map and writes simulated hydrological variables. This first run uses an 80 × 120 Mekong map at 0.25-degree resolution and 40 days of daily runoff. It demonstrates that the model builds and runs; it is not a scientific validation of the sample results.

### What you will do

1. Obtain the repository and inspect its bundled inputs.
2. Check the Fortran and Python environment.
3. Prepare `config/Mkinclude`, then compile the model.
4. Run the sample and inspect its logs and output files.

### Checkpoint

By the end, the tutorial-local `out/` directory should contain five 40-record binary output variables, GrADS CTL files, a model log, a namelist, and end-of-run Restart files.

## 2. Download CaMa-Flood

### Purpose

The tested code is on the persistent `feature/gosh-refactor` development branch. A separate v5 release-package assembly is not currently available, so begin from this repository branch.

### Command

Run this from the directory where you keep projects. Choose any suitable parent directory in place of `~/work`.

```sh
mkdir -p ~/work
cd ~/work
git clone --branch feature/gosh-refactor --depth 1 --single-branch \
  https://github.com/global-hydrodynamics/CaMa-Flood_v4.git CaMa-Flood_v5
cd CaMa-Flood_v5
```

### Observed output

The branch clone completed successfully. To test this still-uncommitted tutorial, its current files were copied into that disposable checkout before building and running; the temporary absolute path is omitted because it is not a user-facing path.

### Explanation

The `--branch` option selects the maintained work branch; `--depth 1` retrieves its current snapshot without the full commit history. You can instead use an existing checkout, provided it contains the same v5 files and bundled Mekong inputs.

### Checkpoint

From the repository root, the following directories should exist:

```sh
ls
```

Observed repository entries included:

```text
AGENTS.md  adm  config  docs  gosh_sample  inp  map  proj_sample
research   src_main  s01-setup_fortran.sh  s02-python_setup.sh
```

This is an excerpt of the listing; additional files and directories were present.

## 3. Explore the package

### Purpose

The source, map, runoff, and tutorial files have different roles. Confirming that bundled data is present helps distinguish a missing-input problem from a build or run problem.

### Command

Run these commands at the repository root:

```sh
pwd
ls -1 src_main | head -n 12
ls -1 tutorial
cat map/mekong_15min/params.txt
cat map/mekong_15min/inpdim_test-mek_1deg.txt
ls -l inp/test-mek_1deg/runoff/Roff____20000101.one \
  inp/test-mek_1deg/runoff/Roff____20000209.one
```

### Observed output

The repository contained `tutorial/README.md`, both shared guidelines, and the Tutorial01 and Tutorial02 directories. The map metadata reported 80 cells east-west, 120 cells north-south, 10 floodplain layers, 0.25-degree grid spacing, and bounds 90–110°E and 5–35°N. The input-dimension file reported a 20 × 30 runoff grid and named `inpmat_test-mek_1deg.bin`. The first and last runoff files needed by this 40-day run were present.

The captured `ls -1 tutorial` output was:

```text
README.md
manual_authoring_guidelines.md
tutorial01-get_started
tutorial02-mekong_netcdf_4exp
tutorial_development_guidelines.md
```

### Explanation

The map grid is the model's river/floodplain grid. The runoff grid is coarser; the input matrix maps runoff values to the model grid. The `.one` suffix is part of the bundled daily Binary runoff filename convention.

### Checkpoint

The map, input-matrix description, binary input matrix, and daily runoff files from 2000-01-01 through 2000-02-09 must be available. The model's exclusive end time is 2000-02-10 00:00, so the last included daily input is February 9.

## 4. Set up Fortran and Python

### Purpose

The Fortran setup script checks the compiler and required libraries with small compile/run probes. The Python setup script can prepare the repository-local environment used by other tools. These readiness checks do not run a CaMa-Flood simulation.

### Command

At the repository root, run the Fortran setup interactively:

```sh
bash s01-setup_fortran.sh
```

Review the recommended configuration and accept creation of `./Mkinclude` when prompted. Then activate it by copying it to the v5 configuration directory:

```sh
mkdir -p config
cp Mkinclude config/Mkinclude
```

The standard setup is NetCDF-enabled even though Tutorial01 uses Binary runoff, Binary maps, and Binary model outputs. The build uses NetCDF-Fortran for the standard v5 configuration.

The Python setup is optional for this tutorial but useful for the broader repository workflows:

```sh
bash s02-python_setup.sh
```

### Observed output

On the tested Mac, `s01` reported:

```text
System: macOS / Apple Silicon (arm64)
  [OK] GNU Make           GNU Make 3.81
  [OK] GNU Fortran        GNU Fortran (Homebrew GCC 14.2.0_1) 14.2.0
  [OK] OpenMP             -fopenmp compile, link, and run passed
  [OK] NetCDF-Fortran     4.6.2
  [OK] MPI Fortran        mpifort compile/link test passed
  [READY] mek01 - Compiler, GNU Make, and CTL-tool Python requirements available.
```

The script also found both Mekong sample input sets. The excerpt omits path details and other readiness lines. `s01` distinguishes dependency checks from actually building or running the model.

In this test environment, `s02` found Python 3.15.2 but could not install the `requirements.txt` packages because the package index was unreachable by DNS. The Tutorial01 CTL tools use Python's standard library and the system Python was sufficient to run this tutorial. If you need the optional packages or other Python workflows, rerun `s02-python_setup.sh` when package-index access is available.

### Explanation

The root `Mkinclude` is a staging file for machine-specific compiler settings. The v5 Makefiles read `config/Mkinclude`; the manual copy makes that activation explicit. The legacy v4 Makefiles continue to use `adm/Mkinclude`.

### Checkpoint

Confirm that `config/Mkinclude` exists before compiling. Do not edit or replace `adm/Mkinclude` for this v5 tutorial.

## 5. Compile the model

### Purpose

Compilation turns the Fortran source into the executable used by the sample. Tutorial01 uses the v5 source under `src_main/`.

### Command

Run this command from the repository root:

```sh
make -C src_main all
```

### Observed output

The build compiled the model modules and linked `src_main/MAIN_cmf`. The actual build emitted a few unused-import warnings and completed with status 0:

```text
ar -rv libcama.a parkind1.o ... cmf_drv_advance_mod.o
gfortran -fopenmp MAIN_cmf.o ... -o MAIN_cmf -L/opt/homebrew/Cellar/netcdf-fortran/4.6.2/lib -lnetcdff -lnetcdf ...

BUILD_STATUS=0
```

The long object list and earlier compile lines are omitted. On this machine the resulting executable was approximately 600 KB.

### Explanation

`-C src_main` tells `make` to perform the build in that directory while allowing you to issue the command from the repository root. Build commands and executable size vary with compiler and library paths.

### Checkpoint

Check that the executable exists:

```sh
ls -lh src_main/MAIN_cmf
```

Before the clean test build, the observed state was:

```text
src_main/MAIN_cmf: executable not found; compile first.
```

## 6. Inspect the sample input data

### Purpose

This sample needs a river map, a runoff-to-map input matrix, and daily runoff files. All are already included; no extra dataset download or map regeneration is part of the tutorial.

### Command

Inspect the data descriptions and sample files:

```sh
cat map/mekong_15min/params.txt
cat map/mekong_15min/inpdim_test-mek_1deg.txt
ls -lh map/mekong_15min/nextxy_noedge.bin \
  map/mekong_15min/inpmat_test-mek_1deg.bin
ls -lh inp/test-mek_1deg/runoff/Roff____20000101.one \
  inp/test-mek_1deg/runoff/Roff____20000209.one
```

### Observed output

```text
80    grid number (east-west)
120   grid number (north-south)
10    floodplain layers
0.25000000  grid size [deg]
90.000      west edge [deg]
110.000     east edge [deg]
5.000       south edge [deg]
35.000      north edge [deg]
```

The input-dimension description identifies the paired runoff grid as 20 × 30 and the matrix file as `inpmat_test-mek_1deg.bin`. The two displayed runoff filenames were present in the checkout. This output is excerpted from the actual metadata; spacing may differ in your terminal.

### Explanation

The 1-degree forcing grid and 0.25-degree map grid are not interchangeable. `inpmat_test-mek_1deg.bin` carries the mapping used by the sample's `LINTERP` setting. Do not substitute an unrelated input matrix or runoff grid.

### Checkpoint

The setup script reported `mek01 Binary inputs found`. If the listed files or metadata are absent, first check that you downloaded the full repository tree and that the sample data are available.

## 7. Understand the simulation settings

### Purpose

Reading the settings before running makes the experiment reproducible and clarifies which files the model will use.

### Configuration used

The tutorial wrapper runs the maintained [`gosh_sample/mek01-default_run.sh`](../../gosh_sample/mek01-default_run.sh), with two intentional overrides: the output directory is this tutorial's `out/`, and OpenMP uses one thread.

| Setting | Tutorial01 value |
| --- | --- |
| Map | `map/mekong_15min/` (Binary) |
| Runoff | `inp/test-mek_1deg/runoff/` (daily Binary) |
| Input matrix | `inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin` |
| Period | 2000-01-01 00:00 to 2000-02-10 00:00, exclusive end (40 days) |
| Base timestep | 3600 seconds; adaptive stepping enabled |
| Bifurcation | Enabled |
| MPI / OpenMP | Non-MPI; one OpenMP thread in this tutorial |
| Output | Daily Binary: `outflw`, `storge`, `sfcelv`, `flddph`, `fldfrc` |
| Restart | Binary, written at the end of the run |

`mek01-default_run.sh` itself defaults to four OpenMP threads and `out/mek01-default_run/`. The tutorial wrapper overrides these two values only; it does not duplicate or alter the scientific configuration.

### Command

To inspect the settings, run from the repository root:

```sh
sed -n '20,90p' gosh_sample/mek01-default_run.sh
```

### Explanation

The simulation's end time is exclusive. Daily discharge output is an average over each completed output interval, so the first output record is labeled 2000-01-02 00:00 rather than the initial time. Model state outputs such as storage and flood depth are snapshots at output times. These labels follow the current model output convention.

### Checkpoint

Before execution, make sure `config/Mkinclude` is active and the tutorial wrapper points to the intended output directory. The run script prints `CMF_BASE`, `RUNDIR`, `MAPDIR`, and `RUNOFF_DIR` at startup.

## 8. Run Tutorial01

### Purpose

The wrapper keeps generated results separate from other sample runs while reusing the established Mekong experiment settings.

### Command

Run from the repository root:

```sh
bash tutorial/tutorial01-get_started/tutorial01-get_started.sh
```

### Observed output

The test completed successfully. A representative excerpt from the captured run is:

```text
Tutorial output directory does not exist yet: .../tutorial/tutorial01-get_started/out
CMF_BASE = .../CaMa-Flood_v4
RUNDIR = .../tutorial/tutorial01-get_started/out
MAPDIR = .../map/mekong_15min
RUNOFF_DIR = .../inp/test-mek_1deg/runoff
[STEP 1/2] Configure the Mekong simulation
[STEP 2/3] Generate planned binary-output CTLs
Expected output records: 40 (00Z02jan2000 to 00Z10feb2000)
[SUCCESS] .../out/outflw2000.ctl (planned)
...
Exit status: 0
[SUCCESS] mek01-default_run.sh completed
RUN_STATUS=0
```

Absolute path prefixes are abbreviated with `...`; intermediate CTL messages and output are omitted. The existing script's step numbering includes the post-run Restart CTL stage. The model writes its primary log to `out/log_CaMa.txt`; `out/run.stdout.log` was empty in this run because the model reports progress in its own log.

### Explanation

The wrapper runs from `gosh_sample/` as required by `mek01-default_run.sh`, and passes the tutorial output path and one-thread setting as environment overrides. It does not copy the experiment's scientific settings into a second namelist generator.

### Checkpoint

Look for `RUN_STATUS=0` and `[SUCCESS] mek01-default_run.sh completed`. Then inspect `log_CaMa.txt` for the model's detailed progress and end-of-run messages.

## 9. Verify output files

### Purpose

File presence, expected sizes, time records, and finite values provide a basic operational check. These checks do not establish that the simulated hydrology is scientifically correct.

### Command

List the tutorial results and inspect the discharge CTL and model log:

```sh
ls -lh tutorial/tutorial01-get_started/out
cat tutorial/tutorial01-get_started/out/outflw2000.ctl
tail -n 20 tutorial/tutorial01-get_started/out/log_CaMa.txt
```

### Observed output

The captured output directory contained:

```text
-rw-r--r--  1 ...  1.5M  flddph2000.bin
-rw-r--r--  1 ...  1.5M  fldfrc2000.bin
-rw-r--r--  1 ...  4.4K  input_cmf.nam
-rw-r--r--  1 ...  1.6M  log_CaMa.txt
-rw-r--r--  1 ...  1.5M  outflw2000.bin
-rw-r--r--  1 ...  450K  restart2000021000.bin
-rw-r--r--  1 ...   15K  restart2000021000.bin.pth
-rw-r--r--  1 ...  1.5M  sfcelv2000.bin
-rw-r--r--  1 ...  1.5M  storge2000.bin
```

The actual canonical-path listing also included each variable's `.ctl`, a Restart CTL and GrADS visualization copy, `mapdata.txt`, and runtime logs. The listing is excerpted.

The generated discharge CTL reported:

```text
DSET ^outflw2000.bin
OPTIONS yrev little_endian
XDEF 80 LINEAR 90.125 0.25
YDEF 120 LINEAR 5.125 0.25
TDEF 40 LINEAR 00Z02jan2000 1dy
VARS 1
outflw 1 99 discharge (river+floodplain) [m3/s]
ENDVARS
```

The model log ended with:

```text
CMF::DRV_END: simulation finished in:   1.27539062      Seconds
CMF::DRV_END: close logfile
CMF::===== CALCULATION END =====
```

### Explanation

Each of the five Binary model-output files was 1,536,000 bytes: 80 × 120 × 40 records × 4 bytes per value. A standard-library scan found 384,000 float32 values per file and no NaN or infinite values. `outflw` is discharge in m³/s and is a daily mean; `storge` is storage in m³, while `sfcelv`, `flddph`, and `fldfrc` describe state at the output time. The end Restart was accompanied by the bifurcation `.pth` file.

The CTL starts at January 2 because the first daily mean covers the first completed daily interval. The exclusive simulation endpoint is February 10. The model reached its end time and wrote record 40 at that endpoint.

### Checkpoint

Confirm five output binaries, five variable CTLs, `input_cmf.nam`, `log_CaMa.txt`, `run_time.log`, and the end Restart files. The observed `run_time.log` reported status 0 and about two seconds wall-clock time; times vary by system. Do not interpret this file-level check as numerical or observational validation.

## 10. Summary and next steps

### Purpose

This first tutorial establishes a reproducible build-and-run path using bundled sample data.

### Commands used

```sh
bash s01-setup_fortran.sh
cp Mkinclude config/Mkinclude
make -C src_main all
bash tutorial/tutorial01-get_started/tutorial01-get_started.sh
```

### Observed result

The model compiled and completed the 40-day Mekong run in the tested environment. Five daily output variables, their CTL files, logs, and a Restart were created under `tutorial/tutorial01-get_started/out/`.

### Explanation

For a new user, the reset script is not part of normal setup. It is for developers or documentation authors who need to clear this tutorial's generated output and the `src_main/` build before capturing a clean example. It preserves `config/Mkinclude` and removes only this tutorial's `out/` directory.

### Developer-only reset

Use the reset only in a checkout where cleaning `src_main/` and deleting this tutorial's output is acceptable. It asks for confirmation; `--yes` is intended for a disposable test checkout.

```sh
bash tutorial/tutorial01-get_started/tutorial01-reset.sh
```

In the tested disposable checkout, the after-run reset removed the tutorial output and executable and reported that `config/Mkinclude` was preserved unchanged. A before/after checksum was identical.

### Checkpoint and next steps

To repeat the sample, rebuild if needed and run the tutorial command again. For scientific interpretation, visualization, alternate physical schemes, or longer experiments, use a separately reviewed tutorial specification; this short run alone does not establish model accuracy.
