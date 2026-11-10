# Tutorial01: Get Started with CaMa-Flood v5

This tutorial introduces how to obtain and navigate CaMa-Flood, find the files used by a GOSH script, build and run the model, and locate the outputs. The 40-day sample run demonstrates the workflow; it is not a scientific validation of the model.

## 0. Obtain CaMa-Flood

GitHub's default branch contains the files for Tutorial01. Clone the repository:

```bash
git clone https://github.com/global-hydrodynamics/CaMa-Flood_v4.git
```

This creates a local directory named `CaMa-Flood_v4`. If you prefer not to use Git, open the repository on GitHub, choose **Code → Download ZIP**, then extract the archive. Place the checkout in a suitable working location; no fixed machine-specific path is required.

The [official CaMa-Flood website](https://global-hydrodynamics.github.io/CaMa-Flood/) provides a separate downloadable distribution package; access requires registration. The supported v5 package for this tutorial is not ready yet. If you prefer that package, wait for its release; an older package is not a substitute for this v5 workflow.

Tutorial01's learner starting state does not contain a compiled `src_main/MAIN_cmf` executable or generated run results. You will build the executable in Section 3 and run the experiment later in this tutorial.

### Learning Check

- [ ] I can obtain the tutorial files from GitHub's default branch and locate the repository root.
- [ ] I know that I will build `MAIN_cmf` as part of this tutorial.

## 1. Find the repository root

Open a terminal and change to the root of the repository you cloned or extracted. Confirm that you are at its root directory:

```bash
pwd
cmf_root="$(pwd)"
ls -lF
```

`cmf_root` is available for the rest of this tutorial in the same terminal session. It is an ordinary shell variable, so it will not automatically exist in a new terminal window. In the listing, `-l` shows detailed file information and `-F` adds markers: directories end in `/`, executable files normally end in `*`, and symbolic links may end in `@`. These markers are display aids, not part of the actual filenames.

### Expected output

This abbreviated excerpt is from an actual repository-root `ls -lF` on the verified GitHub checkout. Sizes, dates, and owner names vary between computers. `...` marks omitted listing entries.

```text
drwxr-xr-x    7 yamadai  staff   224 Nov  8 10:36 config/
drwxr-xr-x   11 yamadai  staff   352 Nov  5 22:07 gosh_sample/
drwxr-xr-x   11 yamadai  staff   352 Nov  4 21:46 inp/
drwxr-xr-x   10 yamadai  staff   320 Nov  5 22:07 map/
drwxr-xr-x  109 yamadai  staff  3488 Nov  8 10:36 src_main/
drwxr-xr-x    9 yamadai  staff   288 Nov  8 18:03 tutorial/
-rwxr-xr-x    1 yamadai  staff 17484 Nov  8 10:21 s01-setup_fortran.sh*
-rwxr-xr-x    1 yamadai  staff  9199 Nov  5 22:42 s02-python_setup.sh*
```

The `/` suffix distinguishes directories such as `src_main/` and `tutorial/`; the `*` suffix marks the executable setup scripts. The markers are not part of the names. The main directories are `src_main/` (v5 model source), `map/` (river maps and map tools), `inp/` (runoff datasets), `gosh_sample/` (maintained run examples), and `tutorial/` (beginner tutorials). The root setup scripts diagnose and prepare local development environments.

### Learning Check

- [ ] I can locate the model source and the bundled map and runoff directories from the root listing.
- [ ] I can identify directories marked with `/` and executable setup scripts marked with `*`; these markers are not part of the filenames.

## 2. Prepare the local environment

Both setup commands run from the repository root. `s01-setup_fortran.sh` checks the Fortran build environment and offers to generate a suggested configuration. `s02-python_setup.sh` checks Python and offers a repository-local virtual environment for optional Python packages.

```bash
cd "${cmf_root}"
pwd
bash s01-setup_fortran.sh
```

### Fortran setup: output and required actions

The script checks tools and capabilities needed for the selected build, including GNU Make, a Fortran compiler, OpenMP, NetCDF-Fortran, MPI, and sample inputs. A successful check is marked `[OK]`. If a required check is unavailable, the script prints guidance for installing or loading that dependency; address the missing requirement and run the diagnostic again.

After the diagnostic, an interactive terminal displays a prompt like this:

```text
Installed components:
  [OK] GNU Make           GNU Make 3.81
  [OK] GNU Fortran        GNU Fortran (Homebrew GCC 14.2.0_1) 14.2.0
  [OK] OpenMP             -fopenmp compile, link, and run passed
  [OK] NetCDF-Fortran     4.6.2
  [OK] MPI Fortran        mpifort compile/link test passed
...
Recommended configuration:
  Compiler: GNU Fortran (OpenMP enabled)
  NetCDF: Enabled (NF90_DOUBLE read/write test passed)
  MPI: Disabled by default
  Precision: SinglePrec_CMF, matching standard GNU templates
...
Generate ./Mkinclude? [Y/n]: y
[SUCCESS] Generated ./Mkinclude
To activate this configuration:
Existing config/Mkinclude was not changed. Back it up before choosing to replace it.
Legacy v4 builds continue to use adm/Mkinclude.
```

The example shows a user choosing `y`. The suggested file is created at the repository root; the setup script does not overwrite the active `config/Mkinclude`. Review the generated configuration before activating it. Since `config/Mkinclude` already exists, back it up before replacing it, and copy the suggested file only if you intend to change the active v5 configuration:

```bash
cp Mkinclude config/Mkinclude
```

The copy replaces an existing `config/Mkinclude`. If you want to keep the current compiler configuration, do not run the copy command; use the configuration already prepared for your environment.

The Python setup is also run from the repository root:

```bash
bash s02-python_setup.sh
```

If `.venv` does not exist, the script reports a Python interpreter candidate and asks whether to create or update the repository-local environment. In the captured example, the user answered `n`:

```text
Repository environment:
  [--] not present
Selected interpreter for .venv: python3 (...)
Packages will be installed only inside .venv, never into the system interpreter.
Create/update .venv and install requirements? [Y/n]: n
Python environment was not changed.
```

Answer `y` only if you want the optional packages installed in `.venv/`; they are not installed into the system Python. Tutorial01's model run and the included CTL-generation tools do not require those optional packages. Interpreter paths and diagnostic results depend on your computer.

### Learning Check

- [ ] I can tell from the setup output whether the required Fortran checks passed and what to do if one failed.
- [ ] I can identify which configuration file the setup instructions ask me to copy, and where Python setup reports its result.

## 3. Build the model

CaMa-Flood is written in Fortran. A compiler translates its source files into an executable program. Build or update the v5 executable with the repository's standard target, `make all`:

```bash
cd "${cmf_root}"
cd src_main
pwd
make all
ls -lhF MAIN_cmf
```

The build may print unused-import warnings from existing source files. In the checked build, `make all` returned zero and the final command showed:

```text
-rwxr-xr-x  1 yamadai  staff   600K Nov  8 10:36 MAIN_cmf*
```

This confirms that the executable was produced in that checkout; its size and listing details can vary. Do not run `MAIN_cmf` directly from this terminal: the experiment script prepares its Namelist and working directory.

### Learning Check

- [ ] I can locate `MAIN_cmf` under `src_main/` after the build.
- [ ] I can run `make all` and find `MAIN_cmf` in `src_main/`.

## 4. Read the experiment plan and script

A GOSH script is a plain-text shell script that records experiment settings and the commands used to run the model. First read the [Tutorial Design Specification](tutorial_design.md), which describes the purpose, computational conditions, input datasets, and expected products for this experiment.

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
cat tutorial_design.md
cat tutorial01-get_started.sh
```

`cat` prints a file in the terminal. The GOSH script is several screens long, so you do not need to read it all at once. Open it in an ordinary text editor to inspect its settings. `nano` and `vi` or `vim` are terminal editors; Visual Studio Code is a graphical editor that may be installed separately. Other plain-text editors for your operating system are also suitable. A word processor is not suitable because it can add document formatting to the script.

If available, you can inspect without saving using `nano tutorial01-get_started.sh` and exit with Ctrl-X, choosing not to save if asked. In `vi` or `vim`, press Esc, type `:q!`, and press Enter to quit without saving. Editor commands are optional and availability varies by system.

### Experiment settings at a glance

| Category | Tutorial01 setting |
|---|---|
| Project | `PROJ="tutorial01-get_started"` |
| River map | `map/mekong_15min/` |
| Runoff | `inp/test-mek_1deg/`, daily binary files |
| Runoff-to-map mapping | `inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin` in the map directory |
| Period | 2000-01-01 00:00 to 2000-02-10 00:00; end time is exclusive (40 days) |
| Time step | Base `DT=3600` seconds; adaptive time stepping enabled |
| Execution | Non-MPI, one OpenMP thread; bifurcation enabled |
| Output | Daily Binary: `outflw`, `storge`, `sfcelv`, `flddph`, `fldfrc` |
| Output directory | `tutorial/tutorial01-get_started/out/${PROJ}/` |

The rest of the tutorial locates the real input files and shows how the GOSH settings select them.

### Learning Check

- [ ] I can find an experiment condition in `tutorial_design.md` and locate the corresponding setting in the script.

## 5. Inspect the river map

A river map describes the model grid, river connectivity, and physical properties used by the simulation. Return to the repository root, enter the Mekong map directory, and list a few representative files:

```bash
cd "${cmf_root}"
cd map/mekong_15min
pwd
ls -lhF params.txt nextxy_noedge.bin ctmare.bin elevtn.bin rivwth_merit.bin
```

### Expected output

These file names and sizes were observed in the bundled checkout. File permissions and timestamps can differ.

```text
-rw-r--r--  1 yamadai  staff    38K Nov  1 12:02 ctmare.bin
-rw-r--r--  1 yamadai  staff    38K Nov  1 12:02 elevtn.bin
-rw-r--r--  1 yamadai  staff    75K Nov  1 12:02 nextxy_noedge.bin
-rw-r--r--  1 yamadai  staff   384B Nov  1 12:02 params.txt
-rw-r--r--  1 yamadai  staff    38K Nov  1 23:32 rivwth_merit.bin
```

`params.txt` describes map dimensions and geographic extent. `nextxy_noedge.bin` contains the selected river connectivity, `ctmare.bin` the unit catchment areas, `elevtn.bin` elevation, and `rivwth_merit.bin` river width. The tutorial script selects the map and products with these settings:

```sh
MAPDIR="${CMF_BASE}/map/mekong_15min"
#NEXTXY="${MAPDIR}/nextxy.bin"
NEXTXY="${MAPDIR}/nextxy_noedge.bin"
#RIVWTH="${MAPDIR}/rivwth.bin"
RIVWTH="${MAPDIR}/rivwth_merit.bin"
```

The commented lines show alternative map products; the uncommented assignments select the files used here.

### Learning Check

- [ ] I can find `MAPDIR` and identify the connectivity and width files selected by `NEXTXY` and `RIVWTH`.

## 6. Inspect runoff forcing

Runoff forcing is water input supplied to the model over time. The sample uses daily binary files on a grid coarser than the river map. Inspect a few dates, including the last day before the exclusive end date:

```bash
cd "${cmf_root}"
cd inp/test-mek_1deg/runoff
pwd
ls -lhF Roff____20000101.one Roff____20000102.one Roff____20000209.one
```

### Expected output

The three observed sample files are each 2.3 KB; the exact display may round sizes differently.

```text
-rw-r--r--  1 yamadai  staff   2.3K Nov  3 07:06 Roff____20000101.one
-rw-r--r--  1 yamadai  staff   2.3K Nov  3 07:06 Roff____20000102.one
-rw-r--r--  1 yamadai  staff   2.3K Nov  3 07:06 Roff____20000209.one
```

The corresponding GOSH settings identify the dataset, file naming rule, frequency, and unit conversion:

```sh
ROFFTAG="test-mek_1deg"
RUNOFF_DIR="${CMF_BASE}/inp/${ROFFTAG}/runoff"
IFRQ_INP=24
CROFFREQ="DAY"
CROFPRE="Roff____"
CROFSUF=".one"
DROFUNIT=86400000
```

`Roff____` + a date in `YYYYMMDD` form + `.one` makes a daily filename. `IFRQ_INP=24` means the forcing interval is 24 hours. `DROFUNIT` converts the supplied `mm/day` runoff values to the model's `m/s` units. The 40-day interval ends at February 10 00:00, so February 9 is the final daily input date.

### Learning Check

- [ ] I can locate a dated daily runoff file in the listed directory.
- [ ] I can find the GOSH settings that select the runoff directory and construct its filename.

## 7. Inspect runoff-to-map mapping

The runoff grid is 20 × 30 cells, while this Mekong river map is 80 × 120 cells. The input-dimension descriptor and mapping matrix tell the model how runoff-grid values correspond to map cells. These files are stored with the map because the mapping is specific to the map and runoff-grid pairing.

```bash
cd "${cmf_root}"
cd map/mekong_15min
pwd
ls -lhF inpdim_test-mek_1deg.txt inpmat_test-mek_1deg.bin
cat inpdim_test-mek_1deg.txt
```

### Expected output

Observed sizes and the descriptor contents:

```text
-rw-r--r--  1 yamadai  staff   312B Nov  3 08:24 inpdim_test-mek_1deg.txt
-rw-r--r--  1 yamadai  staff   450K Nov  3 08:05 inpmat_test-mek_1deg.bin
```

```text
        80     !! nXX
       120     !! nYY
        10     !! floodplain layer
        20     !! input nXX
        30     !! input nYY
         4     !! input num
inpmat_test-mek_1deg.bin
      90.000     !! west  edge
     110.000     !! east  edge
      35.000     !! north edge
       5.000     !! south edge
```

The script points to this pair and turns on input-matrix mapping with `LINTERP`:

```sh
INPDIM="${MAPDIR}/inpdim_${ROFFTAG}.txt"
INPMAT="${MAPDIR}/inpmat_${ROFFTAG}.bin"
LINTERP=".TRUE."
```

`INPDIM` gives the model and input-grid dimensions and names the matrix file. `INPMAT` is the mapping data itself. With `LINTERP` true, runoff is mapped through this matrix. The supplied files already match this sample; Tutorial01 does not regenerate them.

### Learning Check

- [ ] I can locate the two mapping files and match them to `INPDIM` and `INPMAT`.
- [ ] I can find `LINTERP` in the same GOSH configuration.

## 8. Run the experiment

The script defines the project as `PROJ="tutorial01-get_started"` and constructs `RUNDIR` as the Tutorial01 directory's `out/${PROJ}/`. Keeping each experiment under its own project name makes outputs easier to compare and prevents one experiment from replacing another. Tutorial02 uses the same naming convention for its multiple experiments.

For this tutorial, leave the project name at its default. The script can be rerun: it replaces known products in the selected project's directory, and does not remove the shared `out/` directory, sibling project directories, or unrelated notes in the selected directory.

Run it from the Tutorial01 directory:

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
./tutorial01-get_started.sh
```

### Expected output

This abbreviated excerpt was captured from a completed run. The absolute checkout prefix is shortened; on your computer `RUNDIR` will show your own repository path.

```text
PROJ = tutorial01-get_started
RUNDIR = .../tutorial/tutorial01-get_started/out/tutorial01-get_started
Overwriting previous sample results in: .../out/tutorial01-get_started
...
Expected output records: 40 (00Z02jan2000 to 00Z10feb2000)
...
Exit status: 0
...
[SUCCESS] tutorial01-get_started.sh completed
Results: .../tutorial/tutorial01-get_started/out/tutorial01-get_started
```

The script first writes the experiment Namelist and planned GrADS CTL files, then runs the model and prepares a Restart CTL. Shell messages go to the terminal; detailed model progress is in `log_CaMa.txt`. Exit status zero and the `[SUCCESS]` message mean the workflow completed without a reported command failure.

### Learning Check

- [ ] I can find `PROJ` and the output directory shown in the script's startup messages.
- [ ] I can run the script from its documented directory and recognize successful completion.

## 9. Inspect the results

The GOSH run directory contains the copied executable, Namelist, model log, timing record, Restart, binary outputs, and CTL metadata. Inspect it from the repository root:

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
ls -lhF out/tutorial01-get_started
```

### Expected output

Representative files from an earlier completed run; the current checkout's run directory is created when the script executes. This excerpt omits other entries. `*` is the `-F` marker for the executable copy:

```text
-rwxr-xr-x ... MAIN_cmf*                 600K
-rw-r--r-- ... flddph2000.bin            1.5M
-rw-r--r-- ... input_cmf.nam             4.4K
-rw-r--r-- ... log_CaMa.txt              1.6M
-rw-r--r-- ... outflw2000.bin            1.5M
-rw------- ... outflw2000.ctl            293B
-rw-r--r-- ... restart2000021000.bin     450K
-rw-r--r-- ... sfcelv2000.bin            1.5M
-rw-r--r-- ... storge2000.bin            1.5M
```

The files are under `out/tutorial01-get_started/`, not the repository's shared `out/`. The `outflw2000.ctl` file reports `TDEF 40 LINEAR 00Z02jan2000 1dy`; the model's output timestamps follow its output convention, while the simulation end time is exclusive. The model log ends with `CMF::===== CALCULATION END =====` on the observed successful run. File presence, record metadata, and normal completion are workflow checks, not evidence that the simulated values are scientifically correct.

### Learning Check

- [ ] I can locate the discharge output file `outflw2000.bin` and the model log `log_CaMa.txt`.
- [ ] I can recognize the model's normal-completion message in `log_CaMa.txt`.

## Optional developer reset

Ordinary learners do not need the reset utility. Developers collecting a clean demonstration can use `design/tutorial01-reset.sh` in a disposable checkout. It cleans generated `src_main/` build products and removes only the selected `out/PROJECT/` directory; it preserves `config/Mkinclude`, bundled inputs, the shared `out/` parent, and other project directories. It requires confirmation unless `--yes` is supplied. See the developer-only [design materials](design/README.md) for details.
