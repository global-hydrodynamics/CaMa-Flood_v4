# Tutorial01: Get Started with CaMa-Flood v5

This tutorial builds CaMa-Flood, inspects a small Mekong experiment, and runs it for 40 days. The run demonstrates that the workflow works; it is not a scientific validation of the model.

## 1. Find the repository root

Open a terminal and change to the directory where you cloned CaMa-Flood. Confirm that you are at the repository root:

```bash
pwd
cmf_root="$(pwd)"
ls -l
```

`cmf_root` is available for the rest of this tutorial in the same terminal session. It is an ordinary shell variable, so it will not automatically exist in a new terminal window.

### Expected output

This abbreviated excerpt is from an actual repository-root `ls -l`; sizes, dates, and owner names vary between checkouts.

```text
drwxr-xr-x   ... config
drwxr-xr-x   ... gosh_sample
drwxr-xr-x   ... inp
drwxr-xr-x   ... map
drwxr-xr-x   ... src_main
drwxr-xr-x   ... tutorial
-rwxr-xr-x   ... s01-setup_fortran.sh
-rwxr-xr-x   ... s02-python_setup.sh
```

The main directories are `src_main/` (v5 model source), `map/` (river maps and map tools), `inp/` (runoff datasets), `gosh_sample/` (maintained run examples), and `tutorial/` (beginner tutorials). The root setup scripts diagnose and prepare local development environments.

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

## 3. Build the model

CaMa-Flood is written in Fortran. A compiler translates its source files into an executable program. Build or update the v5 executable with the repository's standard target, `make all`:

```bash
cd "${cmf_root}"
cd src_main
pwd
make all
ls -lh MAIN_cmf
```

The build may print unused-import warnings from existing source files. In the checked build, `make all` returned zero and the final command showed:

```text
-rwxr-xr-x ... MAIN_cmf  600K
```

This confirms that the executable was produced in that checkout; its size and listing details can vary. Do not run `MAIN_cmf` directly from this terminal: the experiment script prepares its Namelist and working directory.

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

## 5. Inspect the river map

A river map describes the model grid, river connectivity, and physical properties used by the simulation. Return to the repository root, enter the Mekong map directory, and list a few representative files:

```bash
cd "${cmf_root}"
cd map/mekong_15min
pwd
ls -lh params.txt nextxy_noedge.bin ctmare.bin elevtn.bin rivwth_merit.bin
```

### Expected output

These file names and sizes were observed in the bundled checkout. File permissions and timestamps can differ.

```text
-rw-r--r-- ... params.txt              384B
-rw-r--r--@ ... nextxy_noedge.bin       75K
-rw-r--r--@ ... ctmare.bin              38K
-rw-r--r--@ ... elevtn.bin              38K
-rw-r--r--@ ... rivwth_merit.bin        38K
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

## 6. Inspect runoff forcing

Runoff forcing is water input supplied to the model over time. The sample uses daily binary files on a grid coarser than the river map. Inspect a few dates, including the last day before the exclusive end date:

```bash
cd "${cmf_root}"
cd inp/test-mek_1deg/runoff
pwd
ls -lh Roff____20000101.one Roff____20000102.one Roff____20000209.one
```

### Expected output

The three observed sample files are each 2.3 KB; the exact display may round sizes differently.

```text
-rw-r--r--@ ... Roff____20000101.one  2.3K
-rw-r--r--@ ... Roff____20000102.one  2.3K
-rw-r--r--@ ... Roff____20000209.one  2.3K
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

## 7. Inspect runoff-to-map mapping

The runoff grid is 20 × 30 cells, while this Mekong river map is 80 × 120 cells. The input-dimension descriptor and mapping matrix tell the model how runoff-grid values correspond to map cells. These files are stored with the map because the mapping is specific to the map and runoff-grid pairing.

```bash
cd "${cmf_root}"
cd map/mekong_15min
pwd
ls -lh inpdim_test-mek_1deg.txt inpmat_test-mek_1deg.bin
cat inpdim_test-mek_1deg.txt
```

### Expected output

Observed sizes and the descriptor contents:

```text
-rw-r--r--@ ... inpdim_test-mek_1deg.txt       312B
-rw-r--r--@ ... inpmat_test-mek_1deg.bin       450K
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

## 9. Inspect the results

The GOSH run directory contains the copied executable, Namelist, model log, timing record, Restart, binary outputs, and CTL metadata. Inspect it from the repository root:

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
ls -lh out/tutorial01-get_started
```

### Expected output

Representative files from the completed run:

```text
-rwxr-xr-x ... MAIN_cmf                  600K
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

## Optional developer reset

Ordinary learners do not need the reset utility. Developers collecting a clean demonstration can use `design/tutorial01-reset.sh` in a disposable checkout. It cleans generated `src_main/` build products and removes only the selected `out/PROJECT/` directory; it preserves `config/Mkinclude`, bundled inputs, the shared `out/` parent, and other project directories. It requires confirmation unless `--yes` is supplied. See the developer-only [design materials](design/README.md) for details.
