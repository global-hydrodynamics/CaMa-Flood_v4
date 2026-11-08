# Tutorial01 — Get Started with CaMa-Flood v5

This tutorial builds or updates the CaMa-Flood v5 model, introduces the bundled Mekong map and runoff samples, and runs a 40-day simulation. It assumes you can enter commands in a terminal; each command section starts from the repository root so it does not depend on where the previous section ended.

The example uses the Mekong 15-minute river map and daily 1-degree runoff from 2000-01-01 00:00 through 2000-02-10 00:00. The end time is exclusive, so runoff through February 9 is included. Successful execution shows that the workflow runs; it does not establish scientific accuracy.

## 1. Get the repository

Run these commands from a directory where you keep projects. Change `~/work` if you use another location.

**Commands**

```bash
mkdir -p ~/work
cd ~/work
git clone --branch feature/gosh-refactor --depth 1 --single-branch \
  https://github.com/global-hydrodynamics/CaMa-Flood_v4.git CaMa-Flood_v5
cd CaMa-Flood_v5
pwd
cmf_root="$(pwd)"
```

`pwd` prints the repository root. The next command saves that absolute path in the shell variable `cmf_root`. Keep using the same terminal window: this variable remains available in that shell session, but a new terminal starts without it, so define it again there.

## 2. Explore the repository

The root contains the v5 source, compiler configuration, sample inputs, maintained workflows, and tutorials. Start by locating these parts; later sections inspect the sample files themselves.

**Commands**

```bash
cd "${cmf_root}"
pwd
ls -l
```

The checkout includes `src_main/` for the v5 model source, `config/` for its active build configuration, `map/` and `inp/` for map and runoff data, `gosh_sample/` for maintained example workflows, and `tutorial/` for beginner materials. Root setup scripts include `s01-setup_fortran.sh` and `s02-python_setup.sh`.

## 3. Prepare the local tools

The Fortran setup script checks compiler and library availability and stages a suggested `Mkinclude` configuration. Review that file before copying it into the existing v5 configuration directory. The Python setup script diagnoses or prepares the repository's optional Python tools.

**Check and stage the Fortran configuration**

```bash
cd "${cmf_root}"
pwd
bash s01-setup_fortran.sh
```

If the setup script staged `Mkinclude` in the repository root and you accept its settings, activate it for v5:

```bash
cd "${cmf_root}"
pwd
cp Mkinclude config/Mkinclude
```

`config/` is part of the checkout, so there is no need to create it. This v5 tutorial uses `config/Mkinclude`; leave the legacy `adm/Mkinclude` configuration alone.

**Check the Python environment**

```bash
cd "${cmf_root}"
pwd
bash s02-python_setup.sh
```

The tutorial's CTL utilities use Python's standard library. Installing the complete optional Python requirements is not necessary for this sample run.

## 4. Build or update the model

`src_main/` contains the v5 Fortran source. The explicit `all` target builds or updates the model executable `MAIN_cmf`. It works whether the executable is not present yet or needs rebuilding; this tutorial does not require a particular starting state.

**Commands**

```bash
cd "${cmf_root}"
cd src_main
pwd
make all
ls -lh MAIN_cmf
```

The final listing should show an executable named `MAIN_cmf`. The tutorial GOSH script copies it into its run directory and starts the model there with the matching namelist. Do not launch `MAIN_cmf` directly from an unrelated directory.

## 5. Find the Tutorial01 experiment settings

Tutorial01 is a standalone GOSH example: its settings are visible in the tutorial directory, so you can connect experiment choices to their input files. It follows the maintained Mekong sample while keeping generated results separate under Tutorial01.

**Commands**

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
ls -l
grep -nE '^# (River map configuration|Daily binary runoff sample|The dimension descriptor)' \
  tutorial01-get_started.sh
```

The matches point to the script's map, runoff, and input-matrix configuration groups. The following sections first locate the actual files, then show the corresponding settings.

## 6. Locate the river map

The Mekong map directory contains a parameter file describing the grid and Binary files that provide river connectivity and physical map fields.

**Commands**

```bash
cd "${cmf_root}"
cd map/mekong_15min
pwd
ls -lh params.txt nextxy_noedge.bin ctmare.bin elevtn.bin rivwth_merit.bin
```

The listing should include files of roughly these sizes: `params.txt` 384 bytes, `nextxy_noedge.bin` 75 KB, and `ctmare.bin`, `elevtn.bin`, and `rivwth_merit.bin` about 38 KB each. `params.txt` describes an 80 × 120 grid with 10 floodplain layers, 0.25-degree spacing, and map edges at 90–110°E and 5–35°N.

The corresponding excerpt from `tutorial01-get_started.sh` is:

```sh
MAPDIR="${CMF_BASE}/map/mekong_15min"
#NEXTXY="${MAPDIR}/nextxy.bin"
NEXTXY="${MAPDIR}/nextxy_noedge.bin"
#RIVWTH="${MAPDIR}/rivwth.bin"
RIVWTH="${MAPDIR}/rivwth_merit.bin"
```

`MAPDIR` selects this directory. `NEXTXY` selects the river-network connectivity file: `nextxy_noedge.bin` excludes river networks affected by the regional boundary, while the commented `nextxy.bin` alternative includes rivers draining outside the domain. `RIVWTH` selects channel width: the default `rivwth_merit.bin` is satellite-derived; `rivwth.bin` is the alternative estimated from upstream area. Other map fields include catchment area, elevation, downstream distance, channel length, floodplain elevation, channel depth, and roughness. The script uses individual Binary map inputs (`LMAPCDF=.FALSE.`) and enables bifurcation routing with `bifprm.csv`.

## 7. Locate the runoff files

Runoff forcing is water entering the river model from a coarser land grid. This sample stores one Binary file per day.

**Commands**

```bash
cd "${cmf_root}"
cd inp/test-mek_1deg/runoff
pwd
ls -lh Roff____20000101.one Roff____20000102.one Roff____20000209.one
```

Each listed daily file is about 2.3 KB. Names follow the pattern `Roff____YYYYMMDD.one`; for example, `Roff____20000101.one` contains the first day's data. The sample values are runoff rates represented in `mm/day` before conversion to the model's `m/s` units.

The related GOSH settings are:

```sh
ROFFTAG="test-mek_1deg"
IFRQ_INP=24
DROFUNIT=86400000
CROFFREQ="DAY"
CROFPRE="Roff____"
CROFSUF=".one"
```

`ROFFTAG` selects this dataset and forms `RUNOFF_DIR`. `CROFPRE` and `CROFSUF` describe the filename prefix and suffix. `IFRQ_INP=24` reads runoff at 24-hour intervals, `CROFFREQ="DAY"` identifies daily grouping, and `DROFUNIT=86400000` supplies the conversion from `mm/day` to `m/s`. The 40-day simulation reads dates from January 1 through February 9, including the month boundary.

## 8. Locate the runoff-to-map input matrix

The runoff grid has fewer cells than the river map. CaMa-Flood uses a pre-generated input matrix to map runoff-grid values onto model map cells; the dimension descriptor identifies the paired grids and matrix file.

**Commands**

```bash
cd "${cmf_root}"
cd map/mekong_15min
pwd
ls -lh inpdim_test-mek_1deg.txt inpmat_test-mek_1deg.bin
cat inpdim_test-mek_1deg.txt
```

The descriptor lists an 80 × 120 map grid, a 20 × 30 runoff grid, four input mappings, and the matrix filename `inpmat_test-mek_1deg.bin`. The matrix file is about 450 KB. These files are already included and paired for this sample; you do not need to generate them.

The script selects the pair with:

```sh
INPDIM="${MAPDIR}/inpdim_${ROFFTAG}.txt"
INPMAT="${MAPDIR}/inpmat_${ROFFTAG}.bin"
LINTERP=".TRUE."
```

The model receives the files as `CDIMINFO` and `CINPMAT`; `LINTERP=".TRUE."` selects the input-matrix mapping path.

## 9. Run Tutorial01

The script creates `out/` when needed. On a rerun it reports that it is overwriting previous sample results and removes only the known Tutorial01-generated executable, namelist, logs, output-variable files, and Restart files. It does not remove the whole `out/` directory or unrelated files in it.

Run from the script's directory:

**Commands**

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
./tutorial01-get_started.sh
```

The experiment runs from 2000-01-01 00:00 to 2000-02-10 00:00 exclusive, with one OpenMP thread, adaptive time stepping, bifurcation routing, and zero-storage initialization. It reads daily Binary runoff and writes daily Binary `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc` outputs. The script generates GrADS CTL descriptions, runs the model, records timing and status, and generates Restart CTLs after a successful run.

The shell prints step messages and a short runtime summary. The model's detailed progress is written separately to `log_CaMa.txt`.

## 10. Inspect the results

The run products are kept under Tutorial01's `out/` directory.

**Commands**

```bash
cd "${cmf_root}"
cd tutorial/tutorial01-get_started
pwd
ls -lh out
```

The checked run included the expected model, Namelist, logs, five output Binary files and CTLs, and an end-of-run Restart with its bifurcation companion. A few representative observed entries were:

```text
outflw2000.bin       1.5M
outflw2000.ctl       293B
restart2000021000.bin 450K
log_CaMa.txt         1.6M
TDEF 40 LINEAR 00Z02jan2000 1dy
```

Each daily output contains 80 × 120 cells for 40 records; at four bytes per float32 value, that is 1,536,000 bytes per file. The `out/` directory is Git-ignored.

In the checked run, the `outflw` CTL described 40 daily records beginning at `00Z02jan2000`, and the Restart was timestamped at the exclusive endpoint, 2000-02-10 00:00. These file and log checks show that the example workflow completed. They do not validate hydrological accuracy or compare discharge against observations.

## Optional developer reset

Ordinary learners do not need to reset the build. Developers preparing a clean demonstration can use [`design/tutorial01-reset.sh`](design/tutorial01-reset.sh) in a disposable checkout. It asks before cleaning unless `--yes` is supplied, removes build products and only Tutorial01's `out/`, and preserves `config/Mkinclude` and bundled input data.
