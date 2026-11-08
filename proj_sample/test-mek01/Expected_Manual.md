# CaMa-Flood v5 — Your First Simulation
## A Reproducible Tutorial Using the Mekong 15-minute Model

**Example project:** `proj_sample/test-mek01/`  
**Simulation period:** January 1 – February 10, 2000 (40 days)  
**Objective:** Learn how to compile CaMa-Flood, understand its input datasets, run a simulation, and inspect the output files.

*This is an example of the tutorial we aim to generate automatically. File listings, configuration excerpts, and output examples shown below are illustrative and must be replaced with information collected from actual files and test runs.*

---

## 1. Introduction

In this tutorial, we will run CaMa-Flood v5 for the Mekong River Basin.

CaMa-Flood is a hydrodynamic model that simulates river discharge, water storage, water surface elevation, and floodplain inundation.

The example simulation uses:

| Item | Configuration |
|---|---|
| Study area | Mekong River Basin |
| River map | `map/mekong_15min/` |
| Map resolution | 15 arc-minutes (0.25°) |
| Map domain | 90°E–110°E, 5°N–35°N |
| Runoff dataset | `test-mek_1deg` |
| Simulation period | 2000-01-01 to 2000-02-10 |
| Time step | 3600 seconds |
| OpenMP threads | 1 |
| Run script | `mek01-default_run.sh` |

### Project structure

The example project is located at:

```text
proj_sample/
└── test-mek01/
    ├── README.md
    └── mek01-default_run.sh
```

The main script manages the simulation settings and executes CaMa-Flood.

We will examine the source code, map data, runoff forcing, and simulation outputs step by step.

## 2. Source Code and Compilation

### 2.1 Where is the source code?

The CaMa-Flood source code is located in:

```text
src_main/
```

List the files:

```bash
ls src_main/
```

**Example output (to be generated from the actual repository):**

```text
MAIN_cmf.F90
...
Makefile
```

CaMa-Flood is written primarily in Fortran.

### 2.2 Compile CaMa-Flood

Use the compilation settings provided with the package.

For example:

```bash
cd src_main/
make clean
make all
```

*The exact compilation commands and configuration files must be verified against the current v5 repository.*

### 2.3 Check successful compilation

After compilation, check that the executable exists.

```bash
ls -lh MAIN_cmf
```

**Expected result:**

```text
MAIN_cmf    [file size]    [date]
```

The executable should exist and be runnable.

## 3. Understanding the River Map

### 3.1 Where is the map?

The river map used in this experiment is:

```text
map/mekong_15min/
```

Check the directory contents:

```bash
ls map/mekong_15min/
```

**Example output (illustrative):**

```text
params.txt
nextxy_noedge.bin
elevtn.bin
uparea.bin
rivlen.bin
rivwth_merit.bin
...
```

### 3.2 What do these files mean?

| File | Description |
|---|---|
| `params.txt` | River map dimensions and geographical configuration |
| `nextxy_noedge.bin` | Downstream connectivity between river grid cells |
| `elevtn.bin` | Elevation information |
| `uparea.bin` | Upstream drainage area |
| `rivlen.bin` | River channel length |
| `rivwth_merit.bin` | River channel width data |

Other required map files should also be listed and explained according to the current v5 configuration.

### 3.3 Inspect the map configuration

Run:

```bash
cat map/mekong_15min/params.txt
```

**Example information extracted from the file:**

```text
Grid dimensions: 80 × 120
Grid resolution: 0.25 degrees
Longitude range: 90–110 E
Latitude range: 5–35 N
```

The actual contents of `params.txt` should be displayed, together with a short explanation of the important parameters.

### Checkpoint 1 — River map

Confirm that:

- The map directory exists.
- `params.txt` can be read.
- All map files required by the experiment are available.

## 4. Understanding the Runoff Input

### 4.1 Where is the runoff forcing?

This experiment uses sample runoff data located in:

```text
inp/test-mek_1deg/runoff/
```

Inspect the directory:

```bash
ls inp/test-mek_1deg/runoff/ | head
```

**Example file naming pattern:**

```text
Roff20000101.one
Roff20000102.one
Roff20000103.one
...
```

*The actual filenames must be obtained from the dataset.*

### 4.2 What is the runoff data format?

The runoff data used in this example is a daily binary dataset.

Important characteristics to document include:

| Property | Description |
|---|---|
| Format | Plain binary |
| Spatial resolution | 1° |
| Temporal resolution | Daily |
| Geographical coverage | Mekong sample domain |
| Variable | Runoff |
| Unit | Verify from the dataset registry |
| Filename convention | Defined by the forcing configuration |

Check the actual dataset metadata and verify that it matches the configuration in the run script.

### 4.3 How does CaMa-Flood read the runoff?

CaMa-Flood uses a mapping table to convert runoff from its original geographical grid to the river model grid.

For this experiment, inspect the corresponding:

```text
inpdim_test-mek_1deg.txt
inpmat_test-mek_1deg.bin
```

These files contain the input grid information and mapping information required by CaMa-Flood.

### Checkpoint 2 — Runoff forcing

Confirm that:

- The runoff directory exists.
- Required runoff files are available throughout the simulation period.
- The runoff format matches the run script settings.
- The corresponding input mapping files exist.

## 5. Understanding the Experiment Settings

The simulation is controlled by:

```text
proj_sample/test-mek01/mek01-default_run.sh
```

Open this script using a text editor.

We will examine several important settings.

### 5.1 Experiment name and output directory

The experiment name determines where the simulation results are stored.

**Illustrative example:**

```bash
PROJ="mek01-default_run"
```

The resulting output directory may be configured as:

```text
out/mek01-default_run/
```

The exact relationship between the experiment name and the output directory must be taken from the actual run script.

### 5.2 Simulation period

```bash
SYEAR=2000
SMON=1
SDAY=1

EYEAR=2000
EMON=2
EDAY=10
```

*Illustrative settings; actual variable names may differ.*

This experiment runs for approximately 40 days, depending on the model's start/end-date convention.

### 5.3 Input datasets

The script specifies:

- River map directory
- Runoff directory
- Input mapping table
- Runoff file format
- Runoff filename prefix and suffix

Identify these settings in the actual run script and explain their meanings.

### 5.4 Output variables

CaMa-Flood can save several different variables.

For example:

| Variable | Description | Unit |
|---|---|---|
| `outflw` | River discharge | m³/s |
| `sfcelv` | Water surface elevation | m |
| `rivdph` | River water depth | m |
| `flddph` | Floodplain water depth | m |

The complete set of enabled output variables should be extracted from the actual experiment settings, with descriptions checked against `variable_table/`.

## 6. Running the Simulation

Navigate to the project directory and run the script.

```bash
cd proj_sample/test-mek01/
bash mek01-default_run.sh
```

The run script prepares the experiment and executes CaMa-Flood.

### 6.1 What happens during execution?

The major steps are:

1. Read the experiment settings.
2. Prepare the experiment directory.
3. Read the river map and runoff data.
4. Initialize the simulation.
5. Perform the time integration.
6. Write simulation outputs.
7. Save the execution logs.

### 6.2 Check the execution log

Inspect the log produced by the simulation.

```bash
tail -n 20 out/mek01-default_run/log_CaMa*
```

*The path and filename should be verified against the actual output.*

The generated manual should show a short excerpt from the actual execution log.

### Checkpoint 3 — Simulation

Confirm that:

- The model executable ran successfully.
- No fatal errors were reported.
- The simulation reached its intended end date.
- The output directory was created.

## 7. Understanding the Simulation Output

### 7.1 Where are the results?

The output directory is specified by the run script.

**Illustrative example:**

```text
out/mek01-default_run/
```

Check the files:

```bash
ls -lh out/mek01-default_run/
```

**Example output:**

```text
input_cmf.nam
log_CaMa...
outflw2000.bin
sfcelv2000.bin
rivdph2000.bin
flddph2000.bin
...
```

The complete file listing should be obtained from the actual simulation.

### 7.2 What are these output files?

| File | Description |
|---|---|
| `input_cmf.nam` | Simulation configuration |
| `log_CaMa...` | Model execution log |
| `outflw2000.bin` | River discharge |
| `sfcelv2000.bin` | Water surface elevation |
| `rivdph2000.bin` | River water depth |
| `flddph2000.bin` | Floodplain water depth |

The actual output list depends on the enabled output variables.

### 7.3 Check whether all output files are complete

A file existing does not necessarily mean that the simulation was successful.

Check:

- Whether the required variables were produced
- Whether the output file sizes are consistent with the documented binary format
- Whether the number of saved time records matches the simulation settings
- Whether the execution log indicates successful completion

### Checkpoint 4 — Simulation outputs

Confirm that all expected output files are present and complete.

## 8. Exercise 1 — Changing the Experiment Name

Now try creating a second experiment.

### 8.1 Change the experiment name

In `mek01-default_run.sh`, change the project or experiment name.

For example:

```bash
PROJ="mek01-exercise"
```

*Use the actual configuration variable in the script.*

### 8.2 Run the simulation again

```bash
bash mek01-default_run.sh
```

### 8.3 Compare the output directories

Check the output directories:

```bash
ls out/
```

You should now find separate output directories for the original and modified experiments, if the run script is configured to use the experiment name in its output path.

**What did you learn?**

Changing the experiment name allows you to manage multiple simulations independently without overwriting previous results.

## 9. Exercise 2 — Changing the Output Variables

CaMa-Flood allows users to select which model variables are saved.

### 9.1 Modify the output settings

Find the output variable settings in `mek01-default_run.sh`.

For example, enable or disable one output variable.

Use a new experiment name so that the original simulation results are preserved.

### 9.2 Run the simulation again

```bash
bash mek01-default_run.sh
```

### 9.3 Compare the output files

Compare the original and modified output directories.

**Example comparison:**

| Variable | Original experiment | Modified experiment |
|---|---|---|
| `outflw` | Yes | Yes |
| `sfcelv` | Yes | No |
| `rivdph` | Yes | Yes |
| `flddph` | Yes | Yes |

*This is an illustrative comparison. The actual table should be generated from the two simulation results.*

**What did you learn?**

The selected output variables determine which data files CaMa-Flood saves.

This allows users to reduce unnecessary output and select variables for their research objectives.

## 10. Summary

Congratulations! You have completed your first CaMa-Flood v5 experiment.

You should now understand:

- Where the CaMa-Flood source code is located and how to compile it
- How the river map is organized
- Where the runoff data is stored and how it is formatted
- How the run script controls a simulation
- Where the results are saved
- How to check whether a simulation completed successfully
- How to manage multiple experiments
- How to change the saved output variables

### Final checklist

| Test | Expected result |
|---|---|
| Source code | Available |
| Compilation | Successful |
| River map | Complete |
| Runoff forcing | Complete |
| Simulation | Completed |
| Output files | Complete |
| Experiment name modification | Separate output |
| Output variable modification | Changed output files |

The next tutorial will introduce scientific visualization of the model results using Python.

---

# Documentation Generation Challenge

The objective of this assignment is **not to manually write the entire tutorial above**.

Instead, develop a method to generate as much of this tutorial as possible from the actual CaMa-Flood v5 repository, configuration files, and simulation results.

In particular:

1. **Automatically collect information:** Obtain file lists, selected file contents, configuration values, execution logs, and output summaries directly from the existing files and scripts.
2. **Generate Markdown documentation:** Insert this information into a readable tutorial containing short explanatory text and executable commands.
3. **Make the process reproducible:** The generated information should be refreshable when the model, input data, configuration, or output files change.
4. **Validate the examples:** Confirm that the commands and file paths shown in the tutorial work in the tested environment.
5. **Separate generated content from human explanations:** Scientific explanations may be written manually, but technical information should come from the actual files whenever practical.

### Starting point

The working directory is:

`proj_sample/test-mek01/`

The existing file `mek01-default_run.sh` is already available.

You may freely experiment within this directory. There is no need to reorganize the main CaMa-Flood repository or prepare an official distribution package at this stage.

### Expected deliverables

- A sample tutorial in Markdown, similar to the example above
- Scripts or another reproducible method to generate its technical content
- A README explaining how to regenerate the tutorial
- A short explanation of which sections are generated automatically and which still require manual editing

**First milestone:** Propose a simple implementation plan before starting major development.

The initial priority is a working proof of concept, not a sophisticated documentation framework. Scientific visualization, PDF generation, continuous integration, and integration into the official CaMa-Flood distribution can be addressed later.