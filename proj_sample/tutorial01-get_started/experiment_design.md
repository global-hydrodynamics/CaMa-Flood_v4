# Experiment Design: tutorial01-get_started

**CaMa-Flood v5 — First Installation and Simulation Tutorial**

Status: Revised draft  
Target directory: `proj_sample/tutorial01-get_started/`

## 1. Purpose

Develop a reproducible first-run tutorial that demonstrates the complete workflow from a newly downloaded CaMa-Flood v5 package to a successful Mekong simulation.

The tutorial covers:

1. Repository or package download.
2. Fortran and Python environment setup using the root `s01` and `s02` procedures.
3. Preparation of `config/Mkinclude`.
4. Compilation of the Fortran executable in `src_main/`.
5. Inspection of bundled map and runoff datasets.
6. Execution of the existing `mek01` sample simulation through `tutorial01-get_started.sh`.
7. Verification of generated files in `proj_sample/tutorial01-get_started/out/`.

The tutorial must be executable without downloading additional map or runoff datasets.

## 2. Files and Initial State

The distribution includes the necessary source code, map data, runoff forcing, and sample execution scripts.

The initial state assumed by the manual is:

| Component | Initial state |
|---|---|
| `config/Mkinclude` | Prepared by the user during environment setup |
| `src_main/` | No compiled objects or executable |
| `tutorial01-get_started/out/` | Does not exist |
| Map and runoff datasets | Already included |
| Python environment | Prepared using `s02` |
| Fortran toolchain | Prepared using `s01` |

The absence of compiled files and model outputs must be reproducible through a dedicated reset script.

## 3. Directory Structure

Proposed structure:

```text
proj_sample/tutorial01-get_started/
├── README.md
├── experiment_design.md
├── manual_guidelines.md
├── tutorial01-get_started.sh
├── tutorial01-reset.sh
├── tutorial01-get_started.md
└── out/                       # Generated, not tracked
```

Additional files for capturing and generating documentation may be introduced if needed.

## 4. Reset Script

Create:

```text
proj_sample/tutorial01-get_started/tutorial01-reset.sh
```

### Purpose

Restore the local development environment to the initial state assumed by the first-run manual.

The reset script is primarily intended for preparing reproducible documentation examples and testing the tutorial from a clean state.

### Required operations

**Step 1 — Clean compiled Fortran files**

Navigate to `src_main/` and execute the repository's supported clean procedure, normally:

```bash
make clean
```

Verify that the main executable and generated build artifacts have been removed.

Inspect the existing Makefile to determine the exact effects of its clean target.

**Step 2 — Remove tutorial-generated outputs**

Remove only the tutorial-specific output directory:

```text
proj_sample/tutorial01-get_started/out/
```

The script must not remove output directories belonging to other tutorials or sample experiments.

**Step 3 — Verify initial state**

Report:

- Whether the CaMa-Flood executable is absent.
- Whether the tutorial output directory is absent.
- Whether `config/Mkinclude` still exists.

The script should print a short status summary.

### Files and settings that must be preserved

The reset script must never delete or modify:

- `config/Mkinclude`.
- Compiler or Python environments.
- Bundled map datasets.
- Bundled runoff forcing.
- Existing `gosh_sample/` outputs.
- Outputs from other tutorials.
- Source code or documentation.
- Git-tracked files.

Do not use `git clean`, broad wildcard deletion, or a cleanup operation that may remove unrelated user files.

### Safety requirements

Before performing cleanup, resolve the repository root and validate that all cleanup paths point to the expected directories.

The script should work regardless of the user's current working directory.

If an existing `make clean` target affects files outside its intended scope, do not invoke it without addressing that risk.

The output removal step must be restricted to the exact Tutorial01 output directory.

The script should clearly report what it removed and what it preserved.

For interactive use, a confirmation prompt or explicit `--yes` option is recommended before deleting existing tutorial outputs.

## 5. Compilation and Environment Setup

The manual should follow the repository-root `s01` and `s02` procedures to prepare the Fortran and Python environments.

Fortran configuration:

- Start from an environment where the active `Mkinclude` has not yet been prepared.
- Use `s01` to generate or select the appropriate configuration.
- Copy the resulting `Mkinclude` into `config/`.
- Confirm the compiler configuration before running `make`.

Compile the model in `src_main/` using the current Makefile.

Confirm that the expected executable is generated.

The reset script must preserve `config/Mkinclude` so repeated compilation tests do not require repeating the environment setup.

## 6. Sample Simulation

Create `tutorial01-get_started.sh` by reusing the existing Mekong example under `gosh_sample/`.

Use the established sample configuration:

- Mekong 15-minute map.
- Bundled sample runoff forcing.
- January 1 to February 10, 2000.
- Model time step: 3600 seconds.
- OpenMP threads: 1.

Confirm exact parameter values against the current sample script.

Write all simulation outputs to:

```text
proj_sample/tutorial01-get_started/out/
```

Avoid duplicating scientific configuration or changing the behavior of the original sample script.

## 7. Verification

The simulation is successful when:

1. The Fortran executable exists after compilation.
2. The bundled map and runoff inputs are available.
3. The simulation completes without fatal errors.
4. The expected output directory is created.
5. The expected output files exist and have plausible sizes and time-record counts.

Use the current output variable registry to determine the expected files.

## 8. Reproducible Documentation Workflow

The development and manual-authoring workflow should be:

```text
Inspect repository and experiment design
                  |
                  v
Prepare Fortran/Python environments
                  |
                  v
Run tutorial01-reset.sh
                  |
                  v
Capture initial directory listings
                  |
                  v
Compile CaMa-Flood
                  |
                  v
Capture compilation output and executable
                  |
                  v
Run tutorial01-get_started.sh
                  |
                  v
Capture runtime output and result files
                  |
                  v
Verify simulation success
                  |
                  v
Generate tutorial01-get_started.md
```

The reset operation should be performed only in a working copy where deleting Tutorial01-generated outputs is acceptable.

The initial-state snapshots, build logs, simulation logs, and verification results should be retained for manual generation.

## 9. Acceptance Criteria

Implementation is complete when:

- `tutorial01-reset.sh` safely restores the clean build/output state.
- `config/Mkinclude` remains unchanged after reset.
- A fresh compilation succeeds.
- The Tutorial01 simulation succeeds.
- Output files are created only in the intended tutorial output directory.
- Real command outputs and before/after file listings can be collected for manual authoring.
- The workflow is reproducible in a clean working copy.

The initial implementation should prioritize reliability and simplicity.