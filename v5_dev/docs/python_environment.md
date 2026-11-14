# Python environments

CaMa-Flood supports two ways to run its Python tools. The repository-local
`.venv` is recommended for most users. Researchers who maintain a shared venv,
Conda environment, or HPC-managed Python can keep using it and manage its
packages themselves. CaMa-Flood does not automatically install into or update
an external environment.

## Recommended: repository-local `.venv`

From the repository root, run the interactive setup:

```bash
./s02-python_setup.sh
```

The script creates or reuses `.venv`, installs `requirements.txt`, and installs
the bundled `cmf_colormap` package there. It does not install packages into
system Python. The script asks before making changes. Without an interactive
terminal it only diagnoses the setup and exits; rerun it from a terminal to
create or update `.venv`.

Activate the environment and run a tutorial:

```bash
source .venv/bin/activate
python tutorial/tutorial02-mekong_netcdf_4exp/v04-annual_max_discharge.py
```

Activation is optional when you call the environment's interpreter directly:

```bash
.venv/bin/python tutorial/tutorial02-mekong_netcdf_4exp/v04-annual_max_discharge.py
```

## Use an existing Python environment

First check which interpreter the shell will use:

```bash
python -c "import sys; print(sys.executable)"
```

From the repository root, install only the bundled colormap package into that
interpreter's environment:

```bash
python -m pip install --no-deps ./visualization/cmf_colormap
```

This is a non-editable installation. `--no-deps` prevents pip from installing
or upgrading dependencies; Matplotlib and any other required packages must
already be available or be installed explicitly by you. Do not apply the full
`requirements.txt` to a shared environment unless you have reviewed and chosen
to do so. Installing from another CaMa-Flood checkout may replace the installed
`cmf_colormap` version. Developers who intentionally need live source edits may
choose an editable install, but it is not the general-user default.

Run the scripts with the same interpreter used for installation. For example,
the visualization runner accepts an explicit interpreter through `PYTHON`:

```bash
PYTHON=python bash tutorial/tutorial02-mekong_netcdf_4exp/v00-visualize_all.sh
```

`python` and `python3` can refer to different environments. Pair `python -m
pip` with scripts launched by that same `python`, or explicitly use the same
absolute interpreter path for both.

## Verify the environment

Run these commands with the Python interpreter you plan to use:

```bash
python -c "import sys; print(sys.executable)"
python -m pip --version
python -c "from cmf_colormap import get_cmap; print(get_cmap('shade_rainbow').name)"
```

The last command should print `shade_rainbow`. The pip output identifies which
Python installation owns that pip.

## Troubleshooting and safety

- **`No module named 'cmf_colormap'`:** install it into the interpreter that
  runs the script. Use the repository-local setup above or the manual
  `--no-deps` command for an existing environment.
- **Missing Matplotlib, NumPy, or NetCDF4:** these are separate dependencies,
  not evidence that `cmf_colormap` itself is missing. Use the local setup, or
  manage the needed packages explicitly in your own environment.
- **Installed into the wrong Python:** compare `python -c "import sys;
  print(sys.executable)"` with `python -m pip --version`. Activate the intended
  environment or use its interpreter's full path for both commands.
- **The intended venv is not active:** activate it with `source
  .venv/bin/activate`, or call `.venv/bin/python` directly.
- **The existing `.venv` cannot be reused:** `s02-python_setup.sh` reports an
  invalid environment and does not delete it. Move or remove that directory
  manually only after deciding whether it contains anything you need, then
  rerun setup from a terminal.
- **Setup runs without a terminal:** the script performs diagnosis only. Run
  it from an interactive terminal to approve environment creation or updates.
- **HPC or managed Python:** use a user-owned venv, Conda environment, or the
  site's supported module environment. Do not modify system Python or a shared
  installation without authorization.

The local `.venv` keeps tutorial packages isolated from system and other
projects. Existing environments remain supported, but their owners control
package changes. Installing the bundled package with `--no-deps` avoids
surprise dependency changes, and the non-editable install avoids tying a shared
environment to a mutable checkout.
