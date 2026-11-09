# Tutorial 02 — Mekong four-experiment comparison

Tutorial 02 compares the four Mekong model configurations described in
[`tutorial_design.md`](tutorial_design.md). Its visualization scripts use the
repository's internal `cmf_colormap` package for Figure 4 (the annual-maximum
discharge map) and require the Python environment prepared by the root setup
script.

From the CaMa-Flood repository root, prepare and activate that environment:

```bash
./s02-python_setup.sh
source .venv/bin/activate
```

Then run the visualization workflow from the same configured environment:

```bash
bash tutorial/tutorial02-mekong_netcdf_4exp/v00-visualize_all.sh
```

If `s02-python_setup.sh` reports that no interactive terminal is available, rerun
it from a terminal so you can confirm the environment update. The workflow expects the Tutorial 02 annual model outputs and map data to be
available. If running Figure 4 directly, use the activated environment's
Python. If `cmf_colormap` is unavailable, rerun the root setup script and
confirm that `.venv` is active before retrying.
