# Comparison with observations

These scripts compare CaMa-Flood discharge (`outflw`), water-surface elevation (`sfcelv`, WSE), and flood-water extent (`fldare`, FWE) outputs with observation data. Observation examples supplied with a distribution package are dummy data because the original observation datasets cannot be redistributed. Replace them with appropriately licensed observations for scientific validation. A GitHub clone does not include the package's `obs_sample/` directory.

## Setup and execution

Run from `etc/validation/`. Starting at the model root (a GitHub checkout or extracted package):

```sh
cd etc/validation
```

Before execution, edit the selected shell script's map, simulation-output, observation, period, and output-format settings. Discharge and WSE also use station lists allocated to the selected river map; WSE requires the observation reference-geoid setting. FWE uses a selected geographical domain. Provide the files expected by the corresponding Python reader.

The wrappers invoke `python` and require NumPy, Matplotlib, and netCDF4 in that environment. They create `map`, `out`, and `obs` links and replace the selected figure/text output directories; check existing outputs before running.

## Examples

| Comparison | Command | Python implementation | Outputs |
| --- | --- | --- | --- |
| Daily discharge | `sh s01-discharge_validation.sh` | [`src/discharge_validation.py`](src/discharge_validation.py) | Figures and paired data under `fig_<TAG>/discharge/` and `txt_<TAG>/discharge/`; Nash–Sutcliffe and Kling–Gupta efficiency statistics. |
| Daily WSE | `sh s02-wse_validation.sh` | [`src/wse_validation.py`](src/wse_validation.py) | Figures and paired data under `fig_<TAG>/wse/` and `txt_<TAG>/wse/`; root mean square error. |
| Monthly FWE | `sh s03-flood_extent_validation.sh` | [`src/flood_extent_validation.py`](src/flood_extent_validation.py) | Time-series figures and paired data under `fig_<TAG>/fwe/` and `txt_<TAG>/fwe/`; correlation coefficient. |

The wrappers call the Python programs with their configured arguments. Run the shell commands above after adapting their settings. For the model experiments that provide simulation outputs, see the [sample execution guide](../../gosh/README.md).
