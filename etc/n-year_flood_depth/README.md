# Legacy N-year flood-depth workflow

This directory contains an older post-processing workflow for estimating flood-depth fields at selected return probabilities from CaMa-Flood annual output files. It can fit annual maxima of either `rivdph` or `storge`, downscale the resulting field using high-resolution map data, and produce a figure. The scripts are retained as legacy examples; their assumptions, dependencies, and file handling have not been modernized or comprehensively validated.

The actual top-level entry point is [`script/main.sh`](script/main.sh). Run it with the current working directory set to `etc/n-year_flood_depth/script`; its configuration files, relative paths, helper executables, temporary products, and figures all assume that directory. The workflow is optional post-processing and is not part of the standard CaMa-Flood simulation run.

## Processing sequence

1. `main.sh` writes `vars.txt`, `glbnames.txt`, `funcs.txt`, `rps.txt`, and `domain.txt` in the current directory, then calls the two stage scripts.
2. `s01-n-year_para_estimate.sh` reads yearly model outputs, computes one annual maximum grid per year, and fits distribution parameters cell by cell.
3. `s02-n-year_flddph_estimate.sh` evaluates the fitted quantile. For `rivdph`, it converts the river-depth quantile to flood depth directly. For `storge`, it first estimates a storage quantile, then uses map geometry to convert that storage to flood depth.
4. `downscale_nflddph` maps the coarse flood-depth field to the requested high-resolution domain using catchment, floodplain-height-difference, and river-width maps. It writes a depth grid and a one-byte mask.
5. `t02-draw_flddph.sh` prepares display-only slope and elevation grids, transforms the downscaled depths with `conv_flood`, and calls `draw_flddph.py` to save a figure.

The shell scripts invoke prebuilt Fortran programs. From the repository root, build the configured targets with `make -C etc/n-year_flood_depth/script/src` after confirming the local compiler settings in [`adm/Mkinclude`](../../adm/Mkinclude). `downscale_nflddph_1deg.F90` is present but is not a target in this Makefile. The currently configured example requests `RES=1min`; the downscaler source also contains branches for `3sec`, `15sec`, and `30sec`. The separate visualization helpers accept additional resolutions in their source, which does not by itself mean the entire workflow has been verified at those resolutions.

## Example configuration and return probability

The committed settings in `main.sh` select:

- Simulation output name: `test4-e2o_ecmwf-glb_15min`.
- Annual output years: 1980 through 2014, inclusive (35 annual files expected).
- Variables: `rivdph` and `storge`.
- Distribution: `GUM` (Gumbel).
- Domain: west/east 102/108 degrees, south/north 9/15 degrees (Mekong example).
- Downscaling resolution: `1min`, `NGRID=1`, and display depth parameter `MAXDPH=11`.
- `RPS=0.01`.

`RPS` is not interpreted uniformly as “years.” Both return-value scripts use this rule: if a value is greater than 1, it is treated as a return period in years and converted to non-exceedance probability `1 - 1/RP`; otherwise it is treated as annual exceedance probability and converted to non-exceedance probability `1 - RPS`. Thus the configured `RPS=0.01` means 1% annual exceedance probability, conventionally a 100-year return level under the annual-maxima and stationary-probability interpretation. It does **not** mean a 0.01-year return period. The comment beside `RPS` in `main.sh` is inconsistent with this calculation. The fit uses annual maxima, and the probability is passed to the distribution's quantile function.

## Calculation pathways

### `rivdph`

The annual maximum of each yearly `rivdphYYYY.bin` file is fitted directly. For the requested probability, `rp2flddph_dis.py` evaluates the fitted river-depth quantile, subtracts the corresponding map's `rivhgt.bin`, and clips negative depths to zero. Ocean cells identified by `rivhgt == -9999` are left as the initialized missing value. The result is written as `Nyear_flddph/flddph_RP<RPS>_<FUNC>.bin`.

### `storge`

The annual maximum of each yearly `storgeYYYY.bin` file is fitted as storage, and `rp2storge_dis.py` evaluates a return-period storage grid. `sto2dph.py` then converts that storage to flood depth using river geometry and the layered floodplain height profile. The script uses `rivlen.bin`, `rivwth.bin`, `rivhgt.bin`, `ctmare.bin`, and `fldhgt.bin` (with ten floodplain layers hard-coded as `NH=10`). It derives channel and floodplain storage capacity by layer and solves for depth within the relevant layer; values above the top modeled layer use a separate expression. Its floodplain-storage offset is initialized to zero in this script.

This pathway estimates an extreme storage state first because the fitted variable is total `storge`, while a flood depth is not obtained directly from that storage distribution's parameters. The geometric conversion is a separate nonlinear step. Consequently, the `storge` and `rivdph` pathways are different calculations and should not be assumed to produce equivalent flood depths.

## Statistical implementation

`annual_max.py` reads every record in each annual file, reshapes it to `(records, ysize, xsize)`, and takes the maximum over records. Values greater than `1.e18` are replaced with `-9999`; annual fields are written as native-endian float32. The configured fitting period therefore requires one complete annual file per requested year. The scripts do not verify temporal completeness or that the annual files use the intended output frequency.

`calc_distributions.py` sorts each cell's annual maxima and calls `lmoments.samlmu` followed by the corresponding `pel*` parameter estimator. The implemented fit/quantile branches are GEV, GAM, PE3, GUM, WEI, and WAK. The default run selects only GUM. Although `main.sh` lists additional distribution names in comments, those names are not implemented by the fitting and return-value scripts in this directory. The code also calculates several AIC-like diagnostics; they do not select a distribution automatically. Some fitting branches use different variability checks over slices of the sample, and failures or sentinel values are handled inconsistently. Treat parameter and fit-diagnostic files as legacy outputs requiring independent review.

For `rivdph`, `annual_max.py` also calculates a mean-annual-maximum-derived `flddph` array, but writes `amax_ave` (the `rivdph` average) to the `flddph-..._anmax_ave.bin` filename. That file therefore does not contain the derived flood-depth array; the later return-level stages do not use it.

The Python statistical code imports NumPy, `lmoments`, and `RegscorePy`; plotting additionally uses Matplotlib. The scripts do not provide a dependency lock file or tested version range. The Fortran utilities use the repository compiler configuration.

## Inputs and map requirements

The scripts expect CaMa-Flood outputs under `$CAMA_FOLDER/out/<GLBNAME>/`, where the default `CAMA_FOLDER` is derived from the working directory in `main.sh`. For each configured year, provide files named `rivdphYYYY.bin` and `storgeYYYY.bin` as required by their respective pathway. Their dimensions are read from the selected map's first two lines in `params.txt`; the reader assumes native float32 grids and annual files that can be reshaped into complete records of that grid size.

The selected coarse map must be supplied through `MAPDIR` (default `map/glb_15min`) and include `params.txt`, `rivhgt.bin`, `rivlen.bin`, `rivwth.bin`, and `ctmare.bin`. The storage conversion also needs `fldhgt.bin` with ten layers in the layout expected by the script. The downscaler and plotting helpers require high-resolution map tiles below `<MAPDIR>/<RES>/`, including `location.txt`; the downscaler reads each relevant tile's `.catmxy.bin`, `.flddif.bin`, and `.rivwth.bin`. Plot preparation reads `.hand.bin` and `.elevtn.bin` from tiles intersecting the requested area. These map products must correspond to the same map and resolution as the model outputs.

## Working directory and outputs

Run from `etc/n-year_flood_depth/script`, after building the Fortran helpers and configuring `main.sh`. The workflow creates relative symlinks named `map` and `inp`, and reads the configuration text files from the current directory. The stage scripts create results under `../result/<GLBNAME>/` for `rivdph` and `../result/<GLBNAME>/STO2DPH/` for `storge` (singular `result`, as implemented). Expected products include:

- `amax/`: one annual maximum grid per year and an average grid.
- `para/`: fitted parameters and fit diagnostics, stored as float64 grids.
- `Nyear_flddph/`: return-level flood depth for the direct `rivdph` pathway or after storage conversion.
- `Nyear_storge/`: return-period storage grid for the storage pathway.
- `<EXPNAME>-<RES>/downscaled_flddph/`: downscaled float32 depth, int8 mask, plus display slope/elevation grids.
- `<EXPNAME>-<RES>/floodrisk_figure/`: figures created by the plotting script.

Binary grids contain no coordinates or metadata; the map parameters, dimensions, variable, year/probability, and distribution in the filename are needed to interpret them. The downscaled mask values are assigned by the Fortran source (including floodplain and permanent-water classes); inspect `downscale_nflddph.F90` before treating them as a standardized product.

## Limitations and maintenance notes

- The workflow was written for a long simulation and the sample configuration requests 35 years. A successful run with shorter or incomplete data does not establish a reliable return-level fit.
- The input files, map layers, record layout, units, and temporal coverage are not validated by the scripts. Confirm they agree before use.
- `main.sh` writes shared configuration files in its working directory; reruns overwrite them. The scripts replace symlinks with `ln -snf`, remove the local `map` link and `ele.bin` at the end of stage 2, and the plotting helper deletes/recreates a local `fig/` directory. Avoid concurrent runs in the same script directory and preserve any needed local products.
- Error checking is limited, and shell scripts do not consistently stop when a command fails. Inspect logs and expected file sizes/coverage rather than relying only on the final exit status.
- Plot output is for visualization: `conv_flood.F90` masks small depths, spreads/caps values, and inserts color-bar anchor values. Use the quantitative downscaled depth file, not the display grid, for analysis.
- The existing guideline refers to `../results/` and says to list that directory; the current scripts write to `../result/`. See [Guideline §4.4](../../doc/CaMa-Flood_Guideline.md#44-flood-hazard-map) for the historical overview, and use this README and the source scripts for current paths and behavior.

This README documents observed implementation behavior, not a validation of the statistical method or the scientific suitability of a particular return-level estimate.
