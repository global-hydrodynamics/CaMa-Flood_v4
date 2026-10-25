# Runoff climatology and coastal adjustment utilities

`runoff_preset/` contains legacy Fortran and Shell preprocessing for ELSE_GPCC-style daily runoff files. It calculates daily climatologies, optionally adjusts runoff at selected coastal input-grid cells, then recalculates the climatology. This is a specialized preprocessing workflow, not a required step in every CaMa-Flood run.

## Workflow

| Script / program | Current role |
| --- | --- |
| [`s01-daily_climatology.sh`](s01-daily_climatology.sh) | Averages daily `Roff____YYYYMMDD.one` grids for 1981–2010 into a 365-day climatology (no February 29) and an annual sum. |
| [`s02-mod_coast.sh`](s02-mod_coast.sh) | For each daily grid from 1979–2010, uses the long-term mean and input matrix to identify mapped coastal input cells. Where mean runoff is non-positive, replaces daily runoff with the minimum positive value among the surrounding eight cells, if one exists. Longitude neighbors wrap at the grid edge. |
| [`s03-dayclm_coastmod.sh`](s03-dayclm_coastmod.sh) | Recalculates the daily climatology and annual sum from `runoff_mod/` for 1981–2010. |
| `src/day_clim`, `src/ave_clim`, `src/mod_runoff`, `src/igetday` | Fortran programs built from the matching sources in [`src/`](src/). |

The scripts generate `ELSE_GPCC_dayclm-1981-2010.one`, `ELSE_GPCC_aveclm-1981-2010.one`, and corresponding `ELSE_GPCC_coastmod_*` files. Daily climatologies contain 365 records; the annual product sums daily climatological rates (script comment: `mm/year`). The readers and writers use direct-access, native-endian float32 grids; dimensions come from the configured `diminfo` file.

## Inputs and running

The Shell examples expect source files under `../../inp/ELSE_GPCC/Roff/`, named `Roff____YYYYMMDD.one`. They also expect `map/` to link to a matching map with `diminfo_1deg.txt` and `inpmat-1deg.bin`. These forcing data and support files are not included in this source directory; the repository source checkout alone is insufficient to run the workflow.

Build the programs with `make -C src` using the repository's active [`adm/Mkinclude`](../../adm/Mkinclude). Then, from `etc/runoff_preset/`, configure paths and years in the scripts and run them in sequence:

```sh
sh s01-daily_climatology.sh
sh s02-mod_coast.sh
sh s03-dayclm_coastmod.sh
```

The scripts write the adjusted daily files to `runoff_mod/`; the coastal adjustment uses `inpmat-1deg.bin` to select runoff-grid cells touched by the river map and writes on the original input grid. The resulting climatology files can be used as runoff products in downstream map-parameter or model workflows, subject to matching their grid, units, and required interpolation setup. This directory does not itself run CaMa-Flood.

No Python package is required. A Fortran compiler configured by `adm/Mkinclude`, the matching map support files, and the external runoff dataset are required.

## Limitations / Future Maintenance

The paths, years, names, and grid settings are hard-coded. The scripts do not verify file completeness, dimensions, units, or subprocess failures. They delete and recreate `dayclm_ori/`, `dayclm_coastmod/`, and `runoff_mod/`; preserve needed data elsewhere and avoid concurrent runs in this directory. Validate the forcing and matching map before using the generated files.
