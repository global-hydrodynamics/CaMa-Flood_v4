# Runoff input data and preparation

The `inp/` directory contains tools for preparing runoff forcing used by CaMa-Flood. Runoff supplies water per unit land area over time; CaMa-Flood converts it to a flow rate for routing through the river network. This checkout includes an ERA5-Land download and conversion workflow with small Japan examples. It is not a catalog of all supported forcing products, and most forcing data used by the `gosh/` examples must be obtained or prepared separately.

## Directory organization

| Path | Purpose and contents |
| --- | --- |
| `download_ERA5/` | Shell and Python tools to download ERA5-Land runoff from the Copernicus Climate Data Store (CDS), convert it to CaMa-Flood NetCDF or plain binary, and write a configuration summary. See the [detailed ERA5-Land guide](download_ERA5/ReadMe.md). |
|  |


| Package directory | Format and grid/time coverage | Unit evidence and relevant example |
| --- | --- | --- |
| `test_1deg/` | Plain binary (`Roff____YYYYMMDD.one`) plus `runoff.ctl`; global 1-degree grid (360 × 180); daily files from 2000-01-01 through 2001-12-31. | The `.ctl` declares runoff in `mm/day`. `gosh/test1-glb_15min.sh` uses `IFRQ_INP=24` and `DROFUNIT=86400000`. |
| `test_15min_nc/` | NetCDF files for 2000 and 2001; `Runoff(time, lat, lon)` on a 0.25-degree global grid (1440 × 600), daily time coordinate. | The sample NetCDF metadata declares `units="mm day-1"`. `gosh/test2-conus_06min_netcdf.sh` uses `IFRQ_INP=24` and `DROFUNIT=86400000`. |
| `test_jpn_1hr/` | Plain binary daily files (`runoff_YYYYMMDD.bin`) plus `runoff.ctl`; Japan grid at 1/60 degree (1500 × 1320), 1-hour records for 2007-09-03 through 2007-09-09. | The `.ctl` declares runoff in `mm/hr`, matching `gosh/test3-jpn_fcast.sh` (`IFRQ_INP=1`, `DROFUNIT=3600000`). |

These sample files are supplied in the data-rich package, not this source checkout. Obtain the package or prepare equivalent inputs before running scripts that reference these paths. The [sample execution guide](../gosh/README.md) describes the workflows; the package `gosh/` scripts show the specific forcing configuration used by each example. File spacing alone does not establish the physical units or whether values are rates or accumulated depths; see [Choosing `DROFUNIT`](#choosing-drofunit).

## Runoff forcing and units

The runoff field is water supplied per unit area over time. It may be stored as:

- **Accumulated depth per record**, for example millimetres accumulated during a 3-hour record (`mm/3 h`).
- **A depth rate**, for example millimetres per hour (`mm/h`) or metres per second (`m/s`).
- **A mass flux**, for example kilograms per square metre per second (`kg m-2 s-1`), which can be converted to water-depth rate using an assumed water density.

Runoff is not river discharge: discharge is a volume flow rate such as `m3/s`. In the forcing code, CaMa-Flood divides runoff values by `DROFUNIT` to obtain a depth rate in `m/s`, then multiplies by catchment area (or input-matrix area weights) to obtain `m3/s`. The model does not infer the runoff variable's physical units from its NetCDF `units` attribute; set the conversion from the actual data definition.

### Choosing `DROFUNIT`

Use the numerical divisor that converts one input value to `m/s`. For values in millimetres accumulated over each forcing record, convert millimetres to metres and divide by the duration of that record in seconds:

```text
DROFUNIT = 1000 × record_duration_seconds
runoff_m_per_s = input_runoff_value / DROFUNIT
```

For other units, derive the factor from the units themselves. These are illustrative examples, not an exhaustive product reference:

| Input value represents | Example `DROFUNIT` | Result after division |
| --- | ---: | --- |
| `m/s` | 1 | `m/s` |
| `mm/s` | 1,000 | `m/s` |
| `mm/h` rate, or `mm` accumulated over 1 hour | 3,600,000 | `m/s` |
| `mm/day` rate, or `mm` accumulated over 24 hours | 86,400,000 | `m/s` |
| `m/day` rate | 86,400 | `m/s` |
| `mm` accumulated over 3 hours | 10,800,000 | `m/s` |
| `mm` accumulated over 6 hours | 21,600,000 | `m/s` |
| `kg m-2 s-1` mass flux | 1,000 | `m/s`, assuming water density of 1,000 kg/m3 |

The file interval and the unit are separate facts. A daily file may contain an amount accumulated over one day, or a rate expressed per day. A 3-hourly record may contain a 3-hour amount, or a rate in `mm/day`. Those values need different interpretation even if their timestamps are 3 hours apart. Check the variable definition, units, time axis, and accumulation/reset semantics before choosing `DROFUNIT`. `IFRQ_INP` should describe the forcing update interval expected by the run configuration; file grouping (daily versus monthly files) does not by itself define either the runoff unit or time step.

For example, the standard daily `gosh/` samples use `DROFUNIT=86400000` for runoff values in `mm/day` (numerically also the same factor for a 24-hour accumulated depth in mm). The included ERA5-Land converters instead produce each-record accumulated depth in mm and write the time-step-specific factor to `runoff_info_CaMaFlood.txt`.

## Runoff grid and the CaMa-Flood map

The runoff grid and CaMa-Flood river map are separate spatial grids. They can differ in resolution, dimensions, extent, and cell locations. The runoff dataset provides values on its own grid; the CaMa-Flood map defines the river network and unit catchments. An input matrix links runoff cells to CaMa-Flood catchments when their grids do not directly match. If the runoff grid does not cover part of the map area, those catchments may receive incomplete or no runoff coverage.

The `GRSIZEIN`, `WESTIN`, `EASTIN`, `NORTHIN`, `SOUTHIN`, and `OLAT` settings used by the input-matrix generator describe the **runoff input grid**, not the CaMa-Flood map. In the current `generate_inpmat` implementation, spacing and coordinates are in degrees; the west/east/south/north limits and grid spacing determine the input dimensions, and `OLAT` selects north-to-south (`NtoS`) or south-to-north (`StoN`) row order. The generator assigns high-resolution map-pixel centers to input cells and accumulates their area for each map catchment/input-cell pair. It expects a regular latitude/longitude grid and skips map pixels outside the configured input-grid limits.

> **The runoff input grid does not need to have the same resolution or geographical domain as the CaMa-Flood river map. Do not copy the map coordinates into runoff-grid settings unless they also describe the runoff dataset.**

Use the runoff product's coordinate metadata to determine its resolution, limits, longitude convention, and latitude order. Then prepare an input matrix that matches both that runoff grid and the selected map. See the [map overview](../map/README.md) and the `src_param` [input-matrix script](../map/src/src_param/s02-generate_inpmat.sh); the Fortran implementation is [`generate_inpmat.F90`](../map/src/src_param/generate_inpmat.F90). This README does not replace their workflow-specific instructions.

## Available ERA5-Land preparation tools

The current workflow is:

```text
s01-download.sh
    ├── s02-convert_CMF_nc.sh  → CaMa-Flood NetCDF forcing
    └── s03-convert_CMF_bin.sh → plain binary forcing + GrADS .ctl
```

The download script requests ERA5-Land runoff for a selected period, region, and time step. The conversion scripts process those source files, and `make_CMF_runoff_info.py` inspects the converted product and writes a text summary of useful grid and forcing settings. See [`download_ERA5/ReadMe.md`](download_ERA5/ReadMe.md) for account setup, commands, all options, and product-specific notes.

### Select a region, period, and time step

In `download_ERA5/s01-download.sh`, configure `NORTH`, `SOUTH`, `WEST`, and `EAST`, then set the inclusive start and end dates with `SYEAR`, `SMONTH`, `SDAY`, `EYEAR`, `EMONTH`, and `EDAY`. For example, values around Japan are `NORTH=46.0`, `SOUTH=24.0`, `WEST=122.0`, and `EAST=154.0`. These select an ERA5-Land download area, not the CaMa-Flood map bounds. The shell wrapper currently passes `--area-is edge`, so these values are interpreted as bounding edges; the Python downloader also supports center limits, which it expands by half a grid cell when requested.

`TSTEP` sets the runoff data time step in hours. The downloader accepts integer values that divide 24 exactly: 1, 2, 3, 4, 6, 8, 12, or 24 hours. `DOWNLOAD_UNIT` controls whether requests are grouped into daily source files (`ERA5-Land_runoff_YYYYMMDD.nc`) or monthly source files (`ERA5-Land_runoff_YYYYMM.nc`). Monthly grouping reduces the number of files/requests; it does **not** mean monthly-averaged runoff. The dates and `TSTEP` determine the actual period and temporal spacing.

Downloading requires a CDS account, API credentials configured for `cdsapi`, and acceptance of the ERA5-Land licence. The Python download/conversion tools also use packages including NumPy, pandas, xarray, and NetCDF4; see the [ERA5-Land guide](download_ERA5/ReadMe.md) and each script's imports for environment details. That guide contains the account setup and full download instructions.

### Convert to CaMa-Flood forcing

- `s02-convert_CMF_nc.sh` and `convert_CMF_nc.py` create NetCDF runoff files grouped by year or month. The converter treats the source ERA5-Land runoff as accumulated depth in metres, differences the accumulation within each UTC day, converts the increments to millimetres, and records `units="mm"` plus the time-step duration in the NetCDF metadata. Here `mm` means millimetres accumulated over each record's time step, not a monthly total or automatically a rate of `mm/s`.
- `s03-convert_CMF_bin.sh` and `convert_CMF_bin.py` create single-precision plain-binary grids and a GrADS `.ctl` file. Values are likewise millimetres accumulated per output record. The Python converter stores sub-daily records together in daily `runoff_YYYYMMDD.bin` files; the `.ctl` describes the record time step and grid. The current shell wrapper sets output time step equal to its input `TIMESTEP_HOURS`.
- Both paths create `runoff_info_CaMaFlood.txt`. It summarizes grid dimensions/coordinates, time step, file naming and relevant forcing settings, including a suggested `DROFUNIT`. Treat this as a configuration aid and confirm it against the actual forcing files and model run.

The existing ERA5-Land tools convert the source product to millimetres accumulated over each selected time step. For those converted files, examples are:

| Converted record contains | `DROFUNIT` |
| --- | ---: |
| 24-hour accumulation in mm | 86,400,000 |
| 3-hour accumulation in mm | 10,800,000 |
| 1-hour accumulation in mm | 3,600,000 |

The binary metadata tool can infer the interval from the `.ctl` time definition. NetCDF summaries use the converted time metadata and configured time step. Review the generated summary, especially when adjusting conversion settings or using files from a different product.

## Connect forcing to a `gosh/` run

The execution script selects the forcing location, format, time step, units, and spatial mapping. The relevant namelist settings are:

| Setting | Role |
| --- | --- |
| `CROFDIR`, `CROFPRE`, `CROFSUF` | Directory, filename prefix, and suffix used to locate plain-binary forcing files. |
| `LINPCDF` | Selects NetCDF (`.TRUE.`) or plain-binary (`.FALSE.`) runoff input; NetCDF support also depends on the executable having been built with the required support. |
| `CROFCDF`, `CVNROF` | NetCDF file path and runoff variable name. Scripts may construct `CROFCDF` for each run period. |
| `IFRQ_INP` | Forcing update interval in hours in the run configuration. Match the record spacing and simulation setup. |
| `DROFUNIT` | Converts the numeric runoff values to `m/s` before multiplication by area. Set it from the actual input units and temporal meaning. |
| `LINTERP`, `CINPMAT` | Enable and select the input matrix for mapping runoff cells to CaMa-Flood catchments. Without matrix interpolation, the forcing grid must meet the model's direct-resolution assumptions. |

For a concrete binary setup, see [`gosh/test1-glb_15min.sh`](../gosh/test1-glb_15min.sh); for a NetCDF setup, see [`gosh/test2-conus_06min_netcdf.sh`](../gosh/test2-conus_06min_netcdf.sh). The [gosh guide](../gosh/README.md) describes the wider sample-run workflow. Ensure the forcing file pattern, grid, Input Matrix, time interval, and `DROFUNIT` all refer to the same prepared product.

## Troubleshooting

| Symptom or risk | Check |
| --- | --- |
| Discharge is far too large or small | Confirm the input variable's units and whether it is an accumulated depth or a rate; derive `DROFUNIT` from those units and the record's temporal meaning. |
| Forcing records are skipped, repeated, or mis-timed | Compare `IFRQ_INP`, the NetCDF time coordinate or binary `.ctl` `tdef`, and the simulation dates. Daily/monthly file grouping is not the same as record spacing. |
| Some catchments have no or incomplete runoff mapping | Check that the runoff grid covers the intended map area and that the Input Matrix was generated for this exact map and runoff grid. |
| Spatial field is shifted or reversed | Check coordinate limits, longitude convention, grid-cell centers versus bounds, `OLAT`, and the order of latitude rows. |
| Model cannot find the forcing | Check `CROFDIR`, prefix, suffix, dates, and the chosen NetCDF or binary mode. |
| NetCDF run reads the wrong or no runoff variable | Confirm `CROFCDF`, `CVNROF`, and the variable in the actual file; NetCDF input requires a compatible build. |

These checks distinguish configuration mismatches from software defects; a suspicious result alone does not establish a code bug.

## Future development: generic runoff metadata analyzer

**TODO:** Develop a product-independent tool that inspects runoff files and produces a readable CaMa-Flood configuration summary. Where metadata permit, it should identify the grid, dimensions, spacing, bounds, longitude convention, row order, time interval, variable units, and temporal accumulation semantics; suggest `GRSIZEIN`, `WESTIN`, `EASTIN`, `NORTHIN`, `SOUTHIN`, `OLAT`, `IFRQ_INP`, and `DROFUNIT`; and warn when values cannot be inferred reliably. Users should be able to review and copy the summary into their preprocessing and run scripts.

`download_ERA5/make_CMF_runoff_info.py` already generates related information for the ERA5-Land conversion workflow and can inspect the converted NetCDF or binary/`.ctl` formats it supports. Its settings, assumptions, and call sites are part of that workflow; it is not established as a fully generic analyzer for arbitrary runoff products. The TODO is a future development direction only; no analyzer is implemented here.

## Related documentation

- [ERA5-Land download and conversion guide](download_ERA5/ReadMe.md)
- [Map and Input Matrix overview](../map/README.md)
- [Input Matrix generation script](../map/src/src_param/s02-generate_inpmat.sh)
- [Sample execution guide](../gosh/README.md)
- [Forcing implementation](../src/cmf_ctrl_forcing_mod.F90)
