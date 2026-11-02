# Runoff input data and preparation

The `inp/` directory contains tools for preparing runoff forcing used by CaMa-Flood. Runoff supplies water per unit land area over time; CaMa-Flood converts it to a flow rate for routing through the river network. The GitHub repository includes an ERA5-Land download and conversion workflow with small Japan examples. Distribution packages also include sample forcing for selected `gosh/` runs. Obtain or prepare additional forcing when the chosen experiment requires data beyond those supplied with the package.

## Directory organization

| Path | Purpose and contents |
| --- | --- |
| `ERA5-Land_download/` | Shell and Python tools to download ERA5-Land runoff from the Copernicus Climate Data Store (CDS), including a configurable global profile, and convert it to CaMa-Flood NetCDF or plain binary. See the [detailed ERA5-Land guide](ERA5-Land_download/ReadMe.md). |
| `ERA5-Land_download/ERA5land-glb-nc/` | Optional, locally downloaded global ERA5-Land source NetCDF. The two-day format-check sample can be read directly by CaMa-Flood with NetCDF input enabled and a matching input matrix; it is not tracked or distributed. See the ERA5-Land guide and registry entry. |
| `ERA5_land/daily/` | Canonical north-south half-grid geometry (90° to −90° cell centers; 3,600 × 1,801 at 0.1°). Only one local daily-named binary sample (1960-01-01) is currently present; its units, missing-value meaning, longitude order, and full coverage are unconfirmed. |
| `ERA5_land/Roff/` | Legacy 1981–2020 daily binary series, 3,600 × 1,800. The maintainer confirms it omits the north-south half-grid shift and is not the canonical geometry. Neither series is tracked or distributed; see both registry entries. |
| `ELSE_GPCC/` | External long-term global 1-degree daily ELSE_GPCC runoff associated with Kim et al. (2009). The `Roff/` data are not tracked by Git and are not included in the source repository or distribution package; only `Roff.ctl` metadata is tracked. The files are binary float32 runoff in `mm/day`, global 360 × 180, with N-to-S row order and `undef=-999`. The tracked CTL time count conflicts with observed source filenames; see the registry notes. [Kim et al. (2009)](https://doi.org/10.1029/2009GL039006). |
| `test-glb_1deg/` | Reproducible global 1-degree, daily two-year test extraction (2000–2001) from `ELSE_GPCC/Roff/`. Scripts and metadata are tracked; generated `Roff/` binaries are Git-ignored pending redistribution review. See the [sample README](test-glb_1deg/README.md). |
| `test-mek_1deg/` | Complete 1-degree Mekong runoff sample, including 11,323 daily binary files, CTL, extraction scripts, and documentation; tracked in GitHub and included in the distribution package. See the [sample README](test-mek_1deg/README.md). |

## Runoff Dataset Registry

Each row in [runoff_datasets.csv](runoff_datasets.csv) represents a CaMa-Flood-ready runoff product or a source product already stored in CaMa-Flood-compatible forcing format. Raw downloads that still require conversion are documented in the relevant workflow guide but are not registry entries until converted to a model-facing product. The registry records runoff-grid parameters needed for matrix generation, data `record_interval_hours` separately from model `IFRQ_INP`, units needed for `DROFUNIT`, and the format, path pattern, NetCDF variable, provenance, and availability. Empty fields are not established values. Use `configuration_status` and `notes` to distinguish confirmed metadata from unresolved runtime settings.

Use the registry's `dataset_id` as `ROFFTAG` when generating the paired `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin` files. This keeps the product identifier and generated matrix tag consistent; the matrix itself remains specific to its target map.

### Available Runoff Samples

The table below summarizes the sample locations and review state. The CSV is the full reference for product grids, file patterns, units, temporal meaning, model settings, provenance, and availability. Package examples describe v4.30; other releases may include different periods or files, so check actual coverage before selecting simulation dates.

| Package directory | Format and grid/time coverage | Unit evidence and relevant example |
| --- | --- | --- |
| `ELSE_GPCC/Roff/` | External source product: global 360 × 180 1-degree daily binary data, nonempty daily files from 1979-01-01 through 2010-12-31 plus a zero-byte file dated 2011-01-01; `mm/day`, little-endian float32, N-to-S, `undef=-999`. It is associated with Kim et al. (2009). Source files are not in Git or distribution packages. | `Roff.ctl` declares 7,671 records starting 1981-01-01, inconsistent with filename coverage; see registry `notes`. |
| `test-glb_1deg/` | Regenerable global 360 × 180 daily plain-binary sample (`Roff____YYYYMMDD.one`) for 2000-01-01 through 2001-12-31, extracted byte-for-byte from ELSE_GPCC. | `mm/day`, `IFRQ_INP=24`, `DROFUNIT=86400000`; scripts and metadata are tracked, generated data are ignored. See the sample README. |
| `test_1deg/` | Legacy package sample on the same global 1-degree grid and dates. The maintainer identifies it as a two-year extraction from ELSE_GPCC. | The `.ctl` declares runoff in `mm/day`. `gosh/test1-glb_15min.sh` uses `IFRQ_INP=24` and `DROFUNIT=86400000`. |
| `test-mek_1deg/` | Mekong 20 × 30 global-grid subset (90–110°E, 5–35°N); 11,323 daily files for 1980-01-01 through 2010-12-31, generated from `ELSE_GPCC/Roff/`. | Source values remain float32 `mm/day`, `NtoS`, `undef=-999`; use the local README for extraction and model settings. The complete sample is tracked in Git and included in the distribution package. |
| `test-E2O_15min_nc/` | NetCDF files for 2000 and 2001; `Runoff(time, lat, lon)` on a 0.25-degree grid (1440 × 600; edges 180°W–180°E, 60°S–90°N), daily time coordinate. | The sample NetCDF metadata declares `units="mm day-1"`. `gosh/test2-conus_06min_netcdf.sh` uses `IFRQ_INP=24` and `DROFUNIT=86400000`. |
| `test_jpn_1hr/` | Original/source reference: daily binary files (`runoff_YYYYMMDD.bin`) plus `runoff.ctl`; Japan 1/60-degree grid (1500 × 1320), 1-hour records for 2007-09-03 through 2007-09-09. Locally available through an ignored symlink; large binaries are not tracked. | Maintainer-confirmed provenance: 1-arcmin simulations using the Today’s Japan system, provided by Yoshimura Laboratory. The maintainer identifies the original CTL `mm/day` label as erroneous and assesses the values as `mm/hour`, consistent with `gosh/test3-jpn_fcast.sh` (`IFRQ_INP=1`, `DROFUNIT=3600000`); this interpretation is not independently verified. |
| `test-jpn_6min_1hr/` | Compact derived sample: seven daily binary files, 250 × 220 at 0.1° (6 arcmin), 24 hourly records per day for 2007-09-03 through 2007-09-09. Produced by 6 × 6 area-weighted aggregation from `test_jpn_1hr/`. | Maintainer confirms redistribution permission. Values and hourly ordering are retained without unit conversion. The new CTL uses `mm/hour` per the maintainer’s assessment that the original `mm/day` label was erroneous; this interpretation is not independently verified. See the local README for reproduction and validation. |
| `ERA5-Land_download/ERA5land-jpn_nc/` | Converted monthly CaMa-Flood NetCDF sample with variable `runoff`. | Tracked in the repository; time labels follow ERA5-Land valid-time semantics. First-record attribution and period-boundary coverage are not independently validated (KG-0019). |
| `ERA5-Land_download/ERA5land-jpn_bin/` | CTL and configuration report for converted daily-file binary output. | The maintainer reports recalculating 31 daily files with one grid per file; this matches the 31 daily NetCDF timestamps and the 24-hour CTL. Binary records remain untracked in Git. |
| `ERA5_land/daily/` | Canonical north-south geometry. The only observed local file, `ERA5-Land_Runoff_daily_19600101.bin`, is headerless float32 with 3,600 × 1,801 values (25,934,400 bytes). | Maintainer confirms the half-grid shift: input-grid limits 90.05 to −90.05 produce centers 90 to −90 under the map-tool convention. Units, missing-value meaning, longitude order, provenance, and long-term coverage are unconfirmed; not ready for a configured simulation. |
| `ERA5_land/Roff/` | Legacy 1981–2020 global daily binary series, 3,600 × 1,800 values per date. | The maintainer confirms the north-south half-grid shift is absent, so this is not the canonical daily geometry. Existing legacy input settings use 90/−90 limits (centers 89.95…−89.95). Metadata and scientific validity remain unverified; see `runoff_datasets.csv`. |

### Data Sources and Acquisition

Package-only data are not added by cloning the GitHub repository. Obtain the v4.30 distribution package or prepare equivalent inputs separately, then check the actual files against the selected run script. For source and acquisition details, see the [Copernicus ERA5-Land dataset page](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land) and the [ERA5-Land workflow guide](ERA5-Land_download/ReadMe.md). The [sample execution guide](../gosh/README.md) describes the package runs. File spacing alone does not establish physical units or whether values are rates or accumulated depths; see [Choosing `DROFUNIT`](#choosing-drofunit).

### Configuring Input Matrix Generation

Copy the runoff-grid fields from the selected CSV row into [s03-generate_inpmat.sh](../map/tools/script_params/s03-generate_inpmat.sh):

```sh
ROFFTAG="test-E2O_15min_nc"
GRSIZEIN=0.25
WESTIN=-180.0
EASTIN=180.0
NORTHIN=90.0
SOUTHIN=-60.0
OLAT="NtoS"
```

These example settings describe the `test-E2O_15min_nc` package product's 0.25-degree NetCDF grid. The bounds are cell edges; the generator computes dimensions from `(EASTIN-WESTIN)/GRSIZEIN` and `(NORTHIN-SOUTHIN)/GRSIZEIN`. `OLAT` must match the runoff array's stored row order. Edit the map-local wrapper settings for the selected product and run it from the target map's `script_params/` directory. It writes `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin` in that map root. Do not use CaMa-Flood map geometry as runoff-grid geometry or generate a simulation matrix from a source-only product before conversion.

Validate the CSV structure and grid arithmetic with the standard-library-only checker:

```sh
python3 inp/validate_runoff_datasets.py
```

The NetCDF headers, CTLs, and packaged data in this inventory were also checked manually. The checker does not require package data, CDS access, or NetCDF Python packages.

### Configuring CaMa-Flood Simulation

Transfer the CSV values into a `gosh/` run configuration, checking the actual file and `configuration_status` first:

| Registry field | CaMa-Flood setting | Meaning |
| --- | --- | --- |
| `record_interval_hours` | `IFRQ_INP` | Expected forcing record interval in hours; confirm it matches the run setup. |
| `DROFUNIT` | `DROFUNIT` | Divisor converting the stored value to `m/s`; derive it from units and record meaning, not file grouping alone. |
| `format` | `LINPCDF` | `binary` uses `.FALSE.`; `NetCDF` uses `.TRUE.`. |
| `LINPDAY` | `LINPDAY` | Selects daily-file mode. The model sets this for binary input; NetCDF can use daily files or one `CROFCDF` covering the simulation period. |
| `directory`, `prefix`, `suffix` | `CROFDIR`, `CROFPRE`, `CROFSUF` | Binary (and daily-file NetCDF) location and date-pattern components. |
| `filename_pattern` | Run-script path construction | Shows where dates occur; yearly/monthly NetCDF files need the script to construct `CROFCDF` for the relevant period. |
| `directory`, `filename_pattern` | `CROFCDF` | NetCDF file path. In single-file mode, set the actual file path; the model does not infer yearly or monthly names from `CROFPRE`/`CROFSUF`. |
| `nc_variable` | `CVNROF` | NetCDF runoff variable name. |

`IFRQ_INP` is the time between forcing records, not the number of files or their grouping. A yearly NetCDF can contain daily records and use `IFRQ_INP=24`. A row marked `needs_maintainer_review` does not provide a complete simulation configuration; do not fill unresolved units or divisors by assumption.

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

## ERA5-Land source and conversion workflow

The repository includes ERA5-Land-specific tools to retrieve runoff from CDS and convert it to CaMa-Flood NetCDF or plain-binary forcing. `ERA5-Land_download/t01-download-global.sh` downloads global monthly NetCDF; `ERA5-Land_download/t03-convert-global-2day-bin.sh` demonstrates binary conversion of the local two-day sample. The [ERA5-Land guide](ERA5-Land_download/ReadMe.md) documents the settings, commands, formats, dependencies, time-semantics caveat, and verification limits. The separately linked `ERA5_land/Roff/` lab series is an existing external product, not an output regenerated by these scripts. The term ERA5-Land is used deliberately; standard ERA5 atmospheric reanalysis is a separate product.

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

`ERA5-Land_download/make_CMF_runoff_info.py` already generates related information for the ERA5-Land conversion workflow and can inspect the converted NetCDF or binary/`.ctl` formats it supports. Its settings, assumptions, and call sites are part of that workflow; it is not established as a fully generic analyzer for arbitrary runoff products. The TODO is a future development direction only; no analyzer is implemented here.

## Related documentation

- [ERA5-Land download and conversion guide](ERA5-Land_download/ReadMe.md)
- [Map and Input Matrix overview](../map/README.md)
- [Input Matrix generation script](../map/src/src_param/s02-generate_inpmat.sh)
- [Sample execution guide](../gosh/README.md)
- [Forcing implementation](../src/cmf_ctrl_forcing_mod.F90)
