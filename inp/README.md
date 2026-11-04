# Runoff input data and preparation

The `inp/` directory contains tools for preparing runoff forcing used by CaMa-Flood. Runoff supplies water per unit land area over time; CaMa-Flood converts it to a flow rate for routing through the river network. The GitHub repository includes an ERA5-Land download and conversion workflow with small Japan examples. Distribution packages also include sample forcing for selected `gosh/` runs. Obtain or prepare additional forcing when the chosen experiment requires data beyond those supplied with the package.

## Using Your Own Runoff Dataset

1. Prepare runoff files in a supported [Binary or NetCDF layout](#supported-input-formats-and-simulation-settings).
2. Identify the input-grid resolution and geographic boundaries, latitude row order, temporal resolution, runoff units and accumulation meaning, missing-value encoding, and filename pattern from the source data and documentation.
3. Copy the shared [`s03-generate_inpmat.sh` template](../map/tools/script_params/s03-generate_inpmat.sh) into the target map's `script_params/`, configure its settings, and [generate the Input Matrix](#preparing-the-input-matrix). The runoff grid may have a different spatial resolution and extent from the CaMa-Flood river map.
4. Configure the relevant `gosh/` simulation script with matching file paths and format, `IFRQ_INP`, `DROFUNIT`, and the matching `CDIMINFO`/`CINPMAT` pair; see [simulation settings](#supported-input-formats-and-simulation-settings).
5. Run a short simulation and check the log, dates, and runoff/discharge values for correct loading and reasonable magnitudes.
6. Optionally document the dataset and its provenance in [`runoff_datasets.csv`](runoff_datasets.csv) when sharing it with other users.

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
| `test-mek_1deg_nc/` | Derived 2000–2001 Mekong annual NetCDF sample (`runoff2000.nc`, `runoff2001.nc`), converted from the same daily source files as `test-mek_1deg/`. See the [dataset README](test-mek_1deg_nc/README.md) and registry entry. |

## Runoff Dataset Registry

[`runoff_datasets.csv`](runoff_datasets.csv) is a human-readable metadata registry of runoff datasets that can be used with CaMa-Flood. The model does not read this CSV at runtime, and registration is optional: users can run their own runoff data without modifying it. They must still identify the dataset's spatial grid, temporal interval and meaning, units, missing-value convention, and file format, then configure those values correctly in the map-preprocessing and simulation scripts.

An entry is recommended when sharing a dataset with collaborators or other users. It records the configuration needed to reproduce the setup, along with file patterns, provenance, and access or distribution information. Empty fields are not established values; check `configuration_status` and `notes` before reusing an entry. For usability and maintenance, use the naming convention `dataset_id = ROFFTAG = runoff dataset directory name`. This is a human-facing convention, not a technical requirement imposed by the model. The generated Input Matrix remains specific to its target map even when two maps use the same `ROFFTAG`.

### Available Runoff Samples

The table below summarizes the sample locations and review state. The CSV is the full reference for product grids, file patterns, units, temporal meaning, model settings, provenance, and availability. Package examples describe v4.30; other releases may include different periods or files, so check actual coverage before selecting simulation dates.

| Package directory | Format and grid/time coverage | Unit evidence and relevant example |
| --- | --- | --- |
| `ELSE_GPCC/Roff/` | External source product: global 360 × 180 1-degree daily binary data, nonempty daily files from 1979-01-01 through 2010-12-31 plus a zero-byte file dated 2011-01-01; `mm/day`, little-endian float32, N-to-S, `undef=-999`. It is associated with Kim et al. (2009). Source files are not in Git or distribution packages. | `Roff.ctl` declares 7,671 records starting 1981-01-01, inconsistent with filename coverage; see registry `notes`. |
| `test-glb_1deg/` | Regenerable global 360 × 180 daily plain-binary sample (`Roff____YYYYMMDD.one`) for 2000-01-01 through 2001-12-31, extracted byte-for-byte from ELSE_GPCC. | `mm/day`, `IFRQ_INP=24`, `DROFUNIT=86400000`; scripts and metadata are tracked, generated data are ignored. See the sample README. |
| `test_1deg/` | Legacy package sample on the same global 1-degree grid and dates. The maintainer identifies it as a two-year extraction from ELSE_GPCC. | The `.ctl` declares runoff in `mm/day`. `gosh/test1-glb_15min.sh` uses `IFRQ_INP=24` and `DROFUNIT=86400000`. |
| `test-mek_1deg/` | Mekong 20 × 30 global-grid subset (90–110°E, 5–35°N); 11,323 daily files for 1980-01-01 through 2010-12-31, generated from `ELSE_GPCC/Roff/`. | Source values remain float32 `mm/day`, `NtoS`, `undef=-999`; use the local README for extraction and model settings. The complete sample is tracked in Git and included in the distribution package. |
| `test-mek_1deg_nc/` | Derived NetCDF version of the Mekong 1-degree sample, split into annual files for 2000 (366 days) and 2001 (365 days). Each file contains `runoff(time, latitude, longitude)` on the same 20 × 30 grid, with CF daily time bounds. | Float32 `mm day-1`, longitude centers 90.5–109.5°E, descending latitude centers 34.5–5.5°N, missing value `-999`. The converter verifies exact float32 round-trip against the corresponding source binary files. GitHub sample tracking is intended; inclusion in a future v5 downloadable package remains to be decided because the v5 packaging workflow is not established. |
| `test-E2O_15min_nc/` | NetCDF files for 2000 and 2001; `Runoff(time, lat, lon)` on a 0.25-degree grid (1440 × 600; edges 180°W–180°E, 60°S–90°N), daily time coordinate. | The sample NetCDF metadata declares `units="mm day-1"`. `gosh/test2-conus_06min_netcdf.sh` uses `IFRQ_INP=24` and `DROFUNIT=86400000`. |
| `test_jpn_1hr/` | Original/source reference: daily binary files (`runoff_YYYYMMDD.bin`) plus `runoff.ctl`; Japan 1/60-degree grid (1500 × 1320), 1-hour records for 2007-09-03 through 2007-09-09. Locally available through an ignored symlink; large binaries are not tracked. | Maintainer-confirmed provenance: 1-arcmin simulations using the Today’s Japan system, provided by Yoshimura Laboratory. The maintainer identifies the original CTL `mm/day` label as erroneous and assesses the values as `mm/hour`, consistent with `gosh/test3-jpn_fcast.sh` (`IFRQ_INP=1`, `DROFUNIT=3600000`); this interpretation is not independently verified. |
| `test-jpn_6min_1hr/` | Compact derived sample: seven daily binary files, 250 × 220 at 0.1° (6 arcmin), 24 hourly records per day for 2007-09-03 through 2007-09-09. Produced by 6 × 6 area-weighted aggregation from `test_jpn_1hr/`. | Maintainer confirms redistribution permission. Values and hourly ordering are retained without unit conversion. The new CTL uses `mm/hour` per the maintainer’s assessment that the original `mm/day` label was erroneous; this interpretation is not independently verified. See the local README for reproduction and validation. |
| `ERA5-Land_download/ERA5land-jpn_nc/` | Converted monthly CaMa-Flood NetCDF sample with variable `runoff`. | Tracked in the repository; time labels follow ERA5-Land valid-time semantics. First-record attribution and period-boundary coverage are not independently validated (KG-0019). |
| `ERA5-Land_download/ERA5land-jpn_bin/` | CTL and configuration report for converted daily-file binary output. | The maintainer reports recalculating 31 daily files with one grid per file; this matches the 31 daily NetCDF timestamps and the 24-hour CTL. Binary records remain untracked in Git. |
| `ERA5_land/daily/` | Canonical north-south geometry. The only observed local file, `ERA5-Land_Runoff_daily_19600101.bin`, is headerless float32 with 3,600 × 1,801 values (25,934,400 bytes). | Maintainer confirms the half-grid shift: input-grid limits 90.05 to −90.05 produce centers 90 to −90 under the map-tool convention. Units, missing-value meaning, longitude order, provenance, and long-term coverage are unconfirmed; not ready for a configured simulation. |
| `ERA5_land/Roff/` | Legacy 1981–2020 global daily binary series, 3,600 × 1,800 values per date. | The maintainer confirms the north-south half-grid shift is absent, so this is not the canonical daily geometry. Existing legacy input settings use 90/−90 limits (centers 89.95…−89.95). Metadata and scientific validity remain unverified; see `runoff_datasets.csv`. |

### Data Sources and Acquisition

Package-only data are not added by cloning the GitHub repository. Obtain the v4.30 distribution package or prepare equivalent inputs separately, then check the actual files against the selected run script. For source and acquisition details, see the [Copernicus ERA5-Land dataset page](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land) and the [ERA5-Land workflow guide](ERA5-Land_download/ReadMe.md). The [sample execution guide](../gosh/README.md) describes the package runs. File spacing alone does not establish physical units or whether values are rates or accumulated depths; see [Choosing `DROFUNIT`](#choosing-drofunit).

## Preparing the Input Matrix

The runoff input grid and CaMa-Flood river map are separate grids. They may have different resolution, dimensions, and geographic extent. Use the runoff product's own regular latitude/longitude grid geometry to configure [the map-local `s03-generate_inpmat.sh` workflow](../map/tools/script_params/s03-generate_inpmat.sh). The workflow maps high-resolution map-pixel centers into runoff cells and accumulates overlap area for each map catchment. If the runoff grid does not cover part of the map, some catchments may have incomplete or no runoff coverage.

Configure the runoff-grid settings in the copied script, for example:

```sh
ROFFTAG="test-E2O_15min_nc"
GRSIZEIN=0.25
WESTIN=-180.0
EASTIN=180.0
NORTHIN=90.0
SOUTHIN=-60.0
OLAT="NtoS"
```

The example describes the `test-E2O_15min_nc` 0.25-degree grid. `GRSIZEIN` is the runoff-cell spacing in degrees; `WESTIN`, `EASTIN`, `NORTHIN`, and `SOUTHIN` are the grid edges. The generator derives dimensions from the edge spans and spacing, and treats cell centers as half a cell inward from each edge. `OLAT` must match stored row order: `NtoS` or `StoN`. It does not read runoff values or validate source coordinates. Generate the matrix only after the product has been converted to the same grid and ordering it will use at runtime.

Copy the workflow into the target map's `script_params/` directory, edit the settings, then run it there (or invoke the copied script by path). It writes `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin` in the map root. Set `CINPMAT` in the simulation's `NFORCE` group to the map-root path for `inpmat_<ROFFTAG>.bin`. Keep the dataset's actual coordinates and longitude convention consistent between the forcing file and matrix; the model does not compare NetCDF coordinate values against the matrix.

The generated pair is linked: set `CDIMINFO` in `NDIMTIME` to `inpdim_<ROFFTAG>.txt` and `CINPMAT` in `NFORCE` to `inpmat_<ROFFTAG>.bin`. The descriptor supplies the map/input dimensions and geographic bounds used with the matrix. Both files are map-specific and must be regenerated for another target map. See the [Map Tools guide](../map/tools/README.md) and [parameter workflow guide](../map/tools/script_params/README.md) for build and map-local setup details. If you optionally add a registry row, check its CSV structure and grid arithmetic with the standard-library-only checker:

```sh
python3 inp/validate_runoff_datasets.py
```

The checker does not require package data, CDS access, or NetCDF Python packages.

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

See the [map overview](../map/README.md) and [Map Tools parameter workflow guide](../map/tools/script_params/README.md) for map-specific requirements. The historical v4 programs under `map/src/` are separate from the v5 workflow described here.

## ERA5-Land source and conversion workflow

The repository includes ERA5-Land-specific tools to retrieve runoff from CDS and convert it to CaMa-Flood NetCDF or plain-binary forcing. `ERA5-Land_download/t01-download-global.sh` downloads global monthly NetCDF; `ERA5-Land_download/t03-convert-global-2day-bin.sh` demonstrates binary conversion of the local two-day sample. The [ERA5-Land guide](ERA5-Land_download/ReadMe.md) documents the settings, commands, formats, dependencies, time-semantics caveat, and verification limits. The separately linked `ERA5_land/Roff/` lab series is an existing external product, not an output regenerated by these scripts. The term ERA5-Land is used deliberately; standard ERA5 atmospheric reanalysis is a separate product.

## Supported Input Formats and Simulation Settings

CaMa-Flood reads runoff from plain Binary or NetCDF. File format, file grouping, record interval, and runoff units are separate settings:

- `LINPCDF` selects Binary (`.FALSE.`) or NetCDF (`.TRUE.`).
- `CROFFREQ` selects how time records are grouped into files. `DAY`, `MON`, and `YEAR` are the recommended modes; `SEQ` is retained for legacy or advanced cases.
- `IFRQ_INP` sets the interval between forcing records in hours; the model derives `DTIN = IFRQ_INP × 3600` seconds.
- `DROFUNIT` converts stored runoff values to a depth rate in `m/s`. It does not select a file or interval.

### File grouping and names

| `CROFFREQ` | Records in each file begin at | Date inserted between `CROFPRE` and `CROFSUF` | Example |
| --- | --- | --- | --- |
| `DAY` | 00:00 of the calendar day | `YYYYMMDD` | `runoff_20000101.bin` |
| `MON` | 00:00 on the first day of the month | `YYYYMM` | `runoff_200001.nc` |
| `YEAR` | 00:00 on January 1 | `YYYY` | `runoff_2000.nc` |
| `SEQ` | The explicitly configured or CF-resolved start time | none | `runoff.bin` or `CROFCDF` |

For Binary `SEQ`, the fixed filename is `CROFDIR/CROFPRE+CROFSUF`; configure all of `SYEARIN`, `SMONIN`, `SDAYIN`, and `SHOURIN` to identify its first record. The model validates that the simulation fits within the file's complete Float32 records. It never infers the start date from the simulation start or filename. NetCDF `SEQ` continues to use `CROFCDF`.

For calendar-grouped modes, each file's record index is calculated from its calendar-period start. The initial implementation requires `IFRQ_INP` to divide 24 hours exactly; unsupported intervals are rejected. Starting partway through a day, month, or year selects the corresponding within-file record. Record times must align exactly to `DTIN`; the reader does not round an off-grid time. The file choice uses normalized model time (`KMIN` converted by `MIN2DATE`), so permissively accepted inputs such as `24:00` resolve to the next canonical date for filename selection. `DATE2MIN` itself is unchanged.

`CROFFREQ` is the new setting. If omitted, the model preserves legacy behavior: Binary uses daily files regardless of `LINPDAY`; NetCDF uses daily files when `LINPDAY=.TRUE.` and one sequence file when `.FALSE.`. `LINPDAY` remains accepted for old Namelists. When both settings are explicitly supplied, `.TRUE.` agrees only with `CROFFREQ="DAY"`, and `.FALSE.` agrees only with `CROFFREQ="SEQ"`; leave `LINPDAY` out when explicitly selecting `MON` or `YEAR`.

### Plain Binary requirements

Each Binary record is headerless direct-access data containing one `NXIN × NYIN` field of 4-byte floating-point values. Longitude/x varies fastest. Latitude row order must match the `OLAT` used to generate the Input Matrix. `LINPEND=.FALSE.` reads native byte order; set it `.TRUE.` when the file uses the opposite byte order. The reader does not use CTL metadata to infer dimensions, coordinates, units, record count, or missing values.

For `DAY`, `MON`, and `YEAR`, the date-tagged filename follows the table above and records are indexed from the model calendar; `SYEARIN` fields do not override these Binary period boundaries. For `SEQ`, each record represents one `DTIN` interval in the fixed file, and all four `SYEARIN`, `SMONIN`, `SDAYIN`, and `SHOURIN` values define its first record. Binary has no calendar metadata, so the user must make its record dates consistent with `LLEAPYR`: Gregorian leap days when `.TRUE.`, or 365-day years when `.FALSE.`. The reader reuses each active file and closes it when changing periods or when forcing is finalized. With `LROSPLIT`, total and subsurface runoff remain separate Binary files with matching grouping and record indices, using `CROF*` and `CSUB*` settings respectively.

### NetCDF requirements and time validation

Build with NetCDF support (`UseCDF_CMF`) and set `LINPCDF=.TRUE.`. The selected numeric runoff variable must be three-dimensional with Fortran-interface shape `(NXIN, NYIN, time)`. Supplied examples use NetCDF dimension order `(time, latitude, longitude)`, with x/longitude varying fastest in each field; arbitrary dimension orders are not guaranteed. `CVNROF` selects runoff, and `CVNSUB` selects the optional subsurface variable when `LROSPLIT` is enabled. The model does not use runoff `units`, coordinate values, or `_FillValue` to convert/check the field against the Input Matrix.

For `DAY`, `MON`, or `YEAR` with an explicit `CROFFREQ`, the reader selects the date-tagged file and caches it until a period switch. With `SYEARIN<=0`, each opened file must provide `CVNTIME` both as the time dimension and as a same-named one-dimensional CF coordinate variable. The existing resolver checks the requested timestamp, time interval, calendar, increasing/evenly spaced records, coverage, supported units, and optional contiguous time bounds. Supported CF units are seconds, minutes, hours, or days `since` a reference date; supported calendars are `standard`, `gregorian`, `proleptic_gregorian`, `365_day`, and `noleap`, and must agree with `LLEAPYR`. With `SYEARIN>0`, the explicit timestamp identifies the first forcing period and must be at the corresponding period boundary (midnight for `DAY`, day 1 at midnight for `MON`, January 1 at midnight for `YEAR`); record positions are still counted from that file-period boundary. For backward compatibility, NetCDF daily mode selected only through legacy `LINPDAY` retains its old within-day indexing and does not validate each file's CF time coordinate.

For NetCDF `SEQ`, set `CROFCDF` to the file containing the series. With `SYEARIN<=0`, `CVNTIME` names the dimension and coordinate variable used to resolve the simulation start from CF metadata. With `SYEARIN>0`, `SYEARIN`, `SMONIN`, `SDAYIN`, and `SHOURIN` specify the explicit first-record time and the existing elapsed-time record indexing is used. NetCDF `SEQ` keeps one file open for the run. NetCDF grouped files are opened and validated when first needed, cached while active, then closed on a file switch or at finalization.

`LWEVAP` behavior is not extended by this refactor. `CMF_FORCING_PUT` expects a third input-buffer field when it is enabled, while the current runoff reader only loads runoff and optional subsurface runoff; do not assume these runoff formats supply evaporation through this path. Verify the relevant coupled setup before enabling it.

### Runtime settings and examples

| Setting | Group | Role |
| --- | --- | --- |
| `CDIMINFO` | `NDIMTIME` | Path to `inpdim_<ROFFTAG>.txt`, the map and input-grid dimensions/bounds for the matrix. |
| `IFRQ_INP` | `NDIMTIME` | Runoff record interval in hours; independent of file grouping and units. |
| `LINPCDF` | `NFORCE` | `.FALSE.` selects Binary; `.TRUE.` selects NetCDF and requires a NetCDF-enabled executable. |
| `CROFFREQ` | `NFORCE` | `DAY`, `MON`, `YEAR`, or `SEQ`; when omitted, use the legacy `LINPDAY` behavior. |
| `LINPDAY` | `NFORCE` | Legacy selector: `.TRUE.` means `DAY`; `.FALSE.` means `SEQ` for NetCDF. Binary's legacy default remains `DAY`. |
| `DROFUNIT` | `NFORCE` | Divisor converting stored runoff to `m/s`; derive it from units and temporal meaning. |
| `CROFDIR`, `CROFPRE`, `CROFSUF` | `NFORCE` | Directory, prefix, and suffix for date-grouped Binary/NetCDF files; the grouping tag is inserted between prefix and suffix. Binary `SEQ` uses the same components without a tag. |
| `CROFCDF` | `NFORCE` | Single NetCDF file used by `SEQ`. |
| `CVNROF`, `CVNSUB` | `NFORCE` | NetCDF variable names for runoff and optional subsurface runoff. |
| `CVNTIME` | `NFORCE` | NetCDF time dimension name; auto-CF mode also requires a same-named coordinate variable. |
| `SYEARIN`, `SMONIN`, `SDAYIN`, `SHOURIN` | `NFORCE` | Explicit first-record time. Required for Binary `SEQ`; for NetCDF `SEQ`, use `SYEARIN<=0` to use CF time. Grouped NetCDF explicit dates must match their first period boundary. |
| `LINPEND` | `NFORCE` | Binary byte-order conversion; false means native order. |
| `LINTERP`, `CINPMAT` | `NFORCE` | Enable Input Matrix mapping and select `inpmat_<ROFFTAG>.bin`. |

`ROFFTAG` is a preprocessing-script setting used in matrix filenames; it is not a model Namelist parameter. For example, the recommended annual NetCDF configuration is:

```text
&NDIMTIME
  CDIMINFO = "/data/map/inpdim_my-runoff.txt"
  IFRQ_INP = 24
/
&NFORCE
  LINPCDF  = .TRUE.
  CROFFREQ = "YEAR"
  DROFUNIT = 86400000       ! mm per 24-hour record -> m/s
  CROFDIR  = "/data/my-runoff"
  CROFPRE  = "runoff"
  CROFSUF  = ".nc"
  CVNROF   = "runoff"
  CVNTIME  = "time"
  SYEARIN  = 0              ! resolve records using each annual file's CF time axis
  LINTERP  = .TRUE.
  CINPMAT  = "/data/map/inpmat_my-runoff.bin"
/
```

The legacy daily Binary convention remains valid:

```text
&NFORCE
  LINPCDF  = .FALSE.
  CROFFREQ = "DAY"
  LINPEND  = .FALSE.
  DROFUNIT = 86400000       ! mm per 24-hour record -> m/s
  CROFDIR  = "/data/my-runoff"
  CROFPRE  = "Roff____"
  CROFSUF  = ".one"
  LINTERP  = .TRUE.
  CINPMAT  = "/data/map/inpmat_my-runoff.bin"
/
```

If `CROFFREQ` is omitted, existing legacy Namelists remain supported. An explicitly stated `LINPDAY` must not contradict an explicitly stated `CROFFREQ`. For any format, compare the requested first/last dates and sample records with the source data, and check plausible runoff/discharge ranges.

The model converts NaN values to its internal `RMIS` sentinel (default `1.0e20`) and clamps negative runoff to zero. It does not generally interpret a CTL `undef` or NetCDF `_FillValue` as missing data. Convert missing source values explicitly to configured `RMIS` or to zero when zero runoff is intended; do not leave positive fill values such as `1.0e20` unless that value matches `RMIS`.

The temporal label and accumulation/reset interpretation must be checked against the dataset documentation. File grouping does not establish whether values are instantaneous rates or accumulated depths. For unit conversion, see [Runoff forcing and units](#runoff-forcing-and-units).

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
- [v5 Input Matrix generation workflow](../map/tools/script_params/s03-generate_inpmat.sh)
- [Sample execution guide](../gosh/README.md)
- [Forcing implementation](../src/cmf_ctrl_forcing_mod.F90)
