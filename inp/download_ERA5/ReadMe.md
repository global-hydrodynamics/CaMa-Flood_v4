# ERA5-Land runoff download and conversion tools

This directory contains a workflow for downloading **ERA5-Land runoff** from the Copernicus Climate Data Store (CDS), converting the accumulated source values to CaMa-Flood forcing files, and writing a configuration summary. It is not a general ERA5 atmospheric-data downloader.

The raw January 2000 Japan NetCDF under `ori_japan/` is a source example. It is not a CaMa-Flood-ready runoff product. The `runoff_japan_nc/` NetCDF is a converted product. The binary directory contains a tracked CTL and report, but its binary records are ignored by Git; see [the sample consistency note](#tracked-japan-example-and-validation-status).

## 1. Tools and files

Run the shell wrappers from this directory. They use paths relative to the current working directory.

| File | Role | Main inputs | Outputs |
| --- | --- | --- | --- |
| `s01-download.sh` | User-edited settings and daily/monthly request launcher | Period, region, timestep, CDS account | Raw NetCDF files in `OUTDIR`; logs/status files in `log_download/` |
| `download_era5land.py` | Builds and submits a CDS request; can print it with `--dry-run` | Request options; `cdsapi` credentials for a real request | One NetCDF file per requested day or month; writes through a temporary `.part` file before replacement |
| `s02-convert_CMF_nc.sh` | User-edited settings for NetCDF conversion | Raw NetCDF files and period | `runoff_YYYY.nc` or `runoff_YYYYMM.nc`, plus `runoff_info_CaMaFlood.txt` |
| `convert_CMF_nc.py` | Converts accumulated values and writes CaMa-Flood-oriented NetCDF | ERA5-Land NetCDF | Float32 runoff variable, time and coordinates, fill value `1.0e20` |
| `s03-convert_CMF_bin.sh` | User-edited settings for binary conversion | Raw NetCDF files and period | Daily binary files `runoff_YYYYMMDD.bin`, `runoff.ctl`, and `runoff_info_CaMaFlood.txt` |
| `convert_CMF_bin.py` | Converts values, writes binary grids, and writes their CTL | ERA5-Land NetCDF | Float32 binary records, little- or big-endian; subdaily records share a daily file |
| `make_CMF_runoff_info.py` | Reads converted NetCDF metadata or binary CTL metadata | Converted output and its settings | Human-readable grid, interval, and namelist summary; it does not generate an Input Matrix |
| `tests/test_era5land_tools.py` | Offline compatibility tests using small synthetic fields | Python test dependencies only | No persistent data; all output fixtures use temporary directories |

The root [`requirements.txt`](../../requirements.txt) lists the project Python dependencies, including pandas, xarray, and NetCDF4 for conversion. Run [`s00-python_setup.sh`](../../s00-python_setup.sh) from the repository root to create `.venv`, install those requirements, and verify the conversion imports. Downloading additionally requires `cdsapi` and CDS credentials. A direct pip installation is also possible:

```bash
python3 -m pip install numpy pandas xarray netCDF4 'cdsapi>=0.7.7'
```

Configure the current CDS personal access token in `~/.cdsapirc` using the two lines provided on the [CDS API setup page](https://cds.climate.copernicus.eu/how-to-api). Accept the dataset terms on the [ERA5-Land dataset page](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land) before requesting data. Do not put credentials in this repository.

## 2. Configure and download source data

Edit the user settings near the top of `s01-download.sh`:

```bash
PYTHON_CMD="python3"
NCPUS=1
DOWNLOAD_UNIT="monthly"    # daily or monthly request/file grouping
OUTDIR="./ori_japan"       # raw source files; this is not model-ready forcing
TSTEP=24                    # timestep in hours; an integer divisor of 24
SYEAR=2000; SMONTH=1; SDAY=1
EYEAR=2000; EMONTH=1; EDAY=31
NORTH=46.0; SOUTH=24.0; WEST=122.0; EAST=154.0
VARIABLE="runoff"
SKIP_EXISTING=1
EXTRA_OPTS="--area-is edge"
```

Set the period endpoints inclusively. `DOWNLOAD_UNIT` groups requests and source filenames; `monthly` does not request a monthly average. The Python downloader accepts timestep hours that divide 24 exactly (1, 2, 3, 4, 6, 8, 12, or 24). It requests times beginning at `00:00` and stepping through the day, so `TSTEP=24` requests `00:00` only. The shell example sets `--area-is edge`, which passes the four configured limits to the CDS request as `[north, west, south, east]`. The Python option `--area-is center` instead expands the supplied center limits by half of `--grid` (default 0.1 degrees) before reordering them. Whether the CDS includes an edge grid cell is not independently checked by these scripts.

The downloader uses dataset identifier `reanalysis-era5-land`, requests the configured variable, day list, time list, area, NetCDF format, and unarchived download format. Daily names are `ERA5-Land_runoff_YYYYMMDD.nc`; monthly-grouped names are `ERA5-Land_runoff_YYYYMM.nc`. A requested partial month is still stored in the monthly-named file, with only the selected dates requested. Existing outputs are skipped by the shell wrapper by default. Set `SKIP_EXISTING=0` to pass `--overwrite`; failed downloads are retried three times by default and incomplete temporary files are removed before retry.

Inspect the generated request without contacting CDS:

```bash
python3 download_era5land.py --year 2000 --month 1 \
  --start-day 1 --end-day 31 --file-unit monthly --time-step 24 \
  --area-nswe 46 24 122 154 --area-is edge \
  --out ./ori_japan/ERA5-Land_runoff_200001.nc --dry-run
```

Then run the configured wrapper when credentials and licence acceptance are ready:

```bash
./s01-download.sh
```

The scripts do not validate the scientific completeness of the downloaded time series or the exact cell inclusion at the selected area boundary. Review the CDS request and returned NetCDF coordinates.

## 3. Conversion behavior and configuration

Both converters assume the selected source variable contains ERA5-Land runoff accumulated in metres. They normalize common coordinate names, sort timestamps, group records by UTC calendar date, difference accumulated values within each date, and multiply increments by 1000 to produce millimetres. The resulting `mm` value is a depth accumulated over a record interval, not a rate. Neither converter currently verifies that the input variable's units attribute is metres or that every expected timestep is present.

For each UTC-date group, the NetCDF converter's default `--first-step keep` retains the first accumulated value; `zero` and `missing` are alternatives. When a difference is negative, both converters substitute the current accumulated value; both then clip negative values to zero by default. The binary converter hardcodes the same first-record and negative-value behavior. These statements describe current implementation only; they do not establish the scientifically correct attribution at a daily boundary.

### NetCDF output

Edit `s02-convert_CMF_nc.sh` to match the downloader's source directory, file grouping, timestep, and dates. Choose `MERGE_UNIT="monthly"` or `"yearly"`. The script creates `runoff_YYYYMM.nc` or `runoff_YYYY.nc` and a `runoff_info_CaMaFlood.txt` summary. The output variable defaults to `runoff`, is stored as float32 with fill value `1.0e20`, is ordered north-to-south by default, and has `units="mm"` plus a `time_step_hours` attribute. It also retains the source timestamps as the output time coordinate.

Example with the tracked source sample:

```bash
./s02-convert_CMF_nc.sh
```

With 24-hour values in mm, the corresponding unit divisor is `DROFUNIT=86400000`; with 3-hour and 1-hour values it is `10800000` and `3600000`, respectively. These are conversions from mm per record interval to m/s. Set `IFRQ_INP` to the actual record interval in hours. Confirm both settings against the output product and the simulation configuration.

### Plain binary output and CTL

Edit `s03-convert_CMF_bin.sh` to match the source settings. This wrapper sets the output timestep equal to its configured input timestep. A direct call to `convert_CMF_bin.py` can aggregate input intervals to a longer output interval if the output is an integer multiple of the input; it cannot disaggregate them. Output is grouped by UTC date in files named `runoff_YYYYMMDD.bin`; a daily timestep creates one 2-D record per day, while a subdaily timestep stores multiple 2-D records in each daily file. Each 2-D record is float32 in west-to-east, north-to-south order. `ENDIAN` controls byte order. Nonfinite values are written as the CTL `undef` value (default `1.0e20`).

The generated `runoff.ctl` describes grid centers using `xdef`/`ydef`, sets `options yrev` for the north-to-south binary rows, and declares the output record timestep and total number of output time samples. The CTL and binary files must come from the same conversion. Existing binary files are overwritten by the current wrapper setting `SKIP_EXISTING=0`; set it to 1 only when reusing matching files.

```bash
./s03-convert_CMF_bin.sh
```

The generated information report derives the interval from the CTL for this binary path. It reports an Input Matrix grid configuration, `IFRQ_INP`, and `DROFUNIT`; it does not create `inpdim_<ROFFTAG>.txt` or `inpmat_<ROFFTAG>.bin`. Generate those using the map-specific Input Matrix workflow for the intended CaMa-Flood map and runoff grid.

## 4. Tracked Japan example and validation status

The tracked raw source is `ori_japan/ERA5-Land_runoff_200001.nc`. Its NetCDF header identifies a `ro` field on a 31-time, 221-latitude, 321-longitude grid with 0.1-degree spacing; the field metadata gives units of metres. The tracked converted NetCDF has 31 time records and describes the variable as millimetres per 24-hour timestep. These files are examples of source and converted products, not a scientifically validated forcing series. Their timestamps follow the ERA5-Land valid-time labels; first-record and period-boundary treatment remains unvalidated in [KG-0019](../../../research/records/knowledge_gaps/KG-0019.md).

The tracked `runoff_japan_bin/runoff.ctl` now declares `tdef 62 ... 12hr`, matching the observed two 321×221 float32 records in each of 31 workstation-local daily files. The binary files are Git-ignored and are not distributed. The CTL's prior 1-day interval was inconsistent with this two-record-per-day layout. The tracked raw and converted NetCDF samples each have 31 timestamps, while the current `s03-convert_CMF_bin.sh` is configured for 24-hour input/output and the current converter derives its CTL record count from output timestamps. Therefore the checked-in sample source and current wrapper do not reproduce the ignored local binary layout; the 12-hour CTL documents the observed local binary layout, not a reproducible or scientifically validated product. A fresh run of the current wrapper would overwrite the local files and produce a different layout. No conversion was run for this correction.

## 5. Scientific limitation: accumulation time labels

CaMa-Flood forcing dates follow the selected runoff dataset's time labels; this workflow does not impose a separate CaMa-Flood date shift. ECMWF documents that an ERA5-Land accumulation with valid time `00:00 UTC` represents the preceding 24 hours ([ECMWF ERA5-Land data documentation](https://confluence.ecmwf.int/spaces/CKB/pages/140385202/ERA5-Land+data+documentation)). The downloader requests `00:00` as the first time of each selected day, and the converters retain source timestamps while grouping/differencing by UTC calendar date. The source timestamp convention is preserved, but physical attribution of the first accumulated value and coverage at the beginning/end of a requested period have not been independently validated. The scripts use Gregorian `datetime`/pandas timestamps; alternate calendars such as `365_day` are not supported by this acquisition/conversion workflow.

The date-label convention is recorded in [KG-0019](../../../research/records/knowledge_gaps/KG-0019.md). Do not change the accumulation algorithm, date labels, or request times as part of that convention clarification. Compatibility tests below document current behavior and are not scientific acceptance tests for the unresolved first-record/boundary attribution.

Other current assumptions to review for a new input product include: 1-D latitude/longitude coordinates for binary output; a regular grid for CTL spacing; valid and sufficiently complete time coordinates; and source values in accumulated metres. The binary writer converts NaNs/nonfinite values to `undef`, while the NetCDF writer stores NaNs using its fill value. Scientific policy for negative runoff, missing cells, forecast resets, and incomplete aggregation bins has not been independently validated.

## 6. Offline tests

Run the tests without CDS credentials or external downloads:

```bash
python3 -m unittest discover -s tests -v
```

They create tiny synthetic NetCDF fields and temporary output files. One arithmetic check verifies differences between known cumulative values within an explicitly assumed forecast cycle, without asserting the physical attribution of its first timestamp. Compatibility checks pin request construction, area-coordinate transformation, current first-record/reset/missing-value rules, NetCDF precision and latitude ordering, binary aggregation and record layout, CTL metadata, and time-step-to-`DROFUNIT` arithmetic. The tests do not prove ERA5-Land date alignment, physical correctness of reset/negative policies, completeness of real downloads, model runtime compatibility, or validity of the tracked binary sample.

The registry [`inp/runoff_datasets.csv`](../runoff_datasets.csv) indexes CaMa-Flood-ready products. Raw downloaded sources such as `ori_japan/` are described here as provenance and processing inputs, not registered as model-ready runoff products.
