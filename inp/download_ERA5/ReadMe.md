# ERA5-Land runoff tools for CaMa-Flood

These scripts download ERA5-Land runoff from the Copernicus Climate Data Store (CDS) and convert it into CaMa-Flood forcing files.

Supported outputs:

- CaMa-Flood NetCDF runoff forcing
- CaMa-Flood / GrADS-style plain binary runoff forcing
- A text summary of grid and namelist settings for CaMa-Flood

---

## 1. Prerequisites

### CDS API setup

Install the CDS API client:

```bash
pip install cdsapi
```

Create your CDS API configuration file:

```bash
~/.cdsapirc
```

Example:

```yaml
url: https://cds.climate.copernicus.eu/api
key: <YOUR_PERSONAL_ACCESS_TOKEN>
```

The token is available from your CDS account page.

### Accept the ERA5-Land licence

Before downloading data, accept the ERA5-Land data licence on the CDS website:

```text
https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land
```

Open the **Download** tab and accept the required licence terms.

If this is not done, the download will fail with an error such as:

```text
403 Client Error: Forbidden
required licences not accepted
```

---

## 2. Scripts

| Script | Purpose |
|---|---|
| `s01-download.sh` | Download original ERA5-Land runoff NetCDF files from CDS |
| `download_era5land.py` | Python downloader called by `s01-download.sh` |
| `s02-convert_CMF_nc.sh` | Convert downloaded files to CaMa-Flood NetCDF forcing |
| `convert_CMF_nc.py` | Python NetCDF converter called by `s02-convert_CMF_nc.sh` |
| `s03-convert_CMF_bin.sh` | Convert downloaded files to CaMa-Flood plain binary forcing and create GrADS `.ctl` |
| `convert_CMF_bin.py` | Python binary converter called by `s03-convert_CMF_bin.sh` |
| `make_CMF_runoff_info.py` | Create CaMa-Flood grid and namelist information text file |

---

## 3. Step 1: Download ERA5-Land runoff

Edit:

```bash
s01-download.sh
```

Key settings:

```bash
DOWNLOAD_UNIT="monthly"   # daily or monthly
OUTDIR="./ori_monthly"
TSTEP=24                  # 24=daily, 12, 6, 3, or 1 hour
```

Set the download period:

```bash
SYEAR=2000
SMONTH=1
SDAY=1

EYEAR=2000
EMONTH=12
EDAY=31
```

Set the download area as north, south, west, east:

```bash
NORTH=90.0
SOUTH=-90.0
WEST=-180.0
EAST=180.0
```

For a regional download, for example Japan:

```bash
NORTH=46.0
SOUTH=24.0
WEST=122.0
EAST=154.0
```

Run:

```bash
chmod +x s01-download.sh
./s01-download.sh
```

### Notes on download settings

`DOWNLOAD_UNIT="monthly"` is usually recommended because it reduces the number of CDS requests.

For global download, use:

```bash
EXTRA_OPTS="--area-is edge"
```

For regional download, either edge or center coordinates can be used depending on how the area is specified. If `--area-is center` is used, the script expands the domain by half a grid cell.

CDS downloads can be slow because the CDS server prepares the requested file before transfer. If many requests fail, reduce parallel jobs:

```bash
NCPUS=1
```

---

## 4. Step 2a: Convert to CaMa-Flood NetCDF forcing

Edit:

```bash
s02-convert_CMF_nc.sh
```

Key settings:

```bash
INDIR="./ori_monthly"
INPUT_FILE_UNIT="monthly"     # daily or monthly
TIMESTEP_HOURS=24             # should match TSTEP in s01-download.sh

OUTDIR="./runoff_year_nc"
MERGE_UNIT="yearly"           # yearly or monthly
```

Set the conversion period:

```bash
START_DATE="2000-01-01"
END_DATE="2000-12-31"
```

Run:

```bash
chmod +x s02-convert_CMF_nc.sh
./s02-convert_CMF_nc.sh
```

Output examples:

```text
runoff_year_nc/runoff_2000.nc
runoff_year_nc/runoff_info_CaMaFlood.txt
```

The converted runoff variable is accumulated runoff over each input timestep:

```text
units = mm
```

Examples:

- `TIMESTEP_HOURS=24` means mm/day
- `TIMESTEP_HOURS=3` means mm/3h
- `TIMESTEP_HOURS=1` means mm/h

---

## 5. Step 2b: Convert to CaMa-Flood plain binary forcing

Edit:

```bash
s03-convert_CMF_bin.sh
```

Key settings:

```bash
INDIR="./ori_monthly"
INPUT_FILE_UNIT="monthly"
TIMESTEP_HOURS=24             # timestep of downloaded NetCDF data
OUTPUT_TIMESTEP_HOURS=24      # timestep of output binary forcing

OUTDIR="./runoff_bin"
BINARY_PREFIX="runoff_"
BINARY_SUFFIX=".bin"
CTL_FILE="${OUTDIR}/runoff.ctl"
```

Run:

```bash
chmod +x s03-convert_CMF_bin.sh
./s03-convert_CMF_bin.sh
```

Output examples:

```text
runoff_bin/runoff_20000101.bin
runoff_bin/runoff_20000102.bin
runoff_bin/runoff.ctl
runoff_bin/runoff_info_CaMaFlood.txt
```

### Binary file convention

Binary files are always written as **daily files**:

```text
runoff_YYYYMMDD.bin
```

For sub-daily forcing, each daily file contains multiple records.

Examples:

| `OUTPUT_TIMESTEP_HOURS` | File name | Records per daily file |
|---:|---|---:|
| 24 | `runoff_YYYYMMDD.bin` | 1 |
| 12 | `runoff_YYYYMMDD.bin` | 2 |
| 6 | `runoff_YYYYMMDD.bin` | 4 |
| 3 | `runoff_YYYYMMDD.bin` | 8 |
| 1 | `runoff_YYYYMMDD.bin` | 24 |

The `.ctl` file uses the same daily file template:

```text
dset ^./runoff_%y4%m2%d2.bin
```

and the `tdef` interval is set to the output timestep, for example:

```text
tdef ... linear 00Z01jan2000 3hr
```

### Temporal aggregation

The binary converter can aggregate from a finer input timestep to a coarser output timestep.

Examples:

```bash
TIMESTEP_HOURS=1
OUTPUT_TIMESTEP_HOURS=3
```

or:

```bash
TIMESTEP_HOURS=3
OUTPUT_TIMESTEP_HOURS=24
```

It cannot create finer data than the downloaded timestep.

---

## 6. CaMa-Flood information file

Both `s02` and `s03` create:

```text
runoff_info_CaMaFlood.txt
```

This file summarizes useful CaMa-Flood settings.

### Input mapping table settings

Example:

```text
GRSIZEIN=0.1
WESTIN=-180.05
EASTIN=179.95
NORTHIN=90.05
SOUTHIN=-90.05
OLAT="NtoS"
```

Use these values when creating the CaMa-Flood input mapping table.

### Common forcing settings

Example:

```text
IFRQ_INP="24"
DROFUNIT="86400000"
```

`DROFUNIT` is automatically calculated as:

```text
DROFUNIT = 1000 * timestep_hours * 3600
```

Examples:

| Time step | DROFUNIT |
|---|---:|
| 1 hour | 3600000 |
| 3 hours | 10800000 |
| 6 hours | 21600000 |
| 12 hours | 43200000 |
| 24 hours | 86400000 |

For binary output, `IFRQ_INP` and `DROFUNIT` should follow `OUTPUT_TIMESTEP_HOURS`.

### NetCDF forcing settings

Example:

```text
CROFPRE="runoff_"
CROFSUF=".nc"
CROFCDF=""
CVNROF="runoff"
```

### Plain binary forcing settings

Example:

```text
CROFPRE="runoff_"
CROFSUF=".bin"
```

The binary file rule is:

```text
runoff_YYYYMMDD.bin
```

For sub-daily binary forcing, multiple records are stored in each daily file.

---

## 7. Typical workflows

### NetCDF forcing workflow

```text
s01-download.sh          monthly ERA5-Land NetCDF download
        ↓
s02-convert_CMF_nc.sh    CaMa-Flood NetCDF forcing
```

### Plain binary forcing workflow

```text
s01-download.sh          monthly ERA5-Land NetCDF download
        ↓
s03-convert_CMF_bin.sh   CaMa-Flood binary forcing + GrADS ctl
```

---

## 8. Troubleshooting

### `python: command not found`

Set:

```bash
PYTHON_CMD="python3"
```

or activate your conda environment before running the script.

### `required licences not accepted`

Accept the ERA5-Land licence on the CDS dataset page.

### Many CDS requests fail

Use:

```bash
DOWNLOAD_UNIT="monthly"
NCPUS=1
```

and rerun. Existing files can be skipped with:

```bash
SKIP_EXISTING=1
```

### Longitude has only one grid point

For global download, avoid expanding the longitude range beyond the CDS domain. Use:

```bash
EXTRA_OPTS="--area-is edge"
WEST=-180.0
EAST=180.0
```

---

## 9. Important assumptions

- ERA5-Land runoff is treated as an accumulated variable in metres.
- The converters compute timestep runoff and convert it to millimetres.
- NetCDF output keeps the selected timestep.
- Binary output uses daily files; sub-daily forcing is stored as multiple records inside each daily file.
- Latitude is written in north-to-south order for CaMa-Flood mapping.
