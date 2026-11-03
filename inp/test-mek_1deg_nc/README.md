# Mekong 1-degree annual NetCDF runoff sample

This sample provides yearly NetCDF runoff for 2000 and 2001, converted from the daily binary fields in [`../test-mek_1deg/`](../test-mek_1deg/). The binary source remains unchanged. The values are format-converted without rescaling and are intended for the v5 single-file NetCDF forcing workflow.

## Contents and data contract

| File | Time records | Size |
| --- | ---: | ---: |
| `runoff2000.nc` | 366 daily records | 888,596 bytes |
| `runoff2001.nc` | 365 daily records | 886,172 bytes |

The files contain `runoff(time, latitude, longitude)` as 32-bit float values on the 20 × 30 grid covering 90–110°E and 5–35°N. Longitude centers increase from 90.5°E; latitude centers decrease from 34.5°N. Runoff units are `mm day-1`; the source missing-value sentinel is `-999`. The `time` coordinate follows CF conventions, with daily bounds and calendar `proleptic_gregorian`. The dataset totals about 1.7 MiB.

This is a derived sample from the same ELSE_GPCC regional data distributed in `test-mek_1deg`; the source product is associated with Kim et al. (2009), [doi:10.1029/2009GL039006](https://doi.org/10.1029/2009GL039006). This provenance does not establish independent scientific validation of the runoff values.

## Regenerate and verify

From the repository root, regenerate the initial two-year sample with:

```sh
python3 -B inp/test-mek_1deg_nc/s00-convert_binary_to_netcdf.py
```

The script reads `inp/test-mek_1deg/runoff/Roff____YYYYMMDD.one` and writes annual files in this directory. It uses the external command-line tools `ncgen` and `ncdump` (from a NetCDF installation), plus Python's standard library. Existing files are protected unless `--overwrite` is specified. The conversion compares the source float32 values against the NetCDF values after writing and reading them. The configured default period can be extended through 2009 in the script.

The initial 2000–2001 generation was checked against all 731 binary daily fields: each NetCDF field round-tripped exactly as float32; the source data had no `-999`, NaN, or Inf values in that period. This is an exact format-conversion check, not scientific validation.

`mek02-longterm_netcdf.sh` selects this dataset using `ROFFTAG="test-mek_1deg_nc"` and reuses the map-specific `inpmat_test-mek_1deg.bin` by setting `INPMAT_TAG="test-mek_1deg"`. The sample was used in the two-year restart-connected v5 run described in [`gosh_sample/README.md`](../../gosh_sample/README.md).

The two compact NetCDF files are intended to be GitHub-tracked sample data. Their inclusion in a future downloadable v5 package remains undecided until the v5 packaging workflow is defined.
