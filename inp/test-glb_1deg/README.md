# Reproducible global 1-degree runoff test sample

This directory rebuilds a two-year, global 1-degree daily runoff sample from the external ELSE_GPCC data under `inp/ELSE_GPCC/Roff/`. The source is identified in the maintainer-provided documentation as the ELSE_GPCC runoff product associated with Kim et al. (2009), “Role of rivers in the seasonal variations of terrestrial water storage over global basins” ([doi:10.1029/2009GL039006](https://doi.org/10.1029/2009GL039006)). The citation identifies provenance; it does not establish redistribution permission.

The default period, 2000-01-01 through 2001-12-31, matches the two-year coverage of the legacy `test_1deg/` sample recorded in the runoff inventory and v4.30 package inspection. The extraction preserves source values and conventions without spatial or temporal transformation:

- Global grid: 360 × 180 cells at 1 degree; cell edges 180°W–180°E and 90°S–90°N.
- Daily records in `Roff____YYYYMMDD.one` files, little-endian float32, with north-to-south rows (`yrev`).
- Unit: `mm/day`, as declared by the source CTL. Missing value: `-999`.
- 731 files, 259,200 bytes each (189,475,200 bytes total).

The tracked source CTL `inp/ELSE_GPCC/Roff.ctl` declares 7,671 daily records beginning 1981-01-01, while source filenames span 1979-01-01 through 2011-01-01; the final 2011-01-01 file is empty, and 11,688 preceding daily files have the expected grid size. The generator therefore selects the documented test period by dated filenames and verifies each selected file; it does not use the source CTL time count as the file inventory. The origin of this CTL/file-coverage discrepancy is unresolved.

## Reproduce and validate

The user-editable settings are at the beginning of [`s01-extract_ELSE_GPCC.sh`](s01-extract_ELSE_GPCC.sh): the source directory and CTL are relative to the script, and the default dates are 2000-01-01 through 2001-12-31. Run from the repository root with:

```sh
sh inp/test-glb_1deg/s01-extract_ELSE_GPCC.sh
```

Alternatively, invoke the shell script by its absolute path from any working directory:

```sh
/path/to/CaMa-Flood_v4/inp/test-glb_1deg/s01-extract_ELSE_GPCC.sh
```

The shell workflow invokes the adjacent standard-library-only [`extract_ELSE_GPCC.py`](extract_ELSE_GPCC.py). It checks the source CTL grid, row order, missing-value sentinel, every requested date and file size, then copies each source file byte-for-byte to `Roff/`. It checks the output date set and sizes, compares every output file with its source, verifies the generated CTL geometry/time axis, and confirms source file sizes and modification times did not change. No external Python packages or model executable are required.

The generated `runoff.ctl` describes the selected 731 daily records and points to `Roff/Roff____%y4%m2%d2.one`. The grid, `mm/day` unit, `-999` undefined value, and little-endian N-to-S binary layout match the source metadata and CaMa-Flood's legacy daily-file forcing convention. For v5 configuration, use `CROFFREQ="DAY"`, `IFRQ_INP=24`, `DROFUNIT=86400000`, `CROFPRE="Roff____"`, and `CROFSUF=".one"`. The runoff grid settings are `GRSIZEIN=1`, `WESTIN=-180`, `EASTIN=180`, `SOUTHIN=-90`, `NORTHIN=90`, and `OLAT=NtoS`; the input matrix remains map-specific. For the existing legacy run script, set `CROFDIR` to `inp/test-glb_1deg/Roff`. This new sample does not alter the legacy `gosh/` script or its `inp/test_1deg/runoff/` path.

## Git and distribution

The README, extraction scripts, CTL, and ignore rule are intended to be tracked. Only `Roff/` binary data are ignored by [`.gitignore`](.gitignore); the directory and its scripts are not broadly ignored. The source files under `inp/ELSE_GPCC/Roff/` are external data and are not tracked or included in the source repository or distribution package; only their CTL metadata is tracked. Generated samples remain local and ignored until redistribution permission is established. Do not add the source or generated binaries to Git without a separate redistribution decision.

The extraction and byte comparisons verify reproducibility and file compatibility, not the independent scientific validity of ELSE_GPCC runoff values.
