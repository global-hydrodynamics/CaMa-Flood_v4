# Global 1-degree ELSE_GPCC test forcing

This test dataset is a time subset of the local ELSE_GPCC global 1-degree
daily runoff dataset (Kim et al., 2009; DOI: 10.1029/2009GL039006).
The long-term source dataset and generated runoff binaries are **not**
included in the Git repository or public distribution.

Run `sh inp/1deg_test/s01-extract_ELSE_GPCC.sh` from the repository root
(or invoke the shell script by its absolute path). The script reads
`inp/ELSE_GPCC/Roff/Roff____YYYYMMDD.one` and copies the inclusive period
2000-01-01 through 2001-12-31 into `inp/1deg_test/Roff/`.

The output has 731 daily files of 259,200 bytes each. Each file retains
the full 360 x 180 global 1-degree grid, little-endian float32,
north-to-south row order, values in mm/day and undef=-999.
No numerical resampling or conversion is performed. The source file
dates and sizes are checked before copying. The existing local
`ELSE_GPCC/Roff.ctl` documents the source grid; the tracked
`Roff.ctl` here documents this 731-day sample.

**Git tracking:** track README, scripts and Roff.ctl, but ignore ONLY
`Roff/` (the generated daily data) within this directory.
This does not imply permission to redistribute the source dataset.

Model configuration: IFRQ_INP=24, DROFUNIT=86400000, LINPDAY=.TRUE.,
CROFPRE=Roff____ and CROFSUF=.one. The runoff grid uses
GRSIZEIN=1, WESTIN=-180, EASTIN=180, SOUTHIN=-90, NORTHIN=90,
OLAT=NtoS; the input matrix remains map-specific.
