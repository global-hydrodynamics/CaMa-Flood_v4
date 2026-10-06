# Simulation calendars

Set the calendar in the existing `NSIMTIME` namelist:

```fortran
&NSIMTIME
  SYEAR=2000, SMON=1, SDAY=1, SHOUR=0,
  EYEAR=2001, EMON=1, EDAY=1, EHOUR=0,
  CALENDAR='365_day'
/
```

`365_day` and `noleap` are case-insensitive aliases. `standard`, `gregorian`
and `proleptic_gregorian` select the existing Gregorian leap-year rule.
Unsupported calendars, including `360_day`, are rejected. Invalid simulation
dates, such as February 29 in a noleap run, terminate with exit code 9.
As before, the
Gregorian rule is applied to all years; this is not a historical Julian /
Gregorian cutover implementation.

Omitting `CALENDAR` preserves `NRUNVER/LLEAPYR`, whose default is `.TRUE.`.
An explicit `CALENDAR` overrides `LLEAPYR`; the effective value is logged.
Existing `LLEAPYR=.FALSE.` configurations also receive the input fixes below.
Start and end dates must exist in the selected calendar. February 29 is not
valid in a 365-day run, including as an end date.

## Clock and forcing

The model's minute counter, step count and date conversion continue to use
`IMDAYS` and the effective `LLEAPYR`. A 365-day run advances directly from
February 28 to March 1, with one timestep of elapsed model time. Every year
has 365 model days. Timestep lengths, flux integration and output accumulation
retain their existing elapsed-time definitions; no extra timestep or synthetic
February 29 forcing is introduced.

NetCDF time coordinates may use `standard` / `gregorian` /
`proleptic_gregorian` or `365_day` / `noleap`. A 365-day simulation can read
both calendars concurrently, for example noleap runoff and Gregorian
atmospheric forcing. Gregorian input records are selected by the same civil
model datetime, not by adding model timesteps to a Gregorian record number.
Thus all daily or sub-daily records on February 29 are omitted, including
across multiple leap years. CF bounds retain the existing preference for an
interval's lower edge, followed by its coordinate. Monthly or yearly NetCDF
reopening through `InputConf%open_next_file` resets the file's time origin
while preserving the elapsed model update schedule. Automatic filename
expansion or file rollover is not added.

For the legacy runoff reader, use CF metadata mode (`NFORCE/SYEARIN<=0`,
the default) when mixing NetCDF calendars. Explicit `SYEARIN>0` mode has no
input-calendar metadata: its records must follow the model calendar. Daily
runoff filenames are generated from the model date, so a 365-day run never
requests a February 29 file.

Annual binary inputs handled by `InputConf` follow the model calendar and
retain their January 1 record convention. Their initial record now accounts
for 365-day years, including a March or later restart in a Gregorian leap
year. Binary files contain no calendar metadata: a Gregorian annual binary
must be converted to the model calendar before use in a 365-day run. Legacy
sea-level and other readers without CF calendar metadata likewise require
records consistent with the model calendar. A Gregorian simulation rejects
noleap NetCDF input, since it would need a February 29 value that does not
exist in that input.

NetCDF inputs retain the existing requirements for regular intervals,
whole-minute coordinates and an aligned initial update. Calendar conversion
does not resample forcing or fill missing intervals.

## Output and restart

NetCDF output time values count elapsed model seconds. A 365-day run writes
`calendar='365_day'` on the time coordinate so timestamps decode correctly
past February. Gregorian output retains its existing metadata.

Binary hydrology and additional-state restart formats are unchanged. Their
filenames use the model datetime. On restart, forcing indices are reconstructed
from the configured start datetime and each input's calendar; raw previous-run
record counters are not carried forward. Use the same simulation calendar
and forcing cadence when resuming. The existing precision and timestep-phase
requirements still apply.

365-day NetCDF restart files also carry `calendar='365_day'`. The reader
rejects an explicit restart calendar that differs from the simulation calendar.
Old restart files without this attribute retain their existing read behavior;
the caller must preserve their calendar setting, as for binary restarts.

## Verification

Run from `src`, using a Python installation containing `numpy` and `netCDF4`,
and a Fortran compiler with the matching NetCDF Fortran library:

```sh
make test-calendar PYTHON=python3 FCMP=gfortran
```

The runner uses disposable build directories and synthetic forcing. It tests
production clock, runoff, `InputConf`, NetCDF output, hydrology restart and
additional binary restart paths in both model precisions with bounds checking.
Cases include common years, Gregorian leap years, both noleap aliases, daily
and three-hour forcing, centered and end-stamped bounds, monthly reopening,
1999–2004 including two leap years, and split/resumed runs at February 28,
March 1 and January 1. It also checks invalid calendars/dates, incompatible
inputs, insufficient Gregorian forcing coverage and mismatched restart
calendars, and runs the existing CF-time and datetime tests.

For an exact comparison of default Gregorian output and binary restart data
against an earlier public commit:

```sh
python3 test/run_noleap_calendar_tests.py --baseline-ref <commit>
```

The fixtures exercise time and I/O together without routing a real catchment
or running a particular experiment dataset.
