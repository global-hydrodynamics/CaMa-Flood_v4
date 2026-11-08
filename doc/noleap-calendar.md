# Simulation calendars

To run without leap days, add `CALENDAR='365_day'` (or `'noleap'`) to
`&NSIMTIME`. The model advances directly from February 28 to March 1.

Omitting `CALENDAR` preserves the existing `NRUNVER/LLEAPYR` setting
(default `.TRUE.`). An explicit setting overrides it. `standard`, `gregorian`
and `proleptic_gregorian` use the existing Gregorian leap-year rule for all
years. Calendar names are case-insensitive.

A 365-day simulation can combine Gregorian and noleap NetCDF runoff,
atmospheric and land inputs. Gregorian February 29 records are skipped by
civil date. Use `NFORCE/SYEARIN<=0` (the default) for CF-based runoff timing.
A Gregorian simulation rejects noleap NetCDF inputs. Unsupported calendars
(including `360_day`) and invalid dates terminate with a nonzero exit status.

NetCDF output and restart files record `calendar='365_day'` for noleap runs.
Explicit restart-calendar mismatches are rejected. Keep the same calendar
when resuming binary or older restart files without calendar metadata.
Binary inputs and legacy readers without CF metadata must already follow
the model calendar. Calendar conversion does not interpolate missing data
or add automatic file rollover.

Run the synthetic clock, forcing, output and restart checks in both precisions
from `src` (requires `numpy`, `netCDF4`, and NetCDF Fortran):

```sh
make test-calendar PYTHON=python3 FCMP=gfortran
python3 test/run_noleap_calendar_tests.py --baseline-ref <master-commit>
```
