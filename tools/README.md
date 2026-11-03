# CaMa-Flood Output Tools

This directory contains standalone tools for inspecting and describing model outputs. They do not modify or rerun the model.

## Generate GrADS CTL files for binary output

`generate_output_ctl.py` creates one GrADS descriptor (`.ctl`) per configured, supported `NX x NY` float32 output. It reads:

- `input_cmf.nam` from the run directory (or `--namelist`), including simulation dates, `DT`, `IFRQ_OUT`, `CVARSOUT`, output paths, and binary/vector settings.
- The map's `params.txt`, located by following `CNEXTXY`, `CGRAREA`, or `CDIMINFO` in the namelist.
- The repository-root [`output_variables.csv`](../output_variables.csv) for names, metadata, data type, dimensions, and missing value.

Run from any working directory:

```sh
python3 tools/generate_output_ctl.py --run-dir out/mek01-default_run --mode planned
python3 tools/generate_output_ctl.py --run-dir out/mek01-default_run --mode existing
```

The byte order defaults to the Python host's native byte order. When outputs were created on another architecture, set it explicitly with `--endian little` or `--endian big`.

### Modes

- `planned` uses the namelist schedule and creates CTLs before or during a run without requiring output binaries or checking their sizes. A CTL may describe records that have not been written yet.
- `existing` considers only configured binary files that exist, infers record counts from file size, and compares them with the namelist schedule. If a file has fewer complete records than expected, it reports the file as incomplete and writes a CTL for the records present. A partial trailing record or excess records is an error and produces no CTL for that file.

Both modes overwrite matching CTL files. Binary data are never created or changed. CTLs are placed beside their corresponding binaries.

### Time axis and data layout

The tool follows `CMF_DRV_ADVANCE`: ordinary outputs are written at the end of a model time step when the ending clock hour is divisible by `IFRQ_OUT` and the minute is zero. The first record is therefore the first scheduled output timestamp after the simulation start. Averages refer to the interval ending at that timestamp; snapshots refer to the state at that timestamp. If the final model step lands on an output boundary, the model can write a record at the configured end timestamp. The tool counts only timestamps reached by the configured outer `DT` steps and uses the timestamps actually implied by the source schedule.

The map extent in `params.txt` is expressed as cell edges. CTLs use cell-center coordinates, an increasing south-to-north `YDEF`, and `OPTIONS yrev` to match CaMa-Flood's north-to-south map rows. Ordinary binary records are direct-access, four-byte float records with `NX * NY` values and no sequential-record markers.

### Scope and limitations

- NetCDF output (`LOUTCDF=.TRUE.`), vector output (`LOUTVEC=.TRUE.`), non-confirmed registry rows, and non-`NX,NY` arrays such as `pthflw` are skipped with a reason.
- Binary output with `LOUTINI=.TRUE.` is rejected because the current Fortran initial-output path advances the shared record counter but skips binary writes.
- Registry entries missing a required confirmed `long_name`, `units`, `binary_dtype`, `dimensions`, or `missing_value` are not used.
- Binary files do not identify their own byte order. Specify `--endian` when the generation host's native order differs from the model-output host.
- `existing` can verify size and record count, but cannot prove that a same-named binary was produced with the current namelist.
- Long-term template CTLs combining multiple annual files are not implemented. Their filename/year-boundary convention needs a separate specification; the current tool creates independent per-file CTLs only.
