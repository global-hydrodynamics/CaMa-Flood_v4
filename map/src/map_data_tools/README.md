# Core map metadata and conversion tools

This directory contains a shared metadata/configuration validator and two Python command-line tools for CaMa-Flood category 11 core river-network products:

- `generate_ctl.py` writes one GrADS CTL file for each of `nextxy.bin`, `ctmare.bin`, `elevtn.bin`, `nxtdst.bin`, `rivlen.bin`, and `fldhgt.bin`.
- `binary_to_netcdf.py` writes one NetCDF core map for the model's `CRIVCLINC` input.
- `map_metadata.py` reads `map/map_variables.csv`, dataset metadata, `params.txt`, validates record layouts and binary sizes, and constructs coordinate arrays.

The tools do not create or modify source map binaries. The NetCDF converter does not create `CRIVPARNC` channel parameters; a simulation using NetCDF map input still needs the separately configured parameter file and any optional-scheme inputs.

## Requirements

Use Python 3.10 or later with NumPy and netCDF4:

```sh
python3 -m pip install -r map/src/map_data_tools/requirements.txt
```

The CTL generator also uses NumPy through the shared module. The optional Fortran compatibility smoke test additionally needs `gfortran`, `nf-config`, and `nc-config`.

## Dataset configuration

Both tools require an explicit dataset configuration. By default they look for `map_metadata.json` in the target map directory; pass `--config` to use another file. Start from [`examples/map_metadata.example.json`](examples/map_metadata.example.json), copy it into the target map directory, and verify every value for that map.

The JSON records:

- `byte_order`: explicitly `little` or `big`.
- `row_order`: explicitly `north_to_south` or `south_to_north`; controls CTL `yrev` and latitude coordinate order.
- `coordinate_mode`: currently only `regular_lonlat_from_params`.
- `files`: paths for the six input binaries, relative to the map directory or absolute.
- `missing_values`: one numeric sentinel per file. The first version preserves this value exactly in NetCDF and uses it for CTL `undef`.

The tools do not infer endianness, row order, coordinate mode, or missing sentinels. Confirm them from the selected dataset metadata, CTLs, provenance, or other authoritative source. `params.txt` must contain the standard eight records `NX`, `NY`, `NLFP`, `GSIZE`, `WEST`, `EAST`, `SOUTH`, and `NORTH`; unsupported/nonuniform geometry is rejected.

## Usage

From the repository root, after copying and editing a config for the selected map:

```sh
python3 map/src/map_data_tools/generate_ctl.py \
  --map-dir map/glb_15min \
  --config map/glb_15min/map_metadata.json

python3 map/src/map_data_tools/binary_to_netcdf.py \
  --map-dir map/glb_15min \
  --config map/glb_15min/map_metadata.json
```

The CTL files are written beside the map binaries by default. The NetCDF output defaults to `river_network.nc` under the map directory. Use `--output-dir` for CTLs or `--output` for NetCDF to place outputs elsewhere. Both tools refuse to overwrite existing outputs unless `--overwrite` is passed. An explicit `--metadata-csv` can select another compatible inventory; category 11 must still contain the six expected core files and variables.

## Data and output contract

The category 11 inventory rows define each variable's short name, long name, units, primitive type, record index/count, symbolic dimensions, and elevation datum. `nextxy.bin` is read as record 1 `nextx`, then record 2 `nexty`; `fldhgt.bin` is read as `NLFP` records. Direct-access Fortran arrays are `(NX,NY)` with x contiguous, represented internally by NumPy as `(NY,NX)` rows.

The NetCDF file contains 1-D `lon` and `lat`, 2-D `nextx`, `nexty`, `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and 3-D `fldhgt`. Disk dimensions are `(lat,lon)` and `(lfp,lat,lon)`, respectively, which the existing Fortran NetCDF API reads into `(NX,NY)` and `(NX,NY,NLFP)` arrays. The synthetic Fortran smoke test exercises those same variable lookups and array extents; it is not a full CaMa-Flood model run.

Numeric map values and 1-based downstream indices are preserved. For this first converter, the explicit input sentinel is written as both `_FillValue` and `missing_value`; it is not converted to `RMIS`. `elevtn` carries `vertical_datum=EGM96`. `-9` and `-10` endpoint codes in `nextxy` remain data; only the configured sentinel is marked missing.

## Validation and tests

Before writing, the shared module checks required metadata, configuration fields, map dimensions, grid spacing, file presence, expected byte size, and supported record counts. Unknown formats and incomplete configurations fail with a clear error.

Run the synthetic tests from the repository root:

```sh
PYTHONPATH=map/src python3 -m unittest discover -s map/src/map_data_tools/tests -v
```

The suite covers non-square dimensions, float32 and two-record int32 maps, multiple floodplain layers, byte order, row orientation, missing values, one-based indices, CTL structure, NetCDF shape/value/fill metadata, and missing/truncated input files. When the NetCDF-Fortran toolchain is available, it compiles and runs a small reader-contract smoke test modeled on `src/cmf_ctrl_maps_mod.F90`.

## Scope limits

Only category 11 is converted. Other CSV categories remain inventory entries and are not implied to be supported by these tools. The supported geometry is a regular longitude/latitude grid described by `params.txt`. The converter's output is compatible with the inspected v4 NetCDF reader interface, but a complete model simulation using this output and all required parameter files has not been established by the unit tests.
