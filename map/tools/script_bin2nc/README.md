# Binary-to-NetCDF map conversion template

This shared template converts registered base-resolution and high-resolution Binary map rasters into individual NetCDF files. Copy the scripts into a map-local `script_bin2nc/` directory and run that copy. The Mekong sample contains a ready-to-use copy at [`map/mekong_15min/script_bin2nc/`](../../mekong_15min/script_bin2nc/).

The scripts reuse [`convert_map_binary_to_netcdf.py`](../code/convert_map_binary_to_netcdf.py), [`convert_highres_binary_to_netcdf.py`](../code/convert_highres_binary_to_netcdf.py), and the authoritative [`variable_table/map_variables.csv`](../../../variable_table/map_variables.csv) registry. `s01`/`s02` handle binaries directly in the map root; `s03` handles registered high-resolution tile binaries.

## Select files and convert

1. Copy the templates to the target map and set `MAP_DIR` in both scripts. For a `script_bin2nc/` directly inside the map directory, set `MAP_DIR="$SCRIPT_DIR/.."`:

   ```sh
   mkdir -p map/your_map/script_bin2nc
   cp map/tools/script_bin2nc/s01-list_binary.sh map/your_map/script_bin2nc/
   cp map/tools/script_bin2nc/s02-convert_netcdf.sh map/your_map/script_bin2nc/
   ```

2. Change to the map-local script directory, then generate the candidate list. The list is written to the current directory as `bin2nc_list.txt`:

   ```sh
   cd map/your_map/script_bin2nc
   ./s01-list_binary.sh
   ```

   The Mekong copy targets `map/mekong_15min/`. Candidates are ordered by numeric `category_id` and then registry row order. A file listed in multiple categories appears once under its first category. Only registered files directly in the map root and supported by the converter are listed. Unregistered binaries and registered but unsupported layouts are reported as warnings. The current `NX,NY,INPN` runoff input matrices are not handled as map rasters and are omitted. If a list already exists in the current directory, `s01` asks before replacing it; non-interactive use preserves it.

3. Review `bin2nc_list.txt` and comment out every file that should not be converted by placing `#` at the beginning of its line. The generated list starts with all supported candidates selected.

4. Convert the remaining entries in list order:

   ```sh
   ./s02-convert_netcdf.sh
   ```

   Files are written to `<map>/netcdf/<binary-stem>.nc`. For example, `nextxy.bin` becomes `nextxy.nc` with `nextx` and `nexty`; `fldhgt.bin` retains all registered floodplain layers. The converter checks registry metadata, map dimensions, record sizes, and reads each written variable/layer back for exact value comparison.

5. Review the conversion log and the NetCDF files in `<map>/netcdf/`. Existing outputs are skipped by default in non-interactive use. In a terminal, each existing output prompts for `y/N`; Enter skips it. Pass `--force` to overwrite selected outputs without prompts:

   ```sh
   ./s02-convert_netcdf.sh --force
   ```

Run both scripts from the same map-local `script_bin2nc/` current directory so `s02` reads the list created by `s01`. The scripts resolve the map, registry, and converter relative to their own locations. They only inspect `.bin` files directly under `MAP_DIR`; high-resolution subdirectories are never searched.

## Build legacy consolidated NetCDF inputs

The individual files written by `s02` are used with `LMAPCDF=.FALSE.`. A separate helper can assemble the historical consolidated containers used with `LMAPCDF=.TRUE.`; the two formats follow different model-reader paths.

The `t01-legacy_netcdf_map.sh` script creates `netcdf/rivclim.nc` with `nextx`, `nexty`, `lon`, `lat`, `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and all `fldhgt` layers. It creates `netcdf/rivpar.nc` with `rivwth`, `rivhgt`, and `rivman`. The defaults match the Mekong simulation examples: `nextxy_noedge.nc` and `rivwth_merit.nc`. Edit `NEXTXY` and `RIVWTH` near the top to choose `nextxy.nc` or `rivwth.nc` instead.

Run the map-local script independently of `s01` and `s02`:

```sh
cd map/mekong_15min/script_bin2nc
./t01-legacy_netcdf_map.sh
```

Existing consolidated outputs are kept in non-interactive use. In a terminal, answer `y` to replace them; Enter or `N` keeps them. Pass `--force` for explicit overwrite. The Python program stages both outputs, reads them back, checks variable values, then replaces the targets.

The current legacy reader additionally uses `gdwdly` from `rivpar.nc` when `LGDWDLY` is enabled (the reader sets it to zero if absent), `elevslope` for `LSLOPEMOUTH`, and `mask_slope` for `LSLPMIX`; the latter two must exist in `rivpar.nc` when their options are enabled. `LMEANSL` reads `meansl` from the separately configured `CMEANSLNC`, and MPI legacy mode reads `mpireg` from `CMPIREGNC`. This generator does not synthesize these optional variables. The basic Mekong containers omit them; do not enable the corresponding option unless its required input is supplied.

Example model settings for the consolidated path:

```fortran
&NMAP
LMAPCDF   = .TRUE.
CRIVCLINC = ".../netcdf/rivclim.nc"
CRIVPARNC = ".../netcdf/rivpar.nc"
CPTHOUT   = ".../bifprm.txt"
/
```

This is only the Map namelist fragment. If bifurcation is used, enable `LPTHOUT=.TRUE.` in `&NRUNVER`; the model also requires its normal dimension, time, and other run settings.

The NetCDF C library and Python standard library are sufficient. Validation here checks source/output variable values and layout; it does not replace a model run. Execution through the legacy consolidated reader remains a separate validation step.

## Convert high-resolution tiles (`s03`)

Copy `s03-convert_highres_netcdf.sh` to the map-local `script_bin2nc/` directory, set `MAP_DIR`, `HIRES`, and `VARS` near the top, then run it from any working directory. The commented `HIRES` and `VARS` examples show the available choices; select only directories and registered file stems present in the target map. The Mekong copy is configured for `1min/`:

```sh
cd map/mekong_15min/script_bin2nc
./s03-convert_highres_netcdf.sh
```

For each selected variable, `s03` calls the shared high-resolution converter. It reads each tile's geometry from `<MAP_DIR>/<HIRES>/location.txt`, and registered variable names, signed type, record index/count, missing value, and dimensions from `variable_table/map_variables.csv`. Unregistered files are ignored; a registered tile with a missing file, inconsistent metadata, or a Binary size that does not match `location.txt` fails explicitly. A multi-record file such as `catmxy` or `downxy` is written as one NetCDF containing all registered variables in record order.

Outputs are individual tiles at `<MAP_DIR>/<HIRES>_nc/<AREA>.<VAR>.nc`; the wrapper also copies `<MAP_DIR>/<HIRES>/location.txt` to `<MAP_DIR>/<HIRES>_nc/location.txt` as geometry metadata. NetCDF variables use `(lat,lon)` dimensions, cell-center coordinates, and the binary's row/record order; data are copied in bounded chunks and checked by reading each chunk back. Output is NetCDF4 without compression. Existing NetCDF outputs are skipped by default; pass `--force` to replace them. The input Binary and source `location.txt` are read-only. A successful conversion verifies file layout and stored values, not scientific correctness or model-reader compatibility.

The NetCDF C library must be installed and discoverable (typically through `nc-config`); Python uses only its standard library.

## Supported formats and dependencies

The workflow supports registered `float32` or `int32` rasters with dimensions `NX,NY` or `NX,NY,NLFP`, including multi-variable direct-access files such as `nextxy.bin` and layered files such as `fldhgt.bin`. Data type, record order, dimensions, missing values, and variable metadata come from the registry and `params.txt`; the Binary inputs are not modified.

Python uses only the standard library. Conversion also requires the NetCDF C shared library to be installed and discoverable, typically through `nc-config`. This workflow validates conversion and stored values; compatibility with model execution is a separate validation step.
