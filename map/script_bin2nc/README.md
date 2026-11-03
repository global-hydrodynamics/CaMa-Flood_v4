# Binary-to-NetCDF map conversion

This two-step workflow converts selected base-resolution Binary map rasters into individual NetCDF files. It reuses [`map/tools/convert_map_binary_to_netcdf.py`](../tools/convert_map_binary_to_netcdf.py) and the authoritative [`map_variables.csv`](../map_variables.csv) registry. It does not convert high-resolution subdirectories such as `1min/` or `15sec/`.

## Select files and convert

1. Generate an editable candidate list:

   ```sh
   ./map/script_bin2nc/s01-list_binary.sh
   ```

   The default map is `map/mekong_15min/`; to choose another map, edit `MAP_DIR` near the top of both scripts to the same directory. The list is written to `<map>/netcdf/bin2nc_list.txt`, ordered by numeric `category_id` and then registry row order. A file listed in multiple categories appears once under its first category. Only registered files directly in the map root and supported by the converter are listed. Unregistered binaries and registered but unsupported layouts are reported as warnings. The current `NX,NY,INPN` runoff input matrices are not handled as map rasters and are omitted.

   If a list already exists, `s01` asks before replacing it. In non-interactive use, it always preserves the existing list.

2. Review `bin2nc_list.txt` and comment out every file that should not be converted by placing `#` at the beginning of its line. The generated list starts with all supported candidates selected.

3. Convert the remaining entries in list order:

   ```sh
   ./map/script_bin2nc/s02-convert_netcdf.sh
   ```

   Files are written to `<map>/netcdf/<binary-stem>.nc`. For example, `nextxy.bin` becomes `nextxy.nc` with `nextx` and `nexty`; `fldhgt.bin` retains all registered floodplain layers. The converter checks registry metadata, map dimensions, record sizes, and reads each written variable/layer back for exact value comparison.

4. Review the conversion log and the NetCDF files in `<map>/netcdf/`. Existing outputs are skipped by default in non-interactive use. In a terminal, each existing output prompts for `y/N`; Enter skips it. Pass `--force` to overwrite selected outputs without prompts:

   ```sh
   ./map/script_bin2nc/s02-convert_netcdf.sh --force
   ```

The scripts resolve paths from their own locations and can be called from any working directory. They only inspect `.bin` files directly under `MAP_DIR`; high-resolution subdirectories are never searched.

## Supported formats and dependencies

The workflow supports registered `float32` or `int32` rasters with dimensions `NX,NY` or `NX,NY,NLFP`, including multi-variable direct-access files such as `nextxy.bin` and layered files such as `fldhgt.bin`. Data type, record order, dimensions, missing values, and variable metadata come from the registry and `params.txt`; the Binary inputs are not modified.

Python uses only the standard library. Conversion also requires the NetCDF C shared library to be installed and discoverable, typically through `nc-config`. This workflow validates conversion and stored values; compatibility with model execution is a separate validation step.
