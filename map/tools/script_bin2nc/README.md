# Binary-to-NetCDF map conversion template

This shared template converts selected base-resolution Binary map rasters into individual NetCDF files. Copy the scripts into a map-local `script_bin2nc/` directory and run that copy. The Mekong sample contains a ready-to-use copy at [`map/mekong_15min/script_bin2nc/`](../../mekong_15min/script_bin2nc/).

The scripts reuse [`convert_map_binary_to_netcdf.py`](../convert_map_binary_to_netcdf.py) and the authoritative [`map_variables.csv`](../../map_variables.csv) registry. They do not convert high-resolution subdirectories such as `1min/` or `15sec/`.

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

## Supported formats and dependencies

The workflow supports registered `float32` or `int32` rasters with dimensions `NX,NY` or `NX,NY,NLFP`, including multi-variable direct-access files such as `nextxy.bin` and layered files such as `fldhgt.bin`. Data type, record order, dimensions, missing values, and variable metadata come from the registry and `params.txt`; the Binary inputs are not modified.

Python uses only the standard library. Conversion also requires the NetCDF C shared library to be installed and discoverable, typically through `nc-config`. This workflow validates conversion and stored values; compatibility with model execution is a separate validation step.
