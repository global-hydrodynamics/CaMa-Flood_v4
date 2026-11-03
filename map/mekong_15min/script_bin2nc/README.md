# Mekong Binary-to-NetCDF map conversion

This map-local copy uses the shared templates in [`map/tools/script_bin2nc/`](../../tools/script_bin2nc/README.md) and converter at `map/tools/convert_map_binary_to_netcdf.py`. It targets the Mekong base-resolution map only; high-resolution subdirectories such as `1min/` and `15sec/` are not scanned.

Run the scripts from this directory (or invoke them by path from another working directory):

```sh
cd map/mekong_15min/script_bin2nc
./s01-list_binary.sh
```

Review `../netcdf/bin2nc_list.txt` and comment out files that should not be converted. Then convert the selected files:

```sh
./s02-convert_netcdf.sh
```

The candidate list follows numeric `category_id` and registry row order. It lists supported, registered `.bin` files directly in the map root once each. Existing lists are not replaced without confirmation; non-interactive use preserves them. The conversion script prompts before replacing each existing NetCDF, skips it in non-interactive mode, and accepts `--force` for an explicit non-interactive overwrite.

The shared template README documents supported raster layouts, warnings, output verification, and configuration for other maps.
