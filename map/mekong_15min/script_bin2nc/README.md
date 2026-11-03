# Mekong Binary-to-NetCDF map conversion

This map-local copy uses the shared templates in [`map/tools/script_bin2nc/`](../../tools/script_bin2nc/README.md) and converter at `map/tools/convert_map_binary_to_netcdf.py`. It targets the Mekong base-resolution map only; high-resolution subdirectories such as `1min/` and `15sec/` are not scanned.

Run the scripts from this directory (or invoke them by path from another working directory):

```sh
cd map/mekong_15min/script_bin2nc
./s01-list_binary.sh
```

Review `bin2nc_list.txt` in this directory and comment out files that should not be converted. Then, without changing directories, convert the selected files:

```sh
./s02-convert_netcdf.sh
```

The candidate list is created in the current directory and follows numeric `category_id` and registry row order. It lists supported, registered `.bin` files directly in the map root once each. Existing lists are not replaced without confirmation; non-interactive use preserves them. The conversion script prompts before replacing each existing NetCDF, skips it in non-interactive mode, and accepts `--force` for an explicit non-interactive overwrite.

The shared template README documents supported raster layouts, warnings, output verification, and configuration for other maps.

## Legacy consolidated NetCDF files

To create the older two-container input format, run the independent `t01` script:

```sh
./t01-legacy_netcdf_map.sh
```

It creates `netcdf/rivclim.nc` (`nextx`, `nexty`, `lon`, `lat`, `ctmare`, `elevtn`, `nxtdst`, `rivlen`, and all `fldhgt` layers) and `netcdf/rivpar.nc` (`rivwth`, `rivhgt`, `rivman`). Defaults select `nextxy_noedge.nc` and `rivwth_merit.nc`; change the `NEXTXY` or `RIVWTH` setting in the script to choose the alternative individual map. Existing outputs are protected by default; answer `y` to overwrite interactively or pass `--force`. Non-interactive runs skip existing outputs.

Use these containers with `LMAPCDF=.TRUE.` and `CRIVCLINC`/`CRIVPARNC` pointing to the generated files. This is the legacy consolidated reader; ordinary individual `.bin`/`.nc` inputs use `LMAPCDF=.FALSE.` instead. If bifurcation is used, also set `LPTHOUT=.TRUE.` in `&NRUNVER` and retain the `CPTHOUT` path in `&NMAP`. The shared README lists optional legacy variables and their controlling settings: `LGDWDLY` (`gdwdly`), `LSLOPEMOUTH` (`elevslope`), `LSLPMIX` (`mask_slope`), `LMEANSL` (separate `CMEANSLNC`), and MPI region input (`CMPIREGNC`). The generator does not fabricate missing data. Its read-back validation is data-level only; a model run through the legacy path is a separate follow-up.
