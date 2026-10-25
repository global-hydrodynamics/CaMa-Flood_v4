# Result map plotting utilities

`result_mapping/` contains small Python examples for inspecting saved CaMa-Flood output grids. They make one image from an output file; they do not read the map geometry or make publication-ready maps with coastlines.

## Programs and inputs

- [`plot.py`](plot.py) reads a raw binary file as native-endian float32 and reshapes it to `(records, ny, nx)`. It averages over all records, masks values below zero and above `1e9`, then plots the resulting 2-D grid.
- [`plot_nc.py`](plot_nc.py) reads a NetCDF file and selected variable with `netCDF4`. It expects coordinate variables named `lat`/`lon` or `latitude`/`longitude`, averages the variable over its first dimension, and flips the rows when latitude is ascending.

Both scripts need the grid dimensions to match the data. For binary input, `nx` and `ny` must come from the map used to produce the output (for example, the first two dimensions in `map/<map-name>/params.txt`). Binary files have no embedded dimensions, coordinates, units, or missing-value metadata. The code treats the first dimension as a sequence of records and does not select a timestep.

## Usage

The checked-in shell examples run from this directory and use repository-relative sample paths:

```sh
sh s01-mapping_plot_binary.sh
```

That example plots `outflw2000.bin` using dimensions read from the global 15-minute map. For NetCDF, see [`s02-mapping_plot_netcdf.sh`](s02-mapping_plot_netcdf.sh), which selects `outflw` from the sample file.

Direct arguments are:

```text
python plot.py <binary-file> <ny> <nx>
python plot_nc.py <netcdf-file> <variable-name>
```

## Dependencies and outputs

Install NumPy and Matplotlib for both scripts; `plot_nc.py` additionally requires `netCDF4`. Each script writes fixed filenames in the current directory: `plot.py` overwrites `test1.jpg` and `test1.pdf`; `plot_nc.py` overwrites `test2.jpg` and `test2.pdf`.

The plots use a fixed `YlOrRd` color map. The binary plot caps its color scale at 50000; both scripts use a placeholder color-bar label (`To be set`). Values below zero and above `1e9` are masked, but other sentinels are not recognized. Confirm the selected variable's units and missing-value convention before interpreting the image.

## Limitations / Future Maintenance

These scripts are minimal legacy examples: they average every record, hard-code output names and much of the styling, and do not validate grid shape against coordinates. A future update could expose timestep selection, output paths, units/labels, and missing-value settings as arguments.
