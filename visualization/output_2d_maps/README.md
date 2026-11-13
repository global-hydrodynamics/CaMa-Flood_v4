# output_2d_maps

Project number: `p02`.

This project demonstrates standard approaches for visualizing two-dimensional
maps of CaMa-Flood simulation outputs, including downscaled visualization.

## Data source

The input source is the simulation output from
`gosh_test/mek02-2year_netcdf`. The initial visualization date is
2001-09-01.

Simulation data are not copied when this project is created. When developing
each figure, copy or extract only the data required for that figure from the
input source.

## Run a figure

From the repository root, prepare and activate the standard Python environment
once, then run the Figure's Shell entry point:

```bash
./s02-python_setup.sh
source .venv/bin/activate
./visualization/output_2d_maps/f01-river_discharge.sh
```

The entry point resolves repository paths relative to its own location. It
stores compact extracted inputs in `out_sample/` and generated project figures
in `figure/`. The standalone extractor can also be run directly when preparing
samples for later figures.

## p02-f01 — River Discharge

Method: `river_discharge`.

| Option | Option identifier | Variable |
| --- | --- | --- |
| Panel | Option name | Variable |
| --- | --- | --- |
| (a) River discharge raster | `river_discharge-raster` | `outflw` |
| (b) River discharge hydrograph | `river_discharge-hydrograph` | `outflw` |

The Figure contains two panels. Panel (a) shows the full-grid outflow map with
the Stung Treng and Prek Kdam station coordinates and names from
`station_map_locations.csv`. Panel (b) shows the corresponding daily modeled
outflow series, with Stung Treng above Prek Kdam. The panels are also saved
individually for reuse.

### Purpose and interpretation

Panel (a) shows the complete Mekong model grid for the daily mean `outflw`
field at the CF time coordinate 2001-09-01 00:00. CaMa-Flood defines
`outflw` as total discharge (`m3/s`) from the river and floodplain pathways.
The sign follows the model's downstream-positive direction: when a reversed
water-level gradient drives flow upstream, `outflw` becomes negative. The
Tonle Sap River near Prek Kdam is a prominent Mekong example of seasonal flow
reversal. The plotted series are model output, not observed discharge.
The timestamp is the end of the averaging interval, so this record represents
the mean from 2001-08-31 00:00 through 2001-09-01 00:00. Negative values
indicate flow opposite to the model-defined positive direction. Negative and
zero values are retained; finite negative values are not treated as missing.

Panel (a) uses the native 80 × 120 grid at 0.25° resolution, with longitude
centers increasing eastward and latitude rows ordered north to south. The map
extent is 90–110°E and 5–35°N. Cells are drawn without interpolation. Missing
cells are white. The custom `diverge_spectral` palette maps negative flow to
red and orange, zero to pale warm yellow, and positive flow to green and blue.
This diverging palette was selected to make the direction change around zero
easy to see while retaining graduated flow magnitude on both sides.
The Shell entry point exposes the colorbar ticks and range as `CELV`, the
zero-centered linear interval half-width as `LINTHRESH`, and the colormap
fraction assigned to that interval as `LINEAR_FRAC`. The default range is
asymmetric (`−3,000` to `+30,000 m3/s`): the two outer branches are logarithmic
and the interval `−10` to `+10 m3/s` is linear. The central interval occupies
10% of the colormap, with zero exactly at its center. The remaining 90% is
split between the negative and positive logarithmic branches in proportion to
their `log10` spans. For the defaults, the negative side including half the
linear interval occupies about 42.5% of the colorbar, and the positive side
about 57.5%; the zero tick is therefore left of the bar's midpoint. The
registered palette is reparameterized for this figure so zero still receives
its pale-yellow center. `CELV` supplies the colorbar ticks and its first and
last values set the range. Tick positions use the same nonlinear transform as
the map. Both colorbar ends extend to disclose out-of-range values. No offset
is added and values are not clipped in the data.
The default `CELV` is `−3,000, −1,000, −300, −100, −30, −10, 0, 10, 30,
100, 300, 1,000, 3,000, 10,000, 30,000 m3/s`.

### Input data and preparation

The source is `out/mek02-2year_netcdf/o_outflw2001.nc`, produced by the
`gosh_test/mek02-2year_netcdf` simulation. The Shell wrapper
`s01-extract_output_variables.sh` calls
`script/extract_output_variables.py`. It reads the CF time coordinate and
selects the unique midnight record for the requested date; it does not infer
the record index from the start date. For f01, it extracts variable `outflw`
at 2001-09-01 00:00 from `o_outflw2001.nc` and writes a one-record NetCDF
containing `time`, `lat`, `lon`, and `outflw` to
`out_sample/outflw_2001-09-01.nc`. The extracted sample retains the original
coordinate values, units, variable attributes, fill value, and time-coordinate
definition, with provenance attributes identifying the source record.

The same Shell workflow also calls `script/extract_station_outflw.py`. It reads
the six selected locations in `station_map_locations.csv` and extracts each
location's daily modeled `outflw` from `o_outflw2000.nc` and
`o_outflw2001.nc`. The CSV's explicitly labeled zero-based Python row/column
indices are used, and checked against the map-cell centers in the source
NetCDF. CaMa-Flood timestamps daily means at interval end, so each sample is
labeled with the preceding represented calendar day. The resulting 2000–2001
series is written as the long-form
`out_sample/station_outflw_daily_2000-2001.csv` and as
`out_sample/station_outflw_daily_2000-2001.nc` with a `(time, station)` flow
array and station metadata. These are simulated flows sampled at station
locations; they are not observed discharge records.

Each extraction is also appended to `out_sample/extraction_log.csv`. The row
records the variable, requested date and selected CF time, source file and
zero-based record index, destination, grid dimensions, data type, and units.
This makes the extraction reproducible and identifies exactly which output
record was used. The extractor accepts comma-separated `--variables` and
`--dates` lists, so additional variables and dates can be extracted without
changing the Python source. Output filenames use
`<variable>_<YYYY-MM-DD>.nc`; annual source files use
`o_<variable><year>.nc`.

The model output variable registry identifies `outflw` as an alias of
`totout`; in the current model output implementation it combines river and
floodplain discharge and does not include bifurcation outflow. The extracted
record has 6,566 valid cells and 3,034 missing cells. Valid values range
from −12,080.566 to 61,059.098 m3/s, with 84 negative and 226 zero cells.
With the default display range, 15 values are below −3,000 m3/s
(−12,080.567 to −3,006.261 m3/s), and 26 are above +30,000 m3/s
(+31,433.152 to +61,059.105 m3/s).

Panel (b) plots daily modeled `outflw` sampled at the two allocated map cells
for 2000–2001. Stung Treng is the Mekong mainstream location. Prek Kdam on the
Tonle Sap River is used as the reverse-flow example: negative values are
retained and indicate flow opposite to the model-defined positive direction,
as can occur when the water-level gradient reverses. Both hydrographs use the
same line color; station names and panel order identify the series. These are
simulated model samples, not observed station discharge.

### Run and output

Run `./visualization/output_2d_maps/f01-river_discharge.sh`. It invokes the
extraction wrapper, draws both panels, then assembles the complete Figure.
To prepare the dated map sample and two-year station series without drawing
f01, run `./visualization/output_2d_maps/s01-extract_output_variables.sh`; its
defaults are `VARIABLES="outflw"` and `DATES="2001-09-01"`. Edit those settings or pass
`--variables` and `--dates` to select more output variables and CF dates. Edit
`TARGET_DATE` in the f01 wrapper to select another daily CF timestamp. Edit
`CELV`, `LINTHRESH`,
and `LINEAR_FRAC` in the same Shell script to adjust the scale without changing
the Python implementation. `CELV` must be a strictly ascending comma-separated
list containing negative values, zero exactly once, and positive values; its
endpoints must extend beyond `±LINTHRESH`. `LINEAR_FRAC` must be between 0 and
1. The script creates compact NetCDF samples and these PNGs:

- `figure/f01a-river_discharge-raster.png`
- `figure/f01b-river_discharge-hydrograph.png`
- `figure/f01-river_discharge.png` (panels (a) and (b), side by side)

Negative values indicate flow opposite to the model-defined positive
direction.

![p02-f01a River Discharge: Raster](figure/f01a-river_discharge-raster.png)

![p02-f01b River Discharge: Hydrograph](figure/f01b-river_discharge-hydrograph.png)

![p02-f01 River Discharge](figure/f01-river_discharge.png)
