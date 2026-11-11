# River Map Visualizations

This project contains raster visualizations of CaMa-Flood river-map variables.
Each figure has a project-root Shell entry point with editable settings and a
Python implementation under `script/`. Working images are written to
`figure/`; only separately approved copies are published in the Gallery.

## Run a figure

From the repository root, prepare and activate the standard Python environment
once, then run a figure's Shell entry point:

```bash
./s02-python_setup.sh
source .venv/bin/activate
./visualization/river_map/f01-rivmap-uparea.sh
```

Entry points resolve repository paths relative to their own location, so they
can also be run from another current directory. Extend this README with a new
descriptive section for each figure added to the project; do not create a
separate README for an individual figure.

## p01-f01 — Upstream drainage area raster map

### Purpose and interpretation

The figure maps upstream drainage area, the area contributing flow to each
computational grid cell. It shows how contributing area varies across the
Mekong 15-minute map; it is a static map attribute and does not require a
simulation.

### Inputs and units

The required files are the Git-tracked
`map/mekong_15min/netcdf/uparea.nc` and its companion
`map/mekong_15min/params.txt`. The NetCDF variable is `uparea(lat, lon)` in
square metres (`m2`), with ascending longitude centres, descending latitude
centres, and `-9999` as its missing value. The map is 80 × 120 cells at 0.25°.
No download or preprocessing is required.

### Processing and display

The plot masks non-finite and non-positive cells, then converts the remaining
areas to square kilometres (`km²`) by dividing by 1,000,000. It uses nearest-
neighbour rendering on the native grid and the sequential `YlGnBu` palette.
Logarithmic color normalization is the default because drainage area spans a
wide range; colorbar intervals therefore represent multiplicative changes.
Normalization limits come from all valid cells in the full map, including
when the displayed geographic extent is zoomed. Linear normalization is also
available. The map is descriptive and does not by itself indicate flow rate
or discharge.

### Settings and execution

Edit `f01-rivmap-uparea.sh` to change input paths, the displayed west/east/
south/north edges, colormap, scale (`log` or `linear`), DPI, output filename,
or Python executable. The Python implementation checks dimensions and
coordinates against `params.txt` and the NetCDF coordinate arrays. The
default working output is
`visualization/river_map/figure/f01-rivmap-uparea.png`; it is regenerable and
Git-ignored. The entry point does not create or overwrite a Gallery image.
