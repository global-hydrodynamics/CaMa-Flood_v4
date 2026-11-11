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

CaMa-Flood uses Flexible Downstream connectivity, so a river cell can link to
a downstream cell beyond the eight immediately neighboring cells (D8). Rivers
may therefore look spatially discontinuous in the `uparea` raster even though
their downstream connections are explicitly represented and treated as
continuous by the model. Such apparent gaps do not necessarily indicate a
disconnected river network or missing data.

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

![Upstream drainage area on the 15-minute grid](figure/f01-rivmap-uparea.png)

### Settings and execution

Edit `f01-rivmap-uparea.sh` to change input paths, the displayed west/east/
south/north edges, colormap, scale (`log` or `linear`), maximum figure
dimensions, DPI, output filename, or Python executable. `MAX_WIDTH` and
`MAX_HEIGHT` are independent maximum dimensions in inches; `0` leaves that
dimension unconstrained. If both are positive, the complete figure is scaled
proportionally to fit within both. If only one is positive, only that limit
constrains the automatically estimated size. If both are zero, a simple
extent-based heuristic chooses a readable size. Suggested maximum dimensions
are 4, 6, 8, and 10 inches (S/M/L/XL). Limits include the title, axes labels,
colorbar, and margins; DPI sets pixel resolution separately. The `p01-f01`
defaults are `MAX_WIDTH=0`, `MAX_HEIGHT=8`, and `DPI=200`.

The Python implementation checks dimensions and coordinates against
`params.txt` and the NetCDF coordinate arrays. The default working output is
`visualization/river_map/figure/f01-rivmap-uparea.png`; it is regenerable and
Git-ignored. The entry point does not create or overwrite a Gallery image.

## p01-f02 Station-to-grid-cell linkage

### Purpose and interpretation

This Figure shows how a station coordinate is associated with the center of a
CaMa-Flood 15-minute grid cell. Panel **f02a** draws each station-to-cell link
with one style. Panel **f02b** uses the linked cell's upstream drainage area
(`uparea`) to scale link color and width, making differences in contributing
area easier to compare. In both panels, the pale raster is the regional
`uparea` map.

The requested view is 98–102°E, 8–20°N. The six Tutorial02 discharge stations
are east of this view, so this Figure instead uses six station locations in
the requested extent from `map/data/GRDC_allocated.csv`. These are selected
for an in-domain illustration and are not the Tutorial02 comparison set.
The station markers use the source's original station coordinates
(`lon_ori`, `lat_ori`); `station_grid_links.csv` records these coordinates and
the corresponding 1-based grid indices. Each linked cell is the 15-minute
cell containing the station coordinate; the scripts check this mapping
against `params.txt` before plotting.

### Inputs, units, and method

The map background comes from the Git-tracked
`map/mekong_15min/netcdf/uparea.nc` (`uparea`, in m²) and
`map/mekong_15min/params.txt`. The plotted drainage-area values are converted
to km² by dividing by 1,000,000. Only finite positive cells are shown, with a
logarithmic background normalization. Panel f02b also uses logarithmic scaling
for its linked-cell colors and line widths; its colorbar reports km². The
figure needs no Tutorial02 run, external download, or simulation output.

![p01-f02 station-to-grid-cell linkage, with panel b styled by linked-cell upstream drainage area](../gallery/p01-f02-station-grid-linkage.png)

### Settings and execution

Run `./visualization/river_map/f02-station-grid-linkage.sh` after the standard
Python setup. The script creates separate panel images and the combined
`figure/f02-station-grid-linkage.png`; all three working images are
regenerable. Edit the Shell settings to change the extent, panel layout,
spacing, maximum complete-figure dimensions, DPI, or Python executable.
The separate Gallery image is a deliberate reviewed copy; rerunning the
workflow does not replace it.
