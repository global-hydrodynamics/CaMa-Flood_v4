# River Map Visualizations

This project contains raster and network visualizations of CaMa-Flood river maps.
Each Figure has a project-root Shell entry point with editable settings and
Python implementations under `script/`. Working images are written to
`figure/`; only separately authorized copies are published in the Gallery.

## Run a figure

From the repository root, prepare and activate the standard Python environment
once, then run a Figure's Shell entry point:

```bash
./s02-python_setup.sh
source .venv/bin/activate
./visualization/river_map/f01-raster_river_map.sh
```

Entry points resolve repository paths relative to their own location, so they
can also be run from another current directory. Extend this README with a
descriptive section for each Figure added to the project.

## p01-f01 — Four Mekong river-map attributes

### Purpose and interpretation

This Figure compares four static attributes of the Mekong 15-minute river
map: upstream drainage area, precomputed basin display colors, river-cell
channel-top elevation, and MERIT Hydro-merged channel width. It requires no
simulation.

CaMa-Flood uses Flexible Downstream connectivity, so a river cell can link to
a downstream cell beyond the eight immediately neighboring cells (D8). Rivers
may therefore look spatially discontinuous in the `uparea` raster even though
their downstream connections are explicitly represented and treated as
continuous by the model. Such apparent gaps do not necessarily indicate a
disconnected river network or missing data.

![Four Mekong river-map panels: upstream drainage area, basin display colors, channel-top elevation, and MERIT Hydro-merged channel width](figure/f01-raster_river_map.png)

### Inputs, units, and methods

The four NetCDF inputs and their roles are:

| Panel | File and variable | Meaning and units | Display |
| --- | --- | --- | --- |
| **f01a** | `map/mekong_15min/netcdf/uparea.nc`: `uparea(lat, lon)` | Upstream contributing area at river-map cells [m²]. | Converted to km²; non-finite and non-positive values are masked; `YlGnBu` with logarithmic normalization by default. |
| **f01b** | `map/mekong_15min/netcdf/bsncol.nc`: `bsncol(lat, lon)` | Precomputed basin display-color index [unitless]. | Shown as discrete qualitative color categories, without logarithmic scaling. Values are display indices, not basin IDs or physical attributes. |
| **f01c** | `map/mekong_15min/netcdf/elevtn.nc`: `elevtn(lat, lon)` | Unit-catchment river-cell channel-top elevation [m, EGM96]. | Continuous terrain palette and linear normalization; missing values are masked. This is not a complete high-resolution terrain DEM. |
| **f01d** | `map/mekong_15min/netcdf/rivwth_merit.nc`: `rivwth(lat, lon)` | MERIT Hydro-merged river-channel width [m]. | Sequential palette with logarithmic normalization; missing and non-positive values are masked. |

All four files are Git-tracked and use the same 80 × 120 grid at 0.25°.
Their coordinates are cell centers: longitude increases west to east and
latitude decreases north to south. `params.txt` supplies the grid dimensions,
resolution, and map bounds used to verify the NetCDF coordinates. The map
missing value is `-9999`. No download, model run, or map parameterization is
needed to generate this Figure.

Panel f01a masks non-finite and non-positive cells, converts area from m² to
km², and uses nearest-neighbour rendering with `YlGnBu`. Logarithmic
normalization is the default because drainage area spans a wide range; its
limits use all valid cells in the map. Drainage area is not flow rate or
discharge.

Panel f01b displays the stored `bsncol` index directly. The index is a
precomputed visualization product derived from basin classification and
neighboring basin colors; it is not a basin identifier. The regional
preparation implementation is [`region_set_map.F90`](../../map/tools/code/region_set_map.F90),
called by [`s01-regional_map.sh`](../../map/tools/script_region/s01-regional_map.sh).
The source labels basins from network connectivity and assigns display colors
to distinguish neighboring basins where possible. This is a map-generation
workflow, not a channel-parameterization step.

Panel f01c shows channel-top elevation only at river cells. Elevations use
metres relative to EGM96; blank areas are not an elevation estimate.

Panel f01d shows an alternative channel-width product. The parameterization
workflow first estimates channel width from annual-mean discharge with a
power-law relationship, then combines that estimate with MERIT Hydro river
widths to make `rivwth_merit`. It is not necessarily the width selected by the
default CaMa-Flood model configuration. The detailed method is documented in
the [channel-parameter workflow](../../map/tools/script_params/README.md#channel-parameter-workflow)
and implemented by [`s01-channel_params.sh`](../../map/tools/script_params/s01-channel_params.sh).

The panels use the same geographic extent, cell-center coordinates, map
orientation, tick convention, and aspect ratio. The maps display stored
attributes; river connections are represented by `nextxy`, not inferred from
geographic proximity.

### Settings and execution

Run `./visualization/river_map/f01-raster_river_map.sh` after the standard
Python setup. Its settings expose all four input paths, the common map extent,
each panel's palette and applicable scale, the 2 × 2 layout, spacing, panel
and complete-Figure size limits, DPI, and Python executable. The Shell script
creates the four panel PNGs and `figure/f01-raster_river_map.png`; these
project-local figures are tracked and can be regenerated. It does not create
or update a Gallery image.

Each panel script checks its variable, units, dimensions, coordinate centers,
and map geometry against `params.txt`. Continuous color normalization uses
valid data across the full map, not only the visible extent.

## p01-f02 — Directed river-network connectivity

### Purpose and interpretation

CaMa-Flood stores downstream connectivity explicitly in `nextxy`; each
positive `nextx`/`nexty` pair gives the one-based destination grid indices.
Unlike a standard D8 network, Flexible Downstream connections may point beyond
the eight neighboring cells. Panel **f02a** draws every valid connection as a
directed arrow: local D8 links are blue and nonlocal Flexible Downstream links
are orange. Panel **f02b** uses the source cell's upstream drainage area to
scale every arrow's color and width logarithmically. Local links are straight;
Flexible Downstream links are curved to reduce overlap with local arrows. Both
panels show a faint `uparea` raster and use the same 98–110°E, 8–20°N extent.
No observation stations or station-to-cell allocations are used.

The map products are read from `map/mekong_15min/netcdf/uparea.nc`
(`uparea`, m²), `map/mekong_15min/netcdf/nextxy_noedge.nc` (`nextx`, `nexty`),
and `map/mekong_15min/params.txt`. Link direction and destination are taken
from the explicit `nextxy` indices, never inferred from geographic proximity.
All valid links are passed to the plot; the map axes clip those outside the
selected view. The current map has 3,150 valid links: 3,022 local and 128
Flexible Downstream. Of these, 1,374 straight source-to-destination segments
intersect the current extent; the other 1,776 remain in the plotted data but
their direct segments lie outside the view. Curved Flexible Downstream arrows
preserve their exact endpoints while bending consistently to make long links
easier to follow.

![p01-f02 directed CaMa-Flood river-network connectivity panels](figure/f02-line_river_map.png)

### Inputs, units, and method

The Git-tracked `nextxy_noedge.nc` and `uparea.nc` have matching `lat, lon`
dimensions and cell-center coordinates. Positive `nextx` and `nexty` values
are one-based array indices; paired non-positive terminal/sentinel values do
not define a downstream link. A link is classified as Flexible Downstream
when either grid-index offset from source to destination is greater than one.
The background area and source-cell areas are converted from m² to km². The
background and f02b arrow colors use logarithmic normalization. The Figure
needs no Tutorial02 run, external download, or simulation output.

### Settings and execution

Run `./visualization/river_map/f02-line_river_map.sh` after the standard Python
setup. It creates `figure/f02a-line_river_map-connectivity.png`,
`figure/f02b-line_river_map-uparea.png`, and their combined
`figure/f02-line_river_map.png`. All three are tracked project assets. Edit the
Shell settings to change the map inputs, common extent, panel layout, spacing,
maximum complete-Figure dimensions, DPI, or Python executable. Plotting never
filters links by extent: arrows with endpoints outside the view are still
provided to Matplotlib, which clips them at the axes boundary. The project
Figure does not create or update a Gallery image; publication requires
separate explicit authorization.

#### Virtual-colorbar layout

Both f02 panels reserve the same explicitly positioned map and vertical
colorbar axes. The f02a colorbar axes are invisible (no ticks, labels, or
frame); f02b fills that same space with its drainage-area colorbar. Shared
dimensions in `script/river_network_layout.py` keep the map rectangles and
complete PNG canvases identical. Both retain the existing equal-degree map
aspect used by the background raster. Saving preserves the entire canvas,
without tight layout or cropping, so the invisible region survives assembly.
The combiner remains unchanged.

For either panel script, `--max-height H --max-width 0` sets total panel
height to H inches; automatic width includes the map aspect, margins, and
reserved colorbar space. A positive width limit proportionally reduces the
complete panel if necessary. The Shell entry point's `MAX_WIDTH` and
`MAX_HEIGHT` still limit the assembled Figure, including spacing and margins.
Each panel prints its canvas and map/colorbar bounds in exported PNG pixels.
Following successful verification, the maintainer adopted this approach in the
[shared virtual-colorbar rule](../figure_making_guideline.md#virtual-colorbars-for-comparable-panels).
The dimensions above remain specific to f02.

Run the geometry/export regression check from the repository root with:
`python visualization/river_map/script/test_river_network_layout.py`.
