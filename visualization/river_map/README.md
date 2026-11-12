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

## p01-f01 — Raster River Map

Method identifier: `raster_river_map`.

| Panel | Option | Option identifier | Map variable |
| --- | --- | --- | --- |
| p01-f01a | Upstream Area | `upstream_area` | `uparea` |
| p01-f01b | Basin | `basin` | `basin` |
| p01-f01c | Basin Color | `basin_color` | `bsncol` |

### Purpose and interpretation

This three-panel Figure shows upstream drainage area, the original basin ID
field, and precomputed basin display colors on the same Mekong 15-minute map.
Upstream area describes contributing land area, not discharge. **Basin IDs are
categorical identifiers**, not continuous physical quantities: numeric
separation and color ordering have no physical interpretation. The Basin panel
preserves the original stored IDs without replacing or renumbering them.

The Basin Color panel instead displays **`bsncol`**, a precomputed display-color
index derived from basin information. It is not the basin ID. It reuses a
small set of colors, considering neighboring basins, to support clear basin
visualization. Its soft opaque palette is suitable as a background for future
map overlays. Different basins can share a display index; the color map cannot
recover their original IDs. The current sample has 414 basin IDs and five
used display-color indices.

CaMa-Flood uses Flexible Downstream connections beyond immediate grid
neighbors. Apparent gaps in an upstream-area raster do not necessarily mean
that the modeled river network is disconnected; connectivity is stored in
`nextxy` rather than inferred from geographic proximity. No simulation or
observation-station data are required.

![Raster River Map](figure/f01-raster_river_map.png)

### Inputs, units, and methods

| Panel | File and variable | Display |
| --- | --- | --- |
| f01a | `map/mekong_15min/netcdf/uparea.nc`: `uparea(lat, lon)`, m² | Convert to km²; mask missing/non-finite/non-positive values; retain `YlGnBu` with logarithmic normalization over all valid map cells. |
| f01b | `map/mekong_15min/netcdf/basin.nc`: `basin(lat, lon)`, unitless | Display original positive integer IDs with one discrete bin per original integer value. Repeat the qualitative `tab20` palette across IDs; no conversion to `bsncol`. A display-only pseudo-log categorical legend expands IDs 1–20 and labels representative higher IDs. |
| f01c | `map/mekong_15min/netcdf/bsncol.nc`: `bsncol(lat, lon)`, unitless | Display the precomputed indices directly with discrete bins for 1–10. Mix the base `tab10` RGB colors with 60% opaque white to obtain distinguishable pastel colors; only used indices are labeled. |

All three Git-tracked inputs share an 80 × 120 grid at 0.25°. Coordinates are
cell centers, longitude increases eastward, latitude decreases southward,
and the missing value is `-9999`. `params.txt` checks coordinates and map
geometry. Missing cells stay white; valid basin IDs are retained exactly.
Repeated raw-ID colors in f01b are a display choice, not adjacency-aware basin
coloring or unique identifiers. Use the numeric raster to identify a basin.
In f01c, neighboring regions with different display indices retain different
colors after lightening; the plot does not recalculate boundaries or indices.

The f01b legend uses **display-only pseudo-log categorical spacing**. IDs 1–20
each occupy a full slot with an individual label. Above 20, intervals ending
at 30, 40, 50, 70, 100, 150, 200, 300 and the maximum ID share one slot each;
only these representative IDs are labeled. Every integer's swatch still uses
its existing map color, but higher-ID swatches are compressed. This custom
legend changes display geometry only: the map retains its discrete
`BoundaryNorm`, original categorical IDs and repeating palette. There is no
`LogNorm`, continuous scientific scale, or physical meaning to legend spacing.
Tick text adapts to configured panel height to avoid overlaps.

### Basin preparation and provenance

The regional preparation workflow is
[`map/tools/script_region/s01-regional_map.sh`](../../map/tools/script_region/s01-regional_map.sh),
documented in the [regional-map guide](../../map/tools/script_region/README.md).
It calls [`region_set_map.F90`](../../map/tools/code/region_set_map.F90), whose
basin/color section labels network paths, orders basin IDs by upstream-grid
count, and then assigns a small set of display colors using neighboring-color
checks. Single-cell basins have a special display color. The plotted `basin`
values are the original IDs already stored in the prepared map; visualization
does not perform that preprocessing again. See the source for the full rules,
rather than interpreting `bsncol` as a separate physical field or basin ID.

The individual-map NetCDF preparation template is
[`map/tools/script_bin2nc/s02-convert_netcdf.sh`](../../map/tools/script_bin2nc/s02-convert_netcdf.sh),
using [`convert_map_binary_to_netcdf.py`](../../map/tools/code/convert_map_binary_to_netcdf.py).
The three NetCDF inputs are already tracked, so plotting requires neither
conversion nor map generation.

### Settings and execution

Run `./visualization/river_map/f01-raster_river_map.sh` after the standard
Python setup. It generates these tracked project assets:

- `figure/f01a-raster_river_map-upstream_area.png`
- `figure/f01b-raster_river_map-basin.png`
- `figure/f01c-raster_river_map-basin_color.png`
- `figure/f01-raster_river_map.png`

Defaults are a horizontal three-panel arrangement, 6-inch total panel height,
200 DPI, and maximum assembled dimensions of 12 × 8 inches. The common extent
remains 90–110°E, 5–35°N, with the existing equal-degree raster aspect.
The Shell settings expose input files, palettes, white mixing, spacing and
size limits. Titles wrap between method and option to stay readable.

All panels reuse the verified explicit map/colorbar layout from f02 through
`script/river_network_layout.py`, with identical reserved colorbar space and
complete canvases. Each f01 panel has a real colorbar; none reduces map height
or triggers independent tight-layout/cropping. Colorbar ends align with map
axes. The unchanged combiner proportionally fits the full horizontal Figure
to its size limits. Generation does not update the Gallery.

Run `python visualization/river_map/script/test_raster_river_map.py` to check
raw IDs, data masks, pastel distinguishability, labels and common map geometry.

## p01-f02 — Line River Map

Method identifier: `line_river_map`.

This method is named Line River Map by maintainer decision. The separately
planned vector visualization is a different method.

| Panel | Option | Option identifier |
| --- | --- | --- |
| p01-f02a | Connectivity Type | `connectivity_type` |
| p01-f02b | Upstream Area | `upstream_area` |

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
setup. It creates `figure/f02a-line_river_map-connectivity_type.png`,
`figure/f02b-line_river_map-upstream_area.png`, and their combined
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
