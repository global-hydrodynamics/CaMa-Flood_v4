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
| p01-f01d | Subbasin Color | `subbasin_color` | `subcol` |

### Purpose and interpretation

This four-panel Figure shows upstream drainage area, the original basin ID
field, basin display colors, and subbasin display colors on the same Mekong
15-minute map.
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

The Subbasin Color panel compares `subcol` against the original basin coloring
in `bsncol`. Here a subbasin is a segment of the directed river network produced
by the upstream-area-increase analysis, not necessarily an independent whole
river basin. `subbsn` stores segment IDs; `subcol` stores reusable display-color
indices assigned to those segments. Neither `bsncol` nor `subcol` is a physical
quantity or a unique identifier. These are optional analysis/visualization
products, not required model simulation inputs.

In f01d, **the main fill remains `subcol`**. Thin black lines mark shared
cell edges where two adjacent positive original `basin` IDs differ, showing
higher-level basin boundaries independently of the display colors. These are
categorical grid boundaries, not interpolated numerical contours. Gray cells
are excluded/missing in `nextxy_noedge`; this is a network-validity mask, not a
land/sea classification. Valid negative terminal codes (`-9` and `-10`) remain
valid network cells. A small key identifies the boundary and mask overlays.

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
| f01d | `map/mekong_15min/subcol-30000.bin`: `subcol`, unitless | Read the prepared little-endian int32 raster directly; use the same `tab10` palette and 60% opaque white mix as f01c. Discrete categories only, nearest-neighbor rendering, no log normalization; only present display indices appear in the colorbar. Overlay original basin-ID boundaries in black and the `nextxy_noedge` mask in neutral gray. |

All main and overlay rasters are Git-tracked and share an 80 × 120 grid at
0.25°. Coordinates are cell centers, longitude increases eastward, latitude
decreases southward, and the missing value is `-9999`. NetCDF coordinates are
checked against `params.txt`; binary geometry comes directly from that file.
Unassigned main-fill cells are white, except where the explicit gray no-edge
mask overlays them in f01d. Valid basin IDs are retained exactly.
Repeated raw-ID colors in f01b are a display choice, not adjacency-aware basin
coloring or unique identifiers. Use the numeric raster to identify a basin.
In f01c, neighboring regions with different display indices retain different
colors after lightening; the plot does not recalculate boundaries or indices.

The f01d binary has exactly one 38,400-byte record: 80 × 120 little-endian
int32 values in Fortran array order, with x varying fastest. Its first row is
the northern row, matching the existing map orientation. `-9999` and any
unassigned zero cells are white in the main fill; the no-edge overlay takes
precedence and shows excluded cells in gray. The sample contains display indices 2–6;
non-sentinel values outside the registered 1–10 range are rejected. The paired
`subbsn-30000.bin` has 64 positive segment IDs, plus zero for unassigned network
cells and `-9999` outside the network. Shared colors do not identify a segment;
use `subbsn` for identity. Plotting reads `subcol`, geometry, the original `netcdf/basin.nc` for boundaries,
and `netcdf/nextxy_noedge.nc` for the gray mask. It never recalculates segmentation
or converts the binary to NetCDF. A cell is gray when either downstream-index
component is missing/non-finite; finite terminal codes are not masked. The
no-edge product excludes boundary-affected river networks as prepared by
[`region_cut_domain.F90`](../../map/tools/code/region_cut_domain.F90). Black
boundaries use basin IDs rather than `bsncol` or `subcol`, whose repeated colors
cannot uniquely identify basin units.

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

### Subbasin preparation and threshold meaning

The reusable [s06 subbasin workflow](../../map/tools/script_params/s06-subbasin.sh)
calls [`analysis_subbasin.F90`](../../map/tools/code/analysis_subbasin.F90).
The [Mekong example](../../map/mekong_15min/script_params/s06-subbasin.sh) uses
`AREA_THRESHOLD_KM2=30000` and names the outputs `subbsn-30000.bin` and
`subcol-30000.bin`. A qualifying link is cut when the current cell's upstream
area exceeds 30,000 km² **and** its downstream cell's upstream area exceeds the
current area by more than 30,000 km². This is an upstream-area-increment
segmentation threshold, not a universal minimum subbasin size: the output
segments are not guaranteed to have at least 30,000 km² each. See the
[river-network analysis documentation](../../map/tools/script_params/README.md#river-network-analysis)
for the workflow and full algorithm references rather than treating display
colors as physical basin attributes.

The prepared binary is already distributed with the Git-tracked Mekong sample.
Users do not need to run this generation workflow merely to make the Figure.

### Settings and execution

Run `./visualization/river_map/f01-raster_river_map.sh` after the standard
Python setup. It generates these tracked project assets:

- `figure/f01a-raster_river_map-upstream_area.png`
- `figure/f01b-raster_river_map-basin.png`
- `figure/f01c-raster_river_map-basin_color.png`
- `figure/f01d-raster_river_map-subbasin_color.png`
- `figure/f01-raster_river_map.png`

Defaults are a **2 × 2 grid**, `FIG_HEIGHT=8` inches, automatic `FIG_WIDTH=0`,
and 200 DPI. The complete Figure includes gaps and margins; the wrapper derives
equal panel cells before rendering. Set either physical dimension to zero for
automatic sizing, or specify both to fit the complete Figure in a fixed box.
`GAP`, `MARGIN`, layout and columns remain editable. The common extent stays
90–110°E, 5–35°N with the existing equal-degree raster aspect. Existing f01a–c
scientific calculations, palettes and titles are unchanged. The grid changes
their allocated physical panel size, not their scientific styling.

`SUBCOL_FILE`, `SUBCOL_CMAP` and `SUBCOL_WHITE_MIX` control the new panel;
defaults are the prepared sample, `tab10` and `0.60`, matching f01c. `BASIN_FILE`
also supplies the boundary IDs; `NEXTXY_NOEDGE_FILE` supplies the exclusion mask. All four
panels reuse `script/river_network_layout.py`, reserving identical map and
colorbar rectangles. Colorbars align with map height. Full canvases are saved
without tight cropping, then the shared combiner assembles the Figure without
resizing its content. The published Gallery remains a separately approved
snapshot; this four-panel development update does not replace it.

Run `python visualization/river_map/script/test_raster_river_map.py` to check
raw IDs, binary orientation, masks, matching pastel palettes, labels and common
geometry under legacy limits and the physical sizing modes.

## p01-f02 — Line River Map

Method identifier: `line_river_map`. Vector visualization remains a separate
future method.

| Panel | Option | Option identifier |
| --- | --- | --- |
| p01-f02a | Connect Type | `connect_type` |
| p01-f02b | Enhance Tickness | `enhance_tickness` |
| p01-f02c | Enhance Color | `enhance_color` |

### Purpose and interpretation

All three panels show directed `nextxy` river-network connections over
**99–109°E, 9–19°N**, with the same map geometry and aligned panel heights.
Upstream drainage area is contributing land area, not discharge.

- **f02a:** thicker black arrows show local D8 connections; orange curved
  arrows distinguish nonlocal Flexible Downstream connections. A faint
  upstream-area raster provides context. Flexible arrowheads are slightly
  smaller (7-point mutation scale), with stronger curvature (`rad=0.22`).
- **f02b:** black lines without arrowheads have enhanced thickness representing
  source-cell upstream area. A logarithmic six-class width scale makes larger
  rivers stand out while retaining small links. A legend gives representative
  areas; the faint raster is the same as f02a. Both straight local links and
  curved flexible links omit arrowheads in this trial. Line widths and exact
  endpoints are preserved; flow direction is not explicitly marked in b.
- **f02c:** `cmf_colormap` **shade-rainbow** (`get_cmap("shade_rainbow")`) arrow colors represent source-cell
  upstream area on a logarithmic scale. Moderate area-based widths reinforce
  the network. Precomputed `bsncol` display indices form a darker charcoal-gray
  categorical background, keeping colored arrows in the foreground.

![p01-f02 Line River Map: connectivity, enhanced thickness, and upstream-area color](figure/f02-line_river_map.png)

### Inputs, units, and methods

All inputs are Git-tracked under `map/mekong_15min/`; no simulation output,
station data, external download, or map preparation is needed.

| Input | Meaning and use |
| --- | --- |
| `netcdf/uparea.nc`: `uparea(lat, lon)`, m² | Convert to km²; read source-cell areas for arrows and a faint logarithmic background in a/b. Missing, non-finite and non-positive areas are excluded. |
| `netcdf/nextxy_noedge.nc`: `nextx`, `nexty` | One-based destination indices; paired non-positive terminal/sentinel values do not define links. |
| `netcdf/bsncol.nc`: `bsncol(lat, lon)`, unitless | Precomputed categorical display indices 1–10 for c's gray background. Missing/out-of-range cells stay white. These are not basin IDs or a physical magnitude. |
| `params.txt` | Grid dimensions, resolution and bounds, checked against NetCDF cell-center coordinates. |

The current map contains 3,150 valid links (3,022 local and 128 Flexible
Downstream). Every valid link is passed to plotting; the axes clip the display
without filtering connections by extent. Link direction comes directly from
`nextxy`, never geographic proximity. A link is Flexible Downstream if either
source/destination grid-index offset exceeds one. Local arrows are straight;
flexible arrows curve between their exact original endpoints.

For b and c, normalize log10(source area) over all valid links in the map,
independently of the plotting extent. Divide the normalized range into six
equal classes, with the maximum in the last class. In b, shaft widths range
from 0.008 to 0.032 inches; in c, from 0.008 to 0.022 inches. Local quivers
use actual shaft widths; flexible strokes use the corresponding widths in
points. In b, arrowheads are disabled; c retains its existing
proportional heads. Narrow local links are drawn before wider
ones. The full-map area range is also used for c's continuous logarithmic
colorbar. Inputs are never modified.

The charcoal shades in c range from `#2b2b2b` to `#3b3b3b`, roughly halving
the prior gray RGB intensities while preserving categorical distinctions.
The gray shades in c distinguish display classes only; their brightness has
no ordered basin meaning. Repeated `bsncol` values do not uniquely identify
basins. See [f01 basin preparation](#basin-preparation-and-provenance) for
preprocessing provenance. The background has no separate quantitative scale.

### Settings and execution

After the standard Python setup (including `cmf_colormap`), run:

```bash
./visualization/river_map/f02-line_river_map.sh
```

One wrapper invokes three independent panel scripts, then one unchanged
`visualization/script/combine_panels.py` assembly step. It creates these
tracked project assets:

- `figure/f02a-line_river_map-connect_type.png`
- `figure/f02b-line_river_map-enhance_tickness.png`
- `figure/f02c-line_river_map-enhance_color.png`
- `figure/f02-line_river_map.png`

The option identifiers are maintainer-specified: `connect_type`,
`enhance_tickness` (spelling retained as requested), and `enhance_color`.
The wrapper exposes input paths, common extent, layout, spacing, complete
Figure dimensions, DPI and Python. `FIG_WIDTH=0` selects automatic width;
`FIG_HEIGHT=8` sets the complete Figure height to 8 inches at 200 DPI. The
layout reserves the requested dimensions across panel cells, including gaps
and margins. Set either dimension to `0` for automatic sizing. No Gallery
outputs are created or updated.

#### Virtual-colorbar layout

All three panels reserve identical map and vertical-colorbar rectangles using
`script/river_network_layout.py`. The colorbar axes in a/b are invisible;
c fills the same slot with its area colorbar. All retain equal-degree map
aspect and save the full canvas without tight layout or cropping. The
combiner pads panels to the requested complete-Figure dimensions without
resizing their content.

`FIG_WIDTH` and `FIG_HEIGHT` are independent complete-Figure dimensions in
inches; `0` selects automatic sizing. Fixed dimensions include panel gaps and
margins. With one dimension automatic, the layout uses it to preserve the
map plotting area and its aspect. Existing `--max-width` and `--max-height`
CLI options remain maximum bounds for older direct callers and may resize the
completed image. Each panel prints exported canvas/map/colorbar pixel bounds.

Run `python visualization/river_map/script/test_river_network_layout.py` for
five sizing scenarios across all three real panels, including export geometry,
labels, hidden slots, extent, link counts and foreground encodings.

## p01-f03 — Vector River Map

### Upstream area

This Figure represents the main CaMa-Flood river network using geographic
unit-catchment outlet locations. Each directed connection follows `nextxy`
from the source map cell to its downstream cell, while `lonlat.nc` supplies
the two cells' `outlon`/`outlat` coordinates. It therefore shows the stored
model topology at outlets, not an observed river centerline. Tutorial02
Figure 1c was the implementation reference; this panel keeps only its main
river network and drainage-area encoding.

![Vector River Map: Upstream Area](figure/f03-vector_river_map.png)

Unlike p01-f02, which draws connections between regular computational
grid-cell centers to emphasize local and Flexible Downstream links, p01-f03
connects the corresponding geographic outlet coordinates. `nextxy` alone
defines the downstream relationship; the script does not infer connections
from outlet proximity. Each displayed segment is straight between its exact
source and destination outlet coordinates. Real river paths may curve or
meander between those points, so the lines must not be interpreted as traced
channel centerlines.

The source cell's `uparea` determines both line color and thickness. The
script converts square metres to km² for interpretation, then divides the
log-transformed areas of all valid-link source cells on the full map into
four quartile classes. Q1 is the lowest-area quartile and Q4 the highest;
their numeric cutoffs are calculated from the input map, not fixed physical
thresholds. Classification is independent of the displayed extent.

#### Required map data

| Relative path | Variables and format | Use |
| --- | --- | --- |
| `map/mekong_15min/netcdf/nextxy_noedge.nc` | NetCDF `nextx`, `nexty` (`lat × lon`); one-based destination indices | Selects the downstream map cell for each main-network connection. Non-positive paired values are terminal or sentinel codes and do not define links. |
| `map/mekong_15min/netcdf/lonlat.nc` | NetCDF `outlon`, `outlat` (`lat × lon`); degrees east/north | Supplies geographic outlet coordinates for each source and destination cell. Missing or invalid coordinates are excluded. |
| `map/mekong_15min/netcdf/uparea.nc` | NetCDF `uparea` (`lat × lon`); m² | Selects valid river cells and provides the source-cell drainage area used for quartile class, color, and width. Values are converted to km² for the legend. |
| `map/mekong_15min/params.txt` | Grid dimensions, resolution, and geographic bounds | Checks NetCDF grid shape and coordinate alignment; the vectors are plotted in geographic coordinates. |

All inputs are Git-tracked map products. No simulation output, station data,
external dataset, or map preparation is required. The script validates that
NetCDF grids align, that one-based downstream indices resolve to valid river
cells, and that each rendered arrow begins and ends at the outlet coordinates
of its `nextxy` source and destination.

#### Settings and execution

After the standard Python setup, run:

```bash
./visualization/river_map/f03-vector_river_map.sh
```

Edit the input paths and `WEST`, `EAST`, `SOUTH`, and `NORTH` extent in the
Shell entry point as needed. `FIG_WIDTH` and `FIG_HEIGHT` set the complete
Figure size in inches (`0` selects automatic sizing); `DPI` controls the PNG
resolution. The script writes the panel and single-panel Figure to
`visualization/river_map/figure/`. The Figure-level PNG is a copy of the panel;
no multi-panel composition or Gallery publication occurs.
