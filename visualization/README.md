# CaMa-Flood Visualization Gallery

Browse CaMa-Flood figures explicitly selected for Gallery publication.
Project-local figures are tracked and documented in their project READMEs;
the combined p01-f01 through p01-f05 figures below are the current
Gallery selections.

## Gallery

### p01 — River Map

#### p01-f01 — Raster River Map

![p01-f01 Raster River Map](gallery/p01-f01-raster-river-map.png)

Four raster panels show Mekong upstream drainage area, basin IDs, basin
display colors, and subbasin display colors. See the
[figure script](river_map/f01-raster_river_map.sh)
and the [project explanation](river_map/README.md#p01-f01--raster-river-map).

#### p01-f02 — Line River Map

![p01-f02 Line River Map](gallery/p01-f02-line-river-map.png)

Three panels compare ways to display CaMa-Flood's local and Flexible
Downstream river connections. See the [figure script](river_map/f02-line_river_map.sh)
and the [project explanation](river_map/README.md#p01-f02--line-river-map).

#### p01-f03 — Vector River Map

![p01-f03 Vector River Map](gallery/p01-f03-vector-river-map.png)

Two panels show outlet-to-outlet main-channel links, with the second panel
adding mapped channel and overland bifurcations and catchment-centroid guides.
See the [figure script](river_map/f03-vector_river_map.sh) and the
[project explanation](river_map/README.md#p01-f03--vector-river-map).

#### p01-f04 — River Map Catchment

![p01-f04 River Map Catchment](gallery/p01-f04-river-map-catchment.png)

Two panels combine 1-minute Mekong unit-catchment pixels with the stored
outlet-based main-channel network and channel bifurcations. Panel a colors
catchments by upstream area; panel b additionally marks source-defined main-
channel and small-stream pixels. Observation stations are omitted. See the
[figure script](river_map/f04-rivmap_catchment.sh) and the
[project explanation](river_map/README.md#p01-f04--river-map-catchment).

#### p01-f05 — High-resolution Background

![p01-f05 High-resolution Background](gallery/p01-f05-high-resolution-background.png)

Six panels compare elevation, a derived eight-neighbor elevation-difference
field, and floodplain height (`flddif`) beneath the same outlet-based
river network. The first row uses grayscale terrain backgrounds and
upstream-area-colored channels; the second uses the `pale_terrain` palette
and black channels. See the [figure script](river_map/f05-hires_background.sh)
and the [project explanation](river_map/README.md#p01-f05--high-resolution-background).

### p02 — Output 2D Maps

#### p02-f01 — River Discharge

![p02-f01 River Discharge](gallery/p02-f01-river-discharge.png)

The two-panel Figure combines a 2001-09-01 Mekong river-discharge raster with
2000–2001 daily discharge hydrographs for Stung Treng and Prek Kdam. The
Tonle Sap series shows modeled flow reversal as negative `outflw`. Its options
are `river_discharge-raster` (a) and `river_discharge-hydrograph` (b); see the
[figure script](output_2d_maps/f01-river_discharge.sh) and the
[project explanation](output_2d_maps/README.md#p02-f01--river-discharge).
