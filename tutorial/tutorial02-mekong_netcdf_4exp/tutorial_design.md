# Tutorial 02: Mekong Multi-Year NetCDF Simulation and Four-Experiment Comparison

**Status:** Researcher-approved four-experiment design, implemented and run for 2000–2003, with output-based checks and reference figures. The learner start includes the working Default script but intentionally omits the three completed alternatives; learners create them by copying and editing configurations. This is a model-behavior comparison, not an observational validation. Scientific interpretation of the selected 15-minute cells and spin-up adequacy remains for maintainer review; see Appendix A.

## 1. Purpose and learning outcomes

This tutorial uses the CaMa-Flood v5 Mekong sample to demonstrate a four-year simulation with annual Restart handoffs and a controlled comparison of river routing, bifurcation flow, and floodplain representation. Users begin with the Default experiment, then copy its script and change a small number of clearly identified settings to create three variants.

By the end, users should be able to:

1. Find the map, runoff, input-matrix, experiment, and output directories used by a v5 run.
2. Read the experiment settings in a GOSH script and explain the main physical switches.
3. Run annual NetCDF runoff, model-output, and Restart workflows.
4. Carry model state across annual runs and retain each year’s results.
5. Create independent experiment variants by copying and editing a configuration.
6. Extract discharge at selected Mekong gauge locations and compare model simulations.
7. Compare 2001 spatial and network patterns of annual maximum absolute daily-mean discharge.
8. Compare the 2000 spin-up-year seasonal hydrograph with 2001–2003, while recognizing that both initial conditions and annual runoff differ and the plot cannot establish spin-up completion.
9. Examine a Mun River tributary, Tonle Sap flow-reversal potential, and Tien and Hau/Bassac delta bifurcation sensitivity at selected model cells.
10. Identify major river channels from upstream drainage area stored in `uparea`.
11. Explain that `nextxy` can connect non-adjacent grid cells and why rasterized network lines may appear spatially discontinuous.
12. Distinguish computational grid-cell centers from unit-catchment outlet coordinates, and identify main-channel, channel-bifurcation, and overland-bifurcation connections.
13. Explain how high-resolution `catmxy` pixels are assigned to coarse unit catchments and how each unit catchment is associated with its mapped outlet.

## 1.1 Required learner activities

The supplied learner starting point is the working `mek_default.sh`, visualization source scripts, shared map/runoff/input-matrix data, and approved reference images in `sample_figure/`. The distributed `figure/` directory contains no generated figures. The completed No Bifurcation, Kinematic Wave, and No Floodplain scripts, their model outputs, and generated figures are intentionally absent.

Learners must copy and edit the Default configuration to create the other three experiment scripts, assign each a distinct `PROJ`, and change only the approved model switches shown in Section 2. They then run the four experiments and the supplied comparison workflow. The transferable skill is creating independent, reproducible CaMa-Flood configurations while preserving shared scientific settings. Completion is observable when the three new scripts use separate output directories, all four configurations have the required outputs, and the comparison figures are generated.

The complete alternative scripts are retained as developer references in `design/reference_mek_*.sh`; this GitHub-only directory is excluded from the learner package. `design/manual_design.md` defines the intended future teaching sequence. A full learner manual does not yet exist, so distribution readiness and learner reproduction remain partial.

The tutorial uses GRDC and MRC station metadata only to locate model grid cells. It does not use or compare observed discharge records.

## 2. Experiment design

The four configurations and their control settings are:

| Experiment | Routing | Floodplain storage representation | `PROJ` | `LKINE` | `LFPLAIN` | `LPTHOUT` | `LADPSTP` |
|---|---|---|---|---|---|---|---|
| Default | Local Inertial | Enabled | `mek_default` | `.FALSE.` | `.TRUE.` | `.TRUE.` | `.TRUE.` |
| No Bifurcation | Local Inertial | Enabled | `mek_nobif` | `.FALSE.` | `.TRUE.` | `.FALSE.` | `.TRUE.` |
| Kinematic Wave | Kinematic Wave | Enabled | `mek_kine` | `.TRUE.` | `.TRUE.` | `.FALSE.` | `.FALSE.` |
| No Floodplain | Kinematic Wave | Disabled | `mek_nofld` | `.TRUE.` | `.FALSE.` | `.FALSE.` | `.FALSE.` |

All four experiments use the same map, runoff, input matrix, simulation dates, output settings, `DT=600` seconds, and other physical parameters. The researcher-approved scheme-specific options are:

- `DT=600` seconds in every experiment.
- `LLEAPYR=.TRUE.` to keep the model calendar aligned with the Gregorian CF runoff time axis, including February 29, 2000.
- `LADPSTP=.TRUE.` for the two Local Inertial cases and `.FALSE.` for the two Kinematic Wave cases. The source warns that adaptive stepping is recommended only with Local Inertial routing; Kinematic Wave should use a suitable fixed step.
- `LPTHOUT=.TRUE.` for Default and `.FALSE.` for the other three cases. Source validation permits bifurcation with Local Inertial routing and stops if it is combined with Kinematic Wave.
- Keep `LFLDOUT=.TRUE.` across all four runs. `LFPLAIN` controls whether floodplain capacity is represented during map initialization; `LFLDOUT` is a separate high-water channel-flow switch. No Floodplain changes `LFPLAIN` only.
- Keep `LSLPMIX=.FALSE.`, reservoir operation off, and all other optional schemes at the same values in every run.

The source accepts `DT=600`: `CMF_CONFIG_CHECK` requires a timestep of at least 60 seconds and an integer multiple of 60. It warns, rather than stopping, if adaptive time stepping is enabled with Kinematic Wave. Actual stability and suitability of the approved timestep/options still require execution evidence.

The Kinematic Wave routing code computes channel discharge from Manning velocity using the local water depth and a slope bounded below by `PMINSLP`; available channel storage limits discharge during a timestep. It can also route floodplain flow when `LFLDOUT` is enabled. The No Floodplain experiment turns off the floodplain storage representation through `LFPLAIN=.FALSE.` while retaining the Kinematic Wave routing configuration.

Interpret the comparisons as follows:

- **Default vs. No Bifurcation:** both use Local Inertial routing, floodplain representation, adaptive time stepping, and `DT=600`; only bifurcation flow changes. This comparison examines bifurcation routing.
- **No Bifurcation vs. Kinematic Wave:** both have floodplain representation and bifurcation flow off, but routing formulation and adaptive-time-step setting both change. This is not a perfectly isolated routing-equation comparison.
- **Kinematic Wave vs. No Floodplain:** both use Kinematic Wave, no bifurcation, and fixed time stepping; only `LFPLAIN` changes. This examines floodplain storage representation within that configuration.
- **Default vs. Kinematic Wave** and **Default vs. No Floodplain** change multiple settings and must not be interpreted as isolating one physical mechanism.

Distinguish changes to physical formulations (`LKINE`, `LPTHOUT`, `LFPLAIN`) from the numerical time-stepping choice (`LADPSTP`).

Do not predict the sign or magnitude of discharge differences. Interpret them only after execution and inspection of the results.

## 3. Domain and input data

### 3.1 River map

Use the existing map at `map/mekong_15min/` without copying its products into the tutorial directory. `params.txt` describes an 80 × 120 grid at 0.25 degrees, with edge bounds 90–110°E and 5–35°N.

Use the individual NetCDF map products under `map/mekong_15min/netcdf/` with `LMAPCDF=.FALSE.` (individual-file mode):

| Namelist setting | File | Role |
|---|---|---|
| `CNEXTXY` | `nextxy_noedge.nc` | River-network downstream indices |
| `CGRAREA` | `ctmare.nc` | Unit-catchment area |
| `CELEVTN` | `elevtn.nc` | Channel-top elevation |
| `CNXTDST` | `nxtdst.nc` | Distance to downstream catchment |
| `CRIVLEN` | `rivlen.nc` | River length within the catchment |
| `CFLDHGT` | `fldhgt.nc` | Layered floodplain elevation profile |
| `CRIVWTH` | `rivwth_merit.nc` | MERIT-derived river width |
| `CRIVHGT` | `rivhgt.nc` | River-channel depth |
| `CRIVMAN` | `rivman.nc` | River Manning roughness |

`params.txt`, the runoff-grid descriptor `inpdim_test-mek_1deg.txt`, and the map-specific `inpmat_test-mek_1deg.bin` remain text/Binary support inputs. Reuse the existing 20 × 30 runoff-to-map input matrix; do not regenerate or duplicate it for the NetCDF runoff representation. `bifprm.nc` is required for Default (`LPTHOUT=.TRUE.`) and not required for the other three experiments.

`LMAPCDF=.TRUE.` selects the separate legacy consolidated NetCDF map interface and must not be used for these individual files.

### 3.2 Runoff

Use the annual NetCDF dataset `inp/test-mek_1deg_nc/`, derived without rescaling from the tracked daily Binary sample `inp/test-mek_1deg/`. The NetCDF runoff field is `runoff(time, latitude, longitude)`, float32, daily `mm day-1`, on a 20 × 30 grid covering the same 90–110°E, 5–35°N extent. The files have CF daily time bounds and a `proleptic_gregorian` calendar. `DROFUNIT=86400000` converts the stored daily depth units to the model’s `m/s` forcing rate; `CROFFREQ="YEAR"`, `CVNTIME="time"`, and `CVNROF="runoff"` select calendar-year files and records. Use the Binary input matrix above.

Annual inputs are `runoff2000.nc` through `runoff2003.nc`. The current branch tracks NetCDF files for 2000–2005, so all four tutorial years are available in a clean checkout. The existing converter supports generating later years from the tracked daily Binary source; the 2006–2009 files are not needed for this tutorial.

### 3.3 Annual simulation and Restart sequence

Run one calendar year per model invocation, with an exclusive January 1 endpoint:

| Segment | Start | Exclusive end | Runoff records |
|---|---|---|---:|
| Spin-up | 2000-01-01 00:00 | 2001-01-01 00:00 | 366 |
| Analysis 1 | 2001-01-01 00:00 | 2002-01-01 00:00 | 365 |
| Analysis 2 | 2002-01-01 00:00 | 2003-01-01 00:00 | 365 |
| Analysis 3 | 2003-01-01 00:00 | 2004-01-01 00:00 | 365 |

For the 2000 segment, set `LRESTART=.FALSE.`. The current initialization first zeros prognostic storage arrays, then initializes river-channel storage along the network so water surface does not fall below the downstream boundary elevation. Therefore, describe this as a cold start without a Restart, not as a guaranteed all-zero storage state.

For 2001–2003, set `LRESTART=.TRUE.` and read the preceding segment’s final Restart. Use `LRESTCDF=.TRUE.` and `IFRQ_RST=0` to write a NetCDF Restart at the end of each segment. With `CVNREST="restart"` and the output directory as `CRESTDIR`, the expected handoff names are `restart2001010100.nc`, `restart2002010100.nc`, and `restart2003010100.nc`. The Restart reader restores river and floodplain storage and the previous-step hydraulic state required by a Normal Restart; enabled scheme states are additionally included when their options are on.

The final endpoint is 2004-01-01 00:00 exclusive. All four experiments completed the four annual segments. Each 2001–2003 log records reading the preceding year's NetCDF Restart, and the corresponding year-end Restart was written. Output interval timestamps map to an uninterrupted 2000-01-01 through 2004-01-01 daily sequence with no gaps or duplicates. See Appendix A.3 for the run evidence.

## 4. Output and comparison methods

### 4.1 Discharge output

Write `outflw`, the river-plus-floodplain discharge mean in `m3/s`, directly through the model’s NetCDF output path for all experiments. The Default experiment also writes `pthflw`, the mean discharge for each bifurcation path and layer; its other three configurations disable bifurcation routing and do not request this variable. Use `LOUTCDF=.TRUE.`, `LOUTVEC=.FALSE.`, and `IFRQ_OUT=24`. Both registry entries are time means, and the NetCDF writer records `cell_methods="time: mean"`. The variables are float32 in NetCDF-4 with the configured compression level. Annual maximum absolute maps therefore represent the maximum magnitude of daily-mean discharge in 2001, not an instantaneous or sub-daily peak.

Set a year-specific `COUTTAG` for each annual run. The current filename rule is `o_` + variable name + tag + `.nc`, so a run with `COUTTAG="2000"` writes `o_outflw2000.nc`. Keep all files for each experiment in its independent directory:

```text
tutorial/tutorial02-mekong_netcdf_4exp/out/
├── mek_default/
├── mek_nobif/
├── mek_kine/
└── mek_nofld/
```

Each experiment directory holds its annual discharge files, dated Restarts, year-specific Namelists, and logs. The repository-root `out/` directory is not used. Generated run artifacts remain untracked.

The model labels daily averages at the end of their averaging interval. Since `CMF_TIME_NEXT` advances to the end time before diagnostics are written, the first daily record is time-stamped 2000-01-02 00:00 and represents the interval beginning 2000-01-01; the final record in a segment is stamped at the exclusive January 1 endpoint. A visualization should label or shift these interval-ending timestamps consistently rather than treating them as daily midpoint values.

Each annual output contains 366 records for 2000 and 365 for each of 2001–2003: 1,461 records per experiment and 16 annual NetCDF discharge files total. All files have shape `(time, latitude, longitude) = (days, 120, 80)`, `outflw` units `m3/s`, and finite unmasked values. The first record is stamped January 2 and represents January 1; the last is stamped the exclusive January 1 boundary and represents December 31.

### 4.2 Selected model locations

These six selected sites are simulation-output sampling locations, not observation-validation sites. Four GRDC allocations supply station locations and their one-based model indices. My Thuan uses the original geographic coordinates recorded for GRDC ID 2369005; that row has no allocated model indices. Its model cell is therefore resolved through the 1-minute catmxy map at the documented point, not copied from the GRDC allocation fields. Can Tho has no selected GRDC allocation here; its geographic reference is the Mekong River Commission (MRC) Water Quality Monitoring station H039803. That report labels the site as Bassac River; this tutorial refers to the corresponding Hau/Bassac delta reach. No observed discharge series is used.

| Location | River / objective | Location source | Model cell `(ix,iy)` | Model-cell center (lon, lat) | Mapped drainage area (km²) |
|---|---|---|---:|---:|---:|
| Vientiane | Mekong mainstream; upper-reach discharge and flood-wave propagation | GRDC 2469072; allocated coordinates 102.613°E, 17.929°N | (51, 69) | 102.625°E, 17.875°N | 304,814.28 |
| Ubon Ratchathani | Mun River tributary; tributary response | GRDC 2969200; allocated coordinates 104.863°E, 15.221°N; allocation metadata identifies the Mun | (60, 80) | 104.875°E, 15.125°N | 106,544.10 |
| Stung Treng | Mekong mainstream in Cambodia; large basin after major tributary contributions | GRDC 2569005; allocated coordinates 105.946°E, 13.537°N | (64, 86) | 105.875°E, 13.625°N | 637,963.19 |
| Prek Kdam | Tonle Sap River; investigate modeled reverse-flow potential | GRDC 2569020; allocated coordinates 104.804°E, 11.821°N; allocation metadata identifies Tonle Sap | (60, 93) | 104.875°E, 11.875°N | 87,707.56 |
| My Thuan | Tien River delta reach; compare with the Hau/Bassac reach under bifurcation | GRDC 2369005 original coordinates 105.900°E, 10.273°N; GRDC row has no allocated ix,iy; mapped with 1-minute catmxy | (64, 99) | 105.875°E, 10.375°N | 1,931.65 |
| Can Tho | Hau/Bassac delta reach; inspect bifurcation sensitivity | MRC WQ station H039803, 105.79775°E, 10.0580°N | (64, 100) | 105.875°E, 10.125°N | 773,786.94 |

Cell centers and downstream indices were read from the Mekong 15-minute map. The individual NetCDF `nextx`/`nexty` values match `nextxy_noedge.bin`; all six selected cells have valid downstream connections. For the four allocated GRDC sites, the table reports the allocation file's `area_CaMa`; for My Thuan and Can Tho, it reports the map's upstream drainage area at the selected cell. These are upstream drainage areas, not the local `ctmare` unit-catchment areas. At Ubon, the chosen allocated cell is identified as the Mun tributary and drains to the next cell (61,79), distinct from the Mekong mainstream location. Prek Kdam drains first to (60,94), where mapped upstream area increases from about 87,708 to 751,186 km²; following `nextxy` downstream reaches the selected Can Tho cell (64,100) after seven links. This confirms a modeled connection through the Tonle Sap/delta network, not a detailed validation of river geometry.

The 0.25-degree grid is coarse relative to station locations. The allocated/model-cell center differs from the station coordinate by a fraction of a cell, and the map's catchment outlet location can differ further from the station point. Can Tho's geographic coordinate and selected model-cell center are deliberately listed separately. The model grid represents this as a delta reach rather than a point gauge or tidal station.

For My Thuan, the map's high-resolution pixel at the documented coordinate is one-based pixel (955,1484); the two `catmxy` records assign it to model cell (64,99). The selected Can Tho point maps to cell (64,100). Their model cells differ, their primary `nextxy` destinations differ—My Thuan (64,99) to (66,101), Can Tho (64,100) to (64,101)—and the associated outlet pixels for both points map back to their assigned cells. This verifies two distinct map reaches for the comparison. It does not prove that either 15-minute cell resolves the local gauge channel or the real-world flow partition.

For Can Tho, `bifprm.nc` contains bifurcation paths 337 and 338 beginning at selected cell (64,100), with downstream destinations (65,100) and (65,101). This establishes that the selected cell is directly associated with configured branches, but only the Default/No Bifurcation output comparison can establish the simulated effect there. It does not establish agreement with real distributary flow.

My Thuan cell (64,99) is the destination of configured paths 326, 327, and 331; `width_1` classifies 327 and 331 as channel paths and 326 as overland. At Can Tho, path 337 is overland and path 338 is a channel bifurcation. These map relationships distinguish the sampled Tien and Hau/Bassac reaches in the configured network. Classification follows the first-layer pathway width: `width_1 > 0` is a channel path; `width_1 <= 0` is an overland path. The figures and output comparisons examine model sensitivity to this topology, not the accuracy of the observed branch partition.

The local-inertial routing computes signed flow in the direction of the map's downstream connection, so a negative `outflw` can represent reverse flow. The Kinematic Wave implementation constrains slope to a positive minimum before calculating downstream flow; it is therefore not expected to represent reversal in the same way. Whether negative flow occurs at Prek Kdam in these runs remains an empirical question. These sites represent the researcher's intended reach contrasts, but their suitability for broader floodplain inference and the ability of this coarse map to resolve local river geometry remain limitations for scientific review. Geographic source coordinates are not discharge observations.

The Can Tho coordinate and Bassac River label are from the [MRC 2018 Lower Mekong Water Quality Monitoring Report](https://www.mrcmekong.org/publications/2018-lower-mekong-water-quality-monitoring-report/). The [MRC PMFM overview](https://pmfm.mrcmekong.org/?showtemplate=1) describes the Tonle Sap River's seasonal reverse-flow connection to the Mekong. These sources establish site context only; neither supplies observed discharge to this tutorial.

### 4.3 Four-panel spatial river-network visualization

The four panels build from a grid attribute to the stored network topology and catchment geometry. Together they show how a regular computational grid represents river cells, downstream connectivity, geographically located unit-catchment outlets, additional bifurcation paths, and irregular high-resolution catchments. The supplied `v01-network_maps.py` script reconstructs the shown links from map products; it does not estimate river connections from geographic proximity.

#### Required Map Data

| Panel | Required product (relative to repository root) | Format, variable, and resolution | Role and conventions |
|---|---|---|---|
| (a) | `map/mekong_15min/netcdf/uparea.nc`; `map/mekong_15min/params.txt` | NetCDF `uparea(lat,lon)`, float32 in m², 80 × 120 at 0.25°. | Colors each computational cell by upstream drainage area over the full model domain. `params.txt` supplies grid dimensions and edge bounds; NetCDF `lon`/`lat` are cell-center coordinates. Station crosses connect to model-cell centers. |
| (b) | `map/mekong_15min/netcdf/uparea.nc`, `map/mekong_15min/netcdf/nextxy_noedge.nc`; `map/mekong_15min/params.txt` | NetCDF `uparea(lat,lon)` in m² and integer `nextx(lat,lon)`, `nexty(lat,lon)`; 80 × 120 at 0.25°. | `nextx`/`nexty` are one-based destination grid indices, not geographic offsets. Positive pairs identify downstream cells; `-9` and `-10` are special terminal/boundary codes, while `-9999` is missing/undefined. Every valid link is drawn, including nonlocal links that extend beyond the eight neighboring cells. Arrows follow the stored source-to-destination indices; only major local links and all nonlocal links receive arrowheads to limit clutter. Station crosses connect to grid centers. |
| (c) | `map/mekong_15min/netcdf/nextxy_noedge.nc`, `map/mekong_15min/netcdf/lonlat.nc`, `map/mekong_15min/netcdf/bsncol.nc`, `map/mekong_15min/netcdf/bifprm.nc`; `map/mekong_15min/1min/1min.catmxy.bin`, `map/mekong_15min/1min/location.txt`; `map/mekong_15min/params.txt` | Grid products are 80 × 120. `lonlat.nc` stores outlet coordinates `outlon(lat,lon)` and `outlat(lat,lon)` in degrees. `bsncol.nc` stores integer display classes 1–5 in `bsncol(lat,lon)`; `-9999` is no-data. `bifprm.nc` has 382 paths and five width layers; `ix`, `iy`, `jx`, `jy` are one-based endpoint-cell indices, and `width(layer,path)` is in metres. `catmxy` contains two int16 records with one-based coarse x/y assignments; `location.txt` defines the 1-minute pixel centers. | `nextxy` supplies main-channel connectivity; each valid link is drawn as an arrow from the source outlet to the downstream outlet, with arrow width increasing across four classes of source-cell `uparea`. The panel extent is 102–108°E, 8–15°N. `bsncol` is the precomputed categorical basin-color raster and is displayed directly; categories are display colors, not physical basin attributes. For this map, layer-1 width greater than zero classifies a channel bifurcation; zero width classifies an overland bifurcation. Channel-bifurcation arrows use four colors based on quantiles of `width(1,path)`, following Figure 1d; the legend uses one representative entry. Main-channel legend entries are also consolidated because their uparea classes are visually intuitive. Bifurcation arrows originate at the `lonlat` outlet for `(ix,iy)` and end at the outlet for `(jx,jy)`. Gray headwater guides connect the mean center coordinates of valid 1-minute pixels assigned to cells with no incoming main-channel link to those cells' `lonlat` outlets. These guides are visual aids, not resolved flow paths. The regional map has edge cells without incoming links whose upstream connectivity outside the map is unavailable; they are classified from the available network and reported as ambiguous candidates. Station coordinates and outlet markers are shown without a connector. The grid-boundary legend symbol is an outlined square, matching the gridded map background. Although `bifprm.nc` also contains start/end longitude and latitude fields, the start fields do not match the source-cell outlets in this map. The file supplies no intermediate path geometry, so plotted bifurcation connections are endpoint-to-endpoint arrows. |
| (d) | `map/mekong_15min/1min/1min.catmxy.bin`, `map/mekong_15min/1min/location.txt`, `map/mekong_15min/netcdf/uparea.nc`, `map/mekong_15min/netcdf/lonlat.nc`, `map/mekong_15min/netcdf/nextxy_noedge.nc`, `map/mekong_15min/netcdf/bifprm.nc`, `map/mekong_15min/params.txt` | Binary `catmxy` has two direct-access int16 records on a 1200 × 1800 1-minute grid: record 1 is `catmxx`, record 2 is `catmyy`. `location.txt` gives geographic bounds and resolution. `bifprm.nc` supplies one-based source/destination cells (`ix`, `iy`, `jx`, `jy`) and `width(layer,path)`. | High-resolution pixel indices start at the northwest; x increases eastward and y southward. Each pixel stores its assigned coarse one-based `(ix,iy)` cell in the paired records; `-999` marks a pixel not assigned to a unit catchment and is shown in light gray; `-9999` is the file's undefined value. Coloring uses the assigned coarse cell's `uparea`; boundaries are drawn where neighboring assignments change. `lonlat` supplies outlet positions and `nextxy` supplies directed links. Main-channel line color and width scale with source-cell `uparea`; channel-bifurcation paths start at the outlet of their indexed source cell and end at the indexed destination outlet, with progressively darker colors and thicker lines for larger pathway widths in `bifprm.nc`. Station locations connect to their catchment outlets; model-grid center markers are omitted. The plotted extent is 104.5–106.8°E, 9.4–11.8°N, covering the Phnom Penh-area bifurcation context and downstream delta stations. |

These products represent distinct information: `uparea` is a model-grid attribute; `nextxy` is primary network connectivity; `lonlat.nc` gives geographic outlet locations; `bsncol.nc` supplies precomputed categorical basin coloring; `bifprm.nc` supplies bifurcation endpoint-cell indices and channel/overland classification; and `catmxy` describes high-resolution pixel assignment to coarse unit catchments. Grid centers, unit-catchment outlets, station reference coordinates, and bifurcation endpoints are not interchangeable locations. In panels (b)–(d), arrowheads point along stored topology rather than inferring flow from map orientation. Panel (b) retains every valid connection, especially nonlocal links that explain why the raster network need not be limited to adjacent cells; arrows clarify direction without omitting links. Panel (c) displays `bsncol` as a subdued categorical raster, draws every valid `nextxy` connection as an outlet-to-outlet arrow, and anchors bifurcation arrows at the source-cell outlet resolved from `(ix,iy)`. Its `bifprm.nc` records do not provide intermediate route geometry, so the figure shows direct endpoint connections. Panel (d) colors and widths visible main-channel lines by source-cell `uparea`, resolves both channel-bifurcation endpoints from the indexed cell outlets in `lonlat.nc`, and scales bifurcation color and line width to pathway width in `bifprm.nc`; unassigned (`-999`) high-resolution pixels are light gray and station markers show the station coordinates and catchment outlets without grid-center circles. The panel (d) extent is expanded to show the Phnom Penh-area branching context together with the delta reaches. Its lighter green color scale uses logarithmic normalization across the valid data range without clipping drainage-area values. The visualization script checks map descriptors and selected `catmxy` assignments before drawing. A future Tutorial 02 manual should include this table and explain which product controls each visible element; manual generation is a separate task and is not part of this specification update.

Panel (a) introduces upstream drainage area as a raster attribute across the full 15-minute domain and relates station coordinates to grid centers. Panel (b) uses explicit links and direction arrows to teach stored connectivity, retaining all ordinary and nonlocal connections; the nonlocal links are essential because they reveal connections beyond the eight neighboring grid cells. Panel (c) focuses on 102–108°E, 8–15°N and relates the regular grid to geographic outlets, precomputed `bsncol` basin-color classes, and directed main-channel, channel-bifurcation, and overland-bifurcation connections. Main-channel arrows follow `nextxy` between cell outlets; four source-cell `uparea` classes use progressively thicker and darker blue arrows, all wider than the thin gray headwater guides. Channel-bifurcation arrow colors follow four quantile groups of the pathway width parameter `width(1,path)`, as in Figure 1d. Bifurcation arrows begin at the outlet of their indexed source cell. Headwater guides connect the mean 1-minute pixel centroid of each cell with no incoming main-channel link to its mapped outlet; they do not represent resolved river paths. Station coordinates and outlet markers appear without a connector in this panel. The main-channel and channel-bifurcation legends each use one representative entry; the grid-boundary key uses a square outline. Panel (d) expands from the lower delta to the Phnom Penh-area bifurcation context and downstream stations, showing how many 1-minute pixels form irregular 15-minute unit catchments and how outlets connect through the stored network. Its main-channel lines also use four progressively thicker and darker blue classes based on source-cell `uparea`; channel-bifurcation lines begin at the outlet resolved from the indexed source grid cell. Gray station connectors in (a), (b), and (d) link station references to grid centers or mapped catchment outlets. Arrows in (b)–(d) communicate the direction encoded by `nextxy` and bifurcation records, not an estimate based on geographic proximity.

The map products contain 6,566 cells with positive upstream area and 3,150 valid downstream connections, including 128 nonlocal links. The bifurcation file contains 77 channel and 305 overland paths under the layer-1 width classification. The 1-minute map assigns 1,416,132 pixels to those 6,566 coarse cells. In the displayed delta extent, all 21 outlet points checked by the script map back to their assigned coarse cells. Across the full map, however, 143 of 6,507 outlet positions within the 1-minute domain fall in pixels whose `catmxy` assignment is a different cell. The figures do not resolve why these map products differ at those locations; this remains a data-provenance/resolution question for review, not a correction made here.

**Learning checks**

- (a) Identify a major channel by following cells with larger upstream area.
- (b) Find a nonlocal `nextxy` link and explain why it need not join adjacent grid centers.
- (c) Distinguish a grid-cell center from its outlet, then identify a main-channel link and the two bifurcation classes.
- (d) Explain how `catmxy` assigns multiple high-resolution pixels to one coarse unit catchment and how its outlet is located.

### 4.4 Spin-up visualization and interpretation

For the Default experiment, Figure 2 compares daily discharge for 2000–2003 at the six selected locations on a common January–December seasonal axis. The 2000 curve is labeled as the spin-up year and drawn more prominently; the other years remain visible for comparison. Users should examine seasonal timing and the magnitude and shape of differences among years, while considering both initialization and forcing variability. Differences do not prove that spin-up is complete because runoff forcing also varies among years.

The 2000 input and model calendar include February 29. The seasonal overlay omits February 29 and maps the other dates to a common non-leap calendar; the source NetCDF retains all 366 daily records. Interannual runoff differs, so this visualization is descriptive and does not demonstrate spin-up sufficiency.

### 4.5 Four-experiment hydrograph comparison

Figure 3 compares daily `outflw` from 2001-05-01 through 2002-03-31 at the six selected model cells. Its 3 × 2 panels are ordered Vientiane, Ubon Ratchathani (Mun), Stung Treng, Prek Kdam (Tonle Sap), My Thuan (Tien), and Can Tho (Hau/Bassac), reading left to right and top to bottom. Every panel uses this same continuous period and its own y-axis range; negative discharge is retained, especially at Prek Kdam. Default is emphasized in the shared legend. Users should compare the four experiment curves, focusing on Tonle Sap flow reversal and how bifurcation affects the Tien and Hau/Bassac delta reaches. These are simulated model differences, not validation against observed discharge or proof of real-world flow partition.

### 4.6 Spatial and network comparison of annual maximum absolute discharge

Figure 4 compares 2001 in a 3 × 4 layout with columns Default, No Bifurcation, Kinematic Wave, and No Floodplain. Row 1 shows `outflw` on the computational grid; Row 2 shows the same annual maximum on the geographic outlet-to-outlet network; Row 3 shows differences between adjacent configurations. Its three maps are centered between their corresponding configuration panels: left `Default − No Bifurcation` (**Impact of bifurcation scheme**), center `No Bifurcation − Kinematic Wave` (**Impact of local inertial equation**), and right `Kinematic Wave − No Floodplain` (**Impact of floodplain activation**). Read the sequence from right to left to follow the requested progression from No Floodplain toward Default. The unused fourth Row 3 slot remains blank. In Rows 2 and 3, bifurcation connections are drawn first, followed by main-channel segments sorted by increasing upstream contributing area so that larger downstream channels are drawn last and remain visible. At every grid cell or path, the absolute-value statistic is `max_t |Q(t)|` over the complete 2001 daily-mean records. `outflw` is in `m3/s`; its source implementation defines it as `D2RIVOUT + D2FLDOUT`, excluding bifurcation outflow (`cmf_calc_stonxt_mod.F90`, `CMF_CALC_STONXT`). Thus each main-channel segment is colored from the value at its source cell. Daily NetCDF records are interval means stamped at their end; 2001 contains 365 represented daily intervals from January 1 through December 31.

Default additionally draws the 77 map-classified channel-bifurcation connections using each connection's `pthflw` channel-layer (`NPTHLEV` layer 1) annual maximum absolute daily mean. The output file `out/mek_default/o_pthflw2001.nc` stores `(time, NPTHLEV, NPTHOUT)` in NetCDF order. Its one-based `pth_id` and endpoint indices are checked against `map/mekong_15min/netcdf/bifprm.nc`; coordinates for each line use the source and destination cell outlets, matching Figure 1c. The file carries all 382 configured path rows and five vertical layers; the figure shows the channel layer where `width(layer 1,path) > 0`. This is the channel geometry used in Figure 1c, not a synthesized estimate.

Only Default enables bifurcation routing (`LPTHOUT=.TRUE.`). No Bifurcation, Kinematic Wave, and No Floodplain disable it; their absolute-value network panels correctly show main-channel connections only. The absence of bifurcation vectors in those panels is a physical configuration difference, not missing diagnostic output. No `pthflw` output is requested for those experiments. In Row 3, each quantity is `Δ(max_t |Q|) = (left configuration) − (right configuration)` on main-channel links only, using the panel labels above. Thin gray dashed bifurcation paths are positional references and do not encode a difference value.

The two discharge variables use daily time means and have matching 365-record coordinates for 2001. Rows 1 and 2 use `shade_rainbow` and the same positive logarithmic scale from 1 m³/s to the largest positive `outflw` or channel-layer `pthflw` maximum, with one colorbar per row. Their colorbars use the fixed labeled ticks 1, 2, 5, 10, 20, 50, 100, 200, 500, 1,000, 2,000, 5,000, 10,000, 20,000, and 50,000 m³/s. Positive values below 1 m³/s use a labeled under-range, while exact zeros are gray and identified separately because zero cannot be placed on a logarithmic axis. Row 3 uses `diverge_rainbow` with symmetric log-like bins at ±10, ±30, ±100, ±300, ±1,000, ±3,000, and ±10,000 m³/s, plus its own colorbar; values outside ±10,000 use the end colors. The raster extent is 98–109.5°E by 8.5–22°N and uses a geographic aspect correction so its panel width aligns with the network rows. The `shade_rainbow` raster style is retained; network rows use Figure 1c's geographic extent and no direction arrows. Rows 2 and 3 include a grayscale 1-minute `elevtn` background transformed with `log1p` and clipped at local 2nd/98th percentiles; a darker gray range makes lowlands stand out. Segment maxima are calculated independently and need not be simultaneous. Because the absolute statistic uses magnitudes, its color does not encode flow direction. This is a configuration comparison, not an observational validation or an accuracy ranking.

## 5. Tutorial structure and deliverables

The visualization workflow is numbered in execution order. Run
`v00-visualize_all.sh` to generate the following figures:

| Step | Script | Figure outputs |
|---|---|---|
| v01 Network | `v01-network_maps.py` | `figure/f01a-upstream_area.png`, `figure/f01b-grid_network.png`, `figure/f01c-outlet_bifurcation.png`, `figure/f01d-delta_catchments.png` |
| v02 Spin-up | `v02-spinup_comparison.py` | `figure/f02-spinup_comparison.png` |
| v03 Discharge | `v03-discharge_comparison.py` | `figure/f03-discharge_comparison.png` |
| v04 Annual maximum | `v04-annual_max_discharge.py` | `figure/f04-annual_max_discharge.png` |

The network script currently emits four related map panels, so all are retained
under the `f01a`–`f01d` subfigure convention. The other steps use `f02`, `f03`, and `f04` prefixes. Figure 3 is the six-panel discharge comparison.

The Tutorial 02 README provides the Python setup and visualization quick start. This design document remains the detailed source for experiment settings, map inputs, and scientific interpretation. A step-by-step learner manual with Learning Checks remains future work. The GitHub development contents retain completed solutions under `design/`; a future learner package must exclude that directory.

## Appendix A. Technical verification and open questions

### A.1 Verification table

| Setting or requirement | Verified behavior | Evidence | Status |
|---|---|---|---|
| `LKINE` routing | `CMF_PHYSICS_ADVANCE` selects `CMF_CALC_OUTFLW_KINE`; the routine applies a Manning kinematic relation to river depth and slope, with a minimum-slope bound and available-storage discharge cap. | `src_main/cmf_ctrl_physics_mod.F90` (`CMF_PHYSICS_ADVANCE`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| `LFPLAIN` | `CMF_TOPO_INIT` sets river storage capacity to a very large value when false, instead of the channel cross-section capacity used when true. The Kinematic routine separately controls floodplain discharge with `LFLDOUT`. | `src_main/cmf_ctrl_maps_mod.F90` (`CMF_TOPO_INIT`); `src_main/cmf_opt_outflw_mod.F90` (`CMF_CALC_OUTFLW_KINE`) | Code-verified |
| Four experiment option compatibility | `LKINE=.TRUE.` with `LPTHOUT=.TRUE.` stops configuration. Adaptive timestep with Kinematic Wave warns but does not stop. `DT=600` passes the configured minimum and minute-multiple checks; the approved combinations completed the sample runs. | `src_main/cmf_ctrl_nmlist_mod.F90` (`CMF_CONFIG_CHECK`); 16 run logs | Code- and runtime-verified for this sample; not a general stability guarantee |
| Cold-start state | `CMF_PROG_INIT` zeros prognostic arrays, then `STORAGE_SEA_SURFACE` fills river storage to avoid an initial water surface below downstream boundary level. | `src_main/cmf_ctrl_vars_mod.F90` (`CMF_PROG_INIT`, `STORAGE_SEA_SURFACE`) | Code-verified |
| Annual Restart continuity | Each experiment wrote NetCDF Restart files at 2001–2004 Jan 1 boundaries; logs for 2001–2003 show reading the preceding boundary file. | `src_main/cmf_ctrl_restart_mod.F90`; `out/<experiment>/log_CaMa_*.txt`; restart NetCDF signatures | Four-year annual handoffs runtime-verified |
| Runoff NetCDF | Grouping `YEAR` resolves `runoffYYYY.nc`; the reader matches CF time metadata, verifies calendar/interval/coverage and requires the first time record to start at the year boundary. The variable is read as `(NXIN,NYIN,time)` through the Fortran NetCDF interface. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_RESOLVE_FILE`, `FORCING_UTIL_GET_CDF`); `inp/test-mek_1deg_nc/README.md`; `runoff_datasets.csv` | Code-verified; 2000–2005 files tracked, including all Tutorial 02 years |
| Runoff units and matrix | Runoff is converted by dividing by `DROFUNIT`; `CINPMAT` supplies the spatial weights. The annual NetCDF values are documented as daily `mm day-1`; use `86400000` and the existing Binary matrix. | `src_main/cmf_ctrl_forcing_mod.F90` (`FORCING_UTIL_ROFF_INTERP`); `inp/test-mek_1deg_nc/README.md`; `map/mekong_15min/inpdim_test-mek_1deg.txt` | Code- and data-verified |
| Individual NetCDF map inputs | `LMAPCDF=.FALSE.` selects per-file extension-based readers. The nine map files loaded in every annual segment are NetCDF; the dimension descriptor and input matrix remain text/Binary support inputs. | `src_main/cmf_ctrl_maps_mod.F90`; four experiments' run logs; `map/mekong_15min/netcdf/` | Code-, data-, and runtime-verified |
| Daily mean `outflw` and `pthflw` | Both registry entries select daily means and NetCDF Float32; `pthflw` is output only when `LPTHOUT` is enabled and carries static pathway IDs/endpoints. The Default 2001 output was regenerated from the existing year-start Restart; its path IDs/endpoints and daily time axis match `bifprm.nc` and `outflw`. | `variable_table/output_variables.csv`; `src_main/cmf_ctrl_output_mod.F90`; `out/mek_default/o_pthflw2001.nc`; `map/mekong_15min/netcdf/bifprm.nc` | Code-, data-, and runtime-verified for the 2001 sample |
| Annual maximum absolute discharge | Figure 4 computes `max_t |Q(t)|` for the grid and network rows, then adjacent-configuration differences (`Default − No Bifurcation`, `No Bifurcation − Kinematic Wave`, and `Kinematic Wave − No Floodplain`) for main-channel links. Their titles identify impact of bifurcation scheme, local inertial equation, and floodplain activation. The three difference maps are centered between the configuration panels they compare. The 1-minute elevation backdrop is cropped to the Figure 1c extent. Absolute rows share a logarithmic range; the difference row uses symmetric log-like bins and explicit end colors beyond ±10,000 m³/s. | `v04-annual_max_discharge.py`; generated figure; four 2001 `outflw` files, Default `pthflw`, and 1-minute `elevtn` | Implemented and output-verified for the 2001 sample |
| Selected sampling locations | Four GRDC records supply allocated one-based cells; My Thuan is resolved from GRDC 2369005's original coordinate through `catmxy`, and Can Tho uses the MRC WQ station coordinate. Their cells and primary downstream links differ; configured bifurcation paths connect the delta reaches. | Section 4.2; GRDC allocation files; map NetCDF/Binary; `bifprm.nc`; MRC 2018 WQ report | Geographic locations and model-grid/network mapping verified; scientific interpretation remains open |
| Four-panel network visualization | Panels use `uparea`, `nextxy`, `lonlat`, `bifprm`, and 1-minute `catmxy` to show grid attributes, connectivity, outlets, bifurcations, and catchment assignment. Figure 1c additionally draws centroids of connectivity-defined headwater cells to their outlets. | `v01-network_maps.py`; products listed in Section 4.3; generated figures | Existing map relationships verified; headwater centroid calculation checked on sample data, but updated figure rendering remains pending because Matplotlib and NetCDF4 are unavailable in this workstation environment. Map-edge headwater candidates may be truncated by the regional domain; whole-map outlet/catmxy mismatches remain unexplained. |
| Spin-up effect | The four years ran and the seasonal overlay was generated. Interannual runoff differs, preventing the plotted differences from isolating an initial-condition transient or establishing spin-up sufficiency. | `v02-spinup_comparison.py`; Default annual output | Runtime complete; scientific interpretation unresolved |

### A.2 Decisions and unresolved prerequisites

1. **Interpretation of selected sites:** The six selected cells have verified map connectivity and generated hydrographs. The maintainer should review whether the 0.25-degree cells adequately represent the intended Mun, Tonle Sap, mainstream, Tien, and Hau/Bassac scientific contrasts.
2. **Spin-up adequacy:** The Default output shows a seasonal overlay but combines initial-condition differences with differing annual runoff. The runs do not establish whether the 2000 cold start is sufficient for later analysis.
3. **Timestep/scientific suitability:** The approved `DT=600` combinations ran to completion without logged errors or non-finite unmasked output. This operational result is not an independent stability or scientific-accuracy assessment.
4. **Observed-flow validation:** No observed discharge series was used. The negative Prek Kdam simulation values and large, oppositely signed Default-minus-No-Bifurcation differences at My Thuan and Can Tho need scientific interpretation against site and grid limitations, not an accuracy claim.
5. **Outlet and high-resolution catchment correspondence:** In the plotted delta extent the checked outlets map back to the expected coarse cells, but this is not true for 143 of 6,507 outlet positions inside the 1-minute domain. The source and intended interpretation of these whole-map mismatches are not established; do not infer a correction from proximity alone.

### A.3 Execution record

- Build: `./gosh_sample/compile_netcdf.sh` succeeded on macOS Apple Silicon with GNU Fortran 14.2.0, NetCDF-Fortran 4.6.2 / NetCDF-C 4.9.3, OpenMP, and `SinglePrec_CMF`. Existing compiler warnings reported unused imports in `cmf_ctrl_restart_mod.F90` and `MAIN_cmf.F90`, plus a duplicate NetCDF link flag; none prevented the build.
- Execution: four scripts ran 2000–2003 sequentially, each with `OMP_NUM_THREADS=4`, producing 16 successful annual model calls. Wall time totals were 231 s (Default), 186 s (No Bifurcation), 115 s (Kinematic Wave), and 110 s (No Floodplain), about 10m42s combined. Per-year elapsed time and status are retained locally under `out/<experiment>/run_time_YEAR.log`.
- Output checks: all 16 NetCDF files contain expected record counts, `(time,latitude,longitude)` dimensions `(days,120,80)`, `outflw` units `m3/s`, no unmasked non-finite values, and a gap-free represented daily timeline of 1,461 days per experiment. The interval-ending January 1 records were assigned to the prior year's December 31 interval.
- Restart checks: each experiment wrote four NetCDF Restart boundary files from 2001-01-01 through 2004-01-01. Logs show each 2001–2003 invocation reading the preceding year's Restart. NetCDF signatures and paths were checked.
- Smoke check: an isolated one-day Default namelist using the same map, runoff, NetCDF output, and Restart interfaces completed and produced one daily output record plus an end Restart.
- Scientific observations: at Prek Kdam, Default had 302 negative days and No Bifurcation 312 of 1,095 days; Kinematic Wave and No Floodplain had none. Default-minus-No-Bifurcation discharge was positive on all 1,095 days at My Thuan (mean +518 m3/s; maximum absolute difference 2,522 m3/s) and negative on all 1,095 days at Can Tho (mean -5,171 m3/s; maximum absolute difference 21,504 m3/s). These are simulated sensitivity results; no observation-based validation was performed.
- Figures: the spin-up comparison, six-site 2001-05-01–2002-03-31 discharge comparison, 3 × 4 annual-maximum/difference figure, and four network-map panels are generated by the visualization scripts. Figure 4 uses the four 2001 `outflw` files, Default's 2001 `pthflw`, and the 1-minute elevation map; the two absolute-value rows have separate colorbars with a shared logarithmic scale, and the difference row has a separate symmetric divergent scale. The map panels use stored connectivity and catchment assignments, not proximity-derived links.
- Limit: successful execution and data checks establish that this workflow ran as configured, not that the configurations are scientifically accurate or that the coarse map resolves all local river processes.

## Appendix B. Development, distribution, and Benchmark integration

### B.1 GitHub and beginner package

The GitHub development checkout keeps complete alternatives under `design/`, along with reset and run evidence. The learner-facing starting files retain only `mek_default.sh`; `mek_nobif.sh`, `mek_kine.sh`, and `mek_nofld.sh` are ignored learner-created files. Approved reference images are in `sample_figure/`; generated figures belong in `figure/`. Model outputs and logs are ignored under `out/`.

No tutorial package assembler currently exists. `doc/make_package.sh` packages the legacy v4 distribution and is not appropriate for these tutorials. The repository can therefore be reviewed through a temporary learner snapshot, but official package compliance remains partial until a suitable distribution process is established. The learner manual also remains a separate, uncompleted authoring task; the README supplies only environment and visualization setup.

### B.2 Optional Benchmark v2 use

The 2001–2003 discharge series may be considered as a small Benchmark v2 dataset after Benchmark v2's scientific and data contracts have been reviewed. No extraction, tracking, or Benchmark integration is authorized here; the tutorial remains complete without that downstream use.
