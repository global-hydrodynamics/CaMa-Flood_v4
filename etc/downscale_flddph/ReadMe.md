# Flood-depth and flood-duration downscaling

## Overview

These tools postprocess saved CaMa-Flood floodplain depth (`flddph`) into high-resolution inundation maps. They combine the simulation's river map with its corresponding high-resolution catchment assignments and relative elevations; they do not run another hydrodynamic simulation.

For the scientific method and tributary-inundation illustrations, see [Note_Downscaling.docx](../../doc/Note_Downscaling.docx) by Dai Yamazaki and the CaMa-Flood development team. The instructions below describe the supplied scripts and their current file conventions.

## Available workflows

| Entry point | Processing | Numerical result |
| --- | --- | --- |
| [`s01-downscale_flddph.sh`](s01-downscale_flddph.sh) | Downscale each selected daily flood-depth record, then plot it. The active method includes tributary inundation. | One high-resolution depth map per day, in metres. |
| [`s11-downscale_duration.sh`](s11-downscale_duration.sh) | Count depth-threshold exceedances on the model grid for one year, map those counts to high-resolution pixels, then plot them. | One annual duration map, in days except for the permanent-water marker described below. |

Both examples use `map/glb_15min`, outputs from `out/test1-glb_15min`, a Mekong-area crop (102–108°E, 9–15°N), and `RES=1min`. The depth example selects September 2000; the duration example selects the full year 2000. Adapt the settings to your files before execution.

## Requirements and compilation

Run from `etc/downscale_flddph/` in a GitHub checkout or extracted distribution package. Starting at the model root:

```sh
cd etc/downscale_flddph
make -C src all
```

[`src/Makefile`](src/Makefile) includes `adm/Mkinclude` and uses its `FC`, `FFLAGS`, and include settings. Configure an available Fortran compiler using the [build guide](../../adm/README.md). The targets are `igetday`, `downscale_flddph`, `downscale_flddph_trib`, `calc_depth-duration`, `downscale_fldprd`, `conv_flood`, `conv_slp`, and `conv_slp_dur`; executables are written under `src/`.

### Input files

| Input, relative to this working directory | Requirement |
| --- | --- |
| `out/flddphYYYY.bin` | Daily floodplain-depth output in metres: one full model-grid, four-byte real record per day, beginning January 1. The wrappers do not read NetCDF or subdaily output. |
| `map/params.txt`, `map/nextxy.bin` | Dimensions, geographical definition, and catchment network matching the simulation. The active tributary method reads both integer coordinate records of `nextxy.bin`. |
| `map/uparea.bin` | Gridded upstream drainage area, additionally required by `downscale_flddph_trib` to identify mainstem connections. |
| `map/<RES>/location.txt` and `<tile>.catmxy.bin`, `<tile>.flddif.bin`, `<tile>.rivwth.bin` | Tile metadata, model-catchment assignment, relative elevation, and river-width/permanent-water mask for the requested resolution and region. |
| `<tile>.downxy.bin`, or the implemented `<tile>.flwdir.bin` fallback | High-resolution downstream connections, additionally required by the tributary method. These must use the map's expected encoding. |
| `<tile>.hand.bin` | Required by the plotting helpers to prepare a shaded background; not used by the numerical depth/duration calculations. |

The top-level scripts create `map` and `out` symbolic links from `MAPDIR` and `OUTDIR`. High-resolution files must correspond to the same river map, rather than merely sharing a resolution label. The tributary method reads a buffer extending **five model-grid cells on each side** of the target crop, so check high-resolution coverage beyond the final output bounds as well.

Even a partial-month depth run uses records numbered from January 1 in the annual input file. Duration processing reads the first 365 or 366 daily records according to `YEAR`; provide a complete year with valid values at active model cells. The tools do not inspect a time axis or validate missing records/data.

### Visualization dependencies

The plotting scripts require a `python` command with NumPy and Matplotlib, including `mpl_toolkits.axes_grid1`. These dependencies and the HAND background are optional for numerical postprocessing, but **both top-level scripts call plotting unconditionally**. Numerical-only use requires preparing the links/work directories and invoking the lower-level stages with the arguments shown in their source. The duration plotter uses Matplotlib's default backend; select an appropriate backend for a headless system.

## Flood-depth downscaling

After reviewing settings and the cleanup notes below, the entry point is `sh s01-downscale_flddph.sh`.

```text
out/flddphYYYY.bin + map and high-resolution layers
  → t01-downscale.sh → downscale_flddph_trib
  → flood/flood_YYYYMMDD.bin
  → t02-draw_flddph.sh → conv_slp + u02-flddph.sh
      → conv_flood + draw_flddph.py → fig/flddph_YYYYMMDD.jpg
  → move flood/ and fig/ to flood_<TAG>/ and fig_<TAG>/
```

[`t01-downscale.sh`](t01-downscale.sh) loops over calendar dates, selects records by day of year, and launches the Fortran downscaler for each requested day. The baseline pixel depth is `max(0, model_flddph - pixel_flddif)` for assigned catchment pixels. A river-width mask imposes a minimum 0.1 m depth at permanent-water pixels.

The active [`downscale_flddph_trib`](src/downscale_flddph_trib.F90) method additionally follows high-resolution connections, distinguishes mainstem and tributary reaches using upstream drainage area, increases tributary depths using upstream/downstream depth information, and applies a backwater adjustment within affected catchments. This is diagnostic postprocessing; its method is illustrated in the scientific note.

The commented call to [`downscale_flddph`](src/downscale_flddph.F90) in `t01-downscale.sh` shows the mainstem/outlet-only alternative. Selecting it requires changing the executable call in that script; there is no top-level method-selection variable.

The plotting chain prepares a HAND-derived background (`slp.bin`) and a display-only depth grid (`dphYYYYMMDD.bin`). **`conv_flood` modifies depths for display**, including scaling/capping, maximum aggregation, and color-anchor values; on wide grids it also spreads wet pixels for visibility. Use `flood_<TAG>/flood_YYYYMMDD.bin` for quantitative depth analysis, rather than the display grid or JPEG.

## Flood-duration downscaling

After reviewing settings and cleanup behavior, the entry point is `sh s11-downscale_duration.sh`.

```text
out/flddphYEAR.bin → calc_depth-duration
  → dat/fldprdYEAR.bin (model-grid depth-exceedance table)
  → t11-downscale.sh → downscale_fldprd → flood/fldprdYEAR.bin
  → t12-draw_duration.sh → conv_slp_dur + draw_duration.py
      → fig/fldprdYEAR.jpg
  → move flood/ and fig/ to flood_<PROJ>/ and fig_<PROJ>/
```

1. [`calc_depth-duration`](src/calc_depth-duration.F90) counts daily records with `flddph > max(0.01, k × INTDPH)` at each active model cell, for `k=0…int(MAXDPH/INTDPH)`. Each exceedance adds one day; it does not interpolate within a day. The first threshold is 0.01 m, even though the threshold series starts at `k=0`.
2. The coarse table contains `int(MAXDPH/INTDPH)+1` threshold records. The shell variable `NREC` represents this **threshold count**, not the number of daily inputs. The third argument passed to `calc_depth-duration` is separately `NDAY` (365/366).
3. [`downscale_fldprd`](src/downscale_fldprd.F90) selects the table record at `min(int(flddif/INTDPH)+1, NREC)` for each pixel with nonnegative relative elevation. This is a discrete lookup, not interpolation. Relative elevations above the table range use the highest threshold; negative relative elevations are assigned zero.
4. Pixels identified as permanent water by the river-width mask are assigned a value of at least **999**. Treat this as a marker, not 999 days of inundation. Unassigned output pixels start at `-9999`.

This workflow maps a model-grid depth-duration relationship. It neither accumulates the daily high-resolution maps from the first workflow nor applies that workflow's tributary/backwater adjustments. Choose `MAXDPH` large enough for the relative elevations/depth range relevant to the analysis and review the permanent-water mask when interpreting durations.

## Configuration parameters

### Shared spatial and input settings

| Setting | Meaning and constraints |
| --- | --- |
| `MAPDIR` | Path to the simulation's river map, relative to the working directory or absolute for your installation. Used as the target of `map`. |
| `OUTDIR` | Directory containing annual daily `flddphYYYY.bin` files. Used as the target of `out`. |
| `WEST`, `EAST`, `SOUTH`, `NORTH` | Edges of the **high-resolution output crop**, in longitude/latitude degrees. Require `WEST < EAST`, `SOUTH < NORTH`, and map/tile coverage. The supplied Python plotters parse these as integers, so use integer-degree bounds with the complete workflows. |
| `RES` | High-resolution map subdirectory/tag. The complete depth-plus-plot and duration workflows share `1min`, `30sec`, `15sec`, and `3sec` branches. Required layers must actually exist at that resolution. Some depth executables also recognize `1sec` and `5sec`; the plotters and duration downscaler do not. |

The target crop and `RES` are separate from the original model grid. Choose bounds aligned to the high-resolution lattice so `(EAST-WEST)/pixel_size` and `(NORTH-SOUTH)/pixel_size` give integral output dimensions. A resolution label alone does not provide the topographic data needed for that region.

### Flood-depth script

| Setting | Meaning; supplied example |
| --- | --- |
| `TAG` | Suffix for final `flood_<TAG>/` and `fig_<TAG>/`; `mek`. |
| `SYEAR`, `SMON`, `EYEAR`, `EMON` | Inclusive first/last months selected from the annual daily input files; September 2000. |
| `NGRID` | Positive integer plotting aggregation factor; `1`. It does not change the numerical downscaling resolution. Choose a factor dividing both output dimensions; the display converters also require at least two output rows for their color anchors. |
| `MAXDPH` | Plotting depth scale in metres; `10.0`. Also affects the display-only transformation in `conv_flood`; it does not cap raw downscaled depths. |

### Flood-duration script

| Setting | Meaning; supplied example |
| --- | --- |
| `PROJ` | Suffix for final `flood_<PROJ>/` and `fig_<PROJ>/`; `mek_day`. The internal plot/file `TAG` is set to `YEAR`. |
| `YEAR` | Year of the annual daily input file; `2000` (366 records). |
| `MAXDPH` | Maximum depth threshold in metres; `20.0`. This controls the numerical lookup table, unlike the depth script's plotting scale. |
| `INTDPH` | Positive depth-threshold spacing in metres; `0.05`. Smaller spacing increases table size. |
| `NREC` | Threshold-record count, computed in Shell as `MAXDPH/INTDPH + 1`; defaults to 401. Keep the ratio integral and verify that the resulting count matches the Fortran `int(MAXDPH/INTDPH)+1`. |

The duration plotting wrapper fixes `NGRID=1` internally and uses a fixed color scale ending at 365 days, including for leap years. It has no top-level depth-scale setting for that figure.

## Output files

Paths below use the supplied `TAG=mek`, `PROJ=mek_day`, and `YEAR=2000`.

| File or directory | Contents |
| --- | --- |
| `flood_mek/flood_YYYYMMDD.bin` | Numerical high-resolution depth in metres, four-byte real, one map per day. Unassigned pixels start at `-9999`; permanent-water pixels have a 0.1 m minimum. |
| `fig_mek/flddph_YYYYMMDD.jpg` | Depth figures from the display-transformed grids. |
| `dat/fldprd2000.bin` | Retained coarse-grid duration table: one four-byte real model-grid record per threshold, storing exceedance counts in days; inactive map cells are `-9999`. |
| `flood_mek_day/fldprd2000.bin` | Numerical high-resolution duration lookup result: days, `999` permanent-water marker, and `-9999` for unassigned pixels. |
| `fig_mek_day/fldprd2000.jpg` | Duration figure using the raw duration grid and a HAND-derived background. |
| `dphYYYYMMDD.bin`, `slp.bin` | Depth-plot intermediates in the working directory, removed by the plotting helpers. `slp.bin` contains transformed HAND shading, not physical slope. |
| `slope/slp2000.bin` | Duration-plot shading intermediate; the entire `slope/` directory is removed at the end of the duration script. |

The numerical high-resolution binaries contain x-fastest rows ordered west-to-east, then north-to-south. Depth writers use one direct-access record for the full grid; the duration writer uses one record per row. With the expected four-byte real/direct-access layout, both are contiguous float32 grids of `4 × nx × ny` bytes, without sequential-record markers. The coarse duration table stacks full model grids in increasing threshold order.

No coordinates or projection metadata are embedded in these `.bin` files. Retain the crop bounds, resolution, dimensions, and map identity alongside results. Byte order and Fortran record-length units are not explicitly set by the tools; match the compiler's binary conventions to the input data and the plotting reader's native `float32` interpretation.

## Execution notes and warnings

**Run only from the intended working directory after checking `MAPDIR`, `OUTDIR`, and all existing output/work directories.**

| Workflow | Existing paths removed or replaced |
| --- | --- |
| Both | `rm -f map` and `rm -f out`, followed by symbolic-link creation. Existing regular files or links with these names can be removed. |
| Depth | `t01` recreates `flood/`; `t02` recreates `fig/`. Plotting removes/replaces `slp.bin` and `dphYYYYMMDD.bin`. At the end, existing `flood_<TAG>/` and `fig_<TAG>/` are recursively deleted before the work directories are renamed. |
| Duration | Deletes and recreates `dat/`, `flood/`, and `fig/` at startup; creates/reuses `slope/` and deletes that entire directory at the end. Existing `flood_<PROJ>/` and `fig_<PROJ>/` are recursively deleted before renaming. |

Do not run these workflows concurrently in the same directory: they share links, work directories, and temporary filenames. Check the output files and process logs before using results; the scripts do not stop consistently on failed commands, and the depth workflow launches background jobs with limited status handling. A failure can leave partial products or still reach later cleanup/rename steps.

## Known Limitations and Future Refactoring

### Verified implementation limitations

- Input filenames, daily record indexing, relative directory layouts, and several plotting defaults are embedded in the scripts. There is no NetCDF reader or automatic time/dimension/missing-data validation in these workflows.
- Supported resolution tags differ between programs; plotting additionally restricts bounds to integer degrees. The display converters transform values and insert color anchors, so their temporary grids are unsuitable for quantitative flood-depth analysis.
- Duration results use discrete thresholds, cap the lookup index at the highest threshold, and include a `999` permanent-water marker. They do not include the daily depth workflow's tributary adjustment.
- The duration table is allocated for the full model grid, and the tributary method allocates high-resolution arrays for the buffered crop. High-resolution domains and parallel daily jobs can require substantial memory. Some programs have pixel-count guards; these are not a complete memory or integer-overflow check.
- Shell arguments are often unquoted, parallel-job limits use process-name counting, and command failures are not consistently propagated. Use paths without spaces with these wrappers. Fortran uses legacy `GETARG` and native direct-access I/O; the two HAND converters and Python plotting scripts duplicate substantial logic. A supported Python/NumPy/Matplotlib version range is not specified.

### Potential improvements, not implemented

1. Add validated configuration/arguments, reliable failure reporting, bounded job management, and safer output-directory handling.
2. Share grid/resolution/date parsing and record binary metadata; validate input shape, time coverage, missing values, and duration markers before processing.
3. Separate quantitative products from display transformations, consolidate plotting helpers, and establish supported dependency versions and small synthetic regression checks.

## Related documentation

- [Scientific downscaling note (Word)](../../doc/Note_Downscaling.docx)
- [River maps and supporting layers](../../map/README.md)
- [Sample simulations and outputs](../../gosh/README.md)
- [Compiler configuration](../../adm/README.md)
- [Additional tools directory guide](../README.md)
- [Model overview](../../README.md)
