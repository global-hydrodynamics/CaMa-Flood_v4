# Step 22 — Initial workflow implementation record

**Status:** Executed and checked on `feature/gosh-refactor`, repository revision
`96cd9daf` (2026-11-09). Point, event, and regional mask choices below remain
provisional pending researcher review. The 1-minute extent is the requested
102–108°E, 9–15°N. This record captures observed
execution, not a final learner manual or scientific validation.

## Inputs and environment

- Shared map input: `map/mekong_15min/netcdf/rivhgt.nc` (read-only).
- Runoff: `inp/test-mek_1deg_nc/runoff2000.nc`.
- Input matrix/dimensions: `map/mekong_15min/inpmat_test-mek_1deg.bin` and
  `inpdim_test-mek_1deg.txt`.
- Model executable: `out/bin/MAIN_cmf_netcdf`.
- Python environment: repository `.venv` with NumPy, netCDF4, xarray,
  pandas, Matplotlib, and `cmf_colormap`.
- Existing downscaler source: `etc/downscale_flddph/src/downscale_flddph_trib.F90`.

No shared map, model source, or existing downscaler source was modified.

## Executed workflow

Run from the repository root (the scripts resolve repository paths from their
own locations):

```sh
./.venv/bin/python tutorial/guide01_how_to_develop_tutorial/workflow_example/s01-prepare_map.py
sh tutorial/guide01_how_to_develop_tutorial/workflow_example/s02-run_control.sh
sh tutorial/guide01_how_to_develop_tutorial/workflow_example/s03-run_deep_channel.sh
./.venv/bin/python tutorial/guide01_how_to_develop_tutorial/workflow_example/a01-analyze_outputs.py --validate-only
./.venv/bin/python tutorial/guide01_how_to_develop_tutorial/workflow_example/a01-analyze_outputs.py
sh tutorial/guide01_how_to_develop_tutorial/workflow_example/s04-downscale_event.sh --force
./.venv/bin/python tutorial/guide01_how_to_develop_tutorial/workflow_example/v01-plot_results.py
```

The 2000 simulations are cold starts with matched Tutorial02 `mek_default`
physics and daily NetCDF output. `CRIVHGT` is the sole intended scientific
input difference. The Control namelist points to the shared `rivhgt.nc`; Deep
Channel points directly to the absolute tutorial-local
`map_modify/rivhgt_deep.nc` path.

Map preparation confirmed a 120×80 Float32 grid, 6,566 valid cells, and 3,034
missing cells. The valid-cell change is +3.0 m, the source mask and coordinates
are preserved, and the shared source SHA-256 remained
`be3f737dff56f3f18d07f880a44c7561827ff7bd4ffad9463fb6e7dea424bb5e`.
The generated local map SHA-256 is
`629d61b7c6a4a30782a24d0bde4742a9fe141327fddb2f47209e21a50198d408`.
Both simulations subsequently read their configured map and completed. Each
produced `outflw`, `rivdph`, `sfcelv`, and `fldfrc` with 366 records on the
same 120×80 grid and matched masks. Output interval-end coordinates run from
2000-01-02 00:00 through 2001-01-01 00:00; each timestamp represents the
preceding daily interval. February 29 is included.

The downscaling adapter reads the selected `rivdph` NetCDF record, checks its
time, grid, and metre units against the corresponding `rivhgt`, then writes
`max(rivdph-rivhgt, 0)` as one little-endian Float32 Binary grid. The unchanged
legacy Fortran downscaler consumes that file and the existing 1-minute map.
The synthetic adapter test checks time selection, mask propagation, clipping,
grid order, record size, and equality with the equivalent legacy Binary
formula. Both real runs were executed over the requested 360×360 extent; the
final assignment counts and depth ranges are recorded below.

## Provisional analysis choices and observed results

- Reference coordinates for Vientiane, Phnom Penh, and Can Tho were borrowed
  from the existing Tutorial02 example. Each is mapped to the nearest valid
  catchment center; coordinates, 1-based model indices, and approximate
  distances are recorded in `analysis/provisional_sites.csv`.
- Event selection is the earliest interval-end record attaining the maximum
  Control `sfcelv` at the provisional Phnom Penh cell. It selects the
  interval-end `2000-09-22T00:00:00`, representing 2000-09-21; peak value is
  12.4974 m.
- The regional-area example uses a provisional 104–108°E, 8–13°N bounding box;
  catchments are selected by center coordinate, with the east and north edges
  exclusive. The boundary-cell treatment and region need researcher review.
- The one-minute plotting/downscaling domain was set to the user's requested
  102–108°E, 9–15°N (360×360 at 1 minute). The existing downscaler reads a
  five-model-cell buffer, giving a 510×510 buffered domain of
  100.75–109.25°E, 7.75–16.25°N in the available map tile. This is
  postprocessing, not an independent 1-minute simulation.

Across all matched valid records, mean `outflw` was 494.45 m³/s (Control) and
502.23 m³/s (Deep Channel); mean `rivdph` was 1.466 and 2.011 m; mean
`sfcelv` was 565.64 and 563.19 m; mean `fldfrc` was 0.01519 and 0.01037.
These are descriptive values for this one-year cold-start experiment, not
validated physical conclusions. `analysis/output_summary.csv` records the
minimum, mean, and maximum for both cases and signed Deep-minus-Control
differences. `analysis/difference_at_event.nc` stores event-time differences.

The six figures in `figure/` were regenerated and visually inspected. Figure
5 uses the common Control-selected event, signed Deep-minus-Control fields,
and symmetric color limits capped at the 98th percentile of absolute values;
the cap is identified in each colorbar. Figure 6 uses the same requested
102–108°E, 9–15°N domain and display scale for both experiments.

## Validation and implementation notes

- `bash -n` passed for all workflow shell scripts; Python byte-compilation
  passed for the workflow and test scripts.
- `tests/test_downscale_input.py` passed.
- `a01-analyze_outputs.py --validate-only` confirmed aligned 366-record time
  axes and the 120×80 grid for both experiments.
- Each model run exited successfully and produced all four nonempty NetCDF
  outputs; `CRIVHGT` paths were checked in the generated namelists and logs.
- The original shared `rivhgt.nc` checksum was checked before and after map
  generation. No symlink or generated file was placed in the shared map tree.
- The unchanged legacy downscaler was compiled locally and run for both cases.
- The expanded 360×360 output has 129,600 pixels per case; 95,543 are assigned
  and 34,057 carry the legacy -9999 unassigned sentinel in each. Control
  depths range from 0 to 17.5362 m and Deep Channel depths from 0 to 16.0039
  m. Each file is 518,400 bytes. Figure 6 masks -9999 pixels separately from
  valid zero-depth pixels.

The output scripts use separate experiment directories under `out/`; the
repository's existing ignore rule excludes those generated model outputs.
Figures, small analysis products, and the generated tutorial map remain local
workflow products for review and tracking decisions. `README.md` is
intentionally not written in this implementation step.

## Remaining review items

Researcher approval is still needed for the three reference-cell selection
coordinates, the event-selection convention if a different Phnom Penh cell is
preferred, and the regional flood-area box and boundary rule. The one-minute
extent is the explicit user request for this update. There is no
observation-based validation. This initial
execution does not establish spin-up adequacy or that the one-year response
generalizes beyond the selected sample year.
