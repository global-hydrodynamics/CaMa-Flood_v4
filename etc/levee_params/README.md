# Levee parameter preparation

## Overview

This directory prepares map-specific levee parameters and demonstrates simulations with the CaMa-Flood levee scheme. It derives an unprotected fraction (`LEVFRC`) from levee-distance data, then estimates crown height (`LEVHGT`) from simulated river depths and flood-protection return periods. This specialized preparation workflow is additional to the [standard sample simulations](../../gosh/README.md).

For scientific rationale, datasets, and illustrations, see the existing [LeveeParameter.docx](LeveeParameter.docx) and [levee-method guide](../../doc/levee.md). The sequence and parameter definitions below follow the current scripts and [model implementation](../../src/cmf_ctrl_levee_mod.F90). Differences from the manual are identified explicitly.

**Use a dedicated copy of the river map.** Several stages write parameters through the local `map` link and replace files used by later simulations.

## Workflow overview

[`auto.sh`](auto.sh) lists the following stages. Its first three commands are commented out; its final three are active even though they are not required just to estimate the two model-input maps.

```text
s00-copy_data.sh                     [commented out in auto.sh]
  → data/levee-distance list + FLOPROS raster
s01-calc_levee_fraction.sh           [commented out in auto.sh]
  → map/levfrc.bin + map/levhgt_sim00.bin (100 m initial crown)
s02-conv_FLOPROS_res.sh              [commented out in auto.sh]
  → map/protect.tif + map/protect.bin

s03-cycle_sim00.sh                   [active]
  → t01-run_cmf_levee.sh sim00 → sim00_out/
  → t02-calc_levee_height.sh sim00 → sim00_calc/levhgt.bin
  → copy to map/levhgt_sim01.bin
s04-cycle_sim01.sh                   [active]
  → t01-run_cmf_levee.sh sim01 → sim01_out/
  → t02-calc_levee_height.sh sim01 → sim01_calc/levhgt.bin
  → copy to map/levhgt_sim02.bin AND map/levhgt.bin
s05-run_levee.sh                     [active; final simulation]
  → use map/levhgt.bin → levee_out/
s06-downscale.sh                     [active; example postprocessing]
  → downscale/ + fig_down/
x01-calc_levbaseheight.sh            [active; optional diagnostics]
  → map/levdst.bin, levbashgt.bin, levphyhgt.bin, storage thresholds, …
```

`sim00` uses a finite 100 m crown as the manual's approximate “infinite levee” condition. The first height estimate is used in `sim01`; the second estimate is copied to the final input. The manual explains that repeated simulations allow upstream/downstream hydraulic interactions to affect the estimated heights. The scripts implement **two estimation cycles**, with no convergence test. `sim02` names the second height estimate; `auto.sh` does not launch a separate `sim02_out/` calibration run. Its final simulation is `s05`, using those heights in `levee_out/`.

Height estimation takes annual maxima of `rivdph`, fits a Gumbel distribution with L-moments, evaluates the depth at each cell's protection return period, retains cells with `levfrc > 0`, and subtracts channel depth `rivhgt`. Negative results become zero; cells with `rivhgt == -9999` remain undefined.

## Scripts and programs

All paths in this table are relative to `etc/levee_params/`.

| Script / program | Purpose | Main inputs | Main outputs |
| --- | --- | --- | --- |
| [`s00-copy_data.sh`](s00-copy_data.sh) | Copy example external data from `map/data/levee/`. | Levee-distance text and FLOPROS GeoTIFF. | Copies under `data/`. |
| [`s01-calc_levee_fraction.sh`](s01-calc_levee_fraction.sh) → [`calc_levfrc.F90`](src/calc_levfrc.F90) | Link the target map and aggregate mainstem levee-distance observations. | Map geometry, high-resolution assignments/upstream area, distance list, selected river width. | `map/levfrc.bin`, `map/levhgt_sim00.bin`, `tmp/tmp.bin`, `tmp/levfrc_stat.txt`. |
| [`s02-conv_FLOPROS_res.sh`](s02-conv_FLOPROS_res.sh) → [`conv_FLOPROS_resolution.py`](src/conv_FLOPROS_resolution.py) | Transfer protection return periods to the model grid by integer-factor nearest-neighbor decimation, without reprojection. | FLOPROS raster, `params.txt`, `uparea.bin` ocean mask. | `map/protect.tif`, `map/protect.bin`. |
| [`s03-cycle_sim00.sh`](s03-cycle_sim00.sh), [`s04-cycle_sim01.sh`](s04-cycle_sim01.sh) | Run a simulation and height fit, then copy the estimate to the next parameter file. | Previous cycle's crown map and common model inputs. | Cycle output/analysis directories and copied height maps shown above. |
| [`t01-run_cmf_levee.sh`](t01-run_cmf_levee.sh) | Run the requested cycle with `LLEVEE=.TRUE.`. | Cycle argument, `levhgt_<CYCLE>.bin`, `levfrc.bin`, model/map/forcing. | `<CYCLE>_out/`: annual fields, logs, restarts, spin-up archives. |
| [`t02-calc_levee_height.sh`](t02-calc_levee_height.sh) → [`calculate_levhgt.py`](src/calculate_levhgt.py) | Annual maxima, return-period fitting, levee masking, and channel-depth subtraction. | `<CYCLE>_out/rivdphYYYY.bin` by default; protection raster and map parameters. | `<CYCLE>_calc/1_annual_max/annual_max_rivdph_YYYY.nc`, `2_probabilistic_depth.bin`, `3_masked_depth.bin`, `levhgt.bin`. |
| [`s05-run_levee.sh`](s05-run_levee.sh) | Final simulation using the selected `map/levhgt.bin`. | Final height/fraction maps and model inputs. | `levee_out/`, including `flddphYYYY.bin` and `levdphYYYY.bin`. |
| [`s06-downscale.sh`](s06-downscale.sh) | Classify protected pixels, downscale one selected day, and plot. | High-resolution layers and final `flddph`/`levdph` outputs. | `downscale/` binaries/control files; `fig_down/flood_lev.jpg`. |
| [`x01-calc_levbaseheight.sh`](x01-calc_levbaseheight.sh) → [`calc_baseheight.py`](src/calc_baseheight.py) | Optional offline geometry/storage diagnostics. | Final fraction/crown maps, area, channel geometry, floodplain profile. | Seven diagnostic maps listed below. |
| [`visualize.py`](visualize.py) | Optional parameter figures; not called by `auto.sh`. | `levfrc`, `levhgt`, `levphyhgt`, `uparea`, `params.txt`. | `image/levfrc.png`, `image/levhgt.png`, `image/levphyhgt.png`. |

The optional downscaling chain uses `define_protect_pix`, `downscale_flddph_levee`, `conv_flood_levee`, and `src/draw_flddph.py`. Its numerical file `downscale/flood_lev.bin` **adds 100 to flooded protected-pixel depths** to distinguish them for plotting. It is not a uniform physical-depth raster without decoding. The display converter additionally clips/spreads values and inserts color anchors; use the source conventions when analyzing these products. `visualize.py` thickens all three parameter overlays by 2×2 pixels for visibility; those PNGs are also presentation products.

## Required input datasets

| Input | Source and current requirements |
| --- | --- |
| River map and channel parameters | Supply a compatible map with `params.txt`, `uparea.bin`, `ctmare.bin`, `rivlen.bin`, `lonlat.bin` (longitude and latitude records), and the selected width file (`rivwth_gwdlr.bin` in the wrapper). Simulations also require their configured network, elevation, channel-depth/roughness, floodplain, bifurcation, and forcing-mapping files. See the [map guide](../../map/README.md). |
| High-resolution mapping for fraction preparation | `map/<HIGHRES>/location.txt` and the first listed tile's `.catmxy.bin`, `.flddif.bin`, and `.uparea.bin`. Catchment coordinates are two int16 records; elevation/area layers are four-byte real. Coarse upstream area is converted from m² to km² before comparison, so the high-resolution upstream-area values must be compatible with that comparison. |
| Levee-distance observations | External `levdis_Zhao2025_glb3sec.txt` by default: headerless numeric rows `latitude longitude upstream_area_km2 distance_m class`. Distance averaging retains classes 0, 3, and 4. The code uses the coordinate-to-catchment mapping, not the list's drainage-area column, for allocation. |
| Protection standard | External `FLOPROS_shp_V1_30s_filled.tif`: the supplied filled protection-return-period raster, in years. The converter requires a north-up regular latitude/longitude source covering the target, with target spacing an integer multiple of source spacing; it does not reproject. |
| Long-term runoff and matching input matrix | Supply the full configured period. Current run scripts select `inp/ERA5_sixmin/Roff____YYYYMMDD.sixmin`, `diminfo_ERA5_sixmin.txt`, and `inpmat_ERA5_sixmin.bin`; these paths do not supply the data themselves. |
| Simulation outputs for fitting | Generated by the cycle runs. Binary mode reads `rivdphYYYY.bin` as float32 and reshapes to `(records, ny, nx)`; NetCDF mode instead expects `o_rivdphYYYY.nc` with variable `rivdph`. The wrapper defaults to binary. |
| Optional offline diagnostics | `x01` additionally reads `grdare.bin`, `rivlen.bin`, `rivwth_gwdlr.bin`, `rivhgt.bin`, and `fldhgt.bin`. The latter contains `NLFP` full-grid height layers. Its `grdare.bin` area input differs from the `ctmare.bin` selected by fraction preparation and the model scripts; resolve that choice for the intended map. |
| Optional downscaling | Also needs `nextxy.bin` and high-resolution `.catmz100.bin`, `.rivwth.bin`, and `.hand.bin`, alongside `.catmxy.bin`/`.flddif.bin`. The example selects a Mississippi crop, `HIRES=1min`, year 2000, day-of-year record 62. |

The distance list, filled FLOPROS raster, long-term forcing, and map layers are external data with their own provenance and usage terms. A GitHub clone does not contain the full inputs. `s00` assumes the two example external datasets have already been placed under `map/data/levee/`.

## Configuration

Settings are distributed across scripts; there is no central configuration file.

| Location / setting | What to check |
| --- | --- |
| `s01`: `MAPDIR`, `HIGHRES`, `LEVLIST`, `LISTRES`, `RIVWTH` | Select a dedicated map copy and compatible observations. Fraction preparation recognizes `HIGHRES=1min/30sec/15sec` and list spacing `3sec/1sec`. `map` is a link into this target. |
| `s02`: `INPTIF`, `PARAMS`, `OUTTIF`, `OUTBIN` | Match the protection raster to the same map. Fitting reads `protect.tif`; `protect.bin` is an additional export. |
| `t01` and `s05`: `BASE`, `FMAP`, `PROG`, forcing paths, `CDIMINFO`, `CINPMAT` | Check the model executable and map/forcing layout. Both wrappers expect to start in this directory. Library-path exports and `OMP_NUM_THREADS=16` are installation-specific examples. |
| Simulation years and spin-up | Both run scripts use `YSTA=1990`, `YEND=2020`, `SPINUP=0`, `NSP=1`. The fitting wrapper separately uses `START_YEAR=1990` and `END_YEAR=2020`; update them together. |
| Time steps and formats | Cycle runs use outer `DT=86400` s; the final `s05` run uses `DT=3600` s. Both use adaptive stepping, daily forcing/output, and binary output by default. Check this difference when comparing calibration and final results. |
| Runoff units: `DROFUNIT` | Current scripts use `86400`, annotated as m/day → m/s. This assumes values in metres per day (or metres accumulated over a day). The [ERA5-Land converters](../../inp/README.md) produce millimetres per record, which need a different divisor; do not copy the current factor without checking the actual files. |
| `t02`: `CMF_FMT`, `MAP_DIR`, `WORK_DIR`, target raster, variable | Match the produced outputs and selected cycle. The Python CLI option is spelled `--cfm-output`. Annual-max files are cached in the work directory. |
| `x01`: area/geometry/profile paths | Use a consistent geometry and the final height map. Its outputs are offline diagnostics, not additional required model inputs. |
| `s06` / `visualize.py` | Review the crop, resolution, selected year/record and filenames. Parameter visualization uses a fixed default crop (130°W–180°E, 60°S–75°N). |

## Output parameters and physical meaning

### Height references and fraction

`rivhgt.bin` is **channel depth**, not absolute riverbed elevation. In the model, riverbed elevation is `elevtn - rivhgt`. Since the fitter computes `LEVHGT = return_period_rivdph - rivhgt`, its crown height is relative to the channel-top/floodplain reference `elevtn`, not measured above the riverbed or above local levee ground.

The offline base height uses the same zero as the `fldhgt` profile. Thus absolute elevations, if needed and if these inputs share a datum, are `elevtn + LEVHGT` for the crown and `elevtn + LEVBASHGT` for the base. Physical structure height is their difference, `LEVPHYHGT = LEVHGT - LEVBASHGT`.

Fraction preparation computes `LEVFRC = min(1, 2 × mean_distance / (ctmare/rivlen))` for accepted levee cells, after enforcing a minimum mean distance of half the selected river width. The source also applies coverage/count thresholds. Cells without accepted levee information are written as `-9999`; the runtime normalizes absent/invalid levees to an unprotected fraction of 1. A fraction of 1 means no protected part of the catchment in the runtime, even if an intermediate crown map is positive.

### Files

Unless stated otherwise, the following binaries are full model-grid float32 fields: `nx × ny` values, x varying west-to-east fastest and rows north-to-south, with no sequential record markers. The Fortran writers use `RECL=4*nx*ny`; Python writes one contiguous float32 field. These are not river-vector arrays.

| File | Definition and units | Producer | Model use |
| --- | --- | --- | --- |
| `levfrc.bin` | Relative unprotected fraction/model levee position, dimensionless. | `calc_levfrc`. | Read through `CLEVFRC` in `NLEVEE`. |
| `levhgt_sim00.bin` | Initial crown height above the channel-top reference: 100 m at cells with `levfrc>0`, zero at other land cells, `-9999` outside land. | `calc_levfrc`. | Crown input for `sim00`; runtime also checks the fraction. |
| `levhgt_sim01.bin`, `levhgt_sim02.bin`, `levhgt.bin` | Estimated crown height above `elevtn`, metres. The fitter clips negative estimates to zero and marks sea with `-9999`. | `calculate_levhgt.py` writes `<CYCLE>_calc/levhgt.bin`; `s03`/`s04` copy it into the map. | Selected through `CLEVHGT`; `s05` reads `levhgt.bin`. |
| `levdst.bin` | Effective lateral distance coordinate, metres: `LEVFRC × A/L`, where the offline wrapper supplies `A=grdare`, `L=rivlen`. | `calc_baseheight.py` via `x01`. | Diagnostic file; runtime computes its own `D2LEVDST`. |
| `levbashgt.bin` | Ground/base height at the levee position, metres above the floodplain-profile zero. | Same offline program. | Diagnostic; runtime counterpart is `D2BASHGT`. |
| `levphyhgt.bin` | Physical levee height, `LEVHGT - LEVBASHGT`, metres. | Same offline program. | Diagnostic/visualization; not read as a levee input. |
| `levbassto.bin` | River-side storage at base height, m³, using the offline zero-channel-storage convention described below. | Same offline program. | Diagnostic; runtime computes `D2LEVBASSTO` internally. |
| `levtopsto.bin` | River-side storage at crown height, m³, under that same convention. | Same offline program. | Diagnostic; runtime computes `D2LEVTOPSTO` internally. |
| `levfilsto.bin` | Storage with river and protected side filled to crown height, m³, under that same convention. | Same offline program. | Diagnostic; runtime computes `D2LEVFILSTO` internally. |
| `levhgt_mod.bin` | Original input crown height, metres, with cells rejected by the offline geometry/crown/base checks set to `-9999`. It is a masked export, not an updated fit. | Same offline program. | Not automatically substituted for `levhgt.bin`. |

`LEVFRC × A/L` is the model's lateral width coordinate. When `A` matches the area used in fraction preparation and the fraction was not capped, it equals twice the width-constrained mean distance used during preparation; do not interpret it as an independent single-bank surveyed distance.

The offline diagnostic program sets **`RIVSTOMAX=0`** and masks sea, invalid geometry, nonpositive crown heights, and nonpositive physical heights to `-9999`. Its input-endian option controls input reading; outputs are native float32. The model instead includes channel storage `rivlen × rivwth × rivhgt`, normalizes invalid fractions, and raises crown height to at least the base height. Therefore these seven exports are not guaranteed to equal the runtime's internal fields. Align area/geometry and storage/masking conventions before comparison.

The model reads only `CLEVFRC` and `CLEVHGT` as levee maps and derives the other geometry/storage quantities at initialization. `x01` does not replace `levhgt.bin`, and the final simulation in `auto.sh` occurs before `x01`.

### Other products

- `protect.tif` / `protect.bin`: regridded protection return periods in years; binary band 1 is explicitly little-endian float32, north-to-south. Ocean is `-9999`.
- `<CYCLE>_calc/`: annual-maximum NetCDF files, return-period and masked river-depth binaries in metres, and the fitted crown map. The annual maxima contain grid dimensions but no geographic coordinate variables.
- `tmp/tmp.bin`: six grid records for mapping diagnostics; `tmp/levfrc_stat.txt` reports location, upstream area, fraction, and coverage statistics.
- `sim00_out/`, `sim01_out/`, `levee_out/`: model fields, logs and restart/spin-up files. Parameter images go under `image/`; the optional downscaling example writes its separate products under `downscale/` and `fig_down/`.

## Running the workflow

1. Obtain external data and a dedicated map copy. Configure `s01` to point to that copy; check all dependent paths and periods before executing any stage.
2. Build the normal `src/MAIN_cmf` with the [standard build workflow](../../gosh/README.md), then build local utilities. From the model root:

   ```sh
   cd etc/levee_params
   make -C src all
   ```

   The local Makefile builds `calc_levfrc`, `define_protect_pix`, `downscale_flddph_levee`, and `conv_flood_levee`, using `FC`/`FFLAGS` from `adm/Mkinclude`. It does not build the model executable.

3. After configuring inputs, run preparation and estimation as individual stages, checking each result before proceeding:

   ```sh
   ./s00-copy_data.sh
   ./s01-calc_levee_fraction.sh
   ./s02-conv_FLOPROS_res.sh
   ./s03-cycle_sim00.sh
   ./s04-cycle_sim01.sh
   ```

   Skip `s00` only if the required `data/` inputs are already present. Confirm finite fractions/heights, the map grid, protection raster alignment, expected simulation years, and the actual fitted sample count. Compare successive crown maps; the workflow does not decide convergence for you.

4. If a final simulation is required, configure and run `./s05-run_levee.sh`. Run `./s06-downscale.sh` only with its additional layers and selected daily outputs. Run `./x01-calc_levbaseheight.sh` for diagnostics, then `python visualize.py` if parameter figures are wanted. These three post-estimation tasks have separate requirements.

`auto.sh` invokes stage names without `./`; the current directory must be on `PATH` for those calls to resolve. It also assumes preparation is complete and invokes final simulation/downscaling/diagnostics unconditionally. Treat it as a record of the example sequence, rather than assuming it can safely initialize an arbitrary installation.

For controlled reruns, `t01` and `t02` accept a cycle argument such as `sim00`. Keep the chosen height map, output directory, and analysis directory together. The height fitter skips existing annual-max files and subsequently reads every `.nc` file in that cache directory, so use a fresh analysis directory when changing years, inputs, or variable selection.

## Dependencies and compatibility

| Component | Verified requirements |
| --- | --- |
| Model and Fortran utilities | Configured Fortran compiler, normal CaMa-Flood executable with the levee scheme selected at runtime, compatible map/forcing, and the four local executables. Shell examples use Bash/POSIX utilities and installation-specific library paths. |
| Protection raster conversion | Python with NumPy and rasterio; regular north-up geographic GeoTIFF input and integer decimation factors. |
| Height fitting | Python with NumPy, netCDF4, xarray, tqdm, tifffile, and lmoments3. xarray is imported even though the main fitting path uses netCDF4. Binary model output still requires these imports. |
| Base/storage diagnostics | Python with NumPy. |
| Optional figures | NumPy and Matplotlib, including `mpl_toolkits.axes_grid1` for the downscaling figure. |

The wrappers invoke `python`; configure that command for the intended environment. No supported package-version matrix is supplied. Most binary readers/writers use native float32/direct-access conventions; the FLOPROS converter explicitly uses little-endian float32. Match endianness, record lengths, grid ordering, and dimensions across the compiler and Python tools. The run scripts do not automatically download data, configure a Python environment, or select compiler settings.

## Manual and implementation differences

| Topic | Verified current behavior |
| --- | --- |
| Workflow extent and convergence | The manual presents fraction/protection preparation, two example estimation cycles, and optional diagnostics, with further iterations if needed. `auto.sh` disables preparation, runs only two fits without a convergence check, and actively calls final simulation, downscaling, and diagnostics. |
| Height reference | Parts of the manual and Python help call `rivhgt` a bed elevation and describe the crown/base as above the riverbed. The simulation and fitting equations instead establish the channel-top/profile reference described above. |
| Optional storage diagnostics | The manual relates the exports to internal model parameters. The offline implementation fixes channel storage to zero and uses its own invalid-levee masking; the runtime includes channel storage and applies different normalization/clamping. Do not assume numerical equivalence. |
| Area files and map requirements | Fraction preparation and the run scripts use `ctmare.bin`; `x01` selects `grdare.bin`. The fraction mapper also reads `lonlat.bin` and high-resolution `.flddif.bin`, while the manual's appendix lists `class.bin`, which this program does not read. |
| Map coverage | Although the manual discusses general map use, `calc_levfrc` reads only the first tile entry in `location.txt`, despite reading the listed tile count. A multi-tile map is not fully traversed by this implementation. |
| Script/source names | The executable source is `src/calc_levfrc.F90`; the manual's `calc_lkevfrc.F90` spelling is a typo. The current calculation intermediates and copied cycle maps are distinguished in the script table above. |

The manual remains the scientific reference. These differences describe the supplied code; they do not alter the manual or implement a revised scientific method.

## File-writing and execution safety

- `s00` overwrites data copies; `s01` uses `ln -sf` to create `map` without explicitly removing an existing directory/link first, then writes fraction/initial-height maps through it. `s02` replaces protection products in the same map. Verify the link target before proceeding.
- `s03` overwrites `levhgt_sim01.bin`; `s04` overwrites both `levhgt_sim02.bin` and `levhgt.bin`. Preserve a copy of existing parameter sets and their associated inputs/results.
- With `SPINUP=0`, `t01` and `s05` delete prior spin-up directories, binary/NetCDF fields, logs, text files, and restarts in their selected run directory. They also replace executable links and yearly namelists and rename spin-up products.
- Height fitting overwrites probability/mask/final-height products while reusing cached annual maxima. Missing annual input files produce warnings and are skipped; the fitting glob can include stale files from other requested periods. A completed process alone does not establish complete coverage or fit quality.
- `x01` overwrites its seven diagnostic maps. `s06` reuses `downscale/` and `fig_down/`, replaces selected products, and removes several fixed-name files in the working directory. Parameter figures overwrite the same three PNGs.
- Run from `etc/levee_params/` and avoid concurrent writers using the same map or work directories. Commands and copies are not consistently checked for failure; the run wrapper ends with `exit 0` independently of the model's exit status.

## Known Limitations and Future Refactoring

### Confirmed current limitations

The workflow has distributed, partly duplicated settings; a first-tile-only fraction mapper; fixed two-cycle control without convergence checks; a protection converter restricted to integer grid decimation; cache reuse without provenance checks; and inconsistent area, vertical-reference wording, and offline/runtime storage conventions. Binary missing-value sentinels are not generally screened before annual-max fitting, and no complete-period or finite-fit validation is enforced. Optional plotting uses fixed crop/date/file conventions, and the run scripts include platform-specific library paths and unchecked destructive cleanup.

### Suggested priorities, not implemented

1. Add input/path validation, reliable error propagation, explicit overwrite controls, and isolated map/run/analysis outputs before automating the sequence.
2. Resolve area selection and height/storage definitions with maintainers, document conversions, and validate offline diagnostics against runtime calculations on small fixtures.
3. Handle all required high-resolution tiles, record cache provenance and year coverage, and add explicit convergence/sample-quality checks with shared configuration and dependency specifications.

## Related documentation

- [Existing levee manual (Word)](LeveeParameter.docx) and [levee-method guide](../../doc/levee.md)
- [Runtime levee controller](../../src/cmf_ctrl_levee_mod.F90) and [map/topography initialization](../../src/cmf_ctrl_maps_mod.F90)
- [Map overview](../../map/README.md), [execution guide](../../gosh/README.md), and [runoff units](../../inp/README.md)
- [General flood-downscaling workflow](../downscale_flddph/ReadMe.md)
- [Additional tools directory guide](../README.md)
