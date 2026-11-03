# CaMa-Flood v5 model source and simulation workflow modernization

## Status

**Phase 0 and Phase 1 (including NetCDF I/O and the two-year Mekong run) are implemented and operationally validated on the maintainer's macOS workstation.** The complete tracked `src/` and `gosh/` trees were copied to `src_main/` and `gosh_sample/`; separate Binary and NetCDF-enabled executables were built. The initial 2000 Mekong annual run, 40-day format comparisons, and 2000–2001 annual-NetCDF/restart workflow completed. These checks establish operational behavior and format consistency for this build and sample, not independent scientific validation or broad platform support.

**Persistent branch:** `feature/gosh-refactor`, created from current `origin/master` at `2841239` on 2026-11-03. No GitHub Issue was requested or created.

The maintainer approved an incremental v5 source and simulation workflow. Keep the legacy `src/` and `gosh/` trees unchanged as v4 references. All source subdirectories are preserved in the v5 copy, including `ICI/`, `sediment/`, `heatlink/`, and supporting libraries. Bifurcation is enabled for the first Mekong runtime check through its namelist setting; the model-wide Fortran initializer is unchanged.

## Phase 0: completed work and evidence

### Source and workflow copies

- `src_main/` contains all 131 files tracked under `src/`, including every tracked subdirectory and its documentation/build files.
- `gosh_sample/` contains all 15 files tracked under `gosh/`, including `etc/` configuration/examples.
- All tracked files were copied. In the final state, only `src_main/Makefile` and its new README intentionally differ from `src/`; only `gosh_sample/compile.sh` and its new README intentionally differ from `gosh/`. All other copied tracked files were byte-compared and match. The ignored workstation `.DS_Store` under `src/` was not copied; it is not a source file. Build products are ignored within `src_main/`.
- No Fortran source file or algorithm was edited. `src/` and `gosh/` remain unchanged.

### Independent build path

The v5 model builds directly from its own tree:

```sh
make -C src_main all
```

This uses `src_main/Makefile`, the existing shared default `adm/Mkinclude`, and the existing nested Makefiles. The directory depth is the same as `src/`, so the current relative include paths resolve without changing `adm/`. Build outputs, including `src_main/MAIN_cmf`, object files, module files, and `libcama.a`, stay under `src_main/`. Clean only this build with:

```sh
make -C src_main clean
```

`gosh_sample/compile.sh` is adapted as a v5-only convenience entry point and delegates to `make -C src_main all`. It does not call `util/`, `map/src/`, `gosh/compile.sh`, or clean legacy build outputs. The original `gosh/compile.sh` remains untouched.

The clean default build completed on 2026-11-03 using GNU Fortran 14.2 with the repository's default OpenMP and `SinglePrec_CMF` flags. It produced `src_main/MAIN_cmf`. The compiler reported pre-existing unused-import warnings in `cmf_ctrl_maps_mod.F90` and `MAIN_cmf.F90`; there were no compile or link errors. An incremental wrapper run initially exposed an interaction between phony `ext-libs` prerequisites and GNU Make's built-in Modula-2 `.mod` rule. In `src_main/Makefile` only, the extension-library ordering prerequisites are now applied when `EXT_LIBS` is non-empty. The v5 build wrapper then completed repeatedly, including when invoked outside the repository working directory. This is a build-rule adjustment only; the legacy Makefile is unchanged. Optional extension builds remain untested.

### Existing v4 architecture and copy implications

The stand-alone `src/MAIN_cmf.F90` calls `CMF_DRV_INPUT`, `CMF_DRV_INIT`, `CMF_DRV_ADVANCE`, and `CMF_DRV_END`. Reusable `cmf_ctrl_*`, `cmf_calc_*`, `cmf_opt_*`, and `yos_cmf_*` modules have explicit module and execution-order dependencies; the modules are not independent plug-ins. See [`src/README.md`](../src/README.md) for the verified v4 architecture.

The complete source inventory includes:

| Path | Responsibility / relevant dependency |
| --- | --- |
| `common/` | General arrays/containers, dates/times, binary/NetCDF, map-frame, text, and numeric utilities; `libcommon.a`. |
| `mod/` | Map/grid and input-dimension support; `libmod.a`. |
| `io/` | Configurable I/O and restart support used by coupled components; `libio.a`. |
| `phys/` | Water, heat-flux/budget, ice and physical constants; `libphys.a`. |
| `heatlink/` | Optional river-temperature/ice coupling. Compile-time `heatlink` hooks and runtime `LHEATLINK` both participate; libraries also depend on common, mod, io, and phys. |
| `sediment/` | Separate sediment state, calculations, forcing, output and restart path selected with `-Dsediment`. |
| `ICI/` | Specialized ICI/ILS driver; depends on external ICI/PALM-time/kind and MPI-related build components and an external build layout. |

The root `src_main/Makefile` builds the standard stand-alone target. Preserving optional source directories does not mean their build configurations have been migrated or validated. `adm/` remains shared and unchanged. `adm/Mkinclude_heatlink` includes a workstation-specific NetCDF path, so it is not a portable generic optional-build configuration. Any future separate v5 compiler include must be introduced only for a concrete need and must not alter v4 settings.

### Legacy script findings

The original `gosh/compile.sh` assumes it is run from `gosh/`; it clean/builds `util/`, legacy `map/src/` tools, and `src/`, and places `MAIN_cmf` in `src/`. It must not be used by v5. The copied `gosh_sample/compile.sh` now builds only `src_main/`.

The other copied `test*.sh`, options, and development examples have not yet been migrated into v5-ready execution examples. Several point to package-only maps/forcing, use current-working-directory-relative paths, clear existing result files, and still contain legacy executable paths. They must not be run as v5 samples until each is explicitly migrated and its paths/output isolation reviewed.

## Phase 1: Mekong simulation (completed)

The first v5 model run uses the committed Mekong map and runoff sample:

- Map: `map/mekong_15min/` (80 × 120 river cells, 10 floodplain layers).
- Runoff: `inp/test-mek_1deg/` (20 × 30 daily float32 grid, `mm/day`; 1980 onward).
- Simulation: 2000-01-01 00:00 through 2001-01-01 00:00, 366 days.
- Execution: non-MPI, one OpenMP thread, adaptive timestep enabled, bifurcation enabled, reservoir operation disabled, zero initial storage.
- Channel width: `rivwth_merit.bin`; bifurcation table: `bifprm.txt`.
- Runoff conversion: `DROFUNIT=86400000`; daily files use `Roff____YYYYMMDD.one`.
- Outputs: daily binary `outflw`, `storge`, `sfcelv`, and `flddph`; bifurcation `pthflw`; final restart.
- Run directory: ignored `out/mek01-default_run/`.

The map-local [`mek03-generate_inpmat-mek1deg.sh`](../map/mekong_15min/script_params/mek03-generate_inpmat-mek1deg.sh) generated `inpdim_test-mek_1deg.txt` and `inpmat_test-mek_1deg.bin` with the existing `param_inpmat_generate` utility. The descriptor records 80 × 120 map cells, 10 floodplain layers, a 20 × 30 runoff grid, maximum input count 4, and bounds 90–110°E / 5–35°N. The binary matrix is 460,800 bytes: 12 32-bit direct-access records (four x-index, four y-index, four overlap-area records). Both products are retained with the GitHub-tracked Mekong sample.

[`gosh_sample/mek01-default_run.sh`](../gosh_sample/mek01-default_run.sh) writes the namelist and runs `src_main/MAIN_cmf` from the experiment directory. The successful model log reports the requested dimensions and interval, reads the binary matrix and all 366 runoff files from `Roff____20000101.one` through `Roff____20001231.one`, and reports `LPTHOUT=T`. `cmf_ctrl_maps_mod.F90` reads the configured bifurcation table when that flag is true. The source namelist initializer remains false; this sample explicitly enables bifurcation as the maintainer directed. No Fortran algorithm or model default was changed.

The initial annual acceptance run completed with 8,784 model steps and 366 daily records for each gridded output. Each 80 × 120 float32 output was 14,054,400 bytes; `pthflw2000.pth` and the final restart products were also nonempty. All values in the four gridded files and bifurcation output were finite; the gridded outputs contain the model's finite `1e20` fill value on missing cells. `outflw` and `pthflw` contain negative values; the routing source permits backflow, so sign alone was not treated as an error. Existing map and runoff inputs were not modified; the generated input descriptor and matrix are the only new map products. This establishes operational behavior for this build and sample, not scientific validity or numerical equivalence with v4. See VR-0083 for evidence and limitations.

### Follow-up: default 40-day month-boundary run

The maintainer later changed `mek01-default_run.sh` to use 2000-01-01 00:00 through 2000-02-10 00:00, an end-exclusive 40-day interval. The script now requires execution from `gosh_sample/`, copies `src_main/MAIN_cmf` into the run directory, and executes the model from that directory so `COUTDIR="./"` keeps outputs together. The model read 40 daily files through `Roff____20000209.one`, reached the requested end time after 960 hourly steps, and wrote 40 records to each gridded output. This verifies date rollover across the month boundary and the current sample script's directory behavior. The earlier annual output directory was preserved under the ignored name `out/mek01-default_run_2000-full-year/`; the current default output is `out/mek01-default_run/`. See VR-0084 for the detailed check. No numerical algorithms or model source were changed.

A subsequent run used four OpenMP threads and exercised the script's runtime summary: start/end timestamps, elapsed wall-clock seconds, and exit status were printed and saved in `run_time.log`. It again completed the same 40-day interval and produced 40 records for each configured map output. The prior one-thread outputs were preserved under the ignored `out/mek01-default_run_40day_1thread/`. See VR-0085. The `CVARSOUT` value was already maintainer-edited in the worktree before this follow-up and was preserved; VR-0085 records the exact setting used for that local run.

### Binary output CTLs

The standalone standard-library tool [`tools/generate_output_ctl.py`](../tools/generate_output_ctl.py) now reads the run namelist, the referenced map's `params.txt`, and the root [`output_variables.csv`](../output_variables.csv). `mek01-default_run.sh` invokes it in `planned` mode before model execution when binary output is selected. `existing` mode can describe complete records currently present and reports incomplete files; it does not alter binary data. Synthetic checks cover the 40-day month boundary, leap-year record count, partial records, and malformed file sizes. No live GrADS application or model simulation was used to validate the generated descriptor. Vector, NetCDF, uncertain-registry, and special-dimension outputs remain unsupported; combined multi-year CTLs are deferred. See [`tools/README.md`](../tools/README.md).

## Incremental feature migration order

The current legacy examples indicate distinct profiles; their numbering is not a mandatory sequence.

| Legacy example | Main feature/data profile | Suggested v5 migration checkpoint |
| --- | --- | --- |
| `test1-glb_15min.sh` | Global binary maps, daily binary runoff, bifurcation, adaptive step, multi-year/restart. | The initial Mekong non-MPI/bifurcation run completed; longer runs and restart remain separate checks. |
| `test2-conus_06min_netcdf.sh` | CONUS and NetCDF runoff. | After required NetCDF map/runoff data and the supported model input contract are confirmed. |
| `test3-jpn_fcast.sh` | Japan high-resolution hourly forcing and forecast/restart directories. | After a tracked/reproducible dataset and restart behavior are established. |
| `test4-e2o_glb_15min.sh` | Global NetCDF runoff, binary model output. | After NetCDF forcing configuration is checked independently. |
| `test5_mpi.sh` | MPI build and multi-rank execution. | After serial sample execution; requires MPI runtime and matching partition map. |
| `test6-reservoir_glb15min.sh` | Reservoir operation and dam data. | After base routing; test scheme data and activation separately. |
| `options_full.sh` | Practical compile/runtime option examples. | Migrate only options verified available in v5 and document compile-time versus runtime controls. |
| `dev_ERA5_hour.sh`, `gosh/etc/` | Development forcing and specialized heat-link configuration. | Later, after dependencies, provenance, and supported combinations are established. |

## Phase 0 and Phase 1 validation and remaining limits

Completed:

- Full tracked-tree copies present, retaining all source and workflow subdirectories.
- All tracked source/workflow files match except the intentionally updated v5 build Makefile/wrapper and added READMEs.
- `make -C src_main all` completed and produced `src_main/MAIN_cmf`.
- The initial 2000 Mekong annual run completed, followed by a 40-day January-February run using the current script default; each read its expected daily runoff files and wrote the corresponding daily output records.
- No legacy `src/` or `gosh/` content, shared build setting, or existing map/runoff product was changed. The two newly generated Mekong `test-mek_1deg` matrix files were added to the sample.

- No v4/v5 numerical comparison or independent scientific validation was performed.
- The NetCDF build was verified only on the current macOS/Homebrew environment; a Linux compiler/library matrix was not tested.
- MPI, `heatlink/`, `sediment/`, and `ICI/` configurations remain unvalidated.

## Phase 1A–1D: NetCDF forcing, output, and annual restart workflow

The maintainer authorized NetCDF I/O work while keeping map inputs in Binary format and leaving the legacy `src/` and `gosh/` trees untouched. `gosh_sample/compile_netcdf.sh` builds a separate `MAIN_cmf_netcdf` from an isolated temporary copy using NetCDF-Fortran and NetCDF-C configuration flags. It does not replace the regular `src_main/MAIN_cmf` build. The verified toolchain was macOS/Homebrew GNU Fortran with `nf-config` and `nc-config`; Linux behavior is designed for the same tools but has not been exercised.

`inp/test-mek_1deg_nc/s00-convert_binary_to_netcdf.py` generates annual `runoff2000.nc` and `runoff2001.nc` from the unchanged 20 × 30 daily Binary sample in `inp/test-mek_1deg/`. The files contain `runoff(time, latitude, longitude)` in `mm day-1`, with CF daily time bounds and `proleptic_gregorian` calendar. The conversion uses `ncgen`/`ncdump`, has an explicit overwrite option, and is described in the adjacent dataset README. All 731 fields were compared after NetCDF round-trip against the source float32 values with exact equality. The two output files total 1,774,768 bytes (about 1.7 MiB). This proves conversion fidelity for these files, not the scientific validity of the underlying runoff product. The registry row records GitHub sample intent; inclusion in a future v5 downloadable package remains an open policy decision because a v5 package assembly workflow is not yet established.

Three 40-day operational comparisons were completed with the Mekong sample:

1. NetCDF-enabled executable, Binary runoff, Binary output versus the prior Binary build: `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc` matched bitwise (maximum absolute difference 0).
2. NetCDF runoff versus Binary runoff, with Binary outputs: the same five output variables matched bitwise. NetCDF input uses the annual file for that run and reuses `inpmat_test-mek_1deg.bin` through the separate `INPMAT_TAG`; no new input matrix was generated.
3. Binary runoff with NetCDF outputs: all five output arrays matched their Binary counterparts bitwise. The NetCDF output had 40 records and matching 80 × 120 coordinates. Inspection found two metadata defects in the model writer: singular `cell_method` and non-CF units `0-1` for dimensionless `fldfrc`. These were corrected to `cell_methods` and units `1`; corresponding `output_variables.csv` names/units were aligned to actual writer metadata. The model calculation was not changed.

The `gosh_sample/mek02-longterm_netcdf.sh` workflow then ran 2000 and 2001 separately with annual NetCDF runoff, NetCDF model output, four OpenMP threads, and binary restart transfer. 2000 completed with 366 daily records; 2001 completed with 365. The 2001 log confirms reading the 2000-end restart and bifurcation restart companion. Across all five variables this yielded 731 daily output records. Output arrays had expected shapes and no NaN/Inf; the model's finite fill values remain present. The script stores annual namelists, logs, runtime/status, outputs, restarts, and completion signatures; successful runs are skipped only when the recorded inputs and executable signature are unchanged. A repeated invocation recognized both completed years and left their results intact.

The two-year run and repeated-run check establish operational restart continuity and safe same-configuration rerun behavior for this setup. They do not independently verify scientific correctness of restart state, validate all optional physics, compare against v4, or establish multi-platform reproducibility. See validation records VR-0087–VR-0089 and the dataset/run READMEs for commands and limits.

