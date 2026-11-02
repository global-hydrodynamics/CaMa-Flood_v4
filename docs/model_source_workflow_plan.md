# CaMa-Flood v5 model source and simulation workflow modernization

## Status

**Phase 0 and the initial Phase 1 Mekong run are implemented and operationally validated.** The complete tracked `src/` and `gosh/` trees were copied to `src_main/` and `gosh_sample/`; `src_main/` builds independently with the existing default compiler configuration. The 2000 Mekong annual run completed with bifurcation enabled. This is an operational smoke test, not independent scientific validation or a v4 numerical comparison.

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

## Phase 1: Mekong default simulation (completed)

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

The process completed with 8,784 model steps and 366 daily records for each gridded output. Each 80 × 120 float32 output is 14,054,400 bytes; `pthflw2000.pth` and the final restart products were also nonempty. All values in the four gridded files and bifurcation output were finite; the gridded outputs contain the model's finite `1e20` fill value on missing cells. `outflw` and `pthflw` contain negative values; the routing source permits backflow, so sign alone was not treated as an error. Existing map and runoff inputs were not modified; the generated input descriptor and matrix are the only new map products. This establishes operational behavior for this build and sample, not scientific validity or numerical equivalence with v4. See VR-0083 for evidence and limitations.

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
- The 2000 Mekong annual simulation completed; the model read the configured input matrix and all expected daily runoff files, and wrote the requested daily output records.
- No legacy `src/` or `gosh/` content, shared build setting, or existing map/runoff product was changed. The two newly generated Mekong `test-mek_1deg` matrix files were added to the sample.

Not performed:

- No optional MPI, NetCDF, sediment, heatlink, or ICI build was attempted.
- No v4/v5 numerical comparison or independent scientific validation was performed.

