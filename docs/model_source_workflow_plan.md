# CaMa-Flood v5 model source and simulation workflow modernization

## Status

**Phase 0 is implemented:** the complete tracked `src/` and `gosh/` trees were copied to `src_main/` and `gosh_sample/`; `src_main/` builds independently with the existing default compiler configuration. Phase 1 has not been run. The Mekong test is the next step, after the maintainer's next instruction.

**Persistent branch:** `feature/gosh-refactor`, created from current `origin/master` at `2841239` on 2026-11-03. No GitHub Issue was requested or created.

The maintainer approved an incremental v5 source and simulation workflow. Keep the legacy `src/` and `gosh/` trees unchanged as v4 references. All source subdirectories are preserved in the v5 copy, including `ICI/`, `sediment/`, `heatlink/`, and supporting libraries. Bifurcation is a standard CaMa-Flood feature and is to be enabled in the first Mekong runtime check. The current task stops after Phase 0's independent compile; Phase 1 runtime execution is deferred.

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

The remaining `gosh_sample/test*.sh`, options, and development examples are initial copies and have not yet been migrated into v5-ready execution examples. Several point to package-only maps/forcing, use current-working-directory-relative paths, clear existing result files, and still contain legacy executable paths. They must not be run as v5 samples until each is explicitly migrated and its paths/output isolation reviewed. Phase 1 should add or prepare a dedicated minimal Mekong example rather than invoke a global/CONUS/Japan legacy recipe.

## Phase 1: Mekong smoke run (approved target, not yet executed)

The first runtime check is to use:

- Map: `map/mekong_15min/` (80 × 120 cells, 10 floodplain layers).
- Runoff: `inp/test-mek_1deg/` (20 × 30 cells, daily float32 `mm/day`, 1980-01-01 onward).
- Parallel mode: non-MPI.
- Bifurcation: enabled, using the sample map's `bifprm.txt` and model map inputs.
- Simulation period: a short period beginning 1980-01-01.

The model time controller computes the step count from end time minus start time. Exact end timestamp, output cadence, and selected outputs will be set in the Phase 1 run configuration and checked against the namelist reader before execution.

The map currently has input matrices for `test-glb_1deg` and `test-E2O_15min_nc`, but not `inpdim_test-mek_1deg.txt` / `inpmat_test-mek_1deg.bin`. The map-local `s03-generate_inpmat.sh` defaults to the E2O grid. Generate the matching 1-degree matrix with the documented domain (90–110°E, 5–35°N, `NtoS`) on a temporary copy of the map. Do not overwrite or add generated matrix files to the tracked sample during the first test. The map and runoff sample are read-only inputs; place model logs/results in a dedicated ignored run directory.

Source inspection confirms that `LPTHOUT` enables bifurcation; when true, `cmf_ctrl_maps_mod.F90` reads the formatted bifurcation table named by `CPTHOUT` (whose source default is `./bifprm.txt`). The Mekong map contains `bifprm.txt`, and the legacy standard example explicitly sets `LPTHOUT=".TRUE."`. The current namelist initializer sets `LPTHOUT=.FALSE.`, however. For the approved first Mekong test, set it explicitly to true; this task does not change the model-wide default. The difference between the maintainer's “standard/default ON” description and the source initializer remains a point to clarify before changing any default.

Practical acceptance checks: the process exits successfully; the log shows the requested map, runoff, matrix, and time range; expected steps and output records are present; output files are nonempty with expected dimensions/time coverage; and no obvious non-finite or structurally implausible result appears. This is an operational smoke test, not a scientific validation or proof of numerical equivalence with v4. Since the numerical source has not been changed, strict v4 comparison is not required unless later evidence or scope warrants it.

## Incremental feature migration order

The current legacy examples indicate distinct profiles; their numbering is not a mandatory sequence.

| Legacy example | Main feature/data profile | Suggested v5 migration checkpoint |
| --- | --- | --- |
| `test1-glb_15min.sh` | Global binary maps, daily binary runoff, bifurcation, adaptive step, multi-year/restart. | Start with the short Mekong non-MPI/bifurcation run; later migrate longer runs and restart as separate checks. |
| `test2-conus_06min_netcdf.sh` | CONUS and NetCDF runoff. | After required NetCDF map/runoff data and the supported model input contract are confirmed. |
| `test3-jpn_fcast.sh` | Japan high-resolution hourly forcing and forecast/restart directories. | After a tracked/reproducible dataset and restart behavior are established. |
| `test4-e2o_glb_15min.sh` | Global NetCDF runoff, binary model output. | After NetCDF forcing configuration is checked independently. |
| `test5_mpi.sh` | MPI build and multi-rank execution. | After serial sample execution; requires MPI runtime and matching partition map. |
| `test6-reservoir_glb15min.sh` | Reservoir operation and dam data. | After base routing; test scheme data and activation separately. |
| `options_full.sh` | Practical compile/runtime option examples. | Migrate only options verified available in v5 and document compile-time versus runtime controls. |
| `dev_ERA5_hour.sh`, `gosh/etc/` | Development forcing and specialized heat-link configuration. | Later, after dependencies, provenance, and supported combinations are established. |

## Phase 0 validation and remaining limits

Completed:

- Full tracked-tree copies present, retaining all source and workflow subdirectories.
- All tracked source/workflow files match except the intentionally updated v5 build Makefile/wrapper and added READMEs.
- `make -C src_main all` completed and produced `src_main/MAIN_cmf`.
- No v4 source, workflow, build setting, map data, or runoff data was changed.

Not performed:

- No model simulation (including Phase 1 Mekong) was run.
- No optional MPI, NetCDF, sediment, heatlink, or ICI build was attempted.
- No v4/v5 numerical comparison or scientific validation was performed.

## Open questions for Phase 1 configuration

1. Confirm the exact short-run end timestamp, output cadence, and output variables. The first forcing record begins 1980-01-01 and the source computes duration as end minus start, but this plan does not choose an unreviewed output contract.
2. Confirm whether the first successful runtime smoke test is sufficient as the v5 baseline before a longer or restart test. Restart and multi-year behavior remain separate validation stages.

