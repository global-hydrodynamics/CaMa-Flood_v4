# Model source and simulation workflow modernization

## Stage boundary

This stage creates a v5 development copy beside, and isolated from, the preserved v4 source and scripts. The maintainer approved feature/gosh-refactor on 2026-11-03 from origin/master at 2841239; no GitHub Issue was requested. The full-source and workflow copy decision, bifurcation-on Mekong target, implementation, and compile evidence are indexed in EV-0260 through EV-0263, DT-0041, and DR-0050.

## Phase 0: copied trees and isolated default build

The complete Git-tracked src/ and gosh/ trees were copied to src_main/ and gosh_sample/. The initial copy retained all feature directories, including ICI, sediment, and heatlink; copying does not indicate that their alternate builds are supported in v5. The copied gosh_sample/compile.sh now builds only src_main/, avoiding the legacy gosh/compile.sh path that cleans/builds util/, map/src/, and src/. Shared adm/ settings were not modified.

make -C src_main all succeeded with GNU Fortran 14.2.0 and the default OpenMP/SinglePrec_CMF configuration, producing src_main/MAIN_cmf. It emitted unused-import warnings at two source locations and no compile/link errors. All tracked source/workflow files match except the intentionally adapted v5 Makefile, compile wrapper, and READMEs. Legacy src/ and gosh/ were not changed. See VR-0082.

Phase 0 performed no simulation or numerical comparison. It does not validate optional MPI, NetCDF, sediment, heatlink, or ICI configurations.

## Phase 1: initial Mekong annual simulation (completed)

At Phase 0 close, the planned smoke run had been deferred to a later instruction; the initial short-run settings in that planning snapshot were superseded by the maintainer's Phase 1 task. Phase 1 then specified a 2000-01-01 through 2001-01-01 run. The maintainer's task and its implementation/validation are recorded in EV-0265 through EV-0267, DT-0042, and VR-0083.

The map-local `mek03-generate_inpmat-mek1deg.sh` created the 20 x 30 runoff mapping for the 80 x 120 Mekong map and the generated `inpdim_test-mek_1deg.txt` / `inpmat_test-mek_1deg.bin` are retained with the sample. `gosh_sample/mek01-default_run.sh` ran one non-MPI 2000 simulation with adaptive timestep, bifurcation enabled, zero initial storage, and reservoir operation disabled. It read 366 daily runoff files, completed 8,784 timesteps through 2001-01-01 00:00, wrote 366 records for four gridded variables and bifurcation flow, and wrote a final restart. The checked output values were finite, including the model's finite `1e20` fill values. Negative flow values are permitted by the source's backflow calculation. This is operational evidence for this sample and build, not scientific validation or v4 numerical equivalence.

The detailed technical boundaries and phased migration order are in docs/model_source_workflow_plan.md.

The new wrapper exposed an incremental build issue during repeated use; the v5-only Makefile dependency guard resolved it. Details and limits are recorded in VR-0082 and the Phase 0 plan.

A final tracked-file comparison confirms all copied files match byte-for-byte except the intended v5 Makefile/wrapper and READMEs. The wrapper also builds when invoked from outside the repository directory. Source inspection found LPTHOUT initializes false, although the approved first test and legacy standard sample set it true; the Phase 1 v5 run set it explicitly without changing the model default.

## Phase 1 follow-up: NetCDF I/O and annual restart run

The maintainer subsequently authorized a separate NetCDF-enabled build, annual Mekong runoff conversion, three 40-day I/O comparisons, and a 2000–2001 run split at the year boundary. This follow-up is recorded in EV-0278–EV-0284, DT-0046, and VR-0087–VR-0090. It remains part of the model-source workflow stage; it does not change the completed initial `mek01` milestone above.

The NetCDF build wrapper compiles an isolated temporary source copy and writes `out/bin/MAIN_cmf_netcdf`, leaving the regular Binary executable/build tree intact. On the verified macOS/Homebrew environment, the NetCDF-enabled executable ran with Binary input and output and matched the Binary-build outputs bitwise for `outflw`, `storge`, `sfcelv`, `flddph`, and `fldfrc` over 40 days. The same variables matched bitwise when switching between Binary and NetCDF runoff input, and when switching between Binary and NetCDF output. The NetCDF metadata comparison found and corrected the writer's `cell_method` attribute spelling and the non-CF dimensionless `fldfrc` unit `0-1`; the standard `cell_methods` attribute and unit `1` are now used. No scientific calculation was changed.

The derived `inp/test-mek_1deg_nc/` sample contains annual CF files for 2000 (366 records) and 2001 (365 records), converted from the existing tracked daily Binary sample without rescaling. All 731 float32 fields matched exactly after conversion and readback. The model run completed 2000 and then 2001 using the 2000-end Binary restart and bifurcation restart companion; the NetCDF outputs cover 731 daily records. A repeated invocation preserved and skipped both completed years when their inputs/executable signatures were unchanged. This establishes operational behavior for the tested configuration, not independent scientific validity or a general proof of restart equivalence.

The annual NetCDF files are intended as repository sample data. Whether to add them to a future downloadable v5 package remains open because the established package script is a legacy v4 workflow and a v5 package assembly contract is not yet defined (KG-0026). The full research validator also currently fails on historical event references to validation IDs missing from the CSV index; the referenced VR Markdown exists for the first failure (`EV-0033` → `VR-0009`). New records for this task cross-reference correctly. See VR-0090; the historical index inconsistency was not broadly repaired as part of this modeling task.

## Individual Binary/NetCDF map inputs

The maintainer approved independent `.bin`/`.nc` selection for existing raster namelist paths while preserving `LMAPCDF=.TRUE.` as the legacy consolidated-container mode. The implementation and end-to-end comparisons are recorded in DT-0047 and VR-0091. `src_main/cmf_ctrl_maps_mod.F90` now routes individual raster reads through shared helpers, validates NetCDF type and dimensions, preserves Binary direct-access records/byte handling, and supports mixed input formats without new namelist keys. `map/tools/code/convert_map_binary_to_netcdf.py` uses the variable registry and `params.txt`; the Mekong core rasters were converted into temporary validation data only.

On macOS/Homebrew GNU Fortran, the Binary-only and NetCDF-enabled builds succeeded. The 40-day Mekong run completed for all-Binary, rivhgt-only NetCDF, nextxy-only NetCDF, and all-nine-core-map-input NetCDF configurations. For each NetCDF-enabled scenario the five selected model output files matched the same-executable all-Binary baseline byte-for-byte. The rebuilt Binary-only executable also matched the prior Binary baseline. Unsupported-extension, missing-variable, dimension-mismatch, and Binary-only `.nc` / legacy-CDF-mode failures were checked. This verifies operational I/O compatibility for the sample, not scientific validity or all optional map pathways. At the initial implementation report, runtime validation of optional groundwater, mean-sea-level, MPI, slope/mask, and legacy consolidated NetCDF inputs remained pending; the subsequent scope clarification below specifically rejects slope/mask options in individual mode and reruns affected cases.


### Individual Map-input scope clarification

After the initial implementation, the maintainer clarified that individual `.bin`/`.nc` dispatch under `LMAPCDF=.FALSE.` must cover only variables read through the old Binary path. The prior individual readers for ECMWF-specific `elevslope` and `mask_slope` were removed. `LSLOPEMOUTH` and `LSLPMIX` now fail early and explicitly in individual-file mode; their legacy consolidated-NetCDF routines remain in their old path. Source inspection found that the historic `elevslope` read follows a close of its NetCDF handle and `SET_SLOPEMIX` uses a handle it does not open. Those pre-existing issues are documented, not repaired here.

In a later bounded task, the maintainer authorized fixing those two legacy NetCDF handle lifecycle bugs without extending individual-file support or changing scientific behavior. `elevslope` is now read while `CRIVPARNC` is open, and `SET_SLOPEMIX` opens and closes `CRIVPARNC` around `mask_slope`. Both build variants and five 40-day Mekong Binary/individual-NetCDF regression cases passed after the fix; five selected outputs remained byte-identical. The legacy slope-option runtime still lacks a suitable consolidated NetCDF fixture. See ADR-0002 and VR-0093.

The corrected source rebuilt in both Binary-only and NetCDF-enabled configurations. The 40-day Mekong Binary-only baseline, NetCDF-executable Binary case, rivhgt-only and nextxy-only mixed cases, and all-nine-core-input NetCDF case completed. The five selected model outputs were byte-identical across these runs, and direct typed comparisons confirmed exact values for all nine converted map products, including both `nextxy` records and ten `fldhgt` layers. Both individual-mode slope guards were exercised and returned status 9 before topography reads. No legacy consolidated NetCDF fixture was found, so that route remains unverified at runtime. The clarification and validation are recorded in ADR-0001 and VR-0092; they supersede the broader slope-input scope in the initial implementation summary while preserving its chronology.


## Restart state-I/O refactor

The maintainer approved a structural refactor of `cmf_ctrl_restart_mod.F90` to make model-state selection and file representation independently readable. The principal `CMF_RESTART_INIT` and `CMF_RESTART_WRITE` procedures now show Normal/Storage-only behavior and option-gated state lists, while private generic `RESTART_UTIL_READ` and `RESTART_UTIL_WRITE` procedures route JPRM/JPRD arrays through the existing Binary or NetCDF implementations. The overloads compile both when `JPRB` is distinct from `JPRD` and when they are the same kind.

The reason for the split is maintainability: developers can review which state belongs to a Restart without tracing file encodings and conversion helpers. The implementation retained existing Binary records, NetCDF variables and precision, optional-state ordering, bifurcation-path behavior, and timing. Default Binary, NetCDF, and double-kind builds passed. One-day Mekong Normal Restart comparisons against the prior source were byte-identical for the tested Restart and selected model-output files in both Binary and NetCDF formats. NetCDF Storage-only write/read completed. Optional schemes, MPI, and coupled runs were not exercised. Details and limitations are in [VR-0117](../records/validation_runs/VR-0117.md).


## Interactive workstation setup

The repository-root `s01-setup_fortran.sh` checks compiler and library readiness with small compile/link tests and offers to generate a machine-specific root `Mkinclude`. It does not install packages or modify `adm/Mkinclude`; the user activates the reviewed configuration manually. `s02-python_setup.sh` diagnoses Python and offers to create or update the local `.venv/` using the unchanged `requirements.txt`. Both scripts ask before filesystem changes and only diagnose when no interactive terminal is attached. Component availability is reported separately from simulation execution. The scripts were tested on macOS/Apple Silicon; unsupported/missing components and user-declined changes were exercised with controlled mocks. See [DT-0074](../development_tasks.csv) and [VR-0119](../records/validation_runs/VR-0119.md).

## Tutorial framework planning

A tutorial-development plan now separates provisional experiment specifications, a reusable authoring guide, and tutorials based on verified implementation and execution evidence. Tutorial 02 is recorded only as a concise illustrative outline; its scientific design is deferred until source and dataset investigation. The plan distinguishes the complete GitHub development artifacts from the smaller beginner-package contents and does not implement either tutorials or packaging. No tutorial execution or result is claimed. See [docs/tutorial_development_plan.md](../../docs/tutorial_development_plan.md) and validation note VR-0120.
