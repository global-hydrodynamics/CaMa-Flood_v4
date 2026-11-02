# Model source and simulation workflow modernization

## Stage boundary

This stage creates a v5 development copy beside, and isolated from, the preserved v4 source and scripts. The maintainer approved feature/gosh-refactor on 2026-11-03 from origin/master at 2841239; no GitHub Issue was requested. The full-source and workflow copy decision, bifurcation-on Mekong target, implementation, and compile evidence are indexed in EV-0260 through EV-0263, DT-0041, and DR-0050.

## Phase 0: copied trees and isolated default build

The complete Git-tracked src/ and gosh/ trees were copied to src_main/ and gosh_sample/. The initial copy retained all feature directories, including ICI, sediment, and heatlink; copying does not indicate that their alternate builds are supported in v5. The copied gosh_sample/compile.sh now builds only src_main/, avoiding the legacy gosh/compile.sh path that cleans/builds util/, map/src/, and src/. Shared adm/ settings were not modified.

make -C src_main all succeeded with GNU Fortran 14.2.0 and the default OpenMP/SinglePrec_CMF configuration, producing src_main/MAIN_cmf. It emitted unused-import warnings at two source locations and no compile/link errors. All tracked source/workflow files match except the intentionally adapted v5 Makefile, compile wrapper, and READMEs. Legacy src/ and gosh/ were not changed. See VR-0082.

Phase 0 performed no simulation or numerical comparison. It does not validate optional MPI, NetCDF, sediment, heatlink, or ICI configurations.

## Phase 1: next approved runtime stage

The maintainer approved a short non-MPI simulation beginning 1980-01-01 with bifurcation enabled, using map/mekong_15min/ and inp/test-mek_1deg/. That runtime work is deliberately deferred to the next instruction. A matching test-mek_1deg input matrix is absent and must be generated on a disposable map copy. Exact run end time and output contract remain to be specified after checking namelist/time-controller behavior. Successful execution will be evidence of operational behavior, not independent scientific validation.

The detailed technical boundaries and phased migration order are in docs/model_source_workflow_plan.md.

The new wrapper exposed an incremental build issue during repeated use; the v5-only Makefile dependency guard resolved it. Details and limits are recorded in VR-0082 and the Phase 0 plan.

A final tracked-file comparison confirms all copied files match byte-for-byte except the intended v5 Makefile/wrapper and READMEs. The wrapper also builds when invoked from outside the repository directory. Source inspection found LPTHOUT initializes false, although the approved first test and legacy standard sample set it true; the first v5 run will set it explicitly without changing the model default.
