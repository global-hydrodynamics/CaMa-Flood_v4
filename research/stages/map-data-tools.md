# Map data/tools modernization stage

## Current boundary and sequence

This project is **in progress**; the end date and final stage commit are not yet known.

- **Project registration/start:** `a91cec8fa73d74fc96966214bbe02f2de1fa643a`, `2026-10-27T11:01:42+09:00`, parent is the documentation merge `11d3fcd`.
- **Current audit snapshot:** local commit `39dc3df7c092bdbd1825cd9e3063d0f669dbe871`, `2026-10-28T15:47:43+09:00`. At task start this commit was one commit ahead of the remote branch; it is not a completion marker.
- **Coordination:** open [Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122); no PR/merge boundary exists yet. The Issue body was last updated before later Oct. 28 commits and does not represent all current progress.
- The branch contains the stage work and the first cross-project research framework. Research logging itself is not owned by this project; it is included here only because this is the current branch where the maintainer authorized these updates.

The visible commit sequence is `a91cec8` (register project), `04b15ba` (status clarification), `7e699d7` (map inventory), `6989ab6` (Python setup), `51d56a5` and `33a3b16` (Mekong sample preparation and complete tracking), `3db9873` (research framework), and `39dc3df` (cross-project research scope clarification). Commit authors are recorded as Dai Yamazaki; Git author metadata does not establish which parts were written by an AI agent.

## Objectives and implementation visible in Git

The project plan aims to improve portability and maintenance of map metadata and map-processing utilities while retaining existing binary and Fortran workflows. Changes through this snapshot include:

- `docs/map_data_inventory.md`, `map/map_variables.csv`, `map/map_metadata_spec.md`, and updated map guidance describing formats, source/reader relationships, and a proposed metadata basis.
- `map/src/map_data_tools/` with category-11 GrADS CTL generation and binary-to-NetCDF tools plus synthetic tests and a Fortran reader-contract smoke test. The implementation is explicitly limited; it does not yet generate all module-level `CRIVCLINC`/`CRIVPARNC` inputs or cover optional schemes.
- Root `requirements.txt` and `s00-python_setup.sh` for a local Python environment.
- Official tracked `map/mekong_15min/`, including map products, metadata, CTLs, bifurcation/input-matrix files, and the complete `1min/` layers; a GitHub Actions workflow validates the committed sample.
- An initial cross-project `research/` framework. Later maintainer instruction explicitly separated research logging from this map project.

The Git diff from `origin/master` to the current snapshot covers 204 changed paths, with 5,845 insertions and 24 deletions; it includes the binary sample data. The diff also contains tracked Python `__pycache__` bytecode under `map/src/map_data_tools/`. This is a verified repository artifact and a maintenance issue, not corrected by this history-reconstruction task.

## Validation evidence from this audit

These are **newly rerun checks on the current source snapshot**, not historical test results from the original commits. Source reference was `39dc3df`; the worktree had only research-record edits during validation.

- `python3 -B map/tests/validate_mekong_sample.py`: 5 tests passed, checking dimensions, binary sizes/counts, network indices, CTL targets, local-path independence, and high-resolution layers.
- `python3 -B -m unittest discover -s map/src/map_data_tools/tests -p 'test_*.py' -v`: 6 tests passed. This included synthetic endian/shape and CTL cases, binary-to-NetCDF checks, and the Fortran NetCDF reader-contract smoke test.
- Environment observed: Darwin 23.3.0 arm64; Python 3.13.3; GNU Fortran 14.2.0; NetCDF 4.6.2. See [`../baseline/environment_inventory.md`](../baseline/environment_inventory.md).
- No full CaMa-Flood simulation was run. Passing sample and converter tests do not establish numerical model equivalence or optional-scheme NetCDF compatibility.

The sample-validation workflow exists in `.github/workflows/map-sample-validation.yml`; GitHub check/status queries for the inspected historical/current refs returned no checks. The workflow's presence is not evidence that it ran successfully on those commits.

## Knowledge gaps, interventions, and next boundaries

Maintainer-confirmed map provenance and unavailable generation workflows are recorded in [`KG-0002`](../records/knowledge_gaps/KG-0002.md) and [`KG-0003`](../records/knowledge_gaps/KG-0003.md), with technical details left in the map inventory/specification. The stage history contains no persistent, timestamped ChatGPT source from which to reconstruct exact alternatives or interventions. No human-intervention event is asserted from commit messages alone; the maintainer can later provide reviewed summaries using the queue document.

The stage remains incomplete. The next work includes the ECMWF-equivalent module-level NetCDF generation contract/code, optional-scheme map support decisions, broader validation, and the project's accepted completion boundary. Do not create `research-stage-map-tools` until the maintainer confirms that boundary.
