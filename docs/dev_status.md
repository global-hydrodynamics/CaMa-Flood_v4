# Development status

Development policy: [AGENTS.md](../AGENTS.md).

Authoritative registry and dashboard snapshot: `origin/master:docs/dev_status.md`.
Latest progress and work claims: a linked GitHub Issue when one is used; otherwise the project status and plan.

Snapshot: branch-local project status proposal, 2026-10-31. Project scope reflects maintainer instructions and awaits reconciliation/publication to `origin/master`.

## Registered active projects

The documentation reorganization project was completed and merged to `master` in PR #121; it is no longer active. Map modernization continues under Issue #122. Research logging is a separate, cross-project case-study activity; its current framework edits use `feature/map-data-tools` by maintainer instruction, but the research activity is not tied to that feature branch and has no dedicated Issue. Historical work is not reconstructed in this status table.

| Project / scope and coordination record | Owner | Persistent working branch | Main modules or files | Current progress | Potential conflicts | Next actions |
| --- | --- | --- | --- | --- | --- | --- |
| Map data/tools modernization ([Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122)) — map metadata, conversion, and processing tools. | Dai Yamazaki (maintainer); Codex implements assigned tasks | `feature/map-data-tools` | Map project plan: `docs/map_data_tools_plan.md`; map metadata and conversion files under `map/`. | Phase B2.3 standard bifurcation migration is implemented as shared `param_set_bifparam` / `param_set_bif_basin` and map-local `s02-bifurcation.sh`. The optional depth/width check raster is now named `check_bif_dphwth.bin` and is emitted only when `check_map` is requested; legacy v4 `map/src/` output remains unchanged. Earlier temporary Mekong migration regression matched the legacy outputs byte-for-byte. This is migration-regression evidence, not scientific validation. | Map preparation, core readers, optional-scheme readers, and format contracts may overlap; no new map task is claimed here. | Continue with MPI-specific changes as a separate `s04-mpi_map_setup.sh` migration; preserve the standard `s02` behavior. Reconcile this branch-local status with the authoritative dashboard when publishing. |
| CaMa-Flood v4-to-v5 research case study (cross-project activity; no dedicated Issue) — evidence-linked record of discussions through validation across successive projects. | Dai Yamazaki (maintainer; primary framework user); Codex assists with record maintenance | No research-specific branch. Current framework update: `feature/map-data-tools`, by explicit maintainer instruction. | Canonical record set: `research/`; policy: `AGENTS.md`; protocol: `research/study_protocol.md`. | Framework approved. Git-based documentation and map-tools stage narratives are recorded; the pre-documentation parent commit is tagged. `EV-0001` and the current task are in the event log. Earlier ChatGPT discussion details remain queued for maintainer-supplied sources. | Research records span project boundaries; preserve the same `research/` history when branches change or merge. Initial use is single-maintainer, with ordinary Git handling and no locking system. | Continue significant event logging across projects. Review the pending stage/branch-name caveat and discussion summaries; specify concrete v4/v5 task comparisons and resolve package-baseline identity and human-participant ethics before formal evaluation. The map-tools project remains ongoing. |

## Coordination requiring maintainer decisions

| Decision | Current state | Next action |
| --- | --- | --- |
| Comparative study protocol | Four evaluation dimensions are maintainer-approved; concrete comparable tasks, rubrics, and v4 package identity remain to be specified | Define task-based evidence and baseline before comparative claims or participant evaluation |
| Dashboard publisher | Maintainer responsibility; named publisher not yet assigned | Assign the person responsible for reconciling and publishing rows to `master` |
| Supported validation configurations | Baseline version, compiler matrix, and required datasets not yet agreed | Record acceptance checks in the project plan and status, or in an Issue if one is used |

## Approved updates awaiting publication

Map data/tool work is tracked by Issue [#122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122). Research logging is a cross-project activity with no dedicated Issue. This dashboard copy is branch-local until reconciled on `origin/master`. The B2.3 update records the standard bifurcation migration; legacy tools and committed map data were not modified, and no PR or merge was made.
