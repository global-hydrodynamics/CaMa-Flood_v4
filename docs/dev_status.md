# Development status

Development policy: [AGENTS.md](../AGENTS.md).

Authoritative registry and dashboard snapshot: `origin/master:docs/dev_status.md`.
Latest progress and work claims: a linked GitHub Issue when one is used; otherwise the project status and plan.

Snapshot: branch-local proposal, 2026-11-03. Reconciled against `origin/master` at `2841239` and GitHub Issues/PRs #122, #125, #126, and #127. The `origin/master` copy remains the authoritative published dashboard until this update is merged.

## Registered active projects

The documentation reorganization project was completed and merged to `master` in PR #121. The Runoff Input Data Tools implementation tranche was merged in PR #127 and Issue #126 is closed. Map modernization remains open but paused between approved stages under Issue #122. Research logging is a separate, cross-project case-study activity with no dedicated Issue. This dashboard summarizes current coordination; detailed historical evidence remains in the linked project and research documents.

| Project / scope and coordination record | Owner | Persistent working branch | Main modules or files | Current progress | Potential conflicts | Next actions |
| --- | --- | --- | --- | --- | --- | --- |
| Map data/tools modernization ([Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122)) — improve map metadata, conversion, and processing tools. | Dai Yamazaki (maintainer); Codex implements assigned stages | `feature/map-data-tools` | [Project plan](map_data_tools_plan.md); [migration plan](map_params_migration_plan.md); map metadata and workflows under `map/`. | The first v5 map-tools implementation tranche—including regionalization, parameter, MPI, allocation, and analysis workflows plus the Mekong development sample—was merged to `master` in PR #125 (`bf82017`). Issue #122 remains open and paused between stages; this tranche does not complete broader map modernization. | Preserve historical v4 tools under `map/src/`. `gosh/` simulation workflow modernization is a separate upcoming development area. | Later approved stages may define model-facing NetCDF map contracts, optional-scheme support, conversion guarantees, and additional validation. The plans and research records retain the technical open questions. Do not interpret existing software checks as independent scientific validation or general numerical equivalence. |
| CaMa-Flood v4-to-v5 research case study (cross-project activity; no dedicated Issue) — evidence-linked record of discussions through validation across successive projects. | Dai Yamazaki (maintainer; primary framework user); Codex assists with record maintenance | No research-specific branch. Current framework update: `feature/map-data-tools`, by maintainer instruction. | Canonical records: [`research/`](../research/README.md); policy: [`AGENTS.md`](../AGENTS.md); protocol: [`research/study_protocol.md`](../research/study_protocol.md). | The approved framework and event-based records span documentation, map tools, and runoff data work. The pre-documentation parent commit is tagged; earlier ChatGPT discussion details remain queued for maintainer-supplied sources. | Preserve the same research history across projects and branch transitions. Initial use is single-maintainer with ordinary Git coordination. | Continue recording material discussions, decisions, implementation, and validation. Concrete v4/v5 task comparisons, package-baseline identity, and human-participant ethics remain for study design. |

## Recently completed project milestone

| Project / coordination record | Merged scope | Deferred or unresolved work |
| --- | --- | --- |
| Runoff Input Data Tools ([Issue #126](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/126), [PR #127](https://github.com/global-hydrodynamics/CaMa-Flood_v4/pull/127); merge commit `2841239`) | The `feature/runoff-data-tools` changes are in `master`. The merged work includes the runoff product registry and validator, dataset/user documentation, regenerable global and Mekong examples, the tracked Japan 6-minute hourly sample, and ERA5-Land acquisition and conversion tooling under `inp/ERA5-Land_download/`. Issue #126 is closed as completed. | End-to-end model testing and generation/integration of annual-mean runoff inputs for channel parameter estimation are not complete. KG-0019 (time-step semantics), KG-0022 (Japan sample unit evidence), KG-0023 (ELSE_GPCC source CTL/coverage), and KG-0024 (linked laboratory ERA5-Land metadata) remain recorded in `research/`. This status does not claim scientific validation of runoff products. |

## Coordination requiring maintainer decisions

| Decision | Current state | Next action |
| --- | --- | --- |
| Comparative study protocol | Four evaluation dimensions are maintainer-approved; concrete comparable tasks, rubrics, and v4 package identity remain to be specified | Define task-based evidence and baseline before comparative claims or participant evaluation |
| Dashboard publisher | Maintainer responsibility; named publisher not yet assigned | Assign the person responsible for reconciling and publishing rows to `master` |
| Supported validation configurations | Baseline version, compiler matrix, and required datasets not yet agreed | Record acceptance checks in the project plan and status, or in an Issue if one is used |

## Technical records

Detailed map-tool implementation boundaries and deferred design questions remain in [`docs/map_data_tools_plan.md`](map_data_tools_plan.md), [`docs/map_params_migration_plan.md`](map_params_migration_plan.md), and the linked `research/` records. Runoff datasets, processing evidence, and scientific uncertainties remain in [`docs/runoff_data_inventory.md`](runoff_data_inventory.md), `inp/runoff_datasets.csv`, and the corresponding knowledge-gap records. This dashboard avoids duplicating those technical details.
