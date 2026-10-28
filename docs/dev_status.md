# Development status

Development policy: [AGENTS.md](../AGENTS.md).

Authoritative registry and dashboard snapshot: `origin/master:docs/dev_status.md`.
Latest progress and work claims: a linked GitHub Issue when one is used; otherwise the project status and plan.

Snapshot: branch-local project status proposal, 2026-10-28. Project scope reflects maintainer instructions in the Codex chat and awaits reconciliation/publication to `origin/master`.

## Registered active projects

The documentation reorganization project was completed and merged to `master` in PR #121; it is no longer active. The map modernization and v5 research logging work below share the persistent branch by explicit maintainer instruction in the Codex chat on 2026-10-28. The research framework has no GitHub Issue, as directed. Historical work is not reconstructed in this status table.

| Project / scope and coordination record | Owner | Persistent working branch | Main modules or files | Current progress | Potential conflicts | Next actions |
| --- | --- | --- | --- | --- | --- | --- |
| Map data/tools modernization and v5 research case study ([Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122) covers map work; research framework has no Issue per maintainer direction) — map metadata/conversion plus a lightweight evidence-linked research logging framework. Approval: map project in Codex chat, 2026-10-25; research logging on this same branch approved in Codex chat, 2026-10-28. | Dai Yamazaki (maintainer); Codex implements assigned tasks | `feature/map-data-tools` | Current task: `research/` framework, research logging guidance in `AGENTS.md`, and this status proposal; map project plan: `docs/map_data_tools_plan.md`. | Official `map/mekong_15min/` sample and category-11 conversion tools are implemented. Phase 1 research logging framework is drafted; actual event/task/validation tables remain empty, and no prior October history was reconstructed. | `AGENTS.md` and this dashboard now cover both cross-cutting research records and map modernization on one maintainer-approved branch. | Review the research protocol, schemas, privacy procedure, and proposed ADR. Then record only prospective or explicitly retrospective evidence-backed work; resolve package-baseline identity and human-participant ethics before formal evaluation. |

## Coordination requiring maintainer decisions

| Decision | Current state | Next action |
| --- | --- | --- |
| Research study protocol and data handling | Phase 1 framework authorized in chat; no Issue requested. Human-participant consent/ethics and exact distribution-package baseline remain unresolved | Review `research/study_protocol.md` and `research/baseline/v4_baseline.md` before formal evaluation or package-level claims |
| Dashboard publisher | Maintainer responsibility; named publisher not yet assigned | Assign the person responsible for reconciling and publishing rows to `master` |
| Supported validation configurations | Baseline version, compiler matrix, and required datasets not yet agreed | Record acceptance checks in the project plan and status, or in an Issue if one is used |

## Approved updates awaiting publication

Map data/tool work is tracked by Issue [#122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122). The research logging framework was explicitly added to the same branch by the maintainer; no separate Issue was requested. These scope/status updates are branch-local proposals until reconciled on `origin/master`. This phase changes documentation and record-validation tooling only; no model algorithms, PR, or merge were changed.
