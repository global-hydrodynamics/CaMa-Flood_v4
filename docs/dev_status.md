# Development status

Development policy: [AGENTS.md](../AGENTS.md).

Authoritative registry and dashboard snapshot: `origin/master:docs/dev_status.md`.
Latest progress and work claims: a linked GitHub Issue when one is used; otherwise the project status and plan.

Snapshot: project-branch planning proposal, 2026-10-27. Map project registration is pending maintainer review and publication to `origin/master`.

## Registered active projects

The documentation reorganization project was completed and merged to `master` in PR #121; it is no longer active. The map data and tools project below is a branch-local registration proposal based on the maintainer's instructions in the Codex chat on 2026-10-25. Its implementation scope is pending maintainer review; a GitHub Issue is optional. Historical projects are not inventoried here.

| Project / scope and coordination record | Owner | Persistent working branch | Main modules or files | Current progress | Potential conflicts | Next actions |
| --- | --- | --- | --- | --- | --- | --- |
| Map data and tools modernization ([Issue #122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122)) — metadata, map-processing utilities, binary/NetCDF interoperability, and compatibility investigation. Approval: maintainer instructions in the Codex chat, 2026-10-25, for the project; the current category and core-conversion task was authorized in the Codex request, 2026-10-28. | Dai Yamazaki (maintainer); Codex implements assigned tasks | `feature/map-data-tools` | Current task: `docs/map_data_inventory.md`; project plan: `docs/map_data_tools_plan.md`. Phase 1 review candidates: `map/src/`, `src/cmf_ctrl_maps_mod.F90`, shared map I/O, and optional-scheme map readers; no implementation ownership assigned yet. | Phase 1 inventory now has numeric categories and record metadata. The current task implements shared metadata/config validation, category-11 CTL generation and binary-to-NetCDF conversion, plus synthetic binary tests and a NetCDF-Fortran reader-contract smoke test. All six tests pass; the tools were also run against the read-only v4.30 `glb_15min` sample, and all seven logical core variables matched their source binary values exactly. No model source or packaged map files were changed. Full model simulation, channel-parameter conversion, and non-core product conversion remain outside this task. | Map-preparation tools, core map readers, optional-scheme map readers, and format contracts may overlap; no implementation claims are recorded. | Review the category-11 tools and their reader-contract tests. Then resolve provisional auxiliary/MPI product meanings and prioritize source tracing for runoff-mapping and specialized products; decide future scope for `CRIVPARNC` and optional schemes. |

## Coordination requiring maintainer decisions

| Decision | Current state | Next action |
| --- | --- | --- |
| Map project implementation scope | Project initiation, planning task, and persistent branch approved in chat; detailed phase scope and acceptance checks not yet agreed; Issue optional | Review `docs/map_data_tools_plan.md`, approve the first implementation phase and its checks, and decide whether an Issue would help coordinate it |
| Dashboard publisher | Maintainer responsibility; named publisher not yet assigned | Assign the person responsible for reconciling and publishing rows to `master` |
| Supported validation configurations | Baseline version, compiler matrix, and required datasets not yet agreed | Record acceptance checks in the project plan and status, or in an Issue if one is used |

## Approved updates awaiting publication

Map data and tools project registration and plan above are proposals on `feature/map-data-tools`, awaiting maintainer review and publication to `origin/master`. Issue [#122](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/122) tracks the project. The category-11 tools and test results are branch-local and await maintainer review. No model/source changes, PR, or merge were performed.
