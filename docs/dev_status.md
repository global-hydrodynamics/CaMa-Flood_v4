# Development status

Development policy: [AGENTS.md](../AGENTS.md).

Authoritative registry and dashboard snapshot: `origin/master:docs/dev_status.md`.
Latest progress and work claims: each project's linked GitHub issue.

Snapshot: project-branch registration proposal, 2026-10-25. Publication to `origin/master`: pending maintainer publication.

## Registered active projects

The documentation project below is approved in the maintainer's instructions in the Codex chat on 2026-10-25. This row is a local registration proposal awaiting publication to the authoritative registry on `origin/master`; a GitHub coordination issue has not yet been linked. Historical projects are not inventoried here.

| Project / scope and approval issue | Owner | Persistent working branch | Main modules or files | Current progress | Potential conflicts | Next actions |
| --- | --- | --- | --- | --- | --- | --- |
| Documentation reorganization — root README, directory-level READMEs, and existing manuals. Approval: maintainer instructions in the Codex chat, 2026-10-25; GitHub issue pending. | Dai Yamazaki (maintainer); Codex implements assigned tasks | `docs/reorganization` | Project scope: `README.md`, component READMEs, and manuals in `doc/` and component directories. Current task: `README.md` and this registration only. | Root README rewritten; internal links and scope checked; local commit authorized. No push or PR authorized. | README and manuals may overlap other documentation work; no other project claims are recorded in the current registry. | Maintainer reviews the local commit, links a coordination issue, and publishes the registration when authorized. Reuse this branch for subsequent approved documentation tasks. |

## Coordination requiring maintainer decisions

| Decision | Current state | Next action |
| --- | --- | --- |
| Documentation project publication | Branch and scope approved in chat; registration is local to `docs/reorganization` | Link a GitHub coordination issue and publish the approved row to `master` through the existing status workflow |
| Dashboard publisher | Maintainer responsibility; named publisher not yet assigned | Assign the person responsible for reconciling and publishing rows to `master` |
| Supported validation configurations | Baseline version, compiler matrix, and required datasets not yet agreed | Record acceptance checks in the first relevant project issue |

## Approved updates awaiting publication

Documentation reorganization registration above awaits maintainer publication. Approval is recorded in the Codex chat; no issue, push, PR, or merge was performed for this task.
