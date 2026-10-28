# Development task record template

Use `development_tasks.csv`; add only one row per substantial task.

- `task_id`: next unused `DT-####`.
- `objective`: concise outcome and approved scope.
- `branch`, `owner`: approved persistent branch and responsible maintainer/developer.
- `start_event_id`, `start_commit`: observed task authorization/start event and exact starting revision.
- `issue_url`, `pr_url`: link if one exists; leave blank if none. Do not duplicate Issue/PR content.
- `agent`, `model_version`: record only what is reliably available.
- `outcome`, `end_event_id`, `end_commit`: actual result and ending revision; distinguish incomplete/failed from complete.
- `validation_ids`: semicolon-separated `VR-####` references.
- `elapsed_seconds`, `human_effort_minutes`, `tokens`, `cost_usd`: optional. Include the paired basis/source fields; label measured versus estimated and leave unavailable values blank.

For a substantial modernization task, add a short evidence-linked note with:

- **Area:** preprocessing / input-output / postprocessing / documentation / testing / development infrastructure / other (select applicable areas).
- **Prior constraint:** the existing limitation, dataset/environment dependency, or knowledge gap the task addresses. Cite evidence; do not infer a cause from the diff alone.
- **Change and enabled capability:** what interface, data contract, or workflow changed, and what new reuse or extension it makes possible. Distinguish implemented behavior from planned work.
- **Scientific and compatibility impact:** whether a scientific method, physical parameterization, or model feature changed (yes / no / unknown); identify rationale, approval evidence, affected behavior, backward-compatibility checks, and numerical differences where applicable.

Keep these details in an existing task note or relevant stage/technical document when that is clearer; do not add CSV columns or duplicate the Issue/PR. For ordinary fixes or external contributions, record a change category only when it materially helps distinguish that work from infrastructure modernization.
