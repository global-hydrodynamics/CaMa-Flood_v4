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
