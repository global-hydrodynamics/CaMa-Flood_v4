# Research record integrity review — 2026-11-06

## Scope and record model

This review covers the shared research history on `feature/gosh-refactor`. `events.csv` is the chronological event index; `development_tasks.csv` and `validation_runs.csv` provide analysis fields. Discussions, knowledge gaps, and human interventions use both CSV indexes and detail files. Decision records are Markdown files in two established directories. Validation detail Markdown is optional and can serve as a standalone evidence record; it need not have a `validation_runs.csv` row. Stage summaries and README files are derived explanations, not replacement event records.

The review inspected all six research CSV tables, their cross-references, all indexed detail directories, both decision directories, local Markdown links, current instructions, and reachable Git history. The initial tracked state contained 390 events, 78 tasks, 59 indexed discussions, 24 indexed knowledge gaps, 16 interventions, 47 indexed validations, and eight decision files. The safety snapshot was made before any edits: `research_bak261106/` contains 258 files and 12 directories; its 270-entry inventory and file SHA-256 values matched the original tree exactly. The backup remains ignored and unmodified.

## Evidence-supported repairs

- Added the missing discussion index rows for `DR-0054`, `DR-0057`, and `DR-0060`, and knowledge-gap index rows for `KG-0018` through `KG-0027`. Their event links, categories, evidence descriptions, statuses, and known paths were taken from the corresponding existing detail files; no detail narrative was rewritten.
- Resolved four duplicated decision identifiers. The original `research/decisions/ADR-0001` through `ADR-0004` remain unchanged. The distinct later records in `research/records/decisions/` are now `ADR-0005` through `ADR-0008`. Only ID tokens, paths, and cross-references changed.
- Repaired malformed CSV structure in `events.csv`: `EV-0326` had evidence paths in the record-reference field and lacked a privacy field; `EV-0335` had commas in its summary outside the quoted field. The summary wording was preserved while restoring its field boundaries.
- Restored the intended field alignment for validation rows `VR-0098` through `VR-0101`. Their result is `pass`; method, interpretation, and evidence strings were moved into their corresponding columns without changing the strings.
- Corrected stale Markdown links after the map metadata registry and ERA5-Land/sample directories moved. A formerly broken `VR-0075` Markdown link is now plain text so the unresolved ID remains visible without a dead link.
- The validator now aggregates findings in a read-only pass, checks row widths, globally unique ADR identifiers, index/detail agreement for discussions/gaps/interventions, multiple event links where the records use them, and relative Markdown links. It recognizes the historical `knowledge_gap` event type and outcome values `pass`, `partial_pass`, and `open`. `--next-ids` prints suggestions without reserving or writing IDs.
- Added four focused validator tests for aggregated dangling-reference reporting, supported multi-event links, malformed row width and duplicate ADR IDs, and read-only ID suggestions. The research README, decision-record instructions, and `AGENTS.md` now describe the synchronized-index workflow.

## Unresolved historical references

The following referenced validation IDs have no current CSV row or detail file, and no authentic file or index row was found in reachable Git history. Their target cannot be established without inventing validation metadata, so references are preserved:

| Missing ID | Current references |
| --- | --- |
| `VR-0021` | `events.csv` `EV-0067` |
| `VR-0032` | `events.csv` `EV-0104`; task `DT-0006` |
| `VR-0059` | `EV-0180`; task `DT-0022` |
| `VR-0060` | `EV-0183`; task `DT-0023` |
| `VR-0061` | `EV-0186`; task `DT-0024` |
| `VR-0062` | `EV-0189`; task `DT-0025` |
| `VR-0063` | `EV-0192`; task `DT-0026` |
| `VR-0068` | `EV-0210`; task `DT-0030` |
| `VR-0075` | `EV-0241`; task `DT-0037`; `KG-0021` |
| `VR-0078` | `EV-0254`; task `DT-0040` |
| `VR-0086` | `EV-0277`; task `DT-0045`; `KG-0025` |

`VR-0021` was checked specifically as requested. It does not exist in the current tree, in the validation CSV, or as a recoverable detail file in the reachable Git history. Adjacent IDs are not sufficient evidence to redirect it. The remaining dangling references are treated the same way.

Three `development_tasks.csv` rows (`DT-0068`, `DT-0071`, `DT-0072`) originally omitted one or two final empty CSV fields. After the maintainer authorized a schema-width-only correction, the missing trailing empty cells were appended. A parsed before/after comparison confirmed that every existing field, including each `start_commit`, remained unchanged; only empty cells were added.

## Validation iterations and limits

Three audit/repair/validation passes were used. The first exposed incomplete index/link and row-shape checking. The second expanded the validator to aggregate errors and then exposed the duplicated ADRs, orphaned discussion/gap details, malformed validation rows, and moved paths. A follow-up pass applied the maintainer-approved trailing-cell correction and verified all existing values were preserved. The final validator reports only 21 references to eleven unrecoverable historical validation IDs.

`python3 -B -m unittest discover -s research/tests -v` passes all four focused tests. `PYTHONPYCACHEPREFIX=/tmp/research-validate-cache python3 -m py_compile research/validate_records.py research/tests/test_validate_records.py` passes. After the approved row-width repair, the repository validator exits nonzero with 21 errors: 11 missing validation IDs in event references and 10 missing validation IDs in task references. The dangling `VR-0075` mention in `KG-0021` remains plain text, not a broken relative link. No scientific code, model data, or execution configuration was changed.

## Maintainer review

The maintainer authorized adding only trailing blank cells to the three short task rows; existing values and commit identifiers were verified unchanged. Missing validation records should not be reconstructed without original evidence.
