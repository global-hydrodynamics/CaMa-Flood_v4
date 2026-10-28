# CaMa-Flood v5 research records

**Status: approved framework; evaluation protocol remains partly proposed.** These records support a long-term case study of the full transition from CaMa-Flood v4 toward v5. Research logging is cross-project work: it is not owned by map modernization, a single feature, or a particular development branch. The records complement (and do not replace) GitHub Issues, pull requests, commits, `AGENTS.md`, and `docs/dev_status.md`. Maintainer approval, implementation, and validation are distinct observable events.

## Record layers

1. [`events.csv`](events.csv) is the chronological index. Each row is one significant, evidenced event with a stable `EV-####` ID. The task, discussion, knowledge-gap, intervention, decision, or validation detail is linked by record ID and evidence reference. The event row is canonical for shared timestamp, actor, short summary, and evidence class.
2. The companion CSVs contain analysis-oriented fields for development tasks and validation runs. Individual Markdown records in `records/` preserve the context needed to understand important discussions, tacit knowledge, human interventions, and longer validations. They do not copy full GitHub discussions or private chat transcripts.

`development_tasks.csv` and `validation_runs.csv` are the structured task/validation tables. Discussions, knowledge gaps, interventions, and decisions have stable IDs and concise Markdown records; their shared chronology is in `events.csv`. Companion tables link to event IDs instead of repeating event dates and actors. Use the templates under [`templates/`](templates/) and the field definitions in [`study_protocol.md`](study_protocol.md).

## Continuity across projects and branches

Keep this one `research/` directory and its record history across successive development projects, branches, and merges. Do not start a new log when a feature branch changes or a project ends. Link events to whichever project, issue, PR, commit, or discussion supplies the evidence. No dedicated research Issue is required. Initially the maintainer is the primary recorder; use ordinary Git review and merge practices, without a separate locking or coordination system. Before adding records on a later branch, ensure it includes the latest merged research history so IDs and chronology continue rather than fork.

## When to record

Record significant design discussions, maintainer approvals, implementation instructions, completed or failed work, important discoveries about undocumented behavior, material human interventions, and validation runs. Do not log every command, routine edit, or AI turn. When the task already has an Issue or PR, link it instead of copying its status or discussion. Issue creation is not required.

For a substantial task, record its objective, approved scope, owner, branch, starting commit, relevant Issue/PR, and agent/model version only when known. At completion, record the resulting commit, outcome, and validation evidence. Leave unavailable or inapplicable metrics blank; mark measured versus estimated effort explicitly. A successful command is evidence that the command ran, not proof of scientific correctness.

## Recording workflow

1. Assign the next unused stable ID. Never recycle an ID.
2. Add the event and reference IDs to `events.csv`. Use a timestamp with timezone when available; use `YYYY-MM-DD` with `timestamp_precision=date` when time is not known.
3. Add the task or validation row where relevant and create a detailed Markdown record from its template when context, alternatives, or evidence need explanation.
4. Separate proposals, maintainer approval, instructions, implementation, and validation. Mark evidence as contemporaneous, retrospective, or prospective.
5. Link public evidence by URL, commit, or repository path. For a private source, store only an approved archive identifier and access classification; never commit raw transcripts, credentials, private paths, or restricted data. If the source cannot be accessed or identified, record that limitation.
6. Have the human maintainer review AI-generated discussion summaries before treating them as research records. Do not include hidden reasoning or infer missing conversation content.
7. Run `python3 research/validate_records.py` after editing record tables. Commit the record update with the work it documents or in a follow-up commit that points to the completed work.

The structured tables now include verified Git stage events, the current task, the annotated baseline milestone, and audit reruns. They do not reconstruct unavailable historical discussions. [`stages/`](stages/README.md) contains the stage narratives; [`baseline/pre_documentation.md`](baseline/pre_documentation.md) describes the selected pre-documentation state. [`examples/illustrative_records.md`](examples/illustrative_records.md) remains fictional and separate from actual research data.

## Evidence archive and privacy

GitHub Issues, PRs, commits, source documents, and test outputs remain their primary records. For a ChatGPT discussion, a maintainer may manually import a concise, reviewed summary and add a link or archive identifier if one is available. Codex task instructions and observable actions may be referenced when accessible. Do not assume that either product can retrieve all conversations automatically.

Public repository records should contain public metadata and reviewed summaries only. Store full transcripts separately under restricted access. `privacy_status` distinguishes a public summary, a restricted reference, and unavailable evidence. Human-participant studies, interviews, consent, and institutional research-ethics review must be resolved before collecting or publishing participant-level evaluation data.

## Layout

- [`study_protocol.md`](study_protocol.md): confirmed v5 principles, primary evaluation dimensions, proposed task-based comparison, evidence categories, and limitations.
- [`development_tasks.csv`](development_tasks.csv), [`validation_runs.csv`](validation_runs.csv): structured task and validation data.
- [`records/`](records/): detailed discussion, knowledge-gap, human-intervention, and decision records.
- [`templates/`](templates/): reusable record formats.
- [`baseline/`](baseline/): release and pre-documentation baselines, comparison procedure, and recorded audit environment.
- [`stages/`](stages/README.md): evidence-based development-stage narratives.
- [`analysis/`](analysis/): later analysis guidance; no comparative study results are claimed.
