# ADR-0003: Treat workflow shell scripts as readable command sequences

- Status: accepted
- Date: 2026-11-01
- Event IDs: `EV-0132`, `EV-0138`; implementation and validation: `EV-0133`, `EV-0134`, `EV-0139`, `EV-0140`, `EV-0141`–`EV-0143`
- Decision owner: Dai Yamazaki (maintainer)

## Context

Defensive checks and bookkeeping had accumulated in v5 map-preparation wrappers, obscuring the scientific processing sequence. The maintainer clarified that these scripts should read like terminal commands a researcher would execute, rather than application frameworks.

## Alternatives considered

- Retain broad shell-level preflight checks, output checks, and generic error handling.
- Keep scripts focused on configuration, working-directory selection, ordered program calls, and simple orchestration; put input validation and detailed diagnostics in the programs that consume the data.

## Decision and rationale

Recommend the second approach for community contributions; it is not mandatory and does not retroactively constrain historical scripts. For the current maintainer-led v5 modernization, it is the AI-agent default unless the maintainer gives task-specific instructions. Keep editable settings together near the top, prefer direct executable calls, and use brief comments to expose scientific steps. Retain `set -eu`, necessary CLI handling, simple loops, and safe temporary cleanup. Do not remove essential correctness, reproducibility, or cleanup to reduce line count.

For s01-s04, standardize plain-ASCII `[STEP]`, `[SUCCESS]`, and prominent `ERROR` banners so failures remain visible after long scientific-program logs. Each script owns its short POSIX exit-trap formatting and current `STEP`; do not add a shared message helper or executable. On failure, print the script, step, and original nonzero status, stop the workflow, and do not print success. s04 combines this trap with temporary-file cleanup and performs the default-partition connectivity analysis before success.

Compiled programs under `map/tools/code/` are the primary home for validation, scientific calculations, and error reporting. The convention is a design responsibility, not a claim that all migrated programs already provide comprehensive diagnostics. Do not duplicate checks in wrappers simply because a consumer program's diagnostics need future improvement.

## Consequences and compatibility constraints

- AI coding agents should not add generic defensive boilerplate by default; consult `AGENTS.md` and `docs/ai_development_workflow.md`.
- Preserve scientific algorithms, numerical parameters, binary layouts, and command interfaces unless separately approved.
- When a shell interface changes, document it and update relevant tests.
- Keep the message pattern self-contained in each workflow script; do not introduce a shared message-formatting dependency.
- Existing `gosh/` and legacy `map/src/` scripts are outside this convention's migration scope unless separately approved.

## Evidence and implementation links

- Permanent rule: [`AGENTS.md`](../../AGENTS.md#shell-script-design-principles)
- Agent workflow: [`docs/ai_development_workflow.md`](../../docs/ai_development_workflow.md)
- Implemented wrappers: [`map/tools/script_params/`](../../map/tools/script_params/)
- Shell/program responsibility notes: [`map/tools/README.md`](../../map/tools/README.md), [`map/tools/script_params/README.md`](../../map/tools/script_params/README.md)
- Focused validation: `VR-0045`–`VR-0048`

## Supersedes / superseded by

None.
