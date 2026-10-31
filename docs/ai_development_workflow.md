# AI-Assisted Development Workflow

This document defines a reusable handoff process among the maintainer, ChatGPT, and Codex. It applies across CaMa-Flood development projects; it is not specific to the current map-parameter work. Repository rules remain authoritative in [`AGENTS.md`](../AGENTS.md), and development evidence is recorded according to [`research/README.md`](../research/README.md).

## Roles and decision authority

1. **Review before implementation:** ChatGPT reviews relevant code, documentation, development plans, risks, and alternatives with the maintainer before implementation is assigned. It should distinguish source-verified facts from inferences and identify unresolved questions.
2. **Maintainer decisions:** The maintainer makes final scientific and architectural decisions, approves the project branch and implementation scope, and resolves material trade-offs. A ChatGPT recommendation or Codex finding is not approval.
3. **Implementation handoff:** After review, the maintainer gives Codex a structured task using the four sections below. Approved scope authorizes routine implementation and proportionate validation within the repository rules; decisions requiring additional approval remain subject to `AGENTS.md`.
4. **Evidence and records:** Codex records material approvals, implementation outcomes, failures or interventions, and validation evidence in the appropriate project documents and `research/` records. Research entries follow the evidence, privacy, and review rules in `research/README.md`. Do not copy private conversation transcripts or present AI-generated discussion summaries as independent evidence.
5. **Status separation:** Label information as one of:
   - **Maintainer-approved decision** — explicitly confirmed by the maintainer, with rationale and constraints.
   - **Proposal** — an option or recommendation awaiting approval.
   - **Verified finding** — supported by inspected source, documentation, data, or an executed check; cite the evidence and limits.
   - **Unresolved question** — information not established by available evidence or a decision still needed.
6. **Escalation:** Return major unresolved scientific or architectural issues to the maintainer before dependent work proceeds. Do not silently choose an assumption that could change scientific behavior, interfaces, compatibility, data policy, or cause substantial rework. Record non-blocking questions and continue independent work as described in `AGENTS.md`.
7. **Review after implementation:** Once Codex has completed the assigned scope, ChatGPT reviews the resulting code and documentation, the actual validation results, and related research records with the maintainer. Use that review to resolve gaps and decide the next task; do not treat successful execution alone as scientific validation.

## Structured task for Codex

Every implementation handoff should use these four sections. Keep the scope and acceptance criteria concrete, and state explicitly which decisions are approved versus still open.

### A. Discussion and approved decisions

Summarize the reviewed problem, alternatives considered, maintainer-approved decisions and rationale, verified facts with evidence, proposals, and unresolved questions. Identify the approved project and working branch.

### B. Implementation scope and restrictions

List the deliverables and permitted files or components. State what must remain unchanged, compatibility and numerical constraints, forbidden actions, and any required approvals before expanding scope.

### C. Validation requirements and acceptance criteria

Specify reproducible checks, inputs or fixtures, expected results, acceptance criteria, and checks that must not be run. Distinguish compilation, software regression, model integration, and scientific validation. State how to handle unavailable data or tools.

### D. Documentation, research records, and completion report

Name the technical and coordination documents to update. Require evidence-linked research records for material events, validated with `python3 research/validate_records.py` when record tables change. Ask for a completion report covering changed files, checks and outcomes, limitations, unresolved decisions, and commit/push state as authorized.

## Reusable prompt template

```markdown
# Task: <short title>

Project: <approved project>
Working branch: `<registered branch>`
Issue/PR: <link, or “none”>

## A. Discussion and approved decisions

- Problem and relevant code/docs reviewed:
- Maintainer-approved decisions and rationale:
- Verified findings and evidence:
- Proposals awaiting approval:
- Unresolved questions and whether they block work:

## B. Implementation scope and restrictions

- Deliverables and allowed files/components:
- Required behavior and compatibility constraints:
- Files/behavior to preserve:
- Prohibited work or approval gates:

## C. Validation requirements and acceptance criteria

- Inputs/fixtures and reproducible commands:
- Expected results and acceptance criteria:
- Checks not to run:
- Missing-data/tool fallback and reporting:

## D. Documentation, research records, and completion report

- Documents and status records to update:
- Research records required:
- Final report must include changed files, actual validation results,
  limitations/open decisions, and whether changes were committed or pushed.
```

The template is a handoff aid, not a substitute for maintainer approval, repository-specific policy, or evidence review. If an instruction conflicts with `AGENTS.md`, follow `AGENTS.md` and surface the conflict before affected implementation.
