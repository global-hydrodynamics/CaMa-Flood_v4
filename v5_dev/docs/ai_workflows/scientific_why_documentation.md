# Scientific Why Documentation Workflow

This reusable workflow is intended primarily for Codex. Invoke it explicitly with a Scientific Why Question List to verify design rationale through maintainer interaction and document confirmed knowledge. It is independent of any particular module or programming language.

Follow the repository's root [`AGENTS.md`](../../AGENTS.md), applicable directory instructions, and source-specific design and style guidance. This workflow does not grant approval for algorithm, numerical, interface, or compatibility changes.

## Stage 1 — Inspect and prepare questions

For each supplied question:

1. Locate the corresponding code in the current repository.
2. Verify the described implementation behavior and correct stale references or misunderstandings.
3. Inspect related code and documentation when needed to establish behavior or rationale.
4. Distinguish verified implementation facts, previously documented rationale, unconfirmed hypotheses, and unresolved questions.
5. If code or existing documentation establishes the rationale, explain the evidence; do not ask the maintainer unnecessarily.

The supplied questions may be incomplete or mistaken. Do not treat a plausible explanation as confirmed design rationale.

## Stage 2 — Ask for maintainer rationale

For every question whose rationale remains unclear, prepare a multiple-choice item with the procedure/component, current source location, verified behavior, and Why question. Offer up to three plausible explanations as **unverified hypotheses**:

- **A / B / C:** Plausible but unconfirmed explanations. Use fewer than three when fewer are reasonable; do not invent options to fill the list.
- **D. Other:** The maintainer provides another explanation.
- **E. Later:** Leave the rationale unresolved for future investigation.

Allow multiple selections when appropriate. Present all questions and complete options together in one response, allowing a compact reply such as `1:A, 2:B+C, 3:D [explanation], 4:E`. Then wait for the maintainer's response before drafting comments. Do not edit files at this stage.

## Stage 3 — Interpret answers and investigate

After the maintainer responds:

1. Treat the maintainer's explanation as evidence of intended rationale, not as proof that every technical detail is correct.
2. Verify consistency with the implementation and investigate questions the maintainer says can be answered from code.
3. Separate source-verified facts from scientific or historical motivation that remains uncertain.
4. Report discrepancies instead of inventing an explanation.
5. Accept “unknown” or “I do not remember” as a valid outcome. Do not repeat questions answerable from available source or documentation.

If an answer only partly resolves the rationale, preserve the confirmed part and identify the specific unresolved part.

## Stage 4 — Draft comments

Prepare all proposed comments together with their intended locations. Use concise English comments, normally two to four lines when useful, explaining why the step exists, the confirmed assumption or design choice, and established limits. Avoid merely restating code.

For an unresolved item explicitly deferred as **E. Later**, propose:

```fortran
! TODO(WHY): Confirm the design rationale for ...
```

When only part of a rationale remains unresolved, document the confirmed part and mark only the unresolved aspect with `TODO(WHY)`. Never present AI hypotheses as confirmed science. Show the complete proposal and wait for explicit maintainer approval before editing any file.

## Stage 5 — Approval, implementation, and verification

After explicit approval, apply only the approved comment or documentation changes and preserve algorithms, numerical behavior, interfaces, and control flow. Follow applicable source conventions and avoid unrelated refactoring. If implementation scope or approval is unclear, ask before proceeding.

Inspect the final diff to confirm it contains only the approved documentation changes, then run `git diff --check`. Report confirmed rationales, remaining TODOs or questions, files changed, and actual verification. Follow the repository's normal commit and push instructions; do not infer permission to merge or rewrite shared history.

## Suggested invocation

> Apply `scientific_why_documentation.md` to the attached Scientific Why Question List. Inspect source and documentation first, ask all unresolved questions in one questionnaire, and do not edit until I approve the proposed comments.
