# Study protocol — draft for maintainer review

## Purpose and status

This protocol proposes a lightweight, evidence-linked case study of the transition of CaMa-Flood from a mature, developer-dependent scientific model toward a more documented, reproducible, testable, and community-maintainable research software platform, including human–AI collaboration. It is a research plan, not a report of demonstrated outcomes.

The research questions are: **RQ1 Knowledge externalization** — how tacit knowledge becomes explicit and reusable; **RQ2 Reproducibility and maintainability** — whether users and developers can install, execute, understand, modify, and validate the model with less dependence on original developers; and **RQ3 Human–AI collaboration** — which tasks agents complete, where they lack context, and which human interventions and governance controls are required.

The central hypothesis is that AI-assisted modernization can externalize and preserve technical knowledge rather than only generate code. This remains a hypothesis; no causal productivity or quality claim is made here.

## Unit of observation and evidence

The primary unit is a bounded development task. Significant events within a task are indexed in `events.csv`; linked records document important discussions, decisions, knowledge gaps, interventions, and validation runs. Evidence sources include GitHub metadata, commits/diffs, reviewed discussion summaries, task instructions available to the participants, reproducible commands/results, and environment/dataset identifiers.

Classify evidence as:

- **Contemporaneous:** recorded with the work or in its original public artifacts.
- **Retrospective:** reconstructed later from evidence that still exists; label the reconstruction date and do not present it as an original contemporaneous note.
- **Prospective:** recorded under this protocol as the work occurs.

Keep source facts, maintainer-confirmed explanations, AI interpretations, and unresolved assumptions distinct. Git commits establish code changes, not the reasons for them. A test run establishes observed execution, not scientific validity.

## Proposed outcomes and evaluation

Future analysis may count or classify: documented assumptions and their verification; successful and failed task outcomes; validation coverage and reproducibility; environment setup and example execution; task completion with and without agent assistance; human intervention types; and changes in access to model knowledge. Numerical comparisons must state source revisions, compiler/library/precision settings, datasets, comparison method, and agreed variable-specific tolerances. Bitwise identity and numerical agreement are different outcomes.

Before formal comparison, publish a task protocol defining participant population, tasks, starting state, allowed assistance, success criteria, timing/effort measures, data handling, and analysis. Prospective evaluation suggestions are not results. Do not infer that AI or documentation caused improvement from descriptive counts alone.

## Data quality and bias controls

- Include material failures, rejected proposals, and unresolved questions; do not select only successful tasks.
- Do not infer missing times, model versions, token usage, effort, or costs. Empty values mean unavailable or not collected; basis fields distinguish measured and estimated metrics.
- Keep the original artifact as the evidence source and use short summaries that point to it. Avoid duplicating Issue/PR content.
- Have the maintainer review AI-generated summaries before treating them as research data. Do not log inaccessible internal reasoning.
- Do not silently rewrite historical records. Corrections are new Git changes and remain traceable.

## Privacy and research ethics

The public repository may contain public metadata and reviewed summaries only. Full chat transcripts, credentials, private filesystem paths, unpublished sensitive research, restricted datasets, and personal participant data must not be committed. Reference restricted evidence by an approved archive ID without copying its contents. If a source is unavailable, say so.

If future evaluation recruits or analyzes human participants or external contributors, determine consent, data minimization, institutional research-ethics review, retention, and publication rules before collection. This framework itself does not grant consent or ethics approval.

## Historical baseline and scope

Systematic logging begins prospectively after this framework is established. Earlier work may be reconstructed later only from surviving evidence and must be labeled retrospective. The Git `v4.30` tag is a candidate source-code baseline; its target commit is recorded in [`baseline/v4_baseline.md`](baseline/v4_baseline.md). The tag alone does not prove identity with a separately distributed package.

Phase 1 establishes schemas and procedures. It does not reconstruct the October 2026 modernization history, freeze a complete runtime environment, conduct human-participant evaluation, or claim a validated scientific comparison.

## Decisions still open

- Which exact downloadable v4.30 package and checksum should be paired with the source tag as the distribution baseline?
- Which numerical regression experiments, input datasets, compilers, and tolerances are scientifically acceptable for v5 comparisons?
- What consent and institutional ethics process applies before participant-level onboarding or AI-assistance studies?
- What stable, access-controlled archive can hold private transcripts, and what citation/retention policy should be used?
