# Knowledge-recovery protocol

## Status and purpose

This is a prospective method for recovering and documenting scientific or technical knowledge during the CaMa-Flood v4-to-v5 transition. It is not a completed inventory, a claim that v5 is better, or a publication result. Apply it to substantial development stages; do not turn every edit into a research exercise. Keep records in the existing `research/` system and preserve the evidence timing labels defined in [`study_protocol.md`](../study_protocol.md).

The method evaluates traceability and accessibility of knowledge, not the number of records created. It must retain successful, incorrect, rejected, partial, and unresolved interpretations. A code change proves what changed; source behavior alone does not prove the original rationale.

## Knowledge categories

Assign one primary category to each distinct finding; add a secondary category only when the same question genuinely spans types. Do not count one finding twice. If one discovery both recovers v4 knowledge and introduces a v5 convention, split it into linked records so the categories remain analytically distinct.

| Type | Meaning | Typical evidence |
| --- | --- | --- |
| **A — Undocumented implementation knowledge** | Behavior present in v4 but not clearly specified in accessible v4 documentation within a stated search scope. | v4 source, tests/data, and a bounded documentation search. |
| **B — Recovered design rationale** | Rationale not derivable from behavior alone, recovered from maintainer explanation or reliable historical evidence. | Reviewed maintainer explanation or inspected historical artifact; distinguish the two. |
| **C — Resolved ambiguity** | Multiple plausible interpretations of existing behavior were considered and a final interpretation was established. | Initial interpretation, correction/decision, and evidence for the final interpretation. |
| **D — New v5 design decision** | Convention or architecture introduced during modernization; it is not recovered v4 knowledge. | Explicit maintainer decision and implementation/specification references. |

The existing gap categories (format, scientific assumption, workflow, dependency, architecture, interface, other) may remain in `knowledge_gaps.csv`; Type A–D belongs in detailed Markdown records unless later analysis demonstrates a need for a CSV field. Do not change schemas merely to make this protocol possible.

## Evidence collection

For each material candidate:

1. **Capture the question first.** Record the precise unknown, discovery trigger, evidence timing, source path and relevant lines/artifact, why the answer matters, and what work depends on it. State whether it is blocking. Do not place a speculative answer in the question field.
2. **Bound the historical search.** Identify the selected v4 baseline and exact revision using [`baseline/v4_baseline.md`](../baseline/v4_baseline.md) and [`baseline/pre_documentation.md`](../baseline/pre_documentation.md) as applicable. Inspect relevant code, comments, manuals, READMEs, configuration and available data. Record what was searched. Say “not found in the inspected sources,” not “never documented,” unless the search supports the broader claim. The v4.30 source tag is not proof of identity with a separately distributed package.
3. **Assess prior documentation.** Classify the knowledge as explicitly documented, present in code but unexplained, partial/ambiguous, maintainer testimony only, new in v5, or not established from available evidence.
4. **Separate trigger from verification.** Record how the candidate arose (source analysis, AI hypothesis, maintainer correction, test/output anomaly, implementation comparison, documentation/metadata audit, or iterative combination) separately from how it was checked.
5. **Verify without inferring intent.** Link the relevant v4 evidence, maintainer/historical evidence if applicable, and implementation/test evidence. State remaining uncertainty and whether scientific meaning was maintainer-confirmed. AI-generated explanations are hypotheses, not proof of developer intent.
6. **Trace formalization.** Link the v5 source comments, contract, registry, README/manual, tests, and/or research records where the knowledge became accessible. Distinguish explanatory text from an executable check or enforced interface contract.
7. **Review classification.** The maintainer reviews AI-generated summaries before they are treated as research findings. Keep rejected candidates and corrections traceable; do not rewrite historical records to make the process appear linear.

Use `research/templates/knowledge_gap.md` for a material gap. Keep `knowledge_gaps.csv` as a compact index; do not force all evidence into CSV columns. A knowledge gap may have multiple linked discovery, resolution, implementation, and validation events.

## Prospective milestone review

At completion of a substantial development stage, run the short checklist in [`docs/ai_development_workflow.md`](../../docs/ai_development_workflow.md). Review:

- scientific assumptions or numerical thresholds without a clear rationale;
- input/output semantics, dependencies, and data contracts that are undocumented or ambiguous;
- historical decisions that cannot be reconstructed confidently;
- contradictions among code, comments, documentation, and metadata;
- assumptions introduced by AI work without sufficient evidence.

Record significant questions before attempting resolution, even when they are answered during the same stage. At handoff, group unresolved scientific and architectural questions for maintainer review, with sources, consequence, blocking status, and independent work that can continue. Do not wait for responses to noncritical questions before continuing unrelated work. If review finds no material new gap, record a review event saying so; do not create a placeholder knowledge-gap record. The review should be brief and proportionate, not a separate audit after every minor commit.

On resolution, link the original question and trigger to the evidence or maintainer explanation, the final interpretation and rationale, and its formalization location. Label the resolution method as source/test analysis, maintainer knowledge, historical artifact, mixed, or unresolved. Update the existing gap record/status rather than creating a duplicate question. Preserve the basis for the resolution and distinguish verified facts, decisions, assumptions, and proposals.

## Existing-record audit at protocol introduction

This is a limited audit of available repository records, not the full v4-to-v5 knowledge inventory. Existing records already identify several candidates and limits:

- [`KG-0002`](../records/knowledge_gaps/KG-0002.md) records maintainer-confirmed global-map provenance and the unavailable detailed original generation procedure. It must not be upgraded into a claim that the 2011 paper documents the private implementation.
- [`KG-0003`](../records/knowledge_gaps/KG-0003.md) records the incomplete module-level NetCDF producer contract; the missing variable/layout/attribute contract remains open.
- [`KG-0004`](../records/knowledge_gaps/KG-0004.md) records unresolved disagreement between selected registry roles and inspected current uses; it explicitly requires maintainer decision before changing preserved assignments.
- [`KG-0005`](../records/knowledge_gaps/KG-0005.md) and recent MPI discussions document current workflow distinctions and retain historical origin/external-use hypotheses as unverified. Current v5 source and workflow evidence does not by itself establish original design intent.
- [`KG-0001`](../records/knowledge_gaps/KG-0001.md) is a provenance/history gap, not by itself evidence of recovered scientific knowledge.

These are not all classified as confirmed Type A–D findings. Candidate areas for a later bounded audit include MPI aggregation versus partitioning (`KG-0005`, `map/src/src_param/set_bif_basin_mpi.F90`, `map/src/src_param/set_mpi_region.F90`), standard versus MPI bifurcation processing (`research/records/discussions/DR-0024.md`, `map/src/src_param/`), deterministic bifurcation ordering (`map/src/src_param/` and `map/tools/code/`), and map metadata/binary conventions (`docs/map_data_inventory.md`, `map/map_metadata_spec.md`). These references identify where to investigate; they do not assert that an item was undocumented or explain why it was designed that way. A systematic comparison against the frozen v4 baseline remains future analysis. No new scientific interpretation is asserted by this protocol update.

## Conversation archives and privacy

The maintainer may cite separately preserved ChatGPT/Codex conversation logs as evidence. Use only an archive ID assigned by the maintainer, source product, verified date/interval, passage locator when inspected, and access classification. Do not invent identifiers, dates, quotations, or summaries; do not commit private transcripts or private filesystem paths. If an archive is inaccessible, record that limit. A maintainer explanation recorded in an approved task may be cited as such without pretending the underlying full archive was inspected.

## Evaluation and limitations

Later evaluation may examine whether milestones applied the checklist, whether material questions were recorded before resolution, resolution provenance, whether formalization is linked, whether negative reviews were retained, and whether questions remain unresolved or were rejected. Use concrete traceability and evidence quality; do not use raw record counts as a proxy for knowledge recovery or quality. Compare documented v4 evidence with formalized v5 knowledge and state the inspection scope. Keep failures and unresolved cases. This protocol does not provide a scoring rubric, establish causal effects, or report comparative results; those require separate maintainer-reviewed study design.
