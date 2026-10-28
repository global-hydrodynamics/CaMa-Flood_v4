# Illustrative records — fictional examples only

**Nothing in this file describes a real CaMa-Flood event, discussion, result, dataset, or contributor.** These examples demonstrate how to distinguish a proposal, an approval, implementation, and validation. Their `EX-*` IDs are not part of the actual ID sequence and must never be copied into the record tables.

## Example 1 — hypothetical discussion and decision

- Example discussion ID: `EX-DR-01`
- Event: A hypothetical team discusses whether a scientific data conversion should change the ordering of fields.
- Proposal: An agent suggests reordering fields to simplify a reader.
- Maintainer decision: The hypothetical maintainer asks to preserve the existing file order until compatibility tests establish that a change is safe.
- Implementation instruction: No change to field order is authorized; document the current contract and add a reader test.
- Implementation: A hypothetical test is added and records the existing order.
- Validation: The test passes for the fixture used; this only validates the tested reader contract, not all production datasets.
- Evidence: `EXAMPLE ONLY — no source artifact exists`.

The real record would use a `DR-####` summary reviewed by the maintainer, separate event rows for approval/instruction/implementation/validation, and actual commit/test references.

## Example 2 — hypothetical knowledge gap and intervention

- Example knowledge gap ID: `EX-KG-01`
- Discovery: A hypothetical map file has no format description. An agent infers a type from the filename.
- Human intervention: A maintainer explains that the inference is unverified and points to the writer implementation.
- Formalization: A hypothetical data-format specification records the writer-backed type and marks another undocumented field unresolved.
- Verification: A size check is run on a named fixture, with its command and result recorded.
- Evidence: `EXAMPLE ONLY — no source artifact exists`.

The real record would separate the agent's inference from the maintainer-confirmed fact, link the source and fixture, and preserve the unresolved field instead of inventing its meaning.
