# ADR-0001: Use an event index with linked research records

- **Status:** Accepted by maintainer as part of the approved framework
- **Date:** 2026-10-28 (date precision; no time recorded)
- **Event IDs:** `EV-0001`
- **Decision owner:** Dai Yamazaki (maintainer)

## Context

The study needs a chronological event log and detailed evidence-linked records, but GitHub already stores task discussion and code review, while full chat transcripts can be private. Copying all content into tables would create duplicate, difficult-to-maintain records.

## Alternatives considered

1. One wide CSV for every entity type: simple file count, but many type-specific blank columns and poor narrative context.
2. Separate CSVs with repeated timestamps, actors, and summaries: easy to filter, but duplicates the event chronology.
3. One canonical event CSV plus narrow task/validation tables and linked Markdown summaries for discussions, gaps, interventions, and decisions.

## Decision

Use alternative 3. `events.csv` is the chronology and canonical source for shared event metadata. `development_tasks.csv` and `validation_runs.csv` hold task- and validation-specific fields for analysis. Other significant records use stable IDs and Markdown templates, with event/task IDs as foreign references. GitHub remains the primary source for Issues/PRs; private transcripts remain in separately restricted archives.

## Consequences

This keeps the log exportable while limiting duplicated data. It requires stable IDs and a lightweight consistency validator. Markdown details are not fully machine-parsed; aggregate analysis should use the CSV tables and consult detail records as evidence.

## Evidence and implementation

Framework files: [`research/README.md`](../README.md), [`events.csv`](../events.csv), and [`validate_records.py`](../validate_records.py). The maintainer approved the framework and cross-project continuity policy in `EV-0001`.
