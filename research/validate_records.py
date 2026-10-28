#!/usr/bin/env python3
"""Validate research record CSV schemas, IDs, controlled values, and references."""
from __future__ import annotations

import csv
import re
import sys
from datetime import date, datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parent
SCHEMAS = {
    "events.csv": ["event_id", "timestamp", "timestamp_precision", "event_type", "actor", "actor_type", "summary", "outcome_status", "evidence_class", "task_id", "record_refs", "evidence_refs", "privacy_status"],
    "development_tasks.csv": ["task_id", "objective", "branch", "owner", "start_event_id", "start_commit", "issue_url", "pr_url", "agent", "model_version", "outcome", "end_event_id", "end_commit", "validation_ids", "elapsed_seconds", "elapsed_basis", "human_effort_minutes", "effort_basis", "tokens", "token_basis", "cost_usd", "cost_basis"],
    "discussions.csv": ["discussion_id", "event_id", "source_type", "source_reference", "summary_path", "review_status", "source_access"],
    "knowledge_gaps.csv": ["gap_id", "event_id", "category", "evidence_basis", "status", "record_path", "formalized_in", "verification_ref"],
    "human_interventions.csv": ["intervention_id", "event_id", "task_id", "category", "trigger", "impact", "record_path", "review_status"],
    "validation_runs.csv": ["validation_id", "event_id", "task_id", "source_commit", "working_tree_state", "command", "environment_ref", "dataset_ref", "expected", "observed", "result", "comparison_method", "absolute_error", "relative_error", "tolerance", "scientific_interpretation", "evidence_refs"],
}
ID_COLUMNS = {
    "events.csv": ("event_id", "EV"),
    "development_tasks.csv": ("task_id", "DT"),
    "discussions.csv": ("discussion_id", "DR"),
    "knowledge_gaps.csv": ("gap_id", "KG"),
    "human_interventions.csv": ("intervention_id", "HI"),
    "validation_runs.csv": ("validation_id", "VR"),
}

def fail(message: str) -> None:
    print(f"ERROR: {message}", file=sys.stderr)
    raise SystemExit(1)

def load_table(filename: str) -> list[dict[str, str]]:
    path = ROOT / filename
    if not path.is_file():
        fail(f"missing {path.relative_to(ROOT)}")
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        if reader.fieldnames != SCHEMAS[filename]:
            fail(f"{filename}: expected header {SCHEMAS[filename]!r}; got {reader.fieldnames!r}")
        rows = list(reader)
    id_col, prefix = ID_COLUMNS[filename]
    seen: set[str] = set()
    for lineno, row in enumerate(rows, start=2):
        record_id = row.get(id_col, "")
        if not re.fullmatch(rf"{prefix}-[0-9]{{4}}", record_id):
            fail(f"{filename}:{lineno}: invalid {id_col} {record_id!r}")
        if record_id in seen:
            fail(f"{filename}:{lineno}: duplicate {record_id}")
        seen.add(record_id)
    return rows

def validate_timestamp(row: dict[str, str], lineno: int) -> None:
    value, precision = row["timestamp"], row["timestamp_precision"]
    try:
        if precision == "date":
            date.fromisoformat(value)
        elif precision in {"minute", "second"}:
            parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
            if parsed.tzinfo is None or parsed.utcoffset() is None:
                fail(f"events.csv:{lineno}: datetime must include a timezone offset")
            if precision == "minute" and parsed.second != 0:
                fail(f"events.csv:{lineno}: minute precision cannot include nonzero seconds")
        else:
            fail(f"events.csv:{lineno}: timestamp_precision must be date, minute, or second")
    except ValueError as exc:
        fail(f"events.csv:{lineno}: invalid ISO 8601 timestamp {value!r}: {exc}")

def check_refs(rows: list[dict[str, str]], field: str, valid: set[str], filename: str, required: bool = False, many: bool = False) -> None:
    for lineno, row in enumerate(rows, start=2):
        value = row.get(field, "")
        if required and not value:
            fail(f"{filename}:{lineno}: {field} is required")
        refs = [item.strip() for item in value.split(";") if item.strip()] if many else ([value] if value else [])
        for reference in refs:
            if reference not in valid:
                fail(f"{filename}:{lineno}: {field} references unknown ID {reference!r}")

def main() -> None:
    tables = {name: load_table(name) for name in SCHEMAS}
    event_ids = {row["event_id"] for row in tables["events.csv"]}
    task_ids = {row["task_id"] for row in tables["development_tasks.csv"]}
    validation_ids = {row["validation_id"] for row in tables["validation_runs.csv"]}
    discussion_ids = {row["discussion_id"] for row in tables["discussions.csv"]}
    gap_ids = {row["gap_id"] for row in tables["knowledge_gaps.csv"]}
    intervention_ids = {row["intervention_id"] for row in tables["human_interventions.csv"]}
    decision_ids = {path.stem for path in (ROOT / "decisions").glob("ADR-*.md")}
    record_ids = task_ids | validation_ids | discussion_ids | gap_ids | intervention_ids | decision_ids

    for lineno, row in enumerate(tables["events.csv"], start=2):
        validate_timestamp(row, lineno)
        if row["event_type"] not in {"project_authorization", "discussion", "maintainer_decision", "implementation_instruction", "implementation", "knowledge_discovery", "human_intervention", "validation", "failure", "research_milestone", "status_update"}:
            fail(f"events.csv:{lineno}: unknown event_type {row['event_type']!r}")
        if row["outcome_status"] not in {"proposed", "approved", "instructed", "implemented", "validated", "failed", "unresolved", "observed"}:
            fail(f"events.csv:{lineno}: unknown outcome_status {row['outcome_status']!r}")
        if row["evidence_class"] not in {"contemporaneous", "retrospective", "prospective"}:
            fail(f"events.csv:{lineno}: unknown evidence_class {row['evidence_class']!r}")
        if row["privacy_status"] not in {"public_summary", "restricted_reference", "unavailable"}:
            fail(f"events.csv:{lineno}: unknown privacy_status {row['privacy_status']!r}")
        if row["task_id"] and row["task_id"] not in task_ids:
            fail(f"events.csv:{lineno}: unknown task_id {row['task_id']!r}")
        for record_id in (item.strip() for item in row["record_refs"].split(";") if item.strip()):
            if record_id not in record_ids:
                fail(f"events.csv:{lineno}: record_refs references unknown ID {record_id!r}")

    for lineno, row in enumerate(tables["discussions.csv"], start=2):
        summary_path = ROOT.parent / row["summary_path"]
        if not summary_path.is_file():
            fail(f"discussions.csv:{lineno}: missing summary_path {row['summary_path']!r}")

    check_refs(tables["development_tasks.csv"], "start_event_id", event_ids, "development_tasks.csv")
    check_refs(tables["development_tasks.csv"], "end_event_id", event_ids, "development_tasks.csv")
    check_refs(tables["discussions.csv"], "event_id", event_ids, "discussions.csv", required=True)
    check_refs(tables["knowledge_gaps.csv"], "event_id", event_ids, "knowledge_gaps.csv", required=True)
    check_refs(tables["human_interventions.csv"], "event_id", event_ids, "human_interventions.csv", required=True)
    check_refs(tables["human_interventions.csv"], "task_id", task_ids, "human_interventions.csv")
    check_refs(tables["validation_runs.csv"], "event_id", event_ids, "validation_runs.csv", required=True)
    check_refs(tables["validation_runs.csv"], "task_id", task_ids, "validation_runs.csv")
    check_refs(tables["development_tasks.csv"], "validation_ids", validation_ids, "development_tasks.csv", many=True)

    print("Research record schemas and IDs are valid.")
    print("Rows:", ", ".join(f"{name}={len(rows)}" for name, rows in tables.items()))
    print("Empty tables are valid for categories with no verified records; the check does not imply missing history.")

if __name__ == "__main__":
    main()
