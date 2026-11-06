#!/usr/bin/env python3
"""Validate research record CSV schemas, IDs, controlled values, and references."""
from __future__ import annotations

import csv
import argparse
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
    "legacy_validation_refs.csv": ["legacy_validation_id", "event_id", "task_id", "likely_scope", "likely_outcome", "candidate_validation_id", "confidence", "evidence_basis", "record_status"],
}
ID_COLUMNS = {
    "events.csv": ("event_id", "EV"),
    "development_tasks.csv": ("task_id", "DT"),
    "discussions.csv": ("discussion_id", "DR"),
    "knowledge_gaps.csv": ("gap_id", "KG"),
    "human_interventions.csv": ("intervention_id", "HI"),
    "validation_runs.csv": ("validation_id", "VR"),
    "legacy_validation_refs.csv": ("legacy_validation_id", "VR"),
}

def load_table(filename: str, root: Path, errors: list[str], unavailable: set[str]) -> list[dict[str, str]]:
    """Read one CSV, retaining usable columns so independent checks can continue."""
    path = root / filename
    rows: list[dict[str, str]] = []
    if not path.is_file():
        errors.append(f"{filename}: missing file")
        unavailable.add(filename)
        return []
    try:
        with path.open(newline="", encoding="utf-8-sig") as stream:
            reader = csv.reader(stream)
            header = next(reader, None)
            expected = SCHEMAS[filename]
            if header != expected:
                errors.append(f"{filename}: expected header {SCHEMAS[filename]!r}; got {header!r}")
            if header is None or any(header.count(column) != 1 for column in expected):
                unavailable.add(filename)
                return []
            column_index = {
                column: header.index(column) if column in header else -1
                for column in expected
            }
            id_col, prefix = ID_COLUMNS[filename]
            seen: set[str] = set()
            for row in reader:
                lineno = reader.line_num
                if len(row) != len(header):
                    errors.append(f"{filename}:{lineno}: expected {len(expected)} columns, found {len(row)}")
                    unavailable.add(filename)
                # Keep the known fields from overlong rows; ignoring such rows hid
                # references and local errors that were still recoverable.
                item = {
                    column: row[index] if 0 <= index < len(row) else ""
                    for column, index in column_index.items()
                }
                record_id = item[id_col]
                if not re.fullmatch(rf"{prefix}-[0-9]{{4}}", record_id):
                    errors.append(f"{filename}:{lineno}: invalid {id_col} {record_id!r}")
                elif record_id in seen:
                    errors.append(f"{filename}:{lineno}: duplicate {record_id}")
                seen.add(record_id)
                rows.append(item)
    except (OSError, UnicodeError, csv.Error) as exc:
        errors.append(f"{filename}: cannot read CSV: {exc}")
        unavailable.add(filename)
        return rows
    return rows

def validate_timestamp(row: dict[str, str], lineno: int, errors: list[str]) -> None:
    value, precision = row["timestamp"], row["timestamp_precision"]
    try:
        if precision == "date":
            date.fromisoformat(value)
        elif precision in {"minute", "second"}:
            parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
            if parsed.tzinfo is None or parsed.utcoffset() is None:
                errors.append(f"events.csv:{lineno}: datetime must include a timezone offset")
                return
            if precision == "minute" and parsed.second != 0:
                errors.append(f"events.csv:{lineno}: minute precision cannot include nonzero seconds")
        else:
            errors.append(f"events.csv:{lineno}: timestamp_precision must be date, minute, or second")
    except ValueError as exc:
        errors.append(f"events.csv:{lineno}: invalid ISO 8601 timestamp {value!r}: {exc}")

def check_refs(rows: list[dict[str, str]], field: str, valid: set[str], filename: str,
               errors: list[str], required: bool = False, many: bool = False) -> None:
    for lineno, row in enumerate(rows, start=2):
        value = row.get(field, "")
        if required and not value:
            errors.append(f"{filename}:{lineno}: {field} is required")
        refs = [item.strip() for item in value.split(";") if item.strip()] if many else ([value] if value else [])
        for reference in refs:
            if reference not in valid:
                errors.append(f"{filename}:{lineno}: {field} references unknown ID {reference!r}")

def validate_records(root: Path = ROOT) -> tuple[dict[str, list[dict[str, str]]], list[str], list[str]]:
    """Return parsed tables, all identifiable errors, and checks skipped on bad inputs."""
    errors: list[str] = []
    skipped: list[str] = []
    unavailable: set[str] = set()
    tables = {name: load_table(name, root, errors, unavailable) for name in SCHEMAS}
    event_ids = {row["event_id"] for row in tables["events.csv"]}
    task_ids = {row["task_id"] for row in tables["development_tasks.csv"]}
    validation_ids = {row["validation_id"] for row in tables["validation_runs.csv"]}
    legacy_validation_rows = tables["legacy_validation_refs.csv"]
    legacy_validation_ids = {row["legacy_validation_id"] for row in legacy_validation_rows}
    discussion_ids = {row["discussion_id"] for row in tables["discussions.csv"]}
    gap_ids = {row["gap_id"] for row in tables["knowledge_gaps.csv"]}
    intervention_ids = {row["intervention_id"] for row in tables["human_interventions.csv"]}
    record_dir = root / "records"
    detail_paths = {
        "DR": sorted((record_dir / "discussions").glob("DR-*.md")),
        "KG": sorted((record_dir / "knowledge_gaps").glob("KG-*.md")),
        "HI": sorted((record_dir / "human_interventions").glob("HI-*.md")),
        # Detailed validation reports are optional companions to validation_runs.csv.
        "VR": sorted((record_dir / "validation_runs").glob("VR-*.md")),
    }
    detail_record_ids = {path.stem for paths in detail_paths.values() for path in paths}
    id_labels = {"DR": "Discussion ID", "KG": "Gap ID", "HI": "Intervention ID", "VR": "Validation ID"}
    for prefix, paths in detail_paths.items():
        for path in paths:
            try:
                content = path.read_text(encoding="utf-8")
            except (OSError, UnicodeError) as exc:
                errors.append(f"{path.relative_to(root)}: cannot read detail record: {exc}")
                continue
            match = re.search(rf"^#\s+({prefix}-\d{{4}})\b", content, re.MULTILINE)
            if not match:
                label = id_labels[prefix]
                match = re.search(rf"^-\s*{re.escape(label)}:\s*`?({prefix}-\d{{4}})\b", content, re.MULTILINE)
            if not match:
                errors.append(f"{path.relative_to(root)}: missing {prefix}-#### heading")
            elif not path.stem.startswith(match.group(1)):
                errors.append(f"{path.relative_to(root)}: filename does not match heading ID {match.group(1)}")
    decision_paths = sorted((root / "decisions").glob("ADR-*.md")) + sorted(
        (record_dir / "decisions").glob("ADR-*.md")
    )
    decision_ids: set[str] = set()
    decision_paths_by_id: dict[str, list[Path]] = {}
    for path in decision_paths:
        try:
            content = path.read_text(encoding="utf-8")
        except (OSError, UnicodeError) as exc:
            errors.append(f"{path.relative_to(root)}: cannot read decision record: {exc}")
            continue
        match = re.search(r"^#\s+(ADR-\d{4})\b", content, re.MULTILINE)
        if not match:
            errors.append(f"{path.relative_to(root)}: missing ADR-#### heading")
            continue
        decision_id = match.group(1)
        decision_ids.add(decision_id)
        decision_paths_by_id.setdefault(decision_id, []).append(path)
        if not path.stem.startswith(decision_id + "-"):
            errors.append(f"{path.relative_to(root)}: filename does not match heading ID {decision_id}")
    for decision_id, paths in decision_paths_by_id.items():
        if len(paths) > 1:
            errors.append(f"duplicate decision ID {decision_id}: " + ", ".join(str(p.relative_to(root)) for p in paths))

    # Detailed Markdown records are valid references even when they have no CSV index row.
    detail_ids = set(detail_record_ids)
    for paths in detail_paths.values():
        for path in paths:
            match = re.match(r"^(DR|KG|HI|VR)-\d{4}", path.stem)
            if match:
                detail_ids.add(match.group(0))
    record_ids = (
        task_ids
        | validation_ids
        | legacy_validation_ids
        | discussion_ids
        | gap_ids
        | intervention_ids
        | decision_ids
        | {path.stem for path in decision_paths}
        | detail_record_ids
    )

    for lineno, row in enumerate(tables["events.csv"], start=2):
        validate_timestamp(row, lineno, errors)
        if row["event_type"] not in {"project_authorization", "discussion", "maintainer_decision", "implementation_instruction", "implementation", "knowledge_discovery", "knowledge_gap", "human_intervention", "validation", "failure", "research_milestone", "status_update"}:
            errors.append(f"events.csv:{lineno}: unknown event_type {row['event_type']!r}")
        if row["outcome_status"] not in {"proposed", "approved", "instructed", "implemented", "validated", "failed", "unresolved", "observed", "pass", "partial_pass", "open"}:
            errors.append(f"events.csv:{lineno}: unknown outcome_status {row['outcome_status']!r}")
        if row["evidence_class"] not in {"contemporaneous", "retrospective", "prospective"}:
            errors.append(f"events.csv:{lineno}: unknown evidence_class {row['evidence_class']!r}")
        if row["privacy_status"] not in {"public_summary", "restricted_reference", "unavailable"}:
            errors.append(f"events.csv:{lineno}: unknown privacy_status {row['privacy_status']!r}")
        if row["task_id"]:
            if "development_tasks.csv" in unavailable:
                skipped.append("events.csv.task_id -> development_tasks.csv is structurally invalid; reference check not reliable")
            elif row["task_id"] not in task_ids:
                errors.append(f"events.csv:{lineno}: unknown task_id {row['task_id']!r}")

    for lineno, row in enumerate(tables["discussions.csv"], start=2):
        summary_path = root.parent / row["summary_path"]
        if not summary_path.is_file():
            errors.append(f"discussions.csv:{lineno}: missing summary_path {row['summary_path']!r}")
    for filename, rows, id_col, directory, prefix in (
        ("discussions.csv", tables["discussions.csv"], "discussion_id", "discussions", "DR"),
        ("knowledge_gaps.csv", tables["knowledge_gaps.csv"], "gap_id", "knowledge_gaps", "KG"),
        ("human_interventions.csv", tables["human_interventions.csv"], "intervention_id", "human_interventions", "HI"),
    ):
        if filename in unavailable:
            continue
        indexed = {row[id_col] for row in rows}
        detail = {path.stem for path in (record_dir / directory).glob(prefix + "-*.md")}
        for record_id in sorted(indexed - detail):
            errors.append(f"{filename}: indexed {record_id} has no detail file in records/{directory}/")
        for record_id in sorted(detail - indexed):
            errors.append(f"records/{directory}/{record_id}.md: detail file is missing its {filename} row")

    for row in tables["validation_runs.csv"]:
        if row["result"] not in {"pass", "partial_pass", "inconclusive", "fail"}:
            errors.append(f"validation_runs.csv: {row['validation_id']} has invalid result {row['result']!r}")

    for row in legacy_validation_rows:
        legacy_id = row["legacy_validation_id"]
        if ("validation_runs.csv" not in unavailable and legacy_id in validation_ids) or legacy_id in detail_ids:
            errors.append(f"legacy_validation_refs.csv: {legacy_id} also exists as a validation record")
        if row["confidence"] not in {"high", "medium", "low"}:
            errors.append(f"legacy_validation_refs.csv: {legacy_id} has invalid confidence {row['confidence']!r}")
        if row["record_status"] != "Original validation record missing; inference only":
            errors.append(f"legacy_validation_refs.csv: {legacy_id} must be explicitly marked inference-only")
        if not row["likely_scope"] or not row["likely_outcome"] or not row["evidence_basis"]:
            errors.append(f"legacy_validation_refs.csv: {legacy_id} requires scope, outcome, and evidence basis")
        if (row["candidate_validation_id"] and "validation_runs.csv" not in unavailable
                and row["candidate_validation_id"] not in validation_ids):
            errors.append(f"legacy_validation_refs.csv: {legacy_id} candidate_validation_id is not an existing validation")
    if "validation_runs.csv" in unavailable and legacy_validation_rows:
        skipped.append("legacy_validation_refs.csv validation-ID checks -> validation_runs.csv is structurally invalid")

    def refs(source: str, field: str, target: str, valid: set[str], *, required: bool = False,
             many: bool = False) -> None:
        if target in unavailable:
            skipped.append(f"{source}.{field} -> {target} is structurally invalid; reference check not reliable")
            return
        check_refs(tables[source], field, valid, source, errors, required=required, many=many)

    refs("development_tasks.csv", "start_event_id", "events.csv", event_ids)
    refs("development_tasks.csv", "end_event_id", "events.csv", event_ids)
    refs("discussions.csv", "event_id", "events.csv", event_ids, required=True, many=True)
    refs("knowledge_gaps.csv", "event_id", "events.csv", event_ids, required=True, many=True)
    refs("human_interventions.csv", "event_id", "events.csv", event_ids, required=True)
    refs("human_interventions.csv", "task_id", "development_tasks.csv", task_ids)
    refs("validation_runs.csv", "event_id", "events.csv", event_ids, required=True)
    refs("validation_runs.csv", "task_id", "development_tasks.csv", task_ids)
    refs("legacy_validation_refs.csv", "event_id", "events.csv", event_ids, required=True)
    refs("legacy_validation_refs.csv", "task_id", "development_tasks.csv", task_ids, required=True)
    if {"validation_runs.csv", "legacy_validation_refs.csv"} & unavailable:
        skipped.append("development_tasks.csv.validation_ids -> validation records are structurally invalid; reference check not reliable")
    else:
        check_refs(
            tables["development_tasks.csv"], "validation_ids", validation_ids | detail_ids | legacy_validation_ids,
            "development_tasks.csv", errors, many=True,
        )
    if any(name in unavailable for name in SCHEMAS):
        skipped.append("events.csv.record_refs -> one or more record indexes are structurally invalid; reference check not reliable")
    else:
        for lineno, row in enumerate(tables["events.csv"], start=2):
            for record_id in (item.strip() for item in row["record_refs"].split(";") if item.strip()):
                if record_id not in record_ids:
                    errors.append(f"events.csv:{lineno}: record_refs references unknown ID {record_id!r}")

    # Verify root-relative paths in structured path fields and local Markdown links.
    for filename, field in (("knowledge_gaps.csv", "record_path"), ("human_interventions.csv", "record_path")):
        for lineno, row in enumerate(tables[filename], start=2):
            target = root.parent / row[field]
            if row[field] and not target.is_file():
                errors.append(f"{filename}:{lineno}: missing {field} {row[field]!r}")
    link_pattern = re.compile(r"!?\[[^]]*\]\((?:<([^>]+)>|([^\s)]+))(?:\s+[^)]*)?\)")
    for markdown in sorted(root.rglob("*.md")):
        try:
            content = markdown.read_text(encoding="utf-8")
        except (OSError, UnicodeError) as exc:
            errors.append(f"{markdown.relative_to(root)}: cannot read Markdown: {exc}")
            continue
        for match in link_pattern.finditer(content):
            target = (match.group(1) or match.group(2) or "").split("#", 1)[0]
            if not target or ":" in target.split("/", 1)[0]:
                continue
            resolved = (markdown.parent / target).resolve()
            if not resolved.exists():
                line = content.count("\n", 0, match.start()) + 1
                errors.append(f"{markdown.relative_to(root)}:{line}: broken local link {target!r}")

    # Skip index/detail comparisons only where the corresponding index could not
    # be trusted. Independent detail-file and Markdown-link checks still ran.
    for filename, directory in (
        ("discussions.csv", "discussions"),
        ("knowledge_gaps.csv", "knowledge_gaps"),
        ("human_interventions.csv", "human_interventions"),
    ):
        if filename in unavailable:
            skipped.append(f"{filename} <-> records/{directory}/ index/detail check not reliable")

    return tables, errors, sorted(set(skipped))


def next_identifiers(tables: dict[str, list[dict[str, str]]], root: Path = ROOT) -> dict[str, str]:
    """Suggest the next unused IDs without reserving or writing them."""
    sources = {
        "EV": [row["event_id"] for row in tables["events.csv"]],
        "DT": [row["task_id"] for row in tables["development_tasks.csv"]],
        "DR": [row["discussion_id"] for row in tables["discussions.csv"]]
        + [path.stem for path in (root / "records/discussions").glob("DR-*.md")],
        "KG": [row["gap_id"] for row in tables["knowledge_gaps.csv"]]
        + [path.stem for path in (root / "records/knowledge_gaps").glob("KG-*.md")],
        "HI": [row["intervention_id"] for row in tables["human_interventions.csv"]]
        + [path.stem for path in (root / "records/human_interventions").glob("HI-*.md")],
        "VR": [row["validation_id"] for row in tables["validation_runs.csv"]]
        + [row["legacy_validation_id"] for row in tables["legacy_validation_refs.csv"]]
        + [path.stem for path in (root / "records/validation_runs").glob("VR-*.md")],
        "ADR": [match.group(1) for path in list((root / "decisions").glob("ADR-*.md"))
                + list((root / "records/decisions").glob("ADR-*.md"))
                if (match := re.search(r"^#\s+(ADR-\d{4})\b", path.read_text(encoding="utf-8"), re.MULTILINE))],
    }
    result: dict[str, str] = {}
    for prefix, values in sources.items():
        numbers = [int(match.group(1)) for value in values
                   if (match := re.fullmatch(rf"{prefix}-(\d{{4}})(?:-.+)?", value))]
        result[prefix] = f"{prefix}-{max(numbers, default=0) + 1:04d}"
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description="Read-only research record consistency checks.")
    parser.add_argument("--next-ids", action="store_true", help="print the next unused ID for each record type")
    args = parser.parse_args()
    tables, errors, skipped = validate_records()
    if errors:
        print(f"Research record validation found {len(errors)} error(s):", file=sys.stderr)
        for message in errors:
            print(f"ERROR: {message}", file=sys.stderr)
    if skipped:
        print(f"Research record validation skipped {len(skipped)} dependent check(s):", file=sys.stderr)
        for message in sorted(set(skipped)):
            print(f"SKIPPED: {message}", file=sys.stderr)
    if (errors or skipped) and not args.next_ids:
        raise SystemExit(1)

    if args.next_ids:
        print("Next unused IDs (suggestions only; not reserved):")
        for record_type, value in next_identifiers(tables).items():
            print(f"  {record_type}: {value}")
    elif not errors:
        print("Research record schemas, IDs, and references are valid.")
        legacy_ids = [row["legacy_validation_id"] for row in tables["legacy_validation_refs.csv"]]
        if legacy_ids:
            print(
                f"NOTE: {len(legacy_ids)} historical validation reference(s) are acknowledged in "
                "legacy_validation_refs.csv as inference only; original validation records were not recovered."
            )
        print("Rows:", ", ".join(f"{name}={len(rows)}" for name, rows in tables.items()))
        print("Empty tables are valid for categories with no verified records; the check does not imply missing history.")

if __name__ == "__main__":
    main()
