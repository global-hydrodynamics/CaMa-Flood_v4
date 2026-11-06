"""Focused tests for the read-only research record validator."""
from __future__ import annotations

import csv
import tempfile
import unittest
from pathlib import Path

from research.validate_records import SCHEMAS, next_identifiers, validate_records


def write_tables(repo: Path) -> Path:
    research = repo / "research"
    research.mkdir(parents=True)
    for filename, columns in SCHEMAS.items():
        with (research / filename).open("w", newline="", encoding="utf-8") as stream:
            csv.writer(stream).writerow(columns)
    return research


class ResearchValidatorTests(unittest.TestCase):
    def test_reports_multiple_dangling_refs_in_one_pass(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            research = write_tables(Path(temporary))
            path = research / "events.csv"
            with path.open("a", newline="", encoding="utf-8") as stream:
                writer = csv.writer(stream)
                for number, missing in ((1, "VR-0099"), (2, "VR-0100")):
                    writer.writerow([
                        f"EV-{number:04d}", "2026-11-06", "date", "validation", "Codex", "agent",
                        "Focused fixture", "validated", "contemporaneous", "", missing, "", "public_summary",
                    ])

            _, errors = validate_records(research)
            self.assertTrue(any("VR-0099" in error for error in errors))
            self.assertTrue(any("VR-0100" in error for error in errors))

    def test_accepts_documented_multi_event_discussion_links(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            research = write_tables(repo)
            records = research / "records/discussions"
            records.mkdir(parents=True)
            detail = records / "DR-0001.md"
            detail.write_text("# DR-0001: Fixture\n", encoding="utf-8")
            events_path = research / "events.csv"
            with events_path.open("a", newline="", encoding="utf-8") as stream:
                writer = csv.writer(stream)
                for number in (1, 2):
                    writer.writerow([
                        f"EV-{number:04d}", "2026-11-06", "date", "discussion", "Codex", "agent",
                        "Focused fixture", "observed", "contemporaneous", "", "DR-0001", "", "public_summary",
                    ])
            with (research / "discussions.csv").open("a", newline="", encoding="utf-8") as stream:
                csv.writer(stream).writerow([
                    "DR-0001", "EV-0001;EV-0002", "fixture", "source", "research/records/discussions/DR-0001.md",
                    "pending", "restricted",
                ])

            _, errors = validate_records(research)
            self.assertEqual(errors, [])

    def test_reports_csv_width_and_duplicate_global_adr_ids(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            research = write_tables(Path(temporary))
            with (research / "events.csv").open("a", encoding="utf-8") as stream:
                stream.write("EV-0001,2026-11-06\n")
            for directory, title in (("decisions", "First"), ("records/decisions", "Second")):
                path = research / directory / "ADR-0001-fixture.md"
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(f"# ADR-0001: {title}\n", encoding="utf-8")

            _, errors = validate_records(research)
            self.assertTrue(any("expected 13 columns, found 2" in error for error in errors))
            self.assertTrue(any("duplicate decision ID ADR-0001" in error for error in errors))

    def test_next_id_suggestions_are_read_only_and_global(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            research = write_tables(Path(temporary))
            decisions = research / "decisions"
            decisions.mkdir()
            (decisions / "ADR-0004-existing.md").write_text("# ADR-0004: Existing\n", encoding="utf-8")
            suggestions = next_identifiers({name: [] for name in SCHEMAS}, research)
            self.assertEqual(suggestions["ADR"], "ADR-0005")
            self.assertFalse((research / "events.csv").read_text(encoding="utf-8").strip().endswith("EV-0001"))


if __name__ == "__main__":
    unittest.main()
