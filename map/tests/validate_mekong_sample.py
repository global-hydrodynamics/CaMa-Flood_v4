#!/usr/bin/env python3
"""Validate the committed regionalization-stage Mekong CTL fixture."""
from __future__ import annotations

import csv
import struct
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MAP = ROOT / "map" / "mekong_15min"


class MekongFixtureTests(unittest.TestCase):
    """Check only products distributed for map-tool tests, not a full model map."""

    def test_geometry_and_input_matrix_metadata(self) -> None:
        values = [float(line.split()[0]) for line in (MAP / "params.txt").read_text().splitlines()]
        self.assertEqual(values[:3], [80.0, 120.0, 10.0])
        self.assertAlmostEqual(values[3], 0.25)
        self.assertEqual(values[4:], [90.0, 110.0, 5.0, 35.0])

        diminfo = (MAP / "diminfo_test-1deg.txt").read_text().splitlines()
        self.assertEqual([int(diminfo[i].split()[0]) for i in (0, 1, 2, 5)], [80, 120, 10, 4])
        self.assertEqual(diminfo[6].strip(), "inpmat_test-1deg.bin")

        inpmat_bytes = 3 * 80 * 120 * 4 * 4  # 3 records, 4 slots, 4 bytes per slot
        self.assertEqual((MAP / "inpmat_test-1deg.bin").stat().st_size, inpmat_bytes)

    def test_core_multirecord_and_multilayer_files(self) -> None:
        cells = 80 * 120
        expected = {
            "nextxy.bin": 2 * cells * 4,
            "nextxy_noedge.bin": 2 * cells * 4,
            "downxy.bin": 2 * cells * 4,
            "lonlat.bin": 2 * cells * 4,
            "fldhgt.bin": 10 * cells * 4,
        }
        for name, size in expected.items():
            with self.subTest(name=name):
                self.assertEqual((MAP / name).stat().st_size, size)

        self.assertEqual(len(struct.unpack(f"<{2*cells}i", (MAP / "nextxy.bin").read_bytes())), 2 * cells)
        self.assertEqual(len(struct.unpack(f"<{10*cells}f", (MAP / "fldhgt.bin").read_bytes())), 10 * cells)

    def test_committed_base_and_high_resolution_binary_inventory(self) -> None:
        expected_base = {
            "basin.bin", "bsncol.bin", "ctmare.bin", "downxy.bin", "elevtn.bin",
            "fldhgt.bin", "grdare.bin", "inpmat_test-1deg.bin", "lonlat.bin",
            "lsmask.bin", "nextxy.bin", "nextxy_noedge.bin", "nxtdst.bin",
            "nxtdst_grid.bin", "rivlen.bin", "rivlen_grid.bin", "rivseq.bin",
            "uparea.bin", "uparea_grid.bin", "upgrid.bin", "width.bin",
        }
        expected_high = {
            "1min.catmxy.bin", "1min.catmz100.bin", "1min.catmzz.bin", "1min.downxy.bin",
            "1min.elevtn.bin", "1min.flddif.bin", "1min.flwdir.bin", "1min.grdare.bin",
            "1min.hand.bin", "1min.rivwth.bin", "1min.uparea.bin", "1min.visual.bin",
        }
        actual_base = {p.name for p in MAP.glob("*.bin")}
        actual_high = {p.name for p in (MAP / "1min").glob("*.bin")}
        self.assertEqual(actual_base, expected_base)
        self.assertEqual(actual_high, expected_high)

    def test_registered_text_metadata_present_in_sample_has_default_status(self) -> None:
        with (ROOT / "map" / "map_variables.csv").open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        sample_text = {
            "params.txt": "params.txt",
            "bifori.txt": "bifori.txt",
            "diminfo_test-1deg.txt": "diminfo_test-1deg.txt",
            "1min/location.txt": "<HIRES>/location.txt",
        }
        for relative, registered_name in sample_text.items():
            with self.subTest(path=relative):
                self.assertTrue((MAP / relative).is_file())
                matches = [row for row in rows if row["binary_filename"] == registered_name]
                self.assertEqual(len(matches), 1)
                self.assertEqual(matches[0]["workflow_stage"], "default")

    def test_binary_byte_counts_match_mekong_grid_and_registered_types(self) -> None:
        base_cells = 80 * 120
        high_cells = 1200 * 1800
        widths = {"int8": 1, "int16": 2, "int32": 4, "float32": 4}
        with (ROOT / "map" / "map_variables.csv").open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        binary_paths = sorted(MAP.rglob("*.bin"))
        self.assertEqual(len(binary_paths), 33)
        for path in binary_paths:
            self.assertGreater(path.stat().st_size, 0, path.relative_to(MAP))
            self.assertLess(path.stat().st_size, 100 * 1024 * 1024, path.relative_to(MAP))
            matches = [row for row in rows if row["binary_filename"] == path.name or
                       row["binary_filename"].startswith("<HIRES>/") and path.parent.name == "1min" and
                       path.name.removeprefix("1min.") in row["binary_filename"]]
            self.assertTrue(matches, f"no registry entry for {path.relative_to(MAP)}")
            self.assertTrue(all(row["workflow_stage"] == "default" for row in matches),
                            f"workflow_stage must be default for {path.relative_to(MAP)}")
            row = matches[0]
            cells = high_cells if path.parent.name == "1min" else base_cells
            if path.name in {"nextxy.bin", "nextxy_noedge.bin", "downxy.bin", "lonlat.bin"} or path.name == "1min.downxy.bin" or path.name == "1min.catmxy.bin":
                records = 2
            elif path.name == "fldhgt.bin":
                records = 10
            elif path.name == "inpmat_test-1deg.bin":
                records = 3
            else:
                records = 1
            dtype = row["dtype"].split(" + ")[0]
            if path.name == "inpmat_test-1deg.bin":
                bytes_per_cell = 4 * 4  # INPN slots, each with a four-byte value
            else:
                self.assertIn(dtype, widths, f"unsupported dtype {dtype} for {path.name}")
                bytes_per_cell = widths[dtype]
            self.assertEqual(path.stat().st_size, cells * records * bytes_per_cell,
                             f"{path.relative_to(MAP)}: dtype/layout size mismatch")

        location = (MAP / "1min" / "location.txt").read_text()
        self.assertIn("1200", location)
        self.assertIn("1800", location)


if __name__ == "__main__":
    unittest.main(verbosity=2)
