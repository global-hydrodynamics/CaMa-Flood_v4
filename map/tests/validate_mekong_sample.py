#!/usr/bin/env python3
"""Validate the tracked Mekong reference map without third-party packages."""
from __future__ import annotations

import math
import struct
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MAP = ROOT / "map" / "mekong_15min"


def numeric_records(filename: str) -> list[float | int]:
    data = (MAP / filename).read_bytes()
    if len(data) % 4:
        raise AssertionError(f"{filename}: byte count is not a multiple of 4")
    return [v[0] for v in struct.iter_unpack("<f" if filename not in {
        "nextxy.bin", "nextxy_inflow.bin", "nextxy_inflow_keep.bin",
        "bifbsn.bin", "bifbsn_mpi.bin", "bifcol.bin", "bifcol_mpi.bin",
        "bifmod.bin", "bifmod_mpi.bin", "bpoint.bin", "bpoint_mpi.bin",
        "mpireg.bin", "mpireg-4.bin", "mpireg-8.bin", "mpireg-16.bin",
        "upbasin.bin", "upst_mask.bin", "upst_mask_keep.bin",
    } else "<i", data)]


class MekongSampleTests(unittest.TestCase):
    def test_dataset_geometry_and_conversion_metadata(self) -> None:
        values = [float(line.split()[0]) for line in (MAP / "params.txt").read_text().splitlines()]
        self.assertEqual(values[:3], [80.0, 120.0, 10.0])
        self.assertAlmostEqual(values[3], 0.25)
        self.assertEqual(values[4:], [90.0, 110.0, 5.0, 35.0])
        config = (MAP / "map_metadata.json").read_text()
        self.assertIn('"dataset_id": "mekong_15min"', config)
        self.assertNotIn("/Users/", config)
        self.assertNotIn("/home/", config)

    def test_required_standard_products_and_binary_sizes(self) -> None:
        nx, ny, nlfp = 80, 120, 10
        cells = nx * ny
        sizes = {
            "nextxy.bin": 2 * cells * 4,
            "ctmare.bin": cells * 4, "elevtn.bin": cells * 4,
            "nxtdst.bin": cells * 4, "rivlen.bin": cells * 4,
            "fldhgt.bin": nlfp * cells * 4,
            "rivwth.bin": cells * 4, "rivhgt.bin": cells * 4,
            "rivman.bin": cells * 4, "rivwth_gwdlr.bin": cells * 4,
            "inpmat_test-1deg.bin": 3 * 4 * cells * 4,
            "bifdph.bin": 2 * cells * 4,
        }
        for name, size in sizes.items():
            with self.subTest(name=name):
                path = MAP / name
                self.assertTrue(path.is_file())
                self.assertEqual(path.stat().st_size, size)
        for name in ("outclm.bin", "rivhgt_inf.bin", "prmwat.bin", "bifprm.txt",
                     "bifbsn.bin", "bifcol.bin", "bifmod.bin", "bpoint.bin",
                     "nextxy_inflow.bin", "nextxy_inflow_keep.bin", "upbasin.bin",
                     "upst_mask.bin", "upst_mask_keep.bin"):
            with self.subTest(name=name):
                self.assertTrue((MAP / name).is_file())

    def test_core_record_counts_and_network_indices(self) -> None:
        cells = 80 * 120
        nextxy = numeric_records("nextxy.bin")
        self.assertEqual(len(nextxy), 2 * cells)
        nextx, nexty = nextxy[:cells], nextxy[cells:]
        for v in nextx:
            self.assertTrue(v == -9999 or v in (-10, -9) or 1 <= v <= 80, v)
        for v in nexty:
            self.assertTrue(v == -9999 or v in (-10, -9) or 1 <= v <= 120, v)
        for name in ("ctmare.bin", "elevtn.bin", "nxtdst.bin", "rivlen.bin",
                     "rivwth.bin", "rivhgt.bin", "rivman.bin", "outclm.bin",
                     "prmwat.bin", "rivwth_gwdlr.bin"):
            vals = numeric_records(name)
            self.assertEqual(len(vals), cells, name)
            self.assertTrue(all(math.isfinite(float(v)) for v in vals), name)
        self.assertEqual(len(numeric_records("fldhgt.bin")), 10 * cells)
        self.assertEqual(len(numeric_records("bifdph.bin")), 2 * cells)
        self.assertEqual(len(numeric_records("inpmat_test-1deg.bin")), 3 * 4 * cells)

    def test_complete_one_minute_layers_and_records(self) -> None:
        high = MAP / "1min"
        nx, ny = 1200, 1800
        cells = nx * ny
        location = (high / "location.txt").read_text()
        self.assertIn("1200", location)
        self.assertIn("1800", location)
        expected_bytes = {
            "1min.catmxy.bin": 2 * cells * 2,
            "1min.downxy.bin": 2 * cells * 2,
            "1min.catmz100.bin": cells, "1min.catmzz.bin": cells,
            "1min.visual.bin": cells,
            "1min.elevtn.bin": cells * 4, "1min.flddif.bin": cells * 4,
            "1min.grdare.bin": cells * 4, "1min.hand.bin": cells * 4,
            "1min.rivwth.bin": cells * 4, "1min.uparea.bin": cells * 4,
        }
        for name, size in expected_bytes.items():
            with self.subTest(name=name):
                self.assertEqual((high / name).stat().st_size, size)
        ctl_files = {p.name for p in high.glob("*.ctl")}
        for name in expected_bytes:
            self.assertIn(name.removesuffix(".bin") + ".ctl", ctl_files)

    def test_ctl_targets_and_repository_independence(self) -> None:
        allowed_unpaired = {"catmpx.ctl", "outfix.ctl", "outupa.ctl", "rivhgt_pth.ctl"}
        missing = []
        for path in MAP.rglob("*.ctl"):
            lines = path.read_text().splitlines()
            dset = next((line.split(None, 1)[1].strip() for line in lines
                         if line.lower().startswith("dset ")), None)
            if dset is None:
                continue
            target = dset.removeprefix("^")
            if not (path.parent / target).is_file():
                missing.append(path.relative_to(MAP).as_posix())
        self.assertEqual(set(missing), allowed_unpaired)
        for path in MAP.rglob("*"):
            if not path.is_file() or path.suffix.lower() == ".bin":
                continue
            self.assertLess(path.stat().st_size, 100 * 1024 * 1024, path.as_posix())
            try:
                text = path.read_text(encoding="utf-8")
            except (UnicodeDecodeError, OSError):
                continue
            self.assertNotIn("/Users/", text, path.as_posix())
            self.assertNotIn("/home/", text, path.as_posix())
        for path in MAP.rglob("*.bin"):
            self.assertGreater(path.stat().st_size, 0, path.as_posix())
            self.assertLess(path.stat().st_size, 100 * 1024 * 1024, path.as_posix())


if __name__ == "__main__":
    unittest.main(verbosity=2)
