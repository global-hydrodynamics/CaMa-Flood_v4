"""Small synthetic tests for the v5 permanent-water calculator."""

from __future__ import annotations

import shutil
import struct
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "map/tools/code/param_water_calc_prmwat.F90"


@unittest.skipUnless(shutil.which("gfortran"), "gfortran is not installed")
class PermanentWaterTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build_dir = tempfile.TemporaryDirectory(prefix="cmf-prmwat-build-")
        cls.executable = Path(cls.build_dir.name) / "param_water_calc_prmwat"
        subprocess.run(
            [
                shutil.which("gfortran") or "gfortran",
                "-O0",
                "-g",
                "-fcheck=all",
                "-ffree-line-length-none",
                str(SOURCE),
                "-o",
                str(cls.executable),
            ],
            check=True,
            cwd=ROOT,
        )

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build_dir.cleanup()

    def setUp(self) -> None:
        self.work = tempfile.TemporaryDirectory(prefix="cmf-prmwat-case-")
        self.map_root = Path(self.work.name)
        self.make_inputs()

    def tearDown(self) -> None:
        self.work.cleanup()

    def make_inputs(self, *, grdare: list[float] | None = None) -> None:
        (self.map_root / "1min").mkdir(exist_ok=True)
        (self.map_root / "params.txt").write_text(
            "2\n2\n1\n0.25\n0\n0.5\n0\n0.5\n", encoding="ascii"
        )
        # Two direct-access int32 records, x then y, in Fortran array order.
        nextx = [1, 2, -9999, 1]
        nexty = [1, 1, -9999, 2]
        (self.map_root / "nextxy.bin").write_bytes(
            struct.pack("<8i", *(nextx + nexty))
        )
        (self.map_root / "ctmare.bin").write_bytes(
            struct.pack("<4f", 100.0, 500000.0, -9999.0, 1000000.0)
        )
        (self.map_root / "1min/location.txt").write_text(
            "1 narea\n"
            "iarea carea west east south north nx ny csize\n"
            "1 area 0 0.0333333333333333 0 0.0333333333333333 2 2 0.0166666666666667\n",
            encoding="ascii",
        )
        # catmxy is two int16 records: x assignment followed by y assignment.
        (self.map_root / "1min/area.catmxy.bin").write_bytes(
            struct.pack("<8h", 1, 1, 2, 2, 1, 1, 1, 2)
        )
        (self.map_root / "1min/area.rivwth.bin").write_bytes(
            struct.pack("<4f", 10.0, -1.0, 10.0, -1.0)
        )
        (self.map_root / "1min/area.flddif.bin").write_bytes(
            struct.pack("<4f", 5.0, 0.0, 6.0, -9999.0)
        )
        if grdare is not None:
            (self.map_root / "1min/area.grdare.bin").write_bytes(
                struct.pack("<4f", *grdare)
            )

    def run_program(self, tag: str = "1min") -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [str(self.executable), tag],
            cwd=self.map_root,
            text=True,
            capture_output=True,
            check=False,
        )

    def test_legacy_classification_flddif_boundary_missing_value_and_cap(self) -> None:
        self.make_inputs(grdare=[0.0004, 0.0008, 0.0001, 0.0003])
        result = self.run_program()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("flddif=-9999", result.stdout)
        self.assertIn("Catchments capped at ctmare: 1", result.stdout)
        values = struct.unpack("<4f", (self.map_root / "prmwat.bin").read_bytes())
        self.assertEqual(values, (100.0, 0.0, -9999.0, 300.0))

    def test_missing_grdare_uses_legacy_fallback(self) -> None:
        result = self.run_program()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("using the legacy scaled rgetarea fallback", result.stdout)
        self.assertIn("Used grdare fallback: T", result.stdout)
        self.assertEqual((self.map_root / "prmwat.bin").stat().st_size, 16)

    def test_invalid_resolution_is_rejected(self) -> None:
        result = self.run_program("10sec")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Unsupported HIRES_TAG", result.stderr)
        self.assertFalse((self.map_root / "prmwat.bin").exists())

    def test_out_of_range_catchment_assignment_is_rejected(self) -> None:
        (self.map_root / "1min/area.catmxy.bin").write_bytes(
            struct.pack("<8h", 3, 1, 2, 2, 1, 1, 1, 2)
        )
        result = self.run_program()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("outside the base map", result.stderr)
        self.assertIn("at pixel (1,1)", result.stderr)
        self.assertFalse((self.map_root / "prmwat.bin").exists())

    def test_out_of_range_catchment_y_assignment_is_rejected(self) -> None:
        (self.map_root / "1min/area.catmxy.bin").write_bytes(
            struct.pack("<8h", 1, 1, 2, 2, 3, 1, 1, 2)
        )
        result = self.run_program()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("outside the base map", result.stderr)
        self.assertIn("at pixel (1,1)", result.stderr)
        self.assertFalse((self.map_root / "prmwat.bin").exists())

    def test_missing_flddif_is_a_clear_error(self) -> None:
        (self.map_root / "1min/area.flddif.bin").unlink()
        result = self.run_program()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("area.flddif.bin", result.stderr)

    def test_invalid_selected_pixel_area_is_rejected(self) -> None:
        self.make_inputs(grdare=[-9999.0, 0.0008, 0.0001, 0.0003])
        result = self.run_program()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Invalid grdare area for selected water pixel", result.stderr)

    def test_nonpositive_network_catchment_area_is_rejected(self) -> None:
        (self.map_root / "ctmare.bin").write_bytes(
            struct.pack("<4f", 0.0, 500000.0, -9999.0, 1000000.0)
        )
        result = self.run_program()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Invalid ctmare at map cell (1,1)", result.stderr)

    def test_wrong_binary_size_is_rejected(self) -> None:
        (self.map_root / "1min/area.rivwth.bin").write_bytes(b"short")
        result = self.run_program()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Wrong size", result.stderr)


if __name__ == "__main__":
    unittest.main()
