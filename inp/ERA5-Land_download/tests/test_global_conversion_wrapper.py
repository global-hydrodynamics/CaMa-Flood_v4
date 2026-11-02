"""Standard-library tests for the global monthly conversion orchestration."""
from __future__ import annotations

import importlib.util
import io
import tempfile
import unittest
from contextlib import redirect_stdout
from datetime import date
from pathlib import Path

TOOLS = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("convert_global_CMF_bin", TOOLS / "convert_global_CMF_bin.py")
assert SPEC is not None and SPEC.loader is not None
global_bin = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(global_bin)


class GlobalChunkHelperTests(unittest.TestCase):
    def test_months_clip_first_and_last_month_to_inclusive_dates(self):
        self.assertEqual(
            list(global_bin.months(date(2000, 1, 30), date(2000, 3, 2))),
            [(date(2000, 1, 30), date(2000, 1, 31)),
             (date(2000, 2, 1), date(2000, 2, 29)),
             (date(2000, 3, 1), date(2000, 3, 2))],
        )

    def test_global_ctl_uses_complete_period_and_original_grid_metadata(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            source = root / "month.ctl"
            destination = root / "runoff.ctl"
            source.write_text(
                "dset ^./runoff_%y4%m2%d2.bin\n"
                "xdef  3600 linear 0 0.1\n"
                "ydef  1801 linear -90 0.1\n"
                "tdef 31 linear 00Z01jan2000 1dy\n"
                "vars 1\nrunoff 0 99 ** runoff [mm/day]\nENDVARS\n",
                encoding="utf-8",
            )
            nx, ny = global_bin.rewrite_ctl(
                source, destination, prefix="runoff_", suffix=".bin",
                start=date(2000, 1, 1), records=366,
            )
            text = destination.read_text(encoding="utf-8")
            self.assertEqual((nx, ny), (3600, 1801))
            self.assertIn("dset  ^./runoff_%y4%m2%d2.bin", text)
            self.assertIn("tdef    366 linear  00Z01jan2000 1dy", text)
            self.assertIn("ydef  1801 linear -90 0.1", text)

    def test_global_dry_run_lists_source_months_without_conversion_dependencies(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            inputs = root / "source"
            inputs.mkdir()
            (inputs / "ERA5-Land_runoff_200002.nc").touch()
            (inputs / "ERA5-Land_runoff_200003.nc").touch()
            old_argv = __import__("sys").argv
            try:
                __import__("sys").argv = [
                    "convert_global_CMF_bin.py", "--start", "2000-02-28", "--end", "2000-03-01",
                    "--indir", str(inputs), "--outdir", str(root / "out"), "--dry-run",
                ]
                output = io.StringIO()
                with redirect_stdout(output):
                    self.assertEqual(global_bin.main(), 0)
                self.assertIn("2000-02-28..2000-02-29", output.getvalue())
                self.assertIn("2000-03-01..2000-03-01", output.getvalue())
                self.assertTrue((root / "out").is_dir())
            finally:
                __import__("sys").argv = old_argv


if __name__ == "__main__":
    unittest.main()
