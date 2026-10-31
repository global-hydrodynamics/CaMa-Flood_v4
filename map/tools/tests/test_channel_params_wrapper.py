"""Check map-local channel-wrapper paths and failure handling with mock tools."""
from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
WRAPPER = ROOT / "map" / "tools" / "script_params" / "s01-channel_params.sh"


class ChannelParamsWrapperTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="cmf-channel-wrapper-")
        self.root = Path(self.temporary.name)
        self.map_root = self.root / "map" / "sample"
        self.script_dir = self.map_root / "script_params"
        self.code_dir = self.root / "map" / "tools" / "code"
        self.script_dir.mkdir(parents=True)
        self.code_dir.mkdir(parents=True)
        (self.root / "map" / "data").mkdir()
        (self.root / "outside").mkdir()
        (self.root / "map" / "data" / "ELSE_GPCC_clm-1981-2010.one").write_bytes(b"runoff")
        for name in ("params.txt", "nextxy.bin", "ctmare.bin", "width.bin",
                     "inpmat_test-1deg.bin"):
            (self.map_root / name).write_bytes(b"input")
        (self.map_root / "diminfo_test-1deg.txt").write_text(
            "80\n120\n10\n360\n180\n4\ninpmat_test-1deg.bin\n"
            "90\n110\n35\n5\n", encoding="ascii"
        )
        self.calls = self.root / "calls.txt"
        for name, outputs in (
            ("param_calc_outclm", ("outclm.bin",)),
            ("param_calc_rivwth", ("rivwth.bin", "rivhgt.bin", "rivman.bin", "rivhgt_inf.bin")),
            ("param_set_merit_width", ("rivwth_merit.bin",)),
        ):
            output_lines = "\n".join(f"printf x > '{output}'" for output in outputs)
            program = self.code_dir / name
            program.write_text(
                "#!/bin/sh\n"
                f"printf '%s|' \"$PWD\" >> \"$CALL_LOG\"\n"
                "for arg do printf '%s|' \"$arg\" >> \"$CALL_LOG\"; done\n"
                "printf '\\n' >> \"$CALL_LOG\"\n"
                f"{output_lines}\n",
                encoding="ascii",
            )
            program.chmod(0o755)
        script = self.script_dir / "s01-channel_params.sh"
        script.write_bytes(WRAPPER.read_bytes())
        script.chmod(0o755)
        self.script = script

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def test_runs_shared_programs_in_map_root_from_an_unrelated_cwd(self) -> None:
        env = {**os.environ, "CALL_LOG": str(self.calls)}
        result = subprocess.run(
            [str(self.script)], cwd=self.root / "outside", env=env,
            text=True, capture_output=True, check=False,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        records = [line.split("|") for line in self.calls.read_text().splitlines()]
        self.assertEqual(len(records), 3)
        self.assertTrue(all(Path(row[0]).resolve() == self.map_root.resolve() for row in records))
        self.assertEqual(records[0][1:], ["bin", "inpmat", "diminfo_test-1deg.txt",
                                          "../data/ELSE_GPCC_clm-1981-2010.one", ""])
        self.assertEqual(records[1][1:], ["bin", "diminfo_test-1deg.txt", "0.1", "0.50",
                                          "0.00", "1.0", "2.50", "0.60", "0.00", "5.0", ""])
        self.assertEqual(records[2][1:], ["diminfo_test-1deg.txt", ""])
        for name in ("outclm.bin", "rivwth.bin", "rivhgt.bin", "rivman.bin",
                     "rivhgt_inf.bin", "rivwth_merit.bin"):
            self.assertTrue((self.map_root / name).is_file(), name)

    def test_reports_missing_runoff_before_invoking_tools(self) -> None:
        (self.root / "map" / "data" / "ELSE_GPCC_clm-1981-2010.one").unlink()
        result = subprocess.run(
            [str(self.script)], cwd=self.root / "outside",
            env={**os.environ, "CALL_LOG": str(self.calls)},
            text=True, capture_output=True, check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("annual runoff climatology missing", result.stderr)
        self.assertFalse(self.calls.exists())


if __name__ == "__main__":
    unittest.main(verbosity=2)
