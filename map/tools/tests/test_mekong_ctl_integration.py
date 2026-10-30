"""Run the map-local CTL wrapper against a temporary copy of Mekong binaries."""
from __future__ import annotations

import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SAMPLE = ROOT / "map" / "mekong_15min"


class MekongCtlWrapperIntegrationTests(unittest.TestCase):
    def test_wrapper_generates_descriptors_for_all_committed_sample_binaries(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-mekong-ctl-") as temporary:
            temp_root = Path(temporary)
            map_root = temp_root / "map"
            fixture = map_root / "mekong_15min"
            (map_root / "tools" / "code").mkdir(parents=True)
            (fixture / "script_region").mkdir(parents=True)
            (fixture / "1min").mkdir()

            shutil.copy2(ROOT / "map" / "map_variables.csv", map_root / "map_variables.csv")
            shutil.copy2(ROOT / "map" / "tools" / "code" / "generate_ctl.py",
                         map_root / "tools" / "code" / "generate_ctl.py")
            shutil.copy2(SAMPLE / "script_region" / "s02-write_ctl.sh",
                         fixture / "script_region" / "s02-write_ctl.sh")
            for name in ("params.txt", "diminfo_test-1deg.txt"):
                shutil.copy2(SAMPLE / name, fixture / name)
            for path in SAMPLE.glob("*.bin"):
                shutil.copy2(path, fixture / path.name)
            shutil.copy2(SAMPLE / "1min" / "location.txt", fixture / "1min" / "location.txt")
            for path in (SAMPLE / "1min").glob("*.bin"):
                shutil.copy2(path, fixture / "1min" / path.name)

            process = subprocess.run(
                ["sh", "s02-write_ctl.sh"], cwd=fixture / "script_region",
                text=True, capture_output=True, check=False,
            )
            self.assertEqual(process.returncode, 0, process.stdout + process.stderr)
            summaries = [tuple(map(int, match)) for match in re.findall(
                r"Summary: generated=(\d+), skipped=(\d+), failed=(\d+), fallback=(\d+)", process.stdout
            )]
            self.assertEqual(len(summaries), 2, process.stdout + process.stderr)
            expected = (len(list(SAMPLE.glob("*.bin"))),
                        len(list((SAMPLE / "1min").glob("*.bin"))) )
            self.assertEqual(summaries, [(expected[0], 0, 0, 0), (expected[1], 0, 0, 0)],
                             process.stdout + process.stderr)

            base_ctls = list(fixture.glob("*.ctl"))
            high_ctls = list((fixture / "1min").glob("*.ctl"))
            self.assertEqual(len(base_ctls), expected[0])
            self.assertEqual(len(high_ctls), expected[1])
            for ctl in base_ctls + high_ctls:
                text = ctl.read_text(encoding="utf-8")
                self.assertIn("options yrev little_endian", text, ctl.name)
                self.assertRegex(text, r"(?m)^dset \^.+\.bin$", ctl.name)
                self.assertNotRegex(text, r"(?m)^\w+_hires\s", ctl.name)

            self.assertIn("xdef 80 linear", (fixture / "nextxy.ctl").read_text())
            self.assertIn("vars 2", (fixture / "nextxy.ctl").read_text())
            self.assertIn("nextx 1 -1,40,4", (fixture / "nextxy.ctl").read_text())
            self.assertIn("undef -9999", (fixture / "nextxy.ctl").read_text())
            noedge_ctl = (fixture / "nextxy_noedge.ctl").read_text()
            self.assertIn("nextx_noedge 1 -1,40,4", noedge_ctl)
            self.assertIn("nexty_noedge 1 -1,40,4", noedge_ctl)
            self.assertIn("downx 1 -1,40,4", (fixture / "downxy.ctl").read_text())
            self.assertIn("downy 1 -1,40,4", (fixture / "downxy.ctl").read_text())
            self.assertIn("zdef 10 linear", (fixture / "fldhgt.ctl").read_text())
            self.assertIn("zdef 4 linear", (fixture / "inpmat_test-1deg.ctl").read_text())
            self.assertIn("xdef 1200 linear", (fixture / "1min" / "1min.catmxy.ctl").read_text())
            catmxy_ctl = (fixture / "1min" / "1min.catmxy.ctl").read_text()
            self.assertIn("catmxx 1 -1,40,2,-1", catmxy_ctl)
            self.assertIn("catmyy 1 -1,40,2,-1", catmxy_ctl)
            high_downxy_ctl = (fixture / "1min" / "1min.downxy.ctl").read_text()
            self.assertIn("downx 1 -1,40,2,-1", high_downxy_ctl)
            self.assertIn("downy 1 -1,40,2,-1", high_downxy_ctl)
            for name, sentinel in (("1min.catmz100.ctl", 247), ("1min.catmzz.ctl", 247),
                                   ("1min.flwdir.ctl", 247), ("1min.visual.ctl", 247),
                                   ("1min.downxy.ctl", -9999)):
                ctl_text = (fixture / "1min" / name).read_text(encoding="utf-8")
                self.assertRegex(ctl_text, rf"(?m)^undef {sentinel}(?:\.0+)?$", name)


if __name__ == "__main__":
    unittest.main(verbosity=2)
