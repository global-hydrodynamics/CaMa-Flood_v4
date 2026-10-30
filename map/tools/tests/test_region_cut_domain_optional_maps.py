"""Exercise optional base-map layers with a tiny synthetic regional crop."""
from __future__ import annotations

import struct
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PROGRAM = ROOT / "map" / "tools" / "code" / "region_cut_domain"


def _write_records(path: Path, values: list[int | float], code: str) -> None:
    path.write_bytes(struct.pack(f"<{len(values)}{code}", *values))


class OptionalRegionalMapTests(unittest.TestCase):
    def test_catmpx_and_class_are_cropped_when_present_and_omitted_when_absent(self) -> None:
        if not PROGRAM.is_file():
            self.skipTest("build map/tools/code/region_cut_domain first")

        for include_optional in (True, False):
            with self.subTest(include_optional=include_optional), tempfile.TemporaryDirectory(
                prefix="cmf-region-optional-"
            ) as temporary:
                root = Path(temporary)
                source = root / "source"
                regional = root / "regional"
                work = regional / "script_region"
                source.mkdir()
                work.mkdir(parents=True)
                (source / "params.txt").write_text(
                    "4\n3\n2\n1\n0\n4\n0\n3\n", encoding="ascii"
                )
                (work / "region_info.txt").write_text(
                    f"{source}/\n1\n3\n1\n3\n", encoding="ascii"
                )

                cells = 4 * 3
                missing = [-9999] * cells
                _write_records(source / "nextxy.bin", missing + missing, "i")
                _write_records(source / "downxy.bin", missing + missing, "i")
                for name in ("elevtn", "ctmare", "grdare", "uparea", "nxtdst", "rivlen", "width"):
                    _write_records(source / f"{name}.bin", [float(i) for i in range(cells)], "f")
                _write_records(source / "fldhgt.bin", [float(i) for i in range(2 * cells)], "f")
                _write_records(source / "lonlat.bin", [float(i) for i in range(2 * cells)], "f")

                expected_catmpx = [102, 103, 202, 203]
                expected_class = [12, 13, 22, 23]
                if include_optional:
                    catmpx = [100 * y + x for y in range(1, 4) for x in range(1, 5)]
                    classmap = [10 * y + x for y in range(1, 4) for x in range(1, 5)]
                    _write_records(source / "catmpx.bin", catmpx, "i")
                    _write_records(source / "class.bin", classmap, "i")

                result = subprocess.run(
                    [str(PROGRAM)], cwd=work, text=True, capture_output=True, check=False
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                if include_optional:
                    actual_catmpx = struct.unpack("<4i", (regional / "catmpx.bin").read_bytes())
                    actual_class = struct.unpack("<4i", (regional / "class.bin").read_bytes())
                    self.assertEqual(list(actual_catmpx), expected_catmpx)
                    self.assertEqual(list(actual_class), expected_class)
                else:
                    self.assertFalse((regional / "catmpx.bin").exists())
                    self.assertFalse((regional / "class.bin").exists())


if __name__ == "__main__":
    unittest.main(verbosity=2)
