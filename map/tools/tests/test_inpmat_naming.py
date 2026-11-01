"""Verify runoff and high-resolution tags remain separate in s03."""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
WRAPPER = ROOT / "map" / "tools" / "script_params" / "s03-generate_inpmat.sh"


class InputMatrixNamingTests(unittest.TestCase):
    def test_rofftag_derives_pair_and_hires_tag_selects_map(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-inpmat-name-") as temporary:
            root = Path(temporary)
            map_root = root / "map" / "sample"
            script_dir = map_root / "script_params"
            code_dir = root / "map" / "tools" / "code"
            hires_dir = map_root / "15sec"
            script_dir.mkdir(parents=True)
            code_dir.mkdir(parents=True)
            hires_dir.mkdir()
            (map_root / "params.txt").write_text("3\n2\n2\n1\n-1.5\n1.5\n-1\n1\n", encoding="ascii")
            (hires_dir / "location.txt").write_text("1\narea west east south north nx ny cell\n1 tile\n", encoding="ascii")
            (hires_dir / "tile.catmxy.bin").write_bytes(b"fixture")

            program = code_dir / "param_generate_inpmat"
            program.write_text(
                "#!/bin/sh\n"
                "hires=$1; inpdim=$8; inpmat=$9\n"
                "printf '%s\\n%s\\n%s\\n' \"$hires\" \"$inpdim\" \"$inpmat\" > called.args\n"
                "printf '3\\n2\\n2\\n360\\n180\\n1\\n%s\\n-1.5\\n1.5\\n1\\n-1\\n' \"$inpmat\" > \"$inpdim\"\n"
                "printf x > \"$inpmat\"\n",
                encoding="ascii",
            )
            program.chmod(0o755)
            script = script_dir / WRAPPER.name
            shutil.copy2(WRAPPER, script)
            script.chmod(0o755)

            result = subprocess.run(
                [str(script)], cwd=root,
                env={**os.environ, "ROFFTAG": "rainfall-025", "HIRES_TAG": "15sec"},
                text=True, capture_output=True, check=False,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertTrue((map_root / "inpdim_rainfall-025.txt").is_file())
            self.assertTrue((map_root / "inpmat_rainfall-025.bin").is_file())
            self.assertEqual((map_root / "inpdim_rainfall-025.txt").read_text().splitlines()[6],
                             "inpmat_rainfall-025.bin")
            self.assertEqual((map_root / "called.args").read_text().splitlines(), [
                "15sec", "inpdim_rainfall-025.txt", "inpmat_rainfall-025.bin"
            ])
            self.assertIn("[STEP 1/1] Generate runoff input matrix", result.stdout)
            self.assertEqual(result.stdout.count("[SUCCESS]"), 1)


if __name__ == "__main__":
    unittest.main()
