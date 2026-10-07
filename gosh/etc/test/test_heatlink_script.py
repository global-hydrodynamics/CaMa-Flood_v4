"""Exercise the actual heatlink launcher with a fake executable and empty data dirs."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

SOURCE = Path(__file__).resolve().parents[1]


class HeatlinkScriptTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="heatlink-script-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.etc = self.root / "gosh/etc"
        self.etc.mkdir(parents=True)
        for name in ("s01-simulation_heatlink.sh", "heat-link.nml", "atm_GSWP3.nml"):
            shutil.copyfile(SOURCE / name, self.etc / name)
        for name in ("map", "runoff", "atm", "mapping", "lsm", "src"):
            (self.root / name).mkdir()
        self.exe = self.root / "src/MAIN_cmf"
        self.exe.write_text("#!/bin/sh\ntest -r input_cmf.nam || exit 9\necho MODEL_STDOUT\necho MODEL_STDERR >&2\n")
        self.exe.chmod(0o755)
        self.env = os.environ.copy()
        self.env.update(MAP_DIR="map", RUNOFF_DIR="runoff", ATM_DIR="atm",
                        INPMAT_DIR_ATM="mapping", INPMAT_DIR_LSM="lsm", RUN_DIR="run")
        self.env.pop("NML_COMMON", None)
        self.env.pop("NML_ATM", None)

    def run_script(self):
        return subprocess.run(["bash", str(self.etc / "s01-simulation_heatlink.sh")],
                              cwd=self.root, env=self.env, capture_output=True, text=True)

    def test_generation_and_execution(self):
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        nml = (self.root / "run/input_cmf.nam").read_text()
        self.assertEqual(nml.count("&input_item "), 7)
        self.assertEqual(nml.count("&input_nc "), 7)
        self.assertNotIn("@", nml)
        self.assertNotIn("&intrp_map", nml)
        self.assertIn(f"diminfo_file='{self.root}/mapping/diminfo.txt'", nml)
        self.assertIn(f"CINPMAT  = '{self.root}/lsm/inpmat_test-1deg.bin'", nml)
        self.assertIn(f"CROFDIR  = '{self.root}/runoff'", nml)
        self.assertIn("EYEAR = 2001", nml)
        self.assertIn("LINPCDF  = .FALSE.", nml)
        self.assertIn("scale=1.0e-2", nml)
        self.assertIn("RIVICE_ENERGY_UNAPPLIED", nml)
        self.assertIn("MODEL_STDOUT", (self.root / "run/run_stdout.log").read_text())
        self.assertIn("MODEL_STDERR", (self.root / "run/run_stderr.log").read_text())
        self.assertFalse((self.root / "run/input").exists())

    def test_directory_defaults(self):
        (self.root / "map/input_mappings/05deg_s-n_0e-360e/mean").mkdir(parents=True)
        del self.env["INPMAT_DIR_ATM"]
        del self.env["INPMAT_DIR_LSM"]
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        nml = (self.root / "run/input_cmf.nam").read_text()
        self.assertIn("05deg_s-n_0e-360e/mean/inpmat.bin", nml)
        self.assertIn(f"CDIMINFO = '{self.root}/map/diminfo_test-1deg.txt'", nml)

    def test_path_escaping_and_template_filenames(self):
        name = "mapping space & pipe| quote' back\\slash"
        (self.root / name).mkdir()
        self.env["INPMAT_DIR_ATM"] = name
        custom = self.root / "custom.nml"
        text = (self.etc / "atm_GSWP3.nml").read_text()
        custom.write_text(text.replace("/diminfo.txt", "/experiment_a.info").replace("/inpmat.bin", "/experiment_b.bin"))
        self.env["NML_ATM"] = "custom.nml"
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        nml = (self.root / "run/input_cmf.nam").read_text()
        escaped = str(self.root / name).replace("'", "''")
        self.assertIn(f"diminfo_file='{escaped}/experiment_a.info'", nml)
        self.assertIn(f"inpmat_file='{escaped}/experiment_b.bin'", nml)

    def test_nonempty_run_is_preserved(self):
        (self.root / "run").mkdir()
        sentinel = self.root / "run/.existing-result"
        sentinel.write_text("keep")
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not empty", result.stderr)
        self.assertEqual(sentinel.read_text(), "keep")
        self.assertFalse((self.root / "run/input_cmf.nam").exists())

    def test_missing_executable(self):
        self.exe.unlink()
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Executable not found", result.stderr)
        self.assertFalse((self.root / "run").exists())

    def test_model_failure_is_propagated(self):
        self.exe.write_text("#!/bin/sh\necho MODEL_FAILURE >&2\nexit 7\n")
        result = self.run_script()
        self.assertEqual(result.returncode, 7)
        self.assertNotIn("Completed successfully", result.stdout)
        self.assertIn("MODEL_FAILURE", (self.root / "run/run_stderr.log").read_text())

    def test_newline_path_rejected(self):
        (self.root / "bad\npath").mkdir()
        self.env["INPMAT_DIR_ATM"] = "bad\npath"
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("must not contain newlines", result.stderr)
        self.assertFalse((self.root / "run").exists())


if __name__ == "__main__":
    unittest.main()
