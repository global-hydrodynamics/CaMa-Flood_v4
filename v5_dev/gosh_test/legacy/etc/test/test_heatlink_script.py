"""Exercise the actual heatlink launcher with a fake executable and small path fixtures."""
import os
import re
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

SOURCE = Path(__file__).resolve().parents[1]


def hydrology_groups(text):
    groups = {}
    for name, body in re.findall(r'(?ms)^&(\w+)\s*\n(.*?)^/\s*$', text):
        if name in HYDROLOGY_GROUPS:
            groups[name] = [line.strip() for line in body.splitlines() if line.strip()]
    return groups


HYDROLOGY_GROUPS = ('NRUNVER', 'NDIMTIME', 'NPARAM', 'NSIMTIME', 'NMAP', 'NRESTART', 'NFORCE', 'NOUTPUT')
# Preserve the original example's complete hydrology configuration.
HYDROLOGY_REFERENCE = '''
&NRUNVER
LADPSTP  = .TRUE.
LPTHOUT  = .TRUE.
LDAMOUT  = .FALSE.
LOUTPUT  = .TRUE.
LRESTART = .FALSE.
LHEATLINK = .TRUE.
LWEVAP    = .FALSE.
/

&NDIMTIME
CDIMINFO = './input/map/diminfo_test-1deg.txt'
DT       = 3600
IFRQ_INP = 24
/

&NPARAM
PMANRIV = 0.03D0
PMANFLD = 0.10D0
PDSTMTH = 10000.D0
PCADP   = 0.7
/

&NSIMTIME
SYEAR = 2000
SMON  = 1
SDAY  = 1
SHOUR = 0
EYEAR = 2001
EMON  = 1
EDAY  = 1
EHOUR = 0
/

&NMAP
LMAPCDF = .FALSE.
CNEXTXY = './input/map/nextxy.bin'
CGRAREA = './input/map/ctmare.bin'
CELEVTN = './input/map/elevtn.bin'
CNXTDST = './input/map/nxtdst.bin'
CRIVLEN = './input/map/rivlen.bin'
CFLDHGT = './input/map/fldhgt.bin'
CRIVWTH = './input/map/rivwth_gwdlr.bin'
CRIVHGT = './input/map/rivhgt.bin'
CRIVMAN = './input/map/rivman.bin'
CPTHOUT = './input/map/bifprm.txt'
/

&NRESTART
CRESTSTO = ''
CRESTDIR = './'
CVNREST  = 'restart'
LRESTCDF = .FALSE.
LRESTDBL = .TRUE.
IFRQ_RST = 0
/

&NFORCE
LINPCDF  = .FALSE.
LINTERP  = .TRUE.
CINPMAT  = './input/map/inpmat_test-1deg.bin'
DROFUNIT = 86400000
CROFDIR  = './input/runoff'
CROFPRE  = 'Roff____'
CROFSUF  = '.one'
/

&NOUTPUT
COUTDIR  = './'
CVARSOUT = 'rivout,rivsto,rivdph,rivvel,fldout,fldsto,flddph,fldfrc,fldare,sfcelv,outflw,storge,pthflw,pthout,maxsto,maxflw,maxdph'
COUTTAG  = '2000'
LOUTVEC  = .FALSE.
LOUTCDF  = .FALSE.
NDLEVEL  = 0
IFRQ_OUT = 24
/
'''


class HeatlinkScriptTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="heatlink-script-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.etc = self.root / "gosh/etc"
        self.etc.mkdir(parents=True)
        for name in ("s01-simulation_heatlink.sh", "heat-link.nml", "atm_GSWP3.nml"):
            shutil.copyfile(SOURCE / name, self.etc / name)
        for name in ("map", "runoff", "atm", "mapping", "src"):
            (self.root / name).mkdir()
        self.exe = self.root / "src/MAIN_cmf"
        self.exe.write_text("#!/bin/sh\ntest -r input_cmf.nam || exit 9\necho MODEL_STDOUT\necho MODEL_STDERR >&2\n")
        self.exe.write_text(self.exe.read_text().replace(
            'test -r input_cmf.nam || exit 9',
            'test -r input_cmf.nam || exit 9\n'
            'test -r input/map/diminfo_test-1deg.txt || exit 10\n'
            'test -r input/map/inpmat_test-1deg.bin || exit 11\n'
            'test -r input/runoff/Roff____20000101.one || exit 12'))
        for relative in ('map/diminfo_test-1deg.txt', 'map/inpmat_test-1deg.bin',
                         'runoff/Roff____20000101.one'):
            (self.root / relative).write_text('original hydrology input')
        self.exe.chmod(0o755)
        self.env = os.environ.copy()
        self.env.update(MAP_DIR="map", RUNOFF_DIR="runoff", ATM_DIR="atm",
                        INPMAT_DIR_ATM="mapping", RUN_DIR="run")
        self.env.pop("INPMAT_DIR_LSM", None)
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
        self.assertIn("CINPMAT  = './input/map/inpmat_test-1deg.bin'", nml)
        self.assertIn("CROFDIR  = './input/runoff'", nml)
        self.assertIn("EYEAR = 2001", nml)
        self.assertIn("LINPCDF  = .FALSE.", nml)
        self.assertIn("scale=1.0e-2", nml)
        self.assertIn("RIVICE_ENERGY_UNAPPLIED", nml)
        self.assertIn("MODEL_STDOUT", (self.root / "run/run_stdout.log").read_text())
        self.assertIn("MODEL_STDERR", (self.root / "run/run_stderr.log").read_text())
        self.assertTrue((self.root / 'run/input/map').is_symlink())
        self.assertTrue((self.root / 'run/input/runoff').is_symlink())
        self.assertEqual((self.root / 'run/input/map').resolve(), self.root / 'map')
        self.assertEqual((self.root / 'run/input/runoff').resolve(), self.root / 'runoff')

    def test_directory_defaults(self):
        (self.root / "map/input_mappings/05deg_s-n_0e-360e/mean").mkdir(parents=True)
        del self.env["INPMAT_DIR_ATM"]
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        nml = (self.root / "run/input_cmf.nam").read_text()
        self.assertIn("05deg_s-n_0e-360e/mean/inpmat.bin", nml)
        self.assertIn("CDIMINFO = './input/map/diminfo_test-1deg.txt'", nml)

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

    def test_hydrology_settings_remain_unchanged(self):
        expected = hydrology_groups(HYDROLOGY_REFERENCE)
        template = (self.etc / 'heat-link.nml').read_text()
        self.assertEqual(hydrology_groups(template), expected)
        self.assertNotIn('@', template)
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        generated = (self.root / 'run/input_cmf.nam').read_text()
        self.assertEqual(hydrology_groups(generated), expected)

    def test_hydrology_paths_use_original_links(self):
        for key, directory in (('MAP_DIR', 'map'), ('RUNOFF_DIR', 'runoff')):
            name = directory + " space & pipe| quote' back\\slash"
            (self.root / directory).rename(self.root / name)
            self.env[key] = name
        # The obsolete override must not redirect the discharge input matrix.
        self.env['INPMAT_DIR_LSM'] = 'nonexistent-lsm-directory'
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        for link, key in (('map', 'MAP_DIR'), ('runoff', 'RUNOFF_DIR')):
            actual = self.root / 'run/input' / link
            self.assertTrue(actual.is_symlink())
            self.assertEqual(actual.resolve(), self.root / self.env[key])
        generated = (self.root / 'run/input_cmf.nam').read_text()
        self.assertEqual(hydrology_groups(generated), hydrology_groups(HYDROLOGY_REFERENCE))

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
