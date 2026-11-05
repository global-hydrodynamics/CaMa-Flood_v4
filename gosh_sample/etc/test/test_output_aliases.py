"""Small end-to-end checks for output-name aliases on the Mekong sample."""

from __future__ import annotations

import csv
import re
import shutil
from array import array
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SAMPLE_SCRIPT = ROOT / "gosh_sample" / "mek01-default_run.sh"
REGISTRY = ROOT / "variable_table" / "output_variables.csv"
BIN_EXECUTABLE = ROOT / "src_main" / "MAIN_cmf"
NC_EXECUTABLE = ROOT / "out" / "bin" / "MAIN_cmf_netcdf"
NCDUMP = shutil.which("ncdump")


def reordered_registry(destination: Path) -> Path:
    with REGISTRY.open(newline="", encoding="utf-8") as source:
        reader = csv.DictReader(source)
        rows = list(reader)
        fields = list(reversed(reader.fieldnames or [])) + ["unused_note"]
        for row in rows:
            row["unused_note"] = 'unused, "quoted" field'
            row["internal_variable"] = "not_a_model_array"
            row["internal_dtype"] = "informational_only"
    with destination.open("w", newline="", encoding="utf-8") as output:
        writer = csv.DictWriter(output, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    return destination


def write_registry(destination: Path, *, duplicate: bool = False,
                   blank_runoff_units: bool = False) -> Path:
    with REGISTRY.open(newline="", encoding="utf-8") as source:
        reader = csv.DictReader(source)
        rows = list(reader)
        fields = reader.fieldnames or []
    if blank_runoff_units:
        next(row for row in rows if row["name"] == "runoff")["units"] = ""
    if duplicate:
        rows.append(dict(next(row for row in rows if row["name"] == "runoff")))
    with destination.open("w", newline="", encoding="utf-8") as output:
        writer = csv.DictWriter(output, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    return destination


def run_alias_case(run_dir: Path, *, netcdf: bool, split: bool, variables: str,
                   registry_path: Path | None = None,
                   omit_registry_settings: bool = False) -> subprocess.CompletedProcess[str]:
    script = SAMPLE_SCRIPT.read_text(encoding="utf-8")
    script = re.sub(r'^RUNDIR=.*$', f'RUNDIR="{run_dir}"', script, count=1, flags=re.MULTILINE)
    script = re.sub(r'^END_MONTH=.*$', "END_MONTH=1", script, count=1, flags=re.MULTILINE)
    script = re.sub(r'^END_DAY=.*$', "END_DAY=2", script, count=1, flags=re.MULTILINE)
    script = re.sub(r'^CVARSOUT=.*$', f'CVARSOUT="{variables}"', script, count=1, flags=re.MULTILINE)
    netcdf_logical = ".TRUE." if netcdf else ".FALSE."
    script = re.sub(r'^LOUTCDF=.*$', f'LOUTCDF="{netcdf_logical}"', script,
                    count=1, flags=re.MULTILINE)
    if netcdf:
        script = script.replace(
            'EXEC_CMF="${CMF_BASE}/src_main/MAIN_cmf"',
            'EXEC_CMF="${CMF_BASE}/out/bin/MAIN_cmf_netcdf"',
        )
    script = script.replace(
        'LADPSTP  = ${LADPSTP}        ! TRUE: use adaptive time stepping.',
        'LADPSTP  = ${LADPSTP}        ! TRUE: use adaptive time stepping.\n'
        f'LROSPLIT = {".TRUE." if split else ".FALSE."}     ! Test split runoff option.',
    )
    script = script.replace(
        'CROFDIR  = "${RUNOFF_DIR}"    ! Directory containing runoff input files.',
        'CROFDIR  = "${RUNOFF_DIR}"    ! Directory containing runoff input files.\n'
        'CSUBDIR  = "${RUNOFF_DIR}"    ! Reuse sample data as split-runoff input.\n'
        'CSUBPRE  = "Roff____"          ! Test subsurface input filename prefix.\n'
        'CSUBSUF  = ".one"             ! Test subsurface input filename suffix.',
    )
    if registry_path is not None:
        script = script.replace(
            'COUTVAR_TABLE = ""', f'COUTVAR_TABLE = "{registry_path}"', 1
        )
    if omit_registry_settings:
        script = re.sub(r'^CMF_BASE = .*\nCOUTVAR_TABLE = .*\n', '', script,
                        count=1, flags=re.MULTILINE)
    test_script = run_dir.parent / f"{run_dir.name}.sh"
    test_script.write_text(script, encoding="utf-8")
    result = subprocess.run(
        ["sh", str(test_script)], cwd=ROOT / "gosh_sample",
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False,
    )
    for log_name in ("run.stdout.log", "log_CaMa.txt"):
        log_path = run_dir / log_name
        if log_path.is_file():
            result.stdout += log_path.read_text(encoding="utf-8", errors="replace")
    return result


def read_netcdf_values(path: Path, variable: str) -> np.ndarray:
    result = subprocess.run([NCDUMP, "-v", variable, str(path)], text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
    match = re.search(rf"\b{re.escape(variable)}\s*=\s*(.*?);", result.stdout, re.DOTALL)
    if match is None:
        raise AssertionError(f"ncdump did not show variable data for {variable}")
    values = re.findall(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[EeDd][-+]?\d+)?", match.group(1))
    return array("f", (float(value.replace("D", "E").replace("d", "e"))
                        for value in values)).tobytes()


@unittest.skipUnless(BIN_EXECUTABLE.is_file(), "build src_main/MAIN_cmf first")
class OutputAliasIntegrationTests(unittest.TestCase):
    def test_existing_namelist_without_registry_settings_still_runs(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-alias-old-nml-") as temporary:
            run_dir = Path(temporary) / "run"
            result = run_alias_case(run_dir, netcdf=False, split=False, variables="outflw",
                                    omit_registry_settings=True)
            self.assertEqual(result.returncode, 0, result.stdout)
            self.assertTrue((run_dir / "outflw2000.bin").is_file())

    def test_binary_alias_pairs_have_identical_values_and_requested_names(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-alias-bin-") as temporary:
            run_dir = Path(temporary) / "run"
            result = run_alias_case(run_dir, netcdf=False, split=True,
                                    variables="runoff,rofsfc,runoffsub,rofsub")
            self.assertEqual(result.returncode, 0, result.stdout)
            for canonical, alias in (("runoff", "rofsfc"), ("runoffsub", "rofsub")):
                self.assertTrue((run_dir / f"{canonical}2000.ctl").is_file())
                self.assertTrue((run_dir / f"{alias}2000.ctl").is_file())
                canonical_file = run_dir / f"{canonical}2000.bin"
                alias_file = run_dir / f"{alias}2000.bin"
                self.assertTrue(canonical_file.is_file())
                self.assertTrue(alias_file.is_file())
                self.assertEqual(canonical_file.read_bytes(), alias_file.read_bytes())

    def test_double_precision_storage_uses_the_same_registry_writer(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-double-") as temporary:
            run_dir = Path(temporary) / "run"
            result = run_alias_case(run_dir, netcdf=False, split=False,
                                    variables="rivsto,fldsto")
            self.assertEqual(result.returncode, 0, result.stdout)
            for name in ("rivsto", "fldsto"):
                output = run_dir / f"{name}2000.bin"
                self.assertTrue(output.is_file())
                self.assertEqual(output.stat().st_size, 80 * 120 * 4)

    @unittest.skipUnless(NC_EXECUTABLE.is_file() and NCDUMP, "build NetCDF model and install ncdump")
    def test_netcdf_alias_pairs_inherit_metadata_and_handle_reordered_csv(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-alias-nc-") as temporary:
            root = Path(temporary)
            registry = reordered_registry(root / "output_variables.csv")
            run_dir = root / "run"
            result = run_alias_case(run_dir, netcdf=True, split=True,
                                    variables="runoff,rofsfc,runoffsub,rofsub",
                                    registry_path=registry)
            self.assertEqual(result.returncode, 0, result.stdout)
            for canonical, alias in (("runoff", "rofsfc"), ("runoffsub", "rofsub")):
                canonical_file = run_dir / f"o_{canonical}2000.nc"
                alias_file = run_dir / f"o_{alias}2000.nc"
                self.assertTrue(canonical_file.is_file())
                self.assertTrue(alias_file.is_file())
                self.assertEqual(read_netcdf_values(canonical_file, canonical),
                                 read_netcdf_values(alias_file, alias))
                header = subprocess.run([NCDUMP, "-h", str(alias_file)], text=True,
                                        stdout=subprocess.PIPE, check=True).stdout
                self.assertIn(f'{alias}:long_name = "', header)
                self.assertIn(f'{alias}:units = "m3/s"', header)
                self.assertIn(f'{alias}:cell_methods = "time: mean"', header)
                self.assertIn(f'{alias}:_FillValue = 1.e+20f', header)

    def test_subsurface_names_fail_consistently_when_split_runoff_is_disabled(self) -> None:
        for name in ("runoffsub", "rofsub"):
            with self.subTest(name=name), tempfile.TemporaryDirectory(prefix="cmf-output-alias-off-") as temporary:
                run_dir = Path(temporary) / "run"
                result = run_alias_case(run_dir, netcdf=False, split=False,
                                        variables=f"outflw,{name}")
                self.assertNotEqual(result.returncode, 0, result.stdout)
                self.assertIn(f"{name} requires LROSPLIT=.TRUE.", result.stdout)
                self.assertFalse((run_dir / f"{name}2000.bin").exists())
                self.assertFalse((run_dir / "outflw2000.bin").exists())

    def test_missing_registry_reports_the_configured_path(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-alias-missing-registry-") as temporary:
            root = Path(temporary)
            missing_registry = root / "missing-output-variables.csv"
            run_dir = root / "run"
            result = run_alias_case(run_dir, netcdf=False, split=True,
                                    variables="runoff", registry_path=missing_registry)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn(f"Cannot open output registry: {missing_registry}", result.stdout)
            self.assertFalse((run_dir / "runoff2000.bin").exists())

    def test_duplicate_registry_names_are_rejected_before_files_open(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-duplicate-registry-") as temporary:
            root = Path(temporary)
            registry = write_registry(root / "duplicate.csv", duplicate=True)
            run_dir = root / "run"
            result = run_alias_case(run_dir, netcdf=False, split=True,
                                    variables="runoff", registry_path=registry)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("Duplicate output registry definition: runoff", result.stdout)
            self.assertFalse((run_dir / "runoff2000.bin").exists())

    def test_missing_required_registry_metadata_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-metadata-") as temporary:
            root = Path(temporary)
            registry = write_registry(root / "missing-units.csv", blank_runoff_units=True)
            run_dir = root / "run"
            result = run_alias_case(run_dir, netcdf=False, split=True,
                                    variables="runoff", registry_path=registry)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("Required output metadata is blank for runoff", result.stdout)
            self.assertFalse((run_dir / "runoff2000.bin").exists())

    @unittest.skipUnless(NC_EXECUTABLE.is_file(), "build the NetCDF model first")
    def test_unknown_output_name_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-unknown-") as temporary:
            run_dir = Path(temporary) / "run"
            result = run_alias_case(run_dir, netcdf=True, split=False,
                                    variables="not_registered")
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("Unknown output name: not_registered", result.stdout)
            self.assertFalse((run_dir / "o_not_registered2000.nc").exists())

    @unittest.skipUnless(NC_EXECUTABLE.is_file(), "build the NetCDF model first")
    def test_unverified_netcdf_pthflw_layout_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-pthflw-nc-") as temporary:
            run_dir = Path(temporary) / "run"
            result = run_alias_case(run_dir, netcdf=True, split=False, variables="pthflw")
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("NetCDF pthflw layout is not verified", result.stdout)
            self.assertFalse((run_dir / "o_pthflw2000.nc").exists())


if __name__ == "__main__":
    unittest.main()
