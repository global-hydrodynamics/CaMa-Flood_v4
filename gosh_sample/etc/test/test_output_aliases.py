"""Focused output-registry integration checks on the Mekong sample."""

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
                   blank_runoff_units: bool = False,
                   pthflw_long_name: str | None = None) -> Path:
    with REGISTRY.open(newline="", encoding="utf-8") as source:
        reader = csv.DictReader(source)
        rows = list(reader)
        fields = reader.fieldnames or []
    if blank_runoff_units:
        next(row for row in rows if row["name"] == "runoff")["units"] = ""
    if pthflw_long_name is not None:
        next(row for row in rows if row["name"] == "pthflw")["long_name"] = pthflw_long_name
    if duplicate:
        rows.append(dict(next(row for row in rows if row["name"] == "runoff")))
    with destination.open("w", newline="", encoding="utf-8") as output:
        writer = csv.DictWriter(output, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    return destination


def run_alias_case(run_dir: Path, *, netcdf: bool, split: bool, variables: str,
                   registry_path: Path | None = None,
                   omit_registry_settings: bool = False,
                   output_interval: int = 24) -> subprocess.CompletedProcess[str]:
    script = SAMPLE_SCRIPT.read_text(encoding="utf-8")
    script = re.sub(r'^RUNDIR=.*$', f'RUNDIR="{run_dir}"', script, count=1, flags=re.MULTILINE)
    script = re.sub(r'^END_MONTH=.*$', "END_MONTH=1", script, count=1, flags=re.MULTILINE)
    script = re.sub(r'^END_DAY=.*$', "END_DAY=2", script, count=1, flags=re.MULTILINE)
    script = re.sub(r'^IFRQ_OUT=.*$', f'IFRQ_OUT={output_interval}', script, count=1, flags=re.MULTILINE)
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


def read_netcdf_numbers(path: Path, variable: str) -> list[float]:
    result = subprocess.run([NCDUMP, "-p", "9,17", "-v", variable, str(path)], text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
    data_section = result.stdout.split("data:", 1)[1]
    match = re.search(rf"\b{re.escape(variable)}\s*=\s*(.*?);", data_section, re.DOTALL)
    if match is None:
        raise AssertionError(f"ncdump did not show variable data for {variable}")
    values = re.findall(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[EeDd][-+]?\d+)?", match.group(1))
    return [float(value.replace("D", "E").replace("d", "e")) for value in values]


def read_netcdf_values(path: Path, variable: str) -> np.ndarray:
    return array("f", read_netcdf_numbers(path, variable)).tobytes()


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

    @unittest.skipUnless(NC_EXECUTABLE.is_file() and NCDUMP, "build NetCDF model and install ncdump")
    def test_pthflw_netcdf_matches_binary_layout_metadata_and_records(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-output-pthflw-") as temporary:
            root = Path(temporary)
            binary_dir = root / "binary"
            netcdf_dir = root / "netcdf"
            binary_result = run_alias_case(binary_dir, netcdf=False, split=False,
                                           variables="pthflw", output_interval=6)
            self.assertEqual(binary_result.returncode, 0, binary_result.stdout)
            registry = write_registry(root / "pthflw-output-variables.csv",
                                      pthflw_long_name="Bifurcation path flow from test registry")
            netcdf_result = run_alias_case(netcdf_dir, netcdf=True, split=False,
                                           variables="pthflw", registry_path=registry,
                                           output_interval=6)
            self.assertEqual(netcdf_result.returncode, 0, netcdf_result.stdout)

            binary_path = binary_dir / "pthflw2000.pth"
            netcdf_path = netcdf_dir / "o_pthflw2000.nc"
            self.assertTrue(binary_path.is_file())
            self.assertTrue(netcdf_path.is_file())
            header = subprocess.run([NCDUMP, "-s", "-h", str(netcdf_path)], text=True,
                                    stdout=subprocess.PIPE, check=True).stdout
            self.assertRegex(header, r"NPTHOUT = 382 ;")
            self.assertRegex(header, r"NPTHLEV = 5 ;")
            self.assertRegex(header, r"float pthflw\(time, NPTHLEV, NPTHOUT\)")
            self.assertIn('pthflw:long_name = \"Bifurcation path flow from test registry\"', header)
            self.assertIn('pthflw:units = \"m3/s\"', header)
            self.assertIn('pthflw:cell_methods = \"time: mean\"', header)
            self.assertIn('pthflw:_DeflateLevel = 3', header)
            self.assertIn('pthflw:_Shuffle = \"true\"', header)
            self.assertNotIn('pthflw:_FillValue', header)

            with REGISTRY.open(newline="", encoding="utf-8") as source:
                pthflw_metadata = next(row for row in csv.DictReader(source)
                                       if row["name"] == "pthflw")
            self.assertEqual(pthflw_metadata["dimensions"],
                             "Binary: NPTHOUT,NPTHLEV; NetCDF: NPTHOUT,NPTHLEV,time")
            self.assertEqual(pthflw_metadata["status"], "confirmed")
            self.assertEqual(pthflw_metadata["netcdf_dtype"], "float32")

            binary_values = binary_path.read_bytes()
            netcdf_bytes = read_netcdf_values(netcdf_path, "pthflw")
            self.assertEqual(binary_values, netcdf_bytes)
            record_bytes = 4 * 382 * 5
            self.assertEqual(len(binary_values) % record_bytes, 0)
            self.assertGreaterEqual(len(binary_values) // record_bytes, 2)
            time_count = re.search(r"time = UNLIMITED ; // \((\d+) currently\)", header)
            self.assertIsNotNone(time_count, header)
            self.assertGreaterEqual(int(time_count.group(1)), 2)
            timestamps = read_netcdf_numbers(netcdf_path, "time")
            self.assertEqual(len(timestamps), int(time_count.group(1)))
            self.assertTrue(all(later > earlier for earlier, later in zip(timestamps, timestamps[1:])))
            self.assertRegex(header, r"time:units = \"seconds since ")
            self.assertTrue(any(value != 0.0 for value in array("f", binary_values)))


if __name__ == "__main__":
    unittest.main()
