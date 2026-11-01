"""Small integration checks for v5 MPI map preprocessing using tracked Mekong data."""
from __future__ import annotations

import csv
from pathlib import Path
import shutil
import struct
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[3]
MAP = ROOT / "map" / "mekong_15min"
CODE = ROOT / "map" / "tools" / "code"
TOOLS = ROOT / "map" / "tools"


@unittest.skipUnless(shutil.which("gfortran"), "gfortran is required")
class MpiMapToolsTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = tempfile.TemporaryDirectory(prefix="cmf-mpi-build-")
        cls.bin = Path(cls.build.name)
        for source in (
            "param_set_bif_basin_mpi.F90",
            "param_set_mpi_region.F90",
            "analysis_check_mpi_bifurcation.F90",
        ):
            subprocess.run(
                [
                    shutil.which("gfortran"),
                    "-O0",
                    "-fcheck=all",
                    "-ffree-line-length-none",
                    str(CODE / source),
                    "-o",
                    str(cls.bin / source.removesuffix(".F90")),
                ],
                check=True,
                capture_output=True,
                text=True,
            )

    @classmethod
    def tearDownClass(cls):
        cls.build.cleanup()

    def make_map(self, root: Path, *, bifprm_text: str | None = None) -> Path:
        root.mkdir(parents=True, exist_ok=True)
        for name in ("params.txt", "basin.bin", "nextxy.bin", "bifprm.txt"):
            shutil.copy2(MAP / name, root / name)
        if bifprm_text is not None:
            (root / "bifprm.txt").write_text(bifprm_text, encoding="ascii")
        (root / "script_params").mkdir(exist_ok=True)
        return root

    def run_mpi(self, map_root: Path, mode: str = "normal", counts=(4, 8, 16)):
        script_dir = map_root / "script_params"
        result = subprocess.run(
            [str(self.bin / "param_set_bif_basin_mpi"), mode],
            cwd=script_dir,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode:
            raise AssertionError(f"MPI aggregation failed: {result.stdout}\n{result.stderr}")
        for count in counts:
            subprocess.run(
                [str(self.bin / "param_set_mpi_region"), str(count)],
                cwd=script_dir,
                check=True,
                capture_output=True,
                text=True,
            )

    def test_sample_maps_are_deterministic_and_diagnostic_counts_reconcile(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-test-") as tmp:
            root = self.make_map(Path(tmp) / "map")
            bifprm_before = (root / "bifprm.txt").read_bytes()
            self.run_mpi(root)
            first = {n: (root / f"mpireg-{n}.bin").read_bytes() for n in (4, 8, 16)}
            self.assertEqual((root / "bifprm.txt").read_bytes(), bifprm_before)
            self.assertTrue((root / ".bifbsn_mpi.tmp").exists())  # s04 removes this after all partitions are made
            for count in (4, 8, 16):
                self.assertEqual((root / f"mpireg-{count}.bin").stat().st_size, 80 * 120 * 4)
            report = subprocess.run(
                [str(self.bin / "analysis_check_mpi_bifurcation"), "mpireg-16.bin", "--csv", "dropped.csv"],
                cwd=root,
                check=True,
                capture_output=True,
                text=True,
            ).stdout
            self.assertIn("Bifurcation pathways: 382", report)
            self.assertIn("Retained by a model-local vector: 355", report)
            self.assertIn("Dropped (both endpoints have a vector, but never on the same rank): 27", report)
            self.assertIn("Invalid/unassigned (an endpoint has no model vector): 0", report)
            self.assertIn("including endpoints in different assigned regions via downstream vector extension: 0", report)
            with (root / "dropped.csv").open(newline="", encoding="ascii") as f:
                rows = list(csv.DictReader(f))
            self.assertEqual(len(rows), 27)
            # A second identical run produces the same region maps byte-for-byte.
            self.run_mpi(root)
            self.assertEqual(first, {n: (root / f"mpireg-{n}.bin").read_bytes() for n in (4, 8, 16)})

    def test_diagnostic_recognizes_model_downstream_vector_extension(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-ghost-") as tmp:
            root = Path(tmp)
            (root / "params.txt").write_text("3\n1\n", encoding="ascii")
            (root / "mpireg-2.bin").write_bytes(struct.pack("<3i", 1, 2, 2))
            # Region 1's cell 1 points downstream to cell 2 in region 2.
            # CALC_1D_SEQ appends cell 2 to region 1's vector because that
            # downstream traversal does not re-check the region label.
            (root / "nextxy.bin").write_bytes(struct.pack("<3i", 2, 3, -1) + struct.pack("<3i", 1, 1, -1))
            (root / "bifprm.txt").write_text(
                "1 1 npath nlev\n1 1 2 1 1.0 10.0 0.5 5.0 10.0 100.0\n",
                encoding="ascii",
            )
            result = subprocess.run(
                [str(self.bin / "analysis_check_mpi_bifurcation"), "mpireg-2.bin"],
                cwd=root,
                check=True,
                capture_output=True,
                text=True,
            ).stdout
            self.assertIn("Retained by a model-local vector: 1", result)
            self.assertIn("different assigned regions via downstream vector extension: 1", result)
            self.assertIn("Dropped (both endpoints have a vector, but never on the same rank): 0", result)

    def test_maxmpi_mode_creates_requested_region_maps(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-max-") as tmp:
            root = self.make_map(Path(tmp) / "map")
            self.run_mpi(root, mode="MaxMPI", counts=(4, 8, 16))
            self.assertEqual((root / ".bifbsn_mpi.tmp").stat().st_size, 80 * 120 * 4)
            for count in (4, 8, 16):
                self.assertEqual((root / f"mpireg-{count}.bin").stat().st_size, 80 * 120 * 4)

    def test_standalone_analysis_wrapper_resolves_map_root_inputs(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-cli-") as tmp:
            repo = Path(tmp) / "repo"
            map_root = repo / "map" / "mekong"
            code_dir = repo / "map" / "tools" / "code"
            script_dir = map_root / "script_analysis"
            code_dir.mkdir(parents=True)
            script_dir.mkdir(parents=True)
            (code_dir / "Makefile").write_text(".PHONY: analysis\nanalysis:\n\t@:\n", encoding="ascii")
            shutil.copy2(self.bin / "analysis_check_mpi_bifurcation", code_dir / "analysis_check_mpi_bifurcation")
            shutil.copy2(TOOLS / "script_analysis" / "a01-check_mpi_bifurcation.sh", script_dir)
            for name in ("params.txt", "nextxy.bin", "bifprm.txt"):
                shutil.copy2(MAP / name, map_root / name)
            (map_root / "mpireg-1.bin").write_bytes(struct.pack("<9600i", *([1] * 9600)))
            result = subprocess.run(
                [str(script_dir / "a01-check_mpi_bifurcation.sh"), "mpireg-1.bin"],
                cwd=tmp,
                check=True,
                capture_output=True,
                text=True,
            ).stdout
            self.assertIn("Bifurcation pathways: 382", result)

    def test_parsed_sort_is_layer_independent_and_tie_deterministic(self):
        fields = []
        for line in (MAP / "bifprm.txt").read_text(encoding="ascii").splitlines()[1:]:
            fields.append(line.split())
        # Convert the sample's five-width rows to a 3-layer input, exercising a
        # different record layout without changing endpoint IDs or elevation.
        rows = [" ".join(row[:7] + row[7:10] + row[12:16]) for row in fields]
        # Add a distinct text row with an identical (ibsn,jbsn,elevation) key;
        # changing latitude supplies a deterministic whole-line tie-break case.
        tie = rows[0].split()
        tie[-4] = "94.126"
        rows.append(" ".join(tie))
        header = f"{len(rows)} 3 npath, nlev"
        shuffled = [header, *reversed(rows)]
        ordered_rows = sorted(
            rows,
            key=lambda row: (int(row.split()[-2]), int(row.split()[-1]), float(row.split()[5]), row),
        )
        ordered = [header, *ordered_rows]
        outputs = []
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-sort-") as tmp:
            for label, contents in (("shuffled", shuffled), ("ordered", ordered)):
                root = self.make_map(Path(tmp) / label / "map", bifprm_text="\n".join(contents) + "\n")
                result = subprocess.run(
                    [str(self.bin / "param_set_bif_basin_mpi"), "normal"],
                    cwd=root / "script_params",
                    check=False,
                    capture_output=True,
                    text=True,
                )
                if result.returncode:
                    raise AssertionError(f"Layer-3 sort case failed: {result.stdout}\n{result.stderr}")
                outputs.append((root / ".bifbsn_mpi.tmp").read_bytes())
            self.assertEqual(outputs[0], outputs[1])

    def test_shell_entry_points_have_valid_syntax(self):
        for script in (
            TOOLS / "script_params" / "s04-mpi_map_setup.sh",
            TOOLS / "script_analysis" / "a01-check_mpi_bifurcation.sh",
        ):
            subprocess.run(["sh", "-n", str(script)], check=True)


if __name__ == "__main__":
    unittest.main()
