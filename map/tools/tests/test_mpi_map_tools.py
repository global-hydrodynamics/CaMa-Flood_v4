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
        subprocess.run(
            [
                shutil.which("gfortran"), "-O0", "-fcheck=all", "-ffree-line-length-none",
                "-J", str(cls.bin), "-c", str(CODE / "mod_map_color.F90"),
                "-o", str(cls.bin / "mod_map_color.o"),
            ],
            check=True,
            capture_output=True,
            text=True,
        )
        for source in (
            "param_mpi_set_basin.F90",
            "param_mpi_set_region.F90",
            "analysis_check_mpi_bifurcation.F90",
            "analysis_mpi_region_color.F90",
        ):
            command = [
                shutil.which("gfortran"), "-O0", "-fcheck=all", "-ffree-line-length-none",
            ]
            if source in {"param_mpi_set_basin.F90", "analysis_mpi_region_color.F90"}:
                command.extend(["-I", str(cls.bin), str(cls.bin / "mod_map_color.o")])
            command.extend([str(CODE / source), "-o", str(cls.bin / source.removesuffix(".F90"))])
            subprocess.run(command, check=True, capture_output=True, text=True)
        subprocess.run(
            [
                shutil.which("gfortran"),
                "-O0",
                "-fcheck=all",
                "-ffree-line-length-none",
                str(ROOT / "map" / "src" / "src_param" / "set_bif_basin_mpi.F90"),
                "-o",
                str(cls.bin / "legacy_set_bif_basin_mpi"),
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
        self.run_basin_aggregation(map_root, mode)
        script_dir = map_root / "script_params"
        for count in counts:
            subprocess.run(
                [str(self.bin / "param_mpi_set_region"), str(count)],
                cwd=script_dir,
                check=True,
                capture_output=True,
                text=True,
            )

    def run_basin_aggregation(self, map_root: Path, mode: str = "normal", check_map: bool = False):
        script_dir = map_root / "script_params"
        args = [str(self.bin / "param_mpi_set_basin"), mode]
        if check_map:
            args.append("check_map")
        result = subprocess.run(
            args,
            cwd=script_dir,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode:
            raise AssertionError(f"MPI aggregation failed: {result.stdout}\n{result.stderr}")
        return result

    def test_optional_diagnostic_matches_legacy_and_preserves_default_outputs(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-check-map-") as tmp:
            root = Path(tmp)
            v5_map = self.make_map(root / "v5" / "map")
            legacy_map = self.make_map(root / "legacy" / "map")
            diagnostic = v5_map / "check_bif_bsnmod_mpi.bin"

            self.run_basin_aggregation(v5_map)
            self.assertTrue((v5_map / "bifcol_mpi.bin").is_file())
            self.assertFalse(list(v5_map.glob("mpireg-*.bin")))
            self.assertFalse(diagnostic.exists())
            previous = b"existing diagnostic must remain untouched"
            diagnostic.write_bytes(previous)
            self.run_basin_aggregation(v5_map)
            self.assertEqual(diagnostic.read_bytes(), previous)
            diagnostic.unlink()

            result = self.run_basin_aggregation(v5_map, check_map=True)
            self.assertIn("Created optional MPI basin diagnostic", result.stdout)
            nx, ny = (
                int(line.split()[0])
                for line in (v5_map / "params.txt").read_text().splitlines()[:2]
            )
            record_size = nx * ny * 4
            basin_ids = struct.unpack(f"<{nx * ny}i", (v5_map / "bifbsn_mpi.tmp").read_bytes())
            basin_colors = struct.unpack(f"<{nx * ny}i", (v5_map / "bifcol_mpi.bin").read_bytes())
            self.assertEqual(len(basin_colors), nx * ny)
            self.assertTrue(set(basin_colors) <= {-9999, *range(1, 11)})
            for basin_id in set(basin_ids) - {-9999, 0}:
                self.assertEqual({color for basin, color in zip(basin_ids, basin_colors) if basin == basin_id}.__len__(), 1)
            data = diagnostic.read_bytes()
            self.assertEqual(len(data), record_size * 2)
            bifmod_record = data[:record_size]
            bpoint_record = data[record_size:]
            self.assertEqual(len(bifmod_record), record_size)
            self.assertEqual(len(bpoint_record), record_size)
            self.assertTrue(set(struct.unpack(f"<{nx * ny}i", bifmod_record)) <= {-9999, 0, 1, 2})
            self.assertTrue(set(struct.unpack(f"<{nx * ny}i", bpoint_record)) <= {-9999, 1, 2, 3})

            legacy = subprocess.run(
                [str(self.bin / "legacy_set_bif_basin_mpi"), "normal"],
                cwd=legacy_map / "script_params",
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(legacy.returncode, 0, legacy.stdout + legacy.stderr)
            self.assertEqual(bifmod_record, (legacy_map / "bifmod_mpi.bin").read_bytes())
            self.assertEqual(bpoint_record, (legacy_map / "bpoint_mpi.bin").read_bytes())
            self.assertFalse((v5_map / "bifmod_mpi.bin").exists())
            self.assertFalse((v5_map / "bpoint_mpi.bin").exists())

            max_v5 = self.make_map(root / "max_v5" / "map")
            max_legacy_map = self.make_map(root / "max_legacy" / "map")
            self.run_basin_aggregation(max_v5, mode="MaxMPI", check_map=True)
            max_legacy = subprocess.run(
                [str(self.bin / "legacy_set_bif_basin_mpi"), "MaxMPI"],
                cwd=max_legacy_map / "script_params",
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(max_legacy.returncode, 0, max_legacy.stdout + max_legacy.stderr)
            max_data = (max_v5 / diagnostic.name).read_bytes()
            max_record_size = nx * ny * 4
            self.assertEqual(max_data[:max_record_size], (max_legacy_map / "bifmod_mpi.bin").read_bytes())
            self.assertEqual(max_data[max_record_size:], (max_legacy_map / "bpoint_mpi.bin").read_bytes())

            ctl_map_dir = root / "ctl_repository" / "map"
            ctl_code = ctl_map_dir / "tools" / "code"
            ctl_target = ctl_map_dir / "mekong_15min"
            ctl_script_dir = ctl_target / "script_params"
            ctl_code.mkdir(parents=True)
            ctl_script_dir.mkdir(parents=True)
            shutil.copy2(CODE / "generate_ctl.py", ctl_code / "generate_ctl.py")
            shutil.copy2(ROOT / "map" / "map_variables.csv", ctl_map_dir / "map_variables.csv")
            shutil.copy2(MAP / "params.txt", ctl_target / "params.txt")
            shutil.copy2(diagnostic, ctl_target / diagnostic.name)
            shutil.copy2(v5_map / "bifcol_mpi.bin", ctl_target / "bifcol_mpi.bin")
            shutil.copy2(TOOLS / "script_params" / "s99-write_ctl.sh", ctl_script_dir)
            ctl_result = subprocess.run(
                ["sh", str(ctl_script_dir / "s99-write_ctl.sh")],
                cwd=root,
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(ctl_result.returncode, 0, ctl_result.stdout + ctl_result.stderr)
            self.assertTrue(
                (ctl_target / "check_bif_bsnmod_mpi.ctl").is_file(),
                ctl_result.stdout + ctl_result.stderr,
            )
            ctl = (ctl_target / "check_bif_bsnmod_mpi.ctl").read_text(encoding="utf-8")
            self.assertIn("vars 2", ctl)
            self.assertLess(ctl.index("bifmod_mpi 1"), ctl.index("bpoint_mpi 1"))

            no_diagnostic = ctl_map_dir / "without_diagnostic"
            no_diagnostic_script_dir = no_diagnostic / "script_params"
            no_diagnostic_script_dir.mkdir(parents=True)
            shutil.copy2(MAP / "params.txt", no_diagnostic / "params.txt")
            shutil.copy2(TOOLS / "script_params" / "s99-write_ctl.sh", no_diagnostic_script_dir)
            absent_result = subprocess.run(
                ["sh", str(no_diagnostic_script_dir / "s99-write_ctl.sh")],
                cwd=root,
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(absent_result.returncode, 0, absent_result.stdout + absent_result.stderr)
            self.assertFalse((no_diagnostic / "check_bif_bsnmod_mpi.ctl").exists())

    def test_sample_maps_are_deterministic_and_diagnostic_counts_reconcile(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-test-") as tmp:
            root = self.make_map(Path(tmp) / "map")
            bifprm_before = (root / "bifprm.txt").read_bytes()
            self.run_mpi(root)
            first = {n: (root / f"mpireg-{n}.bin").read_bytes() for n in (4, 8, 16)}
            self.assertEqual((root / "bifprm.txt").read_bytes(), bifprm_before)
            self.assertTrue((root / "bifbsn_mpi.tmp").exists())  # s04 removes this after all partitions are made
            for count in (4, 8, 16):
                self.assertEqual((root / f"mpireg-{count}.bin").stat().st_size, 80 * 120 * 4)
            self.assertFalse((root / "mpireg.bin").exists())
            self.assertFalse(list(root.glob("bsncol_mpi-*.bin")))
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

    def test_standalone_region_color_can_process_any_numbered_partition(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-color-") as tmp:
            root = self.make_map(Path(tmp) / "map")
            self.run_mpi(root)
            self.assertFalse(list(root.glob("bsncol_mpi-*.bin")))

            nx, ny = (
                int(line.split()[0])
                for line in (root / "params.txt").read_text().splitlines()[:2]
            )
            for count in (8, 16):
                result = subprocess.run(
                    [str(self.bin / "analysis_mpi_region_color"), f"mpireg-{count}.bin"],
                    cwd=root,
                    check=True,
                    capture_output=True,
                    text=True,
                )
                self.assertIn(f"bsncol_mpi-{count}.bin", result.stdout)
                color_path = root / f"bsncol_mpi-{count}.bin"
                self.assertEqual(color_path.stat().st_size, nx * ny * 4)
                regions = struct.unpack(f"<{nx * ny}i", (root / f"mpireg-{count}.bin").read_bytes())
                colors = struct.unpack(f"<{nx * ny}i", color_path.read_bytes())
                self.assertEqual(len(colors), nx * ny)
                self.assertTrue(set(colors) <= {-9999, *range(1, 11)})
                for region in range(1, count + 1):
                    assigned = {color for region_id, color in zip(regions, colors) if region_id == region}
                    self.assertEqual(len(assigned), 1)
            self.assertFalse((root / "mpireg.bin").exists())

            # The existing registry pattern should generate one CTL per concrete N.
            with (ROOT / "map" / "map_variables.csv").open(newline="", encoding="utf-8") as stream:
                reader = csv.DictReader(stream)
                fields = reader.fieldnames
                color_rows = [row for row in reader if row["binary_filename"] in {"bsncol_mpi-*.bin", "bifcol_mpi.bin"}]
            registry = Path(tmp) / "color_registry.csv"
            with registry.open("w", newline="", encoding="utf-8") as stream:
                writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
                writer.writeheader()
                writer.writerows(color_rows)
            ctl = subprocess.run(
                ["python3", str(CODE / "generate_ctl.py"), "--map-dir", str(root), "--metadata-csv", str(registry)],
                check=True,
                capture_output=True,
                text=True,
            )
            self.assertIn("bsncol_mpi-8.ctl", ctl.stdout)
            ctl_text = (root / "bsncol_mpi-8.ctl").read_text(encoding="utf-8")
            self.assertIn("dset ^bsncol_mpi-8.bin", ctl_text)
            self.assertIn("vars 1", ctl_text)
            self.assertIn("bsncol 1", ctl_text)
            self.assertTrue((root / "bifcol_mpi.ctl").is_file())
            self.assertIn("bifcol_mpi 1", (root / "bifcol_mpi.ctl").read_text(encoding="utf-8"))

    def test_maxmpi_mode_creates_requested_region_maps(self):
        with tempfile.TemporaryDirectory(prefix="cmf-mpi-max-") as tmp:
            root = self.make_map(Path(tmp) / "map")
            self.run_mpi(root, mode="MaxMPI", counts=(4, 8, 16))
            self.assertEqual((root / "bifbsn_mpi.tmp").stat().st_size, 80 * 120 * 4)
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
                    [str(self.bin / "param_mpi_set_basin"), "normal"],
                    cwd=root / "script_params",
                    check=False,
                    capture_output=True,
                    text=True,
                )
                if result.returncode:
                    raise AssertionError(f"Layer-3 sort case failed: {result.stdout}\n{result.stderr}")
                outputs.append((root / "bifbsn_mpi.tmp").read_bytes())
            self.assertEqual(outputs[0], outputs[1])

    def test_shell_entry_points_have_valid_syntax(self):
        for script in (
            TOOLS / "script_params" / "s04-mpi_map_setup.sh",
            TOOLS / "script_analysis" / "a01-check_mpi_bifurcation.sh",
        ):
            subprocess.run(["sh", "-n", str(script)], check=True)


if __name__ == "__main__":
    unittest.main()
