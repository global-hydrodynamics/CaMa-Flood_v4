"""Exercise workflow STEP/SUCCESS/ERROR banners with small mock executables."""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SCRIPT_DIR = ROOT / "map" / "tools" / "script_params"


class WorkflowMessageTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="cmf-workflow-messages-")
        self.root = Path(self.temporary.name)
        self.map_root = self.root / "map" / "sample"
        self.script_dir = self.map_root / "script_params"
        self.code_dir = self.root / "map" / "tools" / "code"
        self.script_dir.mkdir(parents=True)
        self.code_dir.mkdir(parents=True)
        (self.map_root / "1min").mkdir()
        (self.map_root / "1min" / "location.txt").write_text("fixture\n", encoding="ascii")
        self.log = self.root / "calls.txt"

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def install(self, script_name: str, executables: tuple[str, ...]) -> Path:
        script = self.script_dir / script_name
        shutil.copyfile(SCRIPT_DIR / script_name, script)
        script.chmod(0o755)
        for name in executables:
            path = self.code_dir / name
            path.write_text(
                "#!/bin/sh\n"
                "NAME=${0##*/}\n"
                "printf '%s %s\\n' \"$NAME\" \"$*\" >> \"$MOCK_LOG\"\n"
                "if [ \"${FAIL_MATCH:-}\" = \"$NAME\" ] || [ \"${FAIL_MATCH:-}\" = \"$NAME:${1:-}\" ]; then\n"
                "    exit \"${FAIL_STATUS:-23}\"\n"
                "fi\n"
                "case \"$NAME\" in\n"
                "    param_mpi_set_basin) : > ../bifbsn_mpi.tmp ; : > ../bifcol_mpi.bin ;;\n"
                "    param_mpi_set_region) : > ../mpireg-$1.bin ;;\n"
                "    analysis_mpi_region_color) : > bsncol_mpi-${1#mpireg-} ;;\n"
                "esac\n",
                encoding="ascii",
            )
            path.chmod(0o755)
        return script

    def run_script(
        self, script: Path, fail_match: str | None = None, cwd: Path | None = None,
        args: tuple[str, ...] = (),
    ) -> subprocess.CompletedProcess[str]:
        env = {**os.environ, "MOCK_LOG": str(self.log)}
        if fail_match:
            env["FAIL_MATCH"] = fail_match
            env["FAIL_STATUS"] = "23"
        return subprocess.run(
            [str(script), *args], cwd=cwd or self.root, env=env, text=True,
            capture_output=True, check=False,
        )

    def calls(self) -> list[str]:
        return self.log.read_text(encoding="ascii").splitlines() if self.log.exists() else []

    def assert_success(self, result: subprocess.CompletedProcess[str], script_name: str) -> None:
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout.count("[SUCCESS]"), 1)
        self.assertIn(f"[SUCCESS] {script_name}", result.stdout)
        self.assertNotIn("[ERROR]", result.stdout + result.stderr)

    def assert_failure(
        self, result: subprocess.CompletedProcess[str], script_name: str,
        step: str, expected_status: int = 23,
    ) -> None:
        self.assertEqual(result.returncode, expected_status, result.stdout + result.stderr)
        self.assertEqual(result.stdout.count("[SUCCESS]"), 0)
        self.assertIn(f"!!! ERROR: {script_name} FAILED !!!", result.stderr)
        self.assertIn(f"!!! Step: {step}", result.stderr)
        self.assertIn(f"!!! Exit status: {expected_status}", result.stderr)

    def test_s01_success_and_failures_stop_at_first_middle_and_final_program(self) -> None:
        executables = ("param_channel_calc_outclm", "param_channel_calc_rivwth", "param_channel_set_merit_width")
        script = self.install("s01-channel_params.sh", executables)
        result = self.run_script(script)
        self.assert_success(result, script.name)
        self.assertEqual([line.split()[0] for line in self.calls()], list(executables))
        self.assertEqual(result.stdout.count("[STEP "), 3)

        cases = (
            ("param_channel_calc_outclm", "Calculate mean river discharge from annual runoff and the input matrix", 1),
            ("param_channel_calc_rivwth", "Estimate channel width, depth, and roughness", 2),
            ("param_channel_set_merit_width", "Merge power-law channel widths with MERIT Hydro widths", 3),
        )
        for executable, step, expected_calls in cases:
            with self.subTest(executable=executable):
                self.log.unlink(missing_ok=True)
                result = self.run_script(script, executable)
                self.assert_failure(result, script.name, step)
                self.assertEqual(len(self.calls()), expected_calls)

    def test_s02_success_and_second_program_failure(self) -> None:
        executables = ("param_bif_set_params", "param_bif_set_basin")
        script = self.install("s02-bifurcation.sh", executables)
        result = self.run_script(script)
        self.assert_success(result, script.name)
        self.assertEqual([line.split()[0] for line in self.calls()], list(executables))
        self.log.unlink()

        result = self.run_script(script, "param_bif_set_basin:")
        self.assert_failure(result, script.name, "Merge basins connected by bifurcation pathways")
        self.assertEqual([line.split()[0] for line in self.calls()], list(executables))

    def test_s03_success_and_program_failure(self) -> None:
        script = self.install("s03-generate_inpmat.sh", ("param_inpmat_generate",))
        result = self.run_script(script)
        self.assert_success(result, script.name)
        self.assertIn("[STEP 1/1]", result.stdout)
        self.log.unlink()

        result = self.run_script(script, "param_inpmat_generate:1min")
        self.assert_failure(result, script.name, "Map river cells to the configured runoff grid")
        self.assertEqual(len(self.calls()), 1)

    def test_regional_script_success_and_bifurcation_failure(self) -> None:
        regional_dir = self.map_root / "script_region"
        regional_dir.mkdir()
        global_map = self.root / "map" / "glb_15min"
        (global_map / "1min").mkdir(parents=True)
        (global_map / "1min" / "location.txt").write_text("fixture\n", encoding="ascii")
        script = regional_dir / "s01-regional_map.sh"
        shutil.copyfile(ROOT / "map" / "tools" / "script_region" / script.name, script)
        script.chmod(0o755)
        executables = (
            "region_cut_domain", "region_cut_bifway", "region_set_map",
            "region_combine_hires", "region_default_inpmat",
        )
        for name in executables:
            path = self.code_dir / name
            path.write_text(
                "#!/bin/sh\n"
                "NAME=${0##*/}\n"
                "printf '%s %s\\n' \"$NAME\" \"$*\" >> \"$MOCK_LOG\"\n"
                "if [ \"${FAIL_MATCH:-}\" = \"$NAME\" ]; then exit \"${FAIL_STATUS:-23}\"; fi\n",
                encoding="ascii",
            )
            path.chmod(0o755)

        result = self.run_script(script, cwd=regional_dir)
        self.assert_success(result, script.name)
        self.assertEqual([line.split()[0] for line in self.calls()], list(executables))
        self.assertEqual(result.stdout.count("[STEP "), 6)
        self.log.unlink()

        result = self.run_script(script, "region_cut_bifway", cwd=regional_dir)
        self.assert_failure(
            result, script.name, "Keep bifurcation pathways inside the regional domain",
        )
        self.assertEqual(
            [line.split()[0] for line in self.calls()],
            ["region_cut_domain", "region_cut_bifway"],
        )

    def test_s04_success_cleans_visible_intermediate_and_reports_connectivity_failure(self) -> None:
        executables = (
            "param_mpi_set_basin", "param_mpi_set_region", "analysis_check_mpi_bifurcation",
        )
        script = self.install("s04-mpi_map_setup.sh", executables)
        result = self.run_script(script)
        self.assert_success(result, script.name)
        self.assertIn("[STEP 3/3]", result.stdout)
        calls = [line.split() for line in self.calls()]
        self.assertEqual([call[0] for call in calls], [
            "param_mpi_set_basin", "param_mpi_set_region", "param_mpi_set_region",
            "param_mpi_set_region", "analysis_check_mpi_bifurcation",
        ])
        self.assertEqual(calls[1:4], [
            ["param_mpi_set_region", "4"],
            ["param_mpi_set_region", "8"],
            ["param_mpi_set_region", "16"],
        ])
        self.assertEqual(calls[4], ["analysis_check_mpi_bifurcation", "mpireg-16.bin"])
        self.assertFalse((self.map_root / "mpireg.bin").exists())
        self.assertTrue((self.map_root / "bifcol_mpi.bin").exists())
        self.assertFalse(list(self.map_root.glob("bsncol_mpi-*.bin")))
        self.assertFalse((self.map_root / "bifbsn_mpi.tmp").exists())
        self.log.unlink()

        result = self.run_script(script, "analysis_check_mpi_bifurcation:mpireg-16.bin")
        self.assert_failure(
            result, script.name,
            "Analyze bifurcation connectivity for mpireg-16.bin",
        )
        self.assertFalse((self.map_root / "bifbsn_mpi.tmp").exists())
        self.assertEqual(self.calls()[-1].split()[0], "analysis_check_mpi_bifurcation")

    def test_s04_partition_loop_failure_stops_later_counts_and_cleans_intermediate(self) -> None:
        script = self.install("s04-mpi_map_setup.sh", (
            "param_mpi_set_basin", "param_mpi_set_region", "analysis_check_mpi_bifurcation",
        ))
        result = self.run_script(script, "param_mpi_set_region:8")
        self.assert_failure(result, script.name, "Generate mpireg-8.bin")
        self.assertEqual(self.calls()[-1].split(), ["param_mpi_set_region", "8"])
        self.assertNotIn("param_mpi_set_region 16", self.calls())
        self.assertNotIn("analysis_check_mpi_bifurcation", "\n".join(self.calls()))
        self.assertFalse((self.map_root / "bifbsn_mpi.tmp").exists())

    def test_s04_forwards_check_map_only_to_basin_aggregation(self) -> None:
        script = self.install("s04-mpi_map_setup.sh", (
            "param_mpi_set_basin", "param_mpi_set_region", "analysis_check_mpi_bifurcation",
        ))
        result = self.run_script(script, args=("check_map",))
        self.assert_success(result, script.name)
        calls = self.calls()
        self.assertEqual(calls[0].split(), ["param_mpi_set_basin", "normal", "check_map"])
        self.assertEqual(calls[-1].split(), ["analysis_check_mpi_bifurcation", "mpireg-16.bin"])
        self.assertNotIn("analysis_mpi_region_color", "\n".join(calls))
        self.assertTrue(all("check_map" not in call for call in calls[1:]))


if __name__ == "__main__":
    unittest.main(verbosity=2)
