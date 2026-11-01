"""Legacy/v5 regression and workflow checks for river-network analysis tools.

All generated rasters are written to temporary map copies. These tests compare
against the preserved v4 programs; they do not claim scientific validation.
"""
from __future__ import annotations

import os
import shutil
import struct
import subprocess
import tempfile
import unittest
from pathlib import Path

from map.tools.code.generate_ctl import generate_directory

ROOT = Path(__file__).resolve().parents[3]
SAMPLE = ROOT / "map" / "mekong_15min"
CODE = ROOT / "map" / "tools" / "code"
LEGACY = ROOT / "map" / "src" / "src_param"
PARAM_SCRIPTS = ROOT / "map" / "tools" / "script_params"
ANALYSIS_SCRIPTS = ROOT / "map" / "tools" / "script_analysis"
MAP_FILES = (
    "params.txt", "nextxy.bin", "basin.bin", "upgrid.bin", "uparea.bin",
    "ctmare.bin", "elevtn.bin", "lonlat.bin", "GRDC_alloc.txt",
)


def run(args: list[str], cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(args, cwd=cwd, text=True, capture_output=True, check=False)


class AnalysisMigrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        compiler_name = os.environ.get("FC", "gfortran").split()[0]
        cls.compiler = shutil.which(compiler_name)
        if cls.compiler is None:
            raise unittest.SkipTest(f"Fortran compiler not available: {compiler_name}")
        cls.tmp = tempfile.TemporaryDirectory(prefix="cmf-analysis-build-")
        cls.root = Path(cls.tmp.name)
        cls.v5_dir = cls.root / "v5"
        cls.legacy_dir = cls.root / "legacy"
        cls.debug_dir = cls.root / "debug"
        for directory in (cls.v5_dir, cls.legacy_dir, cls.debug_dir):
            directory.mkdir()
        flags = ["-O2", "-ffree-line-length-none", "-fimplicit-none"]
        debug_flags = [*flags, "-fcheck=bounds"]
        for stem in ("get_rivinfo", "get_upbasin", "get_subbasin"):
            for source_dir, out_dir, prefix, compile_flags in (
                (LEGACY, cls.legacy_dir, "", flags),
                (CODE, cls.v5_dir, "analysis_", flags),
                (CODE, cls.debug_dir, "analysis_", debug_flags),
            ):
                result = run(
                    [cls.compiler, *compile_flags, str(source_dir / f"{prefix}{stem}.F90"),
                     "-o", str(out_dir / f"{prefix}{stem}")], ROOT,
                )
                if result.returncode:
                    cls.tmp.cleanup()
                    raise RuntimeError(f"Could not compile {prefix}{stem}:\n{result.stdout}{result.stderr}")

    @classmethod
    def tearDownClass(cls) -> None:
        if hasattr(cls, "tmp"):
            cls.tmp.cleanup()

    def make_map(self, label: str) -> tuple[Path, Path]:
        base = self.root / label
        map_dir = base / "map" / "mekong_15min"
        script_params = map_dir / "script_params"
        script_analysis = map_dir / "script_analysis"
        code_link = base / "map" / "tools" / "code"
        script_params.mkdir(parents=True)
        script_analysis.mkdir()
        code_link.parent.mkdir(parents=True)
        shutil.copytree(self.v5_dir, code_link)
        for name in MAP_FILES:
            shutil.copy2(SAMPLE / name, map_dir / name)
        hires = map_dir / "1min"
        hires.mkdir()
        for name in ("location.txt", "1min.catmxy.bin"):
            shutil.copy2(SAMPLE / "1min" / name, hires / name)
        return map_dir, script_params

    def test_legacy_and_v5_binary_outputs_match_and_ctl_generation_works(self) -> None:
        v5_map, v5_params = self.make_map("parity")
        legacy_map = self.root / "parity" / "map" / "legacy"
        legacy_params = legacy_map / "src_param"
        legacy_params.mkdir(parents=True)
        for name in MAP_FILES[:-1]:
            shutil.copy2(v5_map / name, legacy_map / name)
        legacy_hires = legacy_map / "1min"
        legacy_hires.mkdir()
        for name in ("location.txt", "1min.catmxy.bin"):
            shutil.copy2(v5_map / "1min" / name, legacy_hires / name)

        for label, executable_dir, workdir in (
            ("v5", self.v5_dir, v5_params), ("legacy", self.legacy_dir, legacy_params),
        ):
            program_prefix = "analysis_" if label == "v5" else ""
            result = run([str(executable_dir / f"{program_prefix}get_rivinfo"), "xy", "62", "93"], workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_river_info = result.stdout
            else:
                self.assertEqual(result.stdout, v5_river_info)
            result = run(
                [str(executable_dir / f"{program_prefix}get_rivinfo"), "latlon", "11.579", "104.946"],
                workdir,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_geo_info = result.stdout
            else:
                self.assertEqual(result.stdout, v5_geo_info)

            result = run([str(executable_dir / f"{program_prefix}get_upbasin"), "62", "93"], workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_upbasin = (v5_map / "upbasin.bin").read_bytes()
            else:
                self.assertEqual((legacy_map / "upbasin.bin").read_bytes(), v5_upbasin)

            result = run([str(executable_dir / f"{program_prefix}get_subbasin"), "50000"], workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_subbsn = (v5_map / "subbsn.bin").read_bytes()
                v5_subcol = (v5_map / "subcol.bin").read_bytes()
            else:
                self.assertEqual((legacy_map / "subbsn.bin").read_bytes(), v5_subbsn)
                self.assertEqual((legacy_map / "subcol.bin").read_bytes(), v5_subcol)

        record_bytes = 80 * 120 * 4
        self.assertEqual(len(v5_upbasin), record_bytes)
        self.assertEqual(len(v5_subbsn), record_bytes)
        self.assertEqual(len(v5_subcol), record_bytes)
        upstream_values = struct.unpack("<" + "i" * (80 * 120), v5_upbasin)
        subbasin_values = struct.unpack("<" + "i" * (80 * 120), v5_subbsn)
        color_values = struct.unpack("<" + "i" * (80 * 120), v5_subcol)
        self.assertTrue({-9999, 0, 1}.issubset(set(upstream_values)))
        self.assertIn(-999, subbasin_values)
        self.assertIn(0, subbasin_values)
        self.assertTrue(any(value > 0 for value in subbasin_values))
        self.assertIn(-9999, color_values)
        self.assertTrue(all(value == -9999 or 1 <= value <= 10 for value in color_values))
        ctl_dir = self.root / "ctl"
        ctl_dir.mkdir()
        for name in ("params.txt", "upbasin.bin", "subbsn.bin", "subcol.bin"):
            shutil.copy2(v5_map / name, ctl_dir / name)
        result = generate_directory(ctl_dir, ROOT / "map" / "map_variables.csv")
        self.assertEqual(result.generated, 3, result)
        for stem, variable in (("upbasin", "upbasin"), ("subbsn", "subbsn"), ("subcol", "subcol")):
            ctl = (ctl_dir / f"{stem}.ctl").read_text(encoding="utf-8")
            self.assertIn(f"{variable} 1", ctl)
            self.assertIn("undef", ctl)

    def test_bounds_checked_subbasin_and_custom_outputs_do_not_replace_standard(self) -> None:
        map_dir, script_params = self.make_map("bounds")
        standard_subbsn = b"prior-subbasin"
        standard_subcol = b"prior-colors"
        (map_dir / "subbsn.bin").write_bytes(standard_subbsn)
        (map_dir / "subcol.bin").write_bytes(standard_subcol)
        result = run([str(self.debug_dir / "analysis_get_subbasin"), "50000", "explore.bin", "explore-col.bin"], script_params)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual((map_dir / "subbsn.bin").read_bytes(), standard_subbsn)
        self.assertEqual((map_dir / "subcol.bin").read_bytes(), standard_subcol)
        self.assertEqual((script_params / "explore.bin").stat().st_size, 80 * 120 * 4)
        self.assertEqual((script_params / "explore-col.bin").stat().st_size, 80 * 120 * 4)

    def test_standard_scripts_and_analysis_templates_use_temporary_map(self) -> None:
        map_dir, params = self.make_map("scripts")
        analysis = map_dir / "script_analysis"
        for source, target in (
            (PARAM_SCRIPTS / "s06-subbasin.sh", params / "s06-subbasin.sh"),
            (PARAM_SCRIPTS / "s07-upbasin.sh", params / "s07-upbasin.sh"),
            (ANALYSIS_SCRIPTS / "a01-river_info.sh", analysis / "a01-river_info.sh"),
            (ANALYSIS_SCRIPTS / "a02-upstream_basin.sh", analysis / "a02-upstream_basin.sh"),
            (ANALYSIS_SCRIPTS / "a03-subbasin.sh", analysis / "a03-subbasin.sh"),
        ):
            shutil.copy2(source, target)
            target.chmod(0o755)
            syntax = run(["sh", "-n", str(target)], self.root)
            self.assertEqual(syntax.returncode, 0, syntax.stderr)

        result = run([str(params / "s06-subbasin.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual((map_dir / "subbsn.bin").stat().st_size, 80 * 120 * 4)
        self.assertEqual((map_dir / "subcol.bin").stat().st_size, 80 * 120 * 4)

        result = run([str(params / "s07-upbasin.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        selection = (map_dir / "grdc_station_selection.txt").read_text(encoding="utf-8")
        self.assertIn("2569002", selection)
        self.assertIn("\t62\t93", selection)
        self.assertEqual((map_dir / "upbasin.bin").stat().st_size, 80 * 120 * 4)
        mask_values = struct.unpack("<" + "i" * (80 * 120), (map_dir / "upbasin.bin").read_bytes())
        self.assertTrue({-9999, 0, 1}.issubset(set(mask_values)))

        result = run([str(analysis / "a01-river_info.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        result = run([str(analysis / "a02-upstream_basin.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertFalse((map_dir / "analysis_upbasin_62_93.bin").samefile(map_dir / "upbasin.bin"))
        result = run([str(analysis / "a03-subbasin.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertTrue((map_dir / "analysis" / "subbsn_50000km2.bin").exists())
        self.assertTrue((map_dir / "analysis" / "subcol_50000km2.bin").exists())

    def test_missing_or_headerless_grdc_allocation_fails_clearly(self) -> None:
        map_dir, params = self.make_map("invalid-grdc")
        script = params / "s07-upbasin.sh"
        shutil.copy2(PARAM_SCRIPTS / "s07-upbasin.sh", script)
        script.chmod(0o755)
        (map_dir / "GRDC_alloc.txt").write_text("ID lat lon\n", encoding="ascii")
        result = run([str(script)], self.root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("missing required column", result.stderr)

    def test_grdc_selection_filters_invalid_rows_and_keeps_first_tie(self) -> None:
        map_dir, params = self.make_map("grdc-tie")
        script = params / "s07-upbasin.sh"
        shutil.copy2(PARAM_SCRIPTS / "s07-upbasin.sh", script)
        script.chmod(0o755)
        header = "ID lat lon area_Input area_CaMa error diff Type ix1 iy1 ix2 iy2 area1 area2\n"
        rows = (
            "unallocated 10 100 900 900 0 0 -9 62 93 -999 -999 -999 -999\n"
            "bad-area 10 100 -1 200 0 0 1 62 93 -999 -999 -999 -999\n"
            "bad-cell 10 100 200 300 0 0 1 999 93 -999 -999 -999 -999\n"
            "first-tie 11 101 120 500 0 0 1 62 93 -999 -999 120 -999\n"
            "second-tie 12 102 130 500 0 0 1 62 93 -999 -999 130 -999\n"
        )
        (map_dir / "GRDC_alloc.txt").write_text(header + rows, encoding="ascii")
        result = run([str(script)], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        selection = (map_dir / "grdc_station_selection.txt").read_text(encoding="utf-8")
        self.assertIn("first-tie", selection)
        self.assertNotIn("second-tie", selection)
        self.assertIn("\t62\t93", selection)

    def test_missing_grdc_allocation_fails_with_manual_next_step(self) -> None:
        map_dir, params = self.make_map("missing-grdc")
        (map_dir / "GRDC_alloc.txt").unlink()
        script = params / "s07-upbasin.sh"
        shutil.copy2(PARAM_SCRIPTS / "s07-upbasin.sh", script)
        script.chmod(0o755)
        result = run([str(script)], self.root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Run t01-alloc_flow_gauge.sh", result.stderr)


if __name__ == "__main__":
    unittest.main()
