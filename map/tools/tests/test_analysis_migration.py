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
        programs = (
            ("get_rivinfo", "analysis_river_point_info"),
            ("get_upbasin", "analysis_upbasin"),
            ("get_subbasin", "analysis_subbasin"),
            ("get_vecmap", "analysis_river_node_latlon"),
        )
        for legacy_stem, v5_stem in programs:
            for source_dir, out_dir, compile_flags in (
                (LEGACY, cls.legacy_dir, flags),
                (CODE, cls.v5_dir, flags),
                (CODE, cls.debug_dir, debug_flags),
            ):
                stem = legacy_stem if source_dir == LEGACY else v5_stem
                result = run(
                    [cls.compiler, *compile_flags, str(source_dir / f"{stem}.F90"),
                     "-o", str(out_dir / stem)], ROOT,
                )
                if result.returncode:
                    cls.tmp.cleanup()
                    raise RuntimeError(f"Could not compile {stem}:\n{result.stdout}{result.stderr}")

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
        (script_analysis / "code").symlink_to("../../tools/code")
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
            rivinfo = "analysis_river_point_info" if label == "v5" else "get_rivinfo"
            result = run([str(executable_dir / rivinfo), "xy", "62", "93"], workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_river_info = result.stdout
            else:
                self.assertEqual(result.stdout, v5_river_info)
            result = run(
                [str(executable_dir / rivinfo), "latlon", "11.579", "104.946"],
                workdir,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_geo_info = result.stdout
            else:
                self.assertEqual(result.stdout, v5_geo_info)

            upbasin = "analysis_upbasin" if label == "v5" else "get_upbasin"
            upbasin_args = [str(executable_dir / upbasin), "62", "93"]
            if label == "v5":
                upbasin_args.append("../upbasin.bin")
            result = run(upbasin_args, workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_upbasin = (v5_map / "upbasin.bin").read_bytes()
            else:
                self.assertEqual((legacy_map / "upbasin.bin").read_bytes(), v5_upbasin)

            subbasin = "analysis_subbasin" if label == "v5" else "get_subbasin"
            subbasin_args = [str(executable_dir / subbasin), "50000"]
            if label == "v5":
                subbasin_args.extend(("../subbsn.bin", "../subcol.bin"))
            result = run(subbasin_args, workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_subbsn = (v5_map / "subbsn.bin").read_bytes()
                v5_subcol = (v5_map / "subcol.bin").read_bytes()
            else:
                legacy_subbsn = (legacy_map / "subbsn.bin").read_bytes()
                old_values = struct.unpack("<" + "i" * (80 * 120), legacy_subbsn)
                new_values = struct.unpack("<" + "i" * (80 * 120), v5_subbsn)
                nextx_values = struct.unpack("<" + "i" * (80 * 120),
                                             (v5_map / "nextxy.bin").read_bytes()[:80 * 120 * 4])
                for old, new, downstream_x in zip(old_values, new_values, nextx_values):
                    if old > 0:
                        self.assertEqual(new, old)
                    elif downstream_x == -9999:
                        self.assertEqual(new, -9999)
                    else:
                        self.assertEqual(new, 0)
                self.assertEqual((legacy_map / "subcol.bin").read_bytes(), v5_subcol)

        legacy_analysis = legacy_map / "script_analysis"
        legacy_analysis.mkdir()
        for label, executable, workdir in (
            ("v5", self.v5_dir / "analysis_river_node_latlon", v5_map / "script_analysis"),
            ("legacy", self.legacy_dir / "get_vecmap", legacy_analysis),
        ):
            result = run([str(executable), "90", "110", "5", "35"], workdir)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            if label == "v5":
                v5_vector_text = result.stdout
            else:
                self.assertEqual(result.stdout, v5_vector_text)
                self.assertIn("ori-lat", result.stdout.splitlines()[0])

        # The legacy threshold of 30,000 km2 aborts after basin-color setup on
        # this compiler; verify the sample threshold through the v5-only wrapper.
        result = run(
            [str(self.v5_dir / "analysis_subbasin"), "30000",
             str(v5_map / "subbsn-30000.bin"), str(v5_map / "subcol.bin")],
            v5_params,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        v5_subbsn_30000 = (v5_map / "subbsn-30000.bin").read_bytes()

        record_bytes = 80 * 120 * 4
        self.assertEqual(len(v5_upbasin), record_bytes)
        self.assertEqual(len(v5_subbsn), record_bytes)
        self.assertEqual(len(v5_subcol), record_bytes)
        upstream_values = struct.unpack("<" + "i" * (80 * 120), v5_upbasin)
        subbasin_values = struct.unpack("<" + "i" * (80 * 120), v5_subbsn_30000)
        color_values = struct.unpack("<" + "i" * (80 * 120), v5_subcol)
        self.assertTrue({-9999, 0, 1}.issubset(set(upstream_values)))
        self.assertIn(-9999, subbasin_values)
        self.assertIn(0, subbasin_values)
        self.assertTrue(any(value > 0 for value in subbasin_values))
        self.assertIn(-9999, color_values)
        self.assertTrue(all(value == -9999 or 1 <= value <= 10 for value in color_values))
        ctl_dir = self.root / "ctl"
        ctl_dir.mkdir()
        shutil.copy2(v5_map / "upbasin.bin", v5_map / "upbasin-2569002.bin")
        shutil.copy2(v5_map / "subcol.bin", v5_map / "subcol-30000.bin")
        for name in ("params.txt", "upbasin-2569002.bin", "subbsn-30000.bin", "subcol-30000.bin"):
            shutil.copy2(v5_map / name, ctl_dir / name)
        result = generate_directory(ctl_dir, ROOT / "map" / "map_variables.csv")
        self.assertEqual(result.generated, 3, result)
        for stem, variable in (("upbasin-2569002", "upbasin"), ("subbsn-30000", "subbsn"), ("subcol-30000", "subcol")):
            ctl = (ctl_dir / f"{stem}.ctl").read_text(encoding="utf-8")
            self.assertIn(f"{variable} 1", ctl)
            self.assertIn("undef", ctl)

    def test_bounds_checked_subbasin_and_custom_outputs_do_not_replace_standard(self) -> None:
        map_dir, script_params = self.make_map("bounds")
        standard_subbsn = b"prior-subbasin"
        standard_subcol = b"prior-colors"
        standard_upbasin = b"prior-upstream-mask"
        (map_dir / "subbsn.bin").write_bytes(standard_subbsn)
        (map_dir / "subcol.bin").write_bytes(standard_subcol)
        (map_dir / "upbasin.bin").write_bytes(standard_upbasin)
        result = run([str(self.debug_dir / "analysis_subbasin"), "30000"], script_params)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual((map_dir / "subbsn.bin").read_bytes(), standard_subbsn)
        self.assertEqual((map_dir / "subcol.bin").read_bytes(), standard_subcol)
        self.assertEqual((map_dir / "analysis_subbsn_30000.bin").stat().st_size, 80 * 120 * 4)
        self.assertEqual((map_dir / "analysis_subcol_30000.bin").stat().st_size, 80 * 120 * 4)
        result = run([str(self.debug_dir / "analysis_upbasin"), "62", "93"], script_params)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual((map_dir / "upbasin.bin").read_bytes(), standard_upbasin)
        self.assertEqual((map_dir / "analysis_upbasin_62_93.bin").stat().st_size, 80 * 120 * 4)
        result = run([str(self.debug_dir / "analysis_subbasin"), "30000", "explore.bin", "explore-col.bin"], script_params)
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
            (ANALYSIS_SCRIPTS / "a01-river_point_info.sh", analysis / "a01-river_point_info.sh"),
            (ANALYSIS_SCRIPTS / "a02-upbasin.sh", analysis / "a02-upbasin.sh"),
            (ANALYSIS_SCRIPTS / "a03-subbasin.sh", analysis / "a03-subbasin.sh"),
            (ANALYSIS_SCRIPTS / "a04-river_node_latlon.sh", analysis / "a04-river_node_latlon.sh"),
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
        station_mask = map_dir / "upbasin-2569002.bin"
        self.assertEqual(station_mask.stat().st_size, 80 * 120 * 4)
        self.assertFalse((map_dir / "upbasin.bin").exists())
        mask_values = struct.unpack("<" + "i" * (80 * 120), station_mask.read_bytes())
        self.assertTrue({-9999, 0, 1}.issubset(set(mask_values)))

        result = run([str(analysis / "a01-river_point_info.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        result = run([str(analysis / "a02-upbasin.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        analysis_output = analysis / "out_analysis"
        self.assertFalse((analysis_output / "upbasin_62_93.bin").samefile(map_dir / "upbasin-2569002.bin"))
        result = run([str(analysis / "a03-subbasin.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertTrue((analysis_output / "subbsn_50000km2.bin").exists())
        self.assertTrue((analysis_output / "subcol_50000km2.bin").exists())
        result = run([str(analysis / "a04-river_node_latlon.sh")], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        vector_output = analysis_output / "river_node_latlon_90_110_5_35.txt"
        self.assertTrue(vector_output.is_file())
        self.assertIn("ori-lat", vector_output.read_text(encoding="ascii").splitlines()[0])

    def test_mekong_sample_subbasin_wrapper_labels_threshold_output(self) -> None:
        map_dir, params = self.make_map("sample-script")
        script = params / "s06-subbasin.sh"
        shutil.copy2(SAMPLE / "script_params" / "s06-subbasin.sh", script)
        script.chmod(0o755)
        result = run([str(script)], self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual((map_dir / "subbsn-30000.bin").stat().st_size, 80 * 120 * 4)
        self.assertFalse((map_dir / "subbsn.bin").exists())
        self.assertEqual((map_dir / "subcol-30000.bin").stat().st_size, 80 * 120 * 4)
        self.assertFalse((map_dir / "subcol.bin").exists())

    def test_direct_cli_runs_from_map_local_script_analysis(self) -> None:
        map_dir, _ = self.make_map("portable-cli")
        analysis = map_dir / "script_analysis"
        commands = (
            ("analysis_river_point_info", "xy", "62", "93"),
            ("analysis_upbasin", "62", "93"),
            ("analysis_subbasin", "50000"),
            ("analysis_river_node_latlon", "90", "110", "5", "35"),
        )
        outputs = []
        for command in commands:
            result = run([str(analysis / "code" / command[0]), *command[1:]], analysis)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            outputs.append(result.stdout)
        self.assertIn("Point Information", outputs[0])
        self.assertIn("Upstream Area", outputs[1])
        self.assertIn("SET_MAP color basin", outputs[2])
        self.assertIn("ori-lat", outputs[3].splitlines()[0])
        self.assertTrue((map_dir / "analysis_upbasin_62_93.bin").is_file())
        self.assertTrue((map_dir / "analysis_subbsn_50000.bin").is_file())
        self.assertTrue((map_dir / "analysis_subcol_50000.bin").is_file())
        self.assertFalse((map_dir / "upbasin.bin").exists())
        self.assertFalse((map_dir / "subbsn.bin").exists())
        self.assertFalse((map_dir / "subcol.bin").exists())

    def test_link_codes_creates_relative_shared_code_symlinks(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-analysis-link-") as temporary:
            root = Path(temporary)
            code = root / "map" / "tools" / "code"
            template = root / "map" / "tools" / "script_analysis"
            code.mkdir(parents=True)
            template.mkdir(parents=True)
            script = template / "a00-link_codes.sh"
            shutil.copy2(ANALYSIS_SCRIPTS / "a00-link_codes.sh", script)
            result = run([str(script)], self.root)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertTrue((template / "code").is_symlink())
            self.assertEqual(os.readlink(template / "code"), "../code")

            # Re-running preserves the correct relative link.
            result = run([str(script)], self.root)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(os.readlink(template / "code"), "../code")

            # The same script resolves the map-local path from its own location.
            sample_analysis = root / "map" / "mekong_15min" / "script_analysis"
            sample_analysis.mkdir(parents=True)
            shutil.copy2(script, sample_analysis / script.name)
            result = run([str(sample_analysis / script.name)], self.root)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(os.readlink(sample_analysis / "code"), "../../tools/code")

    def test_missing_or_headerless_grdc_allocation_fails_clearly(self) -> None:
        map_dir, params = self.make_map("invalid-grdc")
        script = params / "s07-upbasin.sh"
        shutil.copy2(PARAM_SCRIPTS / "s07-upbasin.sh", script)
        script.chmod(0o755)
        (map_dir / "GRDC_alloc.txt").write_text("ID lat lon\n", encoding="ascii")
        result = run([str(script)], self.root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("contains no valid allocated station", result.stderr)

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
