"""Regression checks for standard s02 outputs and its s01 depth dependency.

The byte-comparison test uses the official Mekong sample and its committed
channel-depth product. The optional end-to-end test regenerates that product
with s01 when CMF_RUNOFF_CLIMATOLOGY points to a compatible runoff file.
All generated files live in temporary workspaces.
"""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from map.tools.code.generate_ctl import generate_directory

ROOT = Path(__file__).resolve().parents[3]
SAMPLE = ROOT / "map" / "mekong_15min"
CODE_SOURCE = ROOT / "map" / "tools" / "code"
LEGACY_SOURCE = ROOT / "map" / "src" / "src_param"
S02_TEMPLATE = ROOT / "map" / "tools" / "script_params" / "s02-bifurcation.sh"
S01_TEMPLATE = ROOT / "map" / "tools" / "script_params" / "s01-channel_params.sh"
MAP_INPUTS = (
    "params.txt", "nextxy.bin", "basin.bin", "bifori.txt",
    "inpdim_test-1deg.txt",
)
CHANNEL_INPUTS = (
    *MAP_INPUTS, "ctmare.bin", "width.bin", "inpmat_test-1deg.bin",
)
OUTPUTS = ("bifprm.txt", "bifbsn.bin", "bifcol.bin")
CHECK_OUTPUTS = ("check_bif_dphwth.bin", "check_bif_bsnmod.bin")


def run(command: list[str], *, cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)


class S02InputGuardTests(unittest.TestCase):
    def test_missing_channel_depth_is_passed_to_the_program(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-s02-guard-") as temp:
            root = Path(temp)
            map_root = root / "map" / "sample"
            script_dir = map_root / "script_params"
            code_dir = root / "map" / "tools" / "code"
            outside = root / "outside"
            script_dir.mkdir(parents=True)
            code_dir.mkdir(parents=True)
            outside.mkdir()

            for name in MAP_INPUTS:
                shutil.copy2(SAMPLE / name, map_root / name)
            for name in ("param_set_bifparam", "param_set_bif_basin"):
                executable = code_dir / name
                executable.write_text("#!/bin/sh\necho parameter program invoked\nexit 99\n", encoding="ascii")
                executable.chmod(0o755)
            script = script_dir / "s02-bifurcation.sh"
            shutil.copy2(S02_TEMPLATE, script)
            script.chmod(0o755)

            result = run([str(script)], cwd=outside)

            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("parameter program invoked", result.stdout)
            for output in OUTPUTS:
                self.assertFalse((map_root / output).exists(), output)

    def test_unknown_check_option_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="cmf-s02-usage-") as temp:
            root = Path(temp)
            map_root = root / "map" / "sample"
            script_dir = map_root / "script_params"
            code_dir = root / "map" / "tools" / "code"
            script_dir.mkdir(parents=True)
            code_dir.mkdir(parents=True)
            script = script_dir / "s02-bifurcation.sh"
            shutil.copy2(S02_TEMPLATE, script)
            script.chmod(0o755)

            result = run([str(script), "unknown"], cwd=root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Usage:", result.stderr)


class BifurcationMigrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        compiler_name = os.environ.get("FC", "gfortran").split()[0]
        cls.compiler = shutil.which(compiler_name)
        if cls.compiler is None:
            raise unittest.SkipTest(f"Fortran compiler not available: {compiler_name}")

        cls.build_tmp = tempfile.TemporaryDirectory(prefix="cmf-bif-build-")
        cls.build_root = Path(cls.build_tmp.name)
        cls.code_dir = cls.build_root / "map" / "tools" / "code"
        cls.code_dir.mkdir(parents=True)
        (cls.build_root / "adm").mkdir()
        shutil.copy2(ROOT / "adm" / "Mkinclude", cls.build_root / "adm" / "Mkinclude")
        shutil.copy2(CODE_SOURCE / "Makefile", cls.code_dir / "Makefile")
        for source in CODE_SOURCE.glob("param_*.F90"):
            shutil.copy2(source, cls.code_dir / source.name)

        result = run(["make", "-C", str(cls.code_dir), "param"], cwd=ROOT)
        if result.returncode:
            cls.build_tmp.cleanup()
            raise RuntimeError("shared parameter-tool build failed:\n" + result.stdout + result.stderr)

        cls.legacy_dir = cls.build_root / "legacy"
        cls.legacy_dir.mkdir()
        flags = ["-O3", "-Wall", "-cpp", "-ffree-line-length-none", "-fimplicit-none"]
        for stem in ("set_bifparam", "set_bif_basin"):
            result = run(
                [cls.compiler, *flags, str(LEGACY_SOURCE / f"{stem}.F90"),
                 "-o", str(cls.legacy_dir / stem)],
                cwd=ROOT,
            )
            if result.returncode:
                cls.build_tmp.cleanup()
                raise RuntimeError(f"legacy {stem} build failed:\n" + result.stdout + result.stderr)

    @classmethod
    def tearDownClass(cls) -> None:
        if hasattr(cls, "build_tmp"):
            cls.build_tmp.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory(prefix="cmf-bif-map-")
        self.root = Path(self.temp.name)
        (self.root / "outside").mkdir()
        (self.root / "map" / "tools").mkdir(parents=True)
        (self.root / "map" / "tools" / "code").symlink_to(self.code_dir, target_is_directory=True)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def create_map(self, tag: str, *, channel_inputs: bool = False,
                   include_depth: bool = True) -> Path:
        map_root = self.root / "map" / tag
        (map_root / "script_params").mkdir(parents=True)
        (map_root / "src_param").mkdir()
        names = CHANNEL_INPUTS if channel_inputs else MAP_INPUTS
        for name in names:
            shutil.copy2(SAMPLE / name, map_root / name)
        if include_depth:
            shutil.copy2(SAMPLE / "rivhgt.bin", map_root / "rivhgt.bin")
        shutil.copy2(S02_TEMPLATE, map_root / "script_params" / "s02-bifurcation.sh")
        shutil.copy2(S01_TEMPLATE, map_root / "script_params" / "s01-channel_params.sh")
        shutil.copy2(self.legacy_dir / "set_bifparam", map_root / "src_param" / "set_bifparam")
        shutil.copy2(self.legacy_dir / "set_bif_basin", map_root / "src_param" / "set_bif_basin")
        return map_root

    def run_s02(self, map_root: Path, *args: str) -> subprocess.CompletedProcess[str]:
        script = map_root / "script_params" / "s02-bifurcation.sh"
        return run([str(script), *args], cwd=self.root / "outside")

    def test_basin_program_rejects_unknown_argument(self) -> None:
        result = run([str(self.code_dir / "param_set_bif_basin"), "unknown"],
                     cwd=self.root / "outside")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("optional argument must be check_map", result.stdout)

    def test_bifparam_program_rejects_biflayer_outside_supported_range(self) -> None:
        for biflayer in ("0", "11", "not-a-number"):
            with self.subTest(biflayer=biflayer):
                result = run(
                    [str(self.code_dir / "param_set_bifparam"), "rivhgt.bin", "bifprm.txt",
                     biflayer, "inpdim_test-1deg.txt"],
                    cwd=self.root,
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("BIFLAYER must be an integer from 1 to 10", result.stdout)

    def test_s02_matches_legacy_and_optional_check_map(self) -> None:
        v5 = self.create_map("v5")
        legacy = self.create_map("legacy")

        default = self.run_s02(v5)
        self.assertEqual(default.returncode, 0, default.stdout + default.stderr)
        for name in (*CHECK_OUTPUTS, "bpoint.bin", "bifmod.bin"):
            self.assertFalse((v5 / name).exists(), name)

        legacy_parameter = run(
            [str(self.legacy_dir / "set_bifparam"), "rivhgt.bin", "bifprm.txt", "5",
             "inpdim_test-1deg.txt"],
            cwd=legacy,
        )
        self.assertEqual(legacy_parameter.returncode, 0,
                         legacy_parameter.stdout + legacy_parameter.stderr)
        legacy_basin = run([str(self.legacy_dir / "set_bif_basin")], cwd=legacy / "src_param")
        self.assertEqual(legacy_basin.returncode, 0, legacy_basin.stdout + legacy_basin.stderr)

        optional = self.run_s02(v5, "check_map")
        self.assertEqual(optional.returncode, 0, optional.stdout + optional.stderr)
        for name in OUTPUTS:
            self.assertEqual((v5 / name).read_bytes(), (legacy / name).read_bytes(), name)
        for name in ("bifbsn.bin", "bifcol.bin"):
            self.assertEqual((v5 / name).stat().st_size, 80 * 120 * 4, name)
        self.assertEqual((v5 / "check_bif_dphwth.bin").read_bytes(),
                         (legacy / "bifdph.bin").read_bytes())
        self.assertEqual((v5 / "check_bif_dphwth.bin").stat().st_size, 2 * 80 * 120 * 4)
        self.assertEqual((legacy / "bifdph.bin").stat().st_size, 2 * 80 * 120 * 4)
        record_size = 80 * 120 * 4
        consolidated = (v5 / "check_bif_bsnmod.bin").read_bytes()
        self.assertEqual(len(consolidated), 2 * record_size)
        self.assertEqual(consolidated[:record_size], (legacy / "bifmod.bin").read_bytes())
        self.assertEqual(consolidated[record_size:], (legacy / "bpoint.bin").read_bytes())
        self.assertFalse((v5 / "bifmod.bin").exists())
        self.assertFalse((v5 / "bpoint.bin").exists())

        # The CTL registry describes the two diagnostic records as separate
        # variables in their physical record order.
        ctl_dir = self.root / "ctl_check"
        ctl_dir.mkdir()
        shutil.copy2(SAMPLE / "params.txt", ctl_dir / "params.txt")
        shutil.copy2(v5 / "check_bif_bsnmod.bin", ctl_dir / "check_bif_bsnmod.bin")
        ctl_result = generate_directory(ctl_dir, ROOT / "map" / "map_variables.csv")
        self.assertEqual(ctl_result.generated, 1, ctl_result)
        ctl = (ctl_dir / "check_bif_bsnmod.ctl").read_text(encoding="utf-8")
        self.assertIn("vars 2", ctl)
        self.assertIn("undef -9999", ctl)
        self.assertLess(ctl.index("bifmod 1 -1,40,4"), ctl.index("bpoint 1 -1,40,4"))

    def test_default_mode_preserves_existing_diagnostics_and_legacy_files(self) -> None:
        map_root = self.create_map("preserve_existing")
        sentinels = {}
        for name in (*CHECK_OUTPUTS, "bifmod.bin", "bpoint.bin"):
            sentinels[name] = ("prior:" + name).encode()
            (map_root / name).write_bytes(sentinels[name])

        result = self.run_s02(map_root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        for name, content in sentinels.items():
            self.assertEqual((map_root / name).read_bytes(), content, name)

    def test_s01_produces_depth_consumed_by_s02(self) -> None:
        runoff = os.environ.get("CMF_RUNOFF_CLIMATOLOGY")
        if not runoff or not Path(runoff).is_file():
            self.skipTest("set CMF_RUNOFF_CLIMATOLOGY to compatible distribution runoff data")

        map_root = self.create_map("s01_to_s02", channel_inputs=True, include_depth=False)
        data_dir = self.root / "map" / "data"
        data_dir.mkdir()
        shutil.copy2(runoff, data_dir / "ELSE_GPCC_clm-1981-2010.one")

        s01 = run([str(map_root / "script_params" / "s01-channel_params.sh")],
                  cwd=self.root / "outside")
        self.assertEqual(s01.returncode, 0, s01.stdout + s01.stderr)
        depth = map_root / "rivhgt.bin"
        self.assertTrue(depth.is_file(), "s01 did not produce rivhgt.bin")
        self.assertEqual(depth.stat().st_size, 80 * 120 * 4)

        s02 = self.run_s02(map_root)
        self.assertEqual(s02.returncode, 0, s02.stdout + s02.stderr)
        for name in OUTPUTS:
            self.assertTrue((map_root / name).is_file(), name)
            self.assertGreater((map_root / name).stat().st_size, 0, name)
        for name in (*CHECK_OUTPUTS, "bifmod.bin", "bpoint.bin"):
            self.assertFalse((map_root / name).exists(), name)


if __name__ == "__main__":
    unittest.main(verbosity=2)
