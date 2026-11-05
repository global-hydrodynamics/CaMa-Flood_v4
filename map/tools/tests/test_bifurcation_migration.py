"""Regression checks for standard s02 outputs and its s01 depth dependency.

The byte-comparison test uses the official Mekong sample and its committed
channel-depth product. The end-to-end test uses the Git-tracked runoff sample
by default; CMF_RUNOFF_CLIMATOLOGY can select a compatible alternate input.
All generated files live in temporary workspaces.
"""
from __future__ import annotations

import os
import csv
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
    "lonlat.bin", "inpdim_test-glb_1deg.txt",
)
CHANNEL_INPUTS = (
    *MAP_INPUTS, "ctmare.bin", "width.bin", "inpmat_test-glb_1deg.bin",
)
OUTPUTS = ("bifprm.csv", "bifprm.nc", "bifbsn.bin", "bifcol.bin")
CHECK_OUTPUTS = ("check_bif_dphwth.bin", "check_bif_bsnmod.bin")
NETCDF4_AVAILABLE = __import__("importlib.util", fromlist=["find_spec"]).find_spec("netCDF4") is not None


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
            for name in ("param_bif_set_params", "param_bif_set_basin"):
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
        shutil.copy2(CODE_SOURCE / "mod_map_color.F90", cls.code_dir / "mod_map_color.F90")
        shutil.copy2(CODE_SOURCE / "mod_bifprm_csv.F90", cls.code_dir / "mod_bifprm_csv.F90")
        shutil.copy2(CODE_SOURCE / "convert_bifprm_csv_to_netcdf.py", cls.code_dir / "convert_bifprm_csv_to_netcdf.py")
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
        result = run([str(self.code_dir / "param_bif_set_basin"), "unknown"],
                     cwd=self.root / "outside")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("optional argument must be check_map", result.stdout)

    def test_bifparam_program_rejects_biflayer_outside_supported_range(self) -> None:
        for biflayer in ("0", "11", "not-a-number"):
            with self.subTest(biflayer=biflayer):
                result = run(
                    [str(self.code_dir / "param_bif_set_params"), "rivhgt.bin", "bifprm.txt",
                     biflayer, "inpdim_test-glb_1deg.txt"],
                    cwd=self.root,
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("BIFLAYER must be an integer from 1 to 10", result.stdout)

    @unittest.skipUnless(NETCDF4_AVAILABLE, "Python netCDF4 is required for the s02 converter step")
    def test_s02_matches_legacy_and_optional_check_map(self) -> None:
        v5 = self.create_map("v5")
        legacy = self.create_map("legacy")

        default = self.run_s02(v5)
        self.assertEqual(default.returncode, 0, default.stdout + default.stderr)
        for name in (*CHECK_OUTPUTS, "bpoint.bin", "bifmod.bin"):
            self.assertFalse((v5 / name).exists(), name)

        legacy_parameter = run(
            [str(self.legacy_dir / "set_bifparam"), "rivhgt.bin", "bifprm.txt", "5",
             "inpdim_test-glb_1deg.txt"],
            cwd=legacy,
        )
        self.assertEqual(legacy_parameter.returncode, 0,
                         legacy_parameter.stdout + legacy_parameter.stderr)
        legacy_basin = run([str(self.legacy_dir / "set_bif_basin")], cwd=legacy / "src_param")
        self.assertEqual(legacy_basin.returncode, 0, legacy_basin.stdout + legacy_basin.stderr)

        optional = self.run_s02(v5, "check_map")
        self.assertEqual(optional.returncode, 0, optional.stdout + optional.stderr)
        for name in ("bifbsn.bin", "bifcol.bin"):
            self.assertEqual((v5 / name).read_bytes(), (legacy / name).read_bytes(), name)
        self.assert_csv_matches_legacy(v5 / "bifprm.csv", legacy / "bifprm.txt", SAMPLE / "lonlat.bin")
        self.assert_netcdf_matches_csv(v5 / "bifprm.csv", v5 / "bifprm.nc")
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
        ctl_result = generate_directory(ctl_dir, ROOT / "variable_table" / "map_variables.csv")
        self.assertEqual(ctl_result.generated, 1, ctl_result)
        ctl = (ctl_dir / "check_bif_bsnmod.ctl").read_text(encoding="utf-8")
        self.assertIn("vars 2", ctl)
        self.assertIn("undef -9999", ctl)
        self.assertLess(ctl.index("bifmod 1 -1,40,4"), ctl.index("bpoint 1 -1,40,4"))

    @unittest.skipUnless(NETCDF4_AVAILABLE, "Python netCDF4 is required for the s02 converter step")
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

    @unittest.skipUnless(NETCDF4_AVAILABLE, "Python netCDF4 is required for the s02 converter step")
    def test_s01_produces_depth_consumed_by_s02(self) -> None:
        runoff = Path(os.environ.get(
            "CMF_RUNOFF_CLIMATOLOGY",
            ROOT / "map" / "data" / "ELSE_GPCC_clm-1981-2010.one",
        ))
        self.assertTrue(runoff.is_file(), f"runoff climatology input not found: {runoff}")

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

    @unittest.skipUnless(NETCDF4_AVAILABLE, "Python netCDF4 is required for the CSV-to-NetCDF checks")
    def test_filter_ids_coordinates_and_netcdf_match_for_two_layer_counts(self) -> None:
        import numpy as np
        from netCDF4 import Dataset

        map_root = self.create_map("layer_counts")
        nx, ny = (int(line.split()[0]) for line in (SAMPLE / "params.txt").read_text().splitlines()[:2])
        lonlat = np.fromfile(SAMPLE / "lonlat.bin", dtype="<f4").reshape(2, ny, nx)
        bifori_rows = [line.split() for line in (SAMPLE / "bifori.txt").read_text().splitlines()[1:]]
        for layers, expected_count in ((5, 382), (3, 259)):
            csv_name = f"bifprm-{layers}.csv"
            nc_name = f"bifprm-{layers}.nc"
            result = run(
                [str(self.code_dir / "param_bif_set_params"), "rivhgt.bin", csv_name,
                 str(layers), "inpdim_test-glb_1deg.txt"],
                cwd=map_root,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            first_csv = (map_root / csv_name).read_bytes()
            repeat = run(
                [str(self.code_dir / "param_bif_set_params"), "rivhgt.bin", csv_name,
                 str(layers), "inpdim_test-glb_1deg.txt"],
                cwd=map_root,
            )
            self.assertEqual(repeat.returncode, 0, repeat.stdout + repeat.stderr)
            self.assertEqual((map_root / csv_name).read_bytes(), first_csv)
            with (map_root / csv_name).open(newline="", encoding="ascii") as stream:
                rows = list(csv.DictReader(stream))
            self.assertEqual(len(rows), expected_count)
            self.assertTrue(all(int(row["pth_id"]) == index for index, row in enumerate(rows, 1)))
            endpoints = [(row["ix"], row["iy"], row["jx"], row["jy"]) for row in rows]
            self.assertEqual(len(endpoints), len(set(endpoints)))
            source = {tuple(row[:4]): row for row in bifori_rows}
            for row in rows:
                ix, iy, jx, jy = (int(row[name]) for name in ("ix", "iy", "jx", "jy"))
                original = source[(str(ix), str(iy), str(jx), str(jy))]
                self.assertEqual(float(row["start_lat"]), float(original[-2]))
                self.assertEqual(float(row["start_lon"]), float(original[-1]))
                self.assertEqual(float(row["end_lon"]), float(lonlat[0, jy - 1, jx - 1]))
                self.assertEqual(float(row["end_lat"]), float(lonlat[1, jy - 1, jx - 1]))
                self.assertEqual(int(row["ibsn"]), min(int(row["ibsn"]), int(row["jbsn"])))
                self.assertEqual(int(row["jbsn"]), max(int(row["ibsn"]), int(row["jbsn"])))

            result = run(
                ["python3", str(CODE_SOURCE / "convert_bifprm_csv_to_netcdf.py"), csv_name, nc_name],
                cwd=map_root,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            with Dataset(map_root / nc_name) as dataset:
                self.assertEqual(len(dataset.dimensions["path"]), expected_count)
                self.assertEqual(len(dataset.dimensions["layer"]), layers)
                self.assertEqual(dataset["width"].dimensions, ("layer", "path"))
                self.assertEqual(dataset["width"].shape, (layers, expected_count))
                self.assertEqual(dataset["pth_id"].dtype, np.dtype("int32"))
                self.assertEqual(dataset["width"].dtype, np.dtype("float32"))
                self.assertEqual(dataset["depth"].units, "m")
                self.assertEqual(dataset["pth_id"].units, "1")
                self.assertEqual(dataset["end_lon"].units, "degrees_east")
                self.assertEqual(dataset["end_lat"].units, "degrees_north")
                self.assertEqual(dataset.getncattr("format_version"), "1.0")
                self.assertIn("Converted without filtering", dataset.getncattr("summary"))
                self.assertEqual(dataset["pth_id"][:].tolist(), [int(row["pth_id"]) for row in rows])
                np.testing.assert_array_equal(dataset["ix"][:], [int(row["ix"]) for row in rows])
                np.testing.assert_array_equal(dataset["end_lon"][:], [float(row["end_lon"]) for row in rows])
                for layer in range(1, layers + 1):
                    np.testing.assert_array_equal(
                        dataset["width"][layer - 1, :],
                        np.asarray([row[f"width_{layer}"] for row in rows], dtype=np.float32),
                    )
                self.assertEqual(dataset["width"].filters()["complevel"], 3)
                self.assertTrue(dataset["width"].filters()["zlib"])
                self.assertTrue(dataset["width"].filters()["shuffle"])
                self.assertEqual(dataset["depth"].filters()["complevel"], 3)
                self.assertEqual(dataset["depth"].getncattr("missing_value"), -9999.0)

    @unittest.skipUnless(NETCDF4_AVAILABLE, "Python netCDF4 is required for converter validation")
    def test_converter_rejects_duplicate_paths_without_replacing_existing_netcdf(self) -> None:
        map_root = self.create_map("duplicate_path")
        generated = run(
            [str(self.code_dir / "param_bif_set_params"), "rivhgt.bin", "bifprm.csv",
             "5", "inpdim_test-glb_1deg.txt"],
            cwd=map_root,
        )
        self.assertEqual(generated.returncode, 0, generated.stdout + generated.stderr)
        with (map_root / "bifprm.csv").open(newline="", encoding="ascii") as stream:
            reader = csv.DictReader(stream)
            fieldnames = reader.fieldnames
            rows = list(reader)
        rows[1]["ix"] = rows[0]["ix"]
        rows[1]["iy"] = rows[0]["iy"]
        rows[1]["jx"] = rows[0]["jx"]
        rows[1]["jy"] = rows[0]["jy"]
        with (map_root / "bifprm.csv").open("w", newline="", encoding="ascii") as stream:
            writer = csv.DictWriter(stream, fieldnames=fieldnames, lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
        target = map_root / "bifprm.nc"
        original = b"existing valid output sentinel"
        target.write_bytes(original)
        result = run(
            ["python3", str(CODE_SOURCE / "convert_bifprm_csv_to_netcdf.py"), "bifprm.csv", "bifprm.nc"],
            cwd=map_root,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("duplicate endpoint tuples", result.stderr)
        self.assertEqual(target.read_bytes(), original)

    def assert_csv_matches_legacy(self, csv_path: Path, legacy_path: Path, lonlat_path: Path) -> None:
        import numpy as np

        legacy_lines = legacy_path.read_text(encoding="ascii").splitlines()
        old_header = legacy_lines[0].split()
        npath, nlayer = map(int, old_header[:2])
        legacy_rows = [line.split() for line in legacy_lines[1:]]
        with csv_path.open(newline="", encoding="ascii") as stream:
            rows = list(csv.DictReader(stream))
        self.assertEqual(len(rows), npath)
        nx, ny = (int(line.split()[0]) for line in (SAMPLE / "params.txt").read_text().splitlines()[:2])
        lonlat = np.fromfile(lonlat_path, dtype="<f4").reshape(2, ny, nx)
        for index, (row, old) in enumerate(zip(rows, legacy_rows), 1):
            self.assertEqual(int(row["pth_id"]), index)
            for column, value in zip(("ix", "iy", "jx", "jy"), old[:4]):
                self.assertEqual(int(row[column]), int(value))
            numeric_fields = ["length", "elevtn", "depth"] + [f"width_{layer}" for layer in range(1, nlayer + 1)]
            old_numeric = old[4 : 7 + nlayer]
            for column, value in zip(numeric_fields, old_numeric):
                self.assertEqual(float(row[column]), float(value), column)
            self.assertEqual(float(row["start_lat"]), float(old[-4]))
            self.assertEqual(float(row["start_lon"]), float(old[-3]))
            self.assertEqual(int(row["ibsn"]), int(old[-2]))
            self.assertEqual(int(row["jbsn"]), int(old[-1]))
            jx, jy = int(row["jx"]), int(row["jy"])
            self.assertEqual(float(row["end_lon"]), float(lonlat[0, jy - 1, jx - 1]))
            self.assertEqual(float(row["end_lat"]), float(lonlat[1, jy - 1, jx - 1]))

    def assert_netcdf_matches_csv(self, csv_path: Path, nc_path: Path) -> None:
        import numpy as np
        from netCDF4 import Dataset

        with csv_path.open(newline="", encoding="ascii") as stream:
            rows = list(csv.DictReader(stream))
        nlayer = sum(name.startswith("width_") for name in rows[0])
        with Dataset(nc_path) as dataset:
            self.assertEqual(len(dataset.dimensions["path"]), len(rows))
            self.assertEqual(len(dataset.dimensions["layer"]), nlayer)
            for name in ("pth_id", "ix", "iy", "jx", "jy", "ibsn", "jbsn"):
                np.testing.assert_array_equal(dataset[name][:], [int(row[name]) for row in rows])
            for name in ("start_lat", "start_lon", "end_lat", "end_lon", "length", "elevtn", "depth"):
                np.testing.assert_array_equal(dataset[name][:], [np.float32(row[name]) for row in rows])
            for layer in range(1, nlayer + 1):
                np.testing.assert_array_equal(dataset["width"][layer - 1, :], [np.float32(row[f"width_{layer}"]) for row in rows])


if __name__ == "__main__":
    unittest.main(verbosity=2)
