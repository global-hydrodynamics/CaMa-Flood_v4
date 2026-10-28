from __future__ import annotations

import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[4]
TOOL_ROOT = ROOT / "map" / "src"
sys.path.insert(0, str(TOOL_ROOT))
from map_data_tools.binary_to_netcdf import Dataset, convert
from map_data_tools.generate_ctl import generate
from map_data_tools.map_metadata import (
    MapDataError, grid_coordinates, load_core_data, read_core_specs,
    read_dataset_config, read_params, read_registry,
)

REGISTRY = ROOT / "map" / "map_variables.csv"


def write_map_fixture(folder: Path, *, byte_order: str = "little", row_order: str = "north_to_south") -> Path:
    folder.mkdir(parents=True, exist_ok=True)
    (folder / "params.txt").write_text(
        "3\n2\n2\n1.0\n-1.5\n1.5\n-1.0\n1.0\n", encoding="ascii"
    )
    # Each row is one y index; x varies fastest, matching Fortran direct records.
    arrays = {
        "nextxy.bin": (np.array([
            [[1, 2, 3], [1, 2, -9]],
            [[1, 2, 2], [2, 1, -9999]],
        ], dtype=np.int32), np.dtype("i4")),
        "ctmare.bin": (np.array([[[1.25, 2.5, 3.75], [4.5, -9999.0, 6.25]]], dtype=np.float32), np.dtype("f4")),
        "elevtn.bin": (np.array([[[10, 11, 12], [13, 14, -9999]]], dtype=np.float32), np.dtype("f4")),
        "nxtdst.bin": (np.array([[[20, 21, 22], [23, 24, -9999]]], dtype=np.float32), np.dtype("f4")),
        "rivlen.bin": (np.array([[[30, 31, 32], [33, 34, -9999]]], dtype=np.float32), np.dtype("f4")),
        "fldhgt.bin": (np.array([
            [[40, 41, 42], [43, 44, -9999]],
            [[50, 51, 52], [53, 54, -9999]],
        ], dtype=np.float32), np.dtype("f4")),
    }
    prefix = "<" if byte_order == "little" else ">"
    for filename, (data, dtype) in arrays.items():
        data.astype(dtype.newbyteorder(prefix), copy=False).tofile(folder / filename)
    config = {
        "schema_version": 1,
        "dataset_id": "synthetic_non_square",
        "byte_order": byte_order,
        "row_order": row_order,
        "coordinate_mode": "regular_lonlat_from_params",
        "files": {name: name for name in arrays},
        "missing_values": {name: -9999 for name in arrays},
    }
    config_path = folder / "map_metadata.json"
    config_path.write_text(json.dumps(config, indent=2) + "\n", encoding="utf-8")
    return config_path


class MapMetadataTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.map_dir = Path(self.temp.name) / "map"
        self.config = write_map_fixture(self.map_dir)
        self.groups = read_core_specs(read_registry(REGISTRY))
        self.params = read_params(self.map_dir)
        self.dataset = read_dataset_config(self.config, self.map_dir)

    def tearDown(self):
        self.temp.cleanup()

    def test_non_square_records_and_one_based_indices(self):
        arrays = load_core_data(self.map_dir, self.groups, self.params, self.dataset)
        self.assertEqual(arrays["nextx"].shape, (2, 3))
        self.assertEqual(int(arrays["nextx"][0, 0]), 1)
        self.assertEqual(int(arrays["nextx"][1, 2]), -9)
        self.assertEqual(int(arrays["nexty"][1, 2]), -9999)
        self.assertEqual(arrays["ctmare"].dtype, np.dtype("float32"))
        self.assertEqual(arrays["fldhgt"].shape, (2, 2, 3))
        self.assertEqual(float(arrays["fldhgt"][1, 1, 1]), 54.0)
        self.assertEqual(float(arrays["fldhgt"][1, 1, 2]), -9999.0)

    def test_endianness_and_row_orientation_are_explicit(self):
        arrays_little = load_core_data(self.map_dir, self.groups, self.params, self.dataset)
        big_dir = Path(self.temp.name) / "big"
        big_config = write_map_fixture(big_dir, byte_order="big", row_order="south_to_north")
        big_cfg = read_dataset_config(big_config, big_dir)
        big_data = load_core_data(big_dir, self.groups, read_params(big_dir), big_cfg)
        np.testing.assert_array_equal(arrays_little["elevtn"], big_data["elevtn"])
        _, north_lat = grid_coordinates(self.params, self.dataset)
        _, south_lat = grid_coordinates(read_params(big_dir), big_cfg)
        np.testing.assert_allclose(north_lat, [0.5, -0.5])
        np.testing.assert_allclose(south_lat, [-0.5, 0.5])

    def test_missing_file_and_wrong_size_raise_clear_errors(self):
        (self.map_dir / "rivlen.bin").unlink()
        with self.assertRaisesRegex(MapDataError, "Missing binary map file"):
            load_core_data(self.map_dir, self.groups, self.params, self.dataset)
        write_map_fixture(self.map_dir)
        with (self.map_dir / "ctmare.bin").open("ab") as stream:
            stream.write(b"x")
        with self.assertRaisesRegex(MapDataError, "size mismatch"):
            load_core_data(self.map_dir, self.groups, self.params, self.dataset)

    def test_ctl_structure_and_overwrite_guard(self):
        output = Path(self.temp.name) / "ctl"
        written = generate(self.map_dir, output, REGISTRY, self.config, False)
        self.assertEqual({p.name for p in written}, {"nextxy.ctl", "ctmare.ctl", "elevtn.ctl", "nxtdst.ctl", "rivlen.ctl", "fldhgt.ctl"})
        ctl = (output / "nextxy.ctl").read_text()
        self.assertIn("options yrev little_endian", ctl)
        self.assertIn("vars 2", ctl)
        self.assertLess(ctl.index("nextx "), ctl.index("nexty "))
        profile = (output / "fldhgt.ctl").read_text()
        self.assertIn("zdef 2 linear 1 1", profile)
        self.assertIn("fldhgt 2 99", profile)
        big_dir = Path(self.temp.name) / "big"
        big_config = write_map_fixture(big_dir, byte_order="big", row_order="south_to_north")
        big_output = Path(self.temp.name) / "big_ctl"
        generate(big_dir, big_output, REGISTRY, big_config, False)
        alternate = (big_output / "nextxy.ctl").read_text()
        self.assertIn("options big_endian", alternate)
        self.assertNotIn("yrev", alternate)
        with self.assertRaisesRegex(MapDataError, "Refusing to overwrite"):
            generate(self.map_dir, output, REGISTRY, self.config, False)

    @unittest.skipIf(Dataset is None, "netCDF4 is not installed; install map_data_tools/requirements.txt")
    def test_netcdf_variables_dimensions_values_and_fill_attributes(self):
        output = Path(self.temp.name) / "river_network.nc"
        convert(self.map_dir, output, REGISTRY, self.config, False)
        with Dataset(output) as nc:
            self.assertEqual(nc.variables["nextx"].dimensions, ("lat", "lon"))
            self.assertEqual(nc.variables["fldhgt"].dimensions, ("lfp", "lat", "lon"))
            self.assertEqual(nc.variables["nextx"].dtype, np.dtype("int32"))
            self.assertEqual(nc.variables["elevtn"].dtype, np.dtype("float32"))
            self.assertEqual(nc.variables["nextx"]._FillValue, -9999)
            self.assertEqual(nc.variables["elevtn"].missing_value, -9999.0)
            self.assertEqual(nc.variables["elevtn"].vertical_datum, "EGM96")
            np.testing.assert_array_equal(nc.variables["nextx"][:].data, [[1, 2, 3], [1, 2, -9]])
            np.testing.assert_array_equal(nc.variables["fldhgt"][:].data[1], [[50, 51, 52], [53, 54, -9999]])
            np.testing.assert_allclose(nc.variables["lon"][:], [-1, 0, 1])
            np.testing.assert_allclose(nc.variables["lat"][:], [0.5, -0.5])

    @unittest.skipIf(Dataset is None or not shutil.which("gfortran") or not shutil.which("nf-config") or not shutil.which("nc-config"),
                     "requires netCDF4 Python, gfortran and nf-config")
    def test_fortran_netcdf_reader_contract_smoke(self):
        nc_path = Path(self.temp.name) / "river_network.nc"
        convert(self.map_dir, nc_path, REGISTRY, self.config, False)
        source = Path(__file__).with_name("verify_reader_contract.f90")
        executable = Path(self.temp.name) / "verify_reader"
        flags = subprocess.check_output(["nf-config", "--fflags"], text=True).split()
        libs = subprocess.check_output(["nf-config", "--flibs"], text=True).split()
        c_libdir = subprocess.check_output(["nc-config", "--libdir"], text=True).strip()
        subprocess.run(["gfortran", *flags, str(source), f"-L{c_libdir}", *libs, "-o", str(executable)], check=True)
        subprocess.run([str(executable), str(nc_path)], check=True)


if __name__ == "__main__":
    unittest.main()
