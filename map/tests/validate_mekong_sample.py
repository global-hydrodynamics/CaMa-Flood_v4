#!/usr/bin/env python3
"""Validate the committed Mekong map sample and its registered binary products."""
from __future__ import annotations

import csv
import struct
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MAP = ROOT / "map" / "mekong_15min"
REGISTRY = ROOT / "map" / "map_variables.csv"
DTYPE_BYTES = {"int8": 1, "int16": 2, "int32": 4, "float32": 4}


def read_map_dimensions() -> dict[str, int]:
    """Read base-map dimensions from the first three params.txt records."""
    values = [int(line.split()[0]) for line in (MAP / "params.txt").read_text().splitlines()[:3]]
    return dict(zip(("NX", "NY", "NLFP"), values, strict=True))


def read_input_dimensions(path: Path) -> dict[str, int]:
    """Read runoff-grid dimensions and INPN from a v5 inpdim descriptor."""
    lines = path.read_text().splitlines()
    return {
        "INPNX": int(lines[3].split()[0]),
        "INPNY": int(lines[4].split()[0]),
        "INPN": int(lines[5].split()[0]),
    }


def registry_rows() -> list[dict[str, str]]:
    with REGISTRY.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def rows_for_sample_file(path: Path, rows: list[dict[str, str]]) -> list[dict[str, str]]:
    """Resolve exact map filenames and the registry's high-resolution templates."""
    matches = []
    for row in rows:
        filename = row["binary_filename"]
        if filename == path.name:
            matches.append(row)
            continue
        prefix = "<HIRES>/<AREA>."
        if filename.startswith(prefix) and path.parent.name == "1min":
            suffix = filename[len(prefix):]
            if path.name == f"{path.parent.name}.{suffix}":
                matches.append(row)
    return matches


class MekongSampleTests(unittest.TestCase):
    """Check current sample products without freezing the full generated inventory."""

    def test_standard_runoff_pair_uses_inpdim_name_and_matching_geometry(self) -> None:
        dims = read_map_dimensions()
        descriptor = MAP / "inpdim_test-1deg.txt"
        lines = descriptor.read_text().splitlines()
        input_dims = read_input_dimensions(descriptor)

        self.assertEqual(dims, {"NX": 80, "NY": 120, "NLFP": 10})
        self.assertEqual(input_dims, {"INPNX": 360, "INPNY": 180, "INPN": 4})
        self.assertEqual([float(lines[i].split()[0]) for i in (7, 8, 9, 10)], [90.0, 110.0, 35.0, 5.0])
        self.assertEqual(lines[6].strip(), "inpmat_test-1deg.bin")

        expected_bytes = dims["NX"] * dims["NY"] * input_dims["INPN"] * (4 + 4 + 4)
        self.assertEqual((MAP / lines[6].strip()).stat().st_size, expected_bytes)

    def test_user_configured_runoff_pair_uses_same_inpdim_convention(self) -> None:
        descriptor = MAP / "inpdim_test-15min_nc.txt"
        lines = descriptor.read_text().splitlines()
        input_dims = read_input_dimensions(descriptor)

        self.assertEqual(input_dims, {"INPNX": 1440, "INPNY": 600, "INPN": 10})
        self.assertEqual(lines[6].strip(), "inpmat_test-15min_nc.bin")
        expected_bytes = 80 * 120 * input_dims["INPN"] * (4 + 4 + 4)
        self.assertEqual((MAP / lines[6].strip()).stat().st_size, expected_bytes)

    def test_core_multirecord_and_multilayer_files(self) -> None:
        dims = read_map_dimensions()
        cells = dims["NX"] * dims["NY"]
        expected = {
            "nextxy.bin": 2 * cells * 4,
            "nextxy_noedge.bin": 2 * cells * 4,
            "downxy.bin": 2 * cells * 4,
            "lonlat.bin": 2 * cells * 4,
            "fldhgt.bin": dims["NLFP"] * cells * 4,
        }
        for name, size in expected.items():
            with self.subTest(name=name):
                self.assertEqual((MAP / name).stat().st_size, size)

        self.assertEqual(len(struct.unpack(f"<{2*cells}i", (MAP / "nextxy.bin").read_bytes())), 2 * cells)
        self.assertEqual(len(struct.unpack(f"<{dims['NLFP']*cells}f", (MAP / "fldhgt.bin").read_bytes())), dims["NLFP"] * cells)

    def test_required_base_and_high_resolution_products_are_present(self) -> None:
        required_base = {
            "nextxy.bin", "ctmare.bin", "elevtn.bin", "nxtdst.bin", "rivlen.bin", "fldhgt.bin",
            "basin.bin", "width.bin", "rivwth.bin", "rivhgt.bin", "rivman.bin",
            "inpmat_test-1deg.bin", "inpdim_test-1deg.txt",
        }
        actual_base = {path.name for path in MAP.iterdir() if path.is_file()}
        self.assertTrue(required_base <= actual_base, f"missing base-map products: {sorted(required_base - actual_base)}")

        required_high = {
            "1min.catmxy.bin", "1min.downxy.bin", "1min.elevtn.bin", "1min.uparea.bin",
        }
        actual_high = {path.name for path in (MAP / "1min").glob("*.bin")}
        self.assertTrue(required_high <= actual_high, f"missing high-resolution products: {sorted(required_high - actual_high)}")

    def test_sample_binary_sizes_match_registry_types_and_dimensions(self) -> None:
        dims = read_map_dimensions()
        location_lines = (MAP / "1min" / "location.txt").read_text().splitlines()
        area = location_lines[2].split()
        dims["NX_HIRES"] = int(area[6])
        dims["NY_HIRES"] = int(area[7])

        rows = registry_rows()
        binary_paths = sorted(MAP.rglob("*.bin"))
        self.assertTrue(binary_paths, "no binary map products found")
        for path in binary_paths:
            relative = path.relative_to(MAP)
            with self.subTest(path=str(relative)):
                self.assertGreater(path.stat().st_size, 0)
                self.assertLess(path.stat().st_size, 100 * 1024 * 1024)

                matches = rows_for_sample_file(path, rows)
                self.assertTrue(matches, f"no map_variables.csv entry for {relative}")
                expected_size = 0
                for row in matches:
                    dtype = row["dtype"].strip().lower()
                    self.assertIn(dtype, DTYPE_BYTES, f"unsupported dtype {dtype!r} for {relative}")
                    variables = [name.strip() for name in row["dimensions"].split(",") if name.strip()]
                    product = 1
                    for variable in variables:
                        if variable == "INPN":
                            descriptor = MAP / path.name.replace("inpmat_", "inpdim_").replace(".bin", ".txt")
                            self.assertTrue(descriptor.is_file(), f"paired input descriptor missing: {descriptor.name}")
                            variable_size = read_input_dimensions(descriptor)["INPN"]
                        else:
                            self.assertIn(variable, dims, f"unresolved dimension {variable!r} for {relative}")
                            variable_size = dims[variable]
                        product *= variable_size
                    expected_size += product * DTYPE_BYTES[dtype]
                self.assertEqual(path.stat().st_size, expected_size, f"{relative}: registry size mismatch")

    def test_required_text_metadata_files_exist(self) -> None:
        for name in ("params.txt", "bifori.txt", "inpdim_test-1deg.txt", "inpdim_test-15min_nc.txt"):
            with self.subTest(path=name):
                self.assertTrue((MAP / name).is_file())

        location = (MAP / "1min" / "location.txt").read_text()
        self.assertIn("1200", location)
        self.assertIn("1800", location)


if __name__ == "__main__":
    unittest.main(verbosity=2)
