from __future__ import annotations

import csv
import tempfile
import unittest
from pathlib import Path

from map.tools.code.generate_ctl import CtlError, generate_directory, missing_sentinel

FIELDS = [
    "category_id", "workflow_stage", "category", "product_kind", "binary_filename",
    "alternative_binary_filename", "name", "long_name", "units", "dtype",
    "dimensions", "record_layout", "missing_value_convention", "record_index",
    "record_count",
]


def row(filename: str, name: str, dtype: str, dimensions: str = "NX,NY", *,
        record_layout: str = "record 1", record_index: str = "1", record_count: str = "1",
        missing: str = "-9999 outside mapped cells") -> dict[str, str]:
    return dict(zip(FIELDS, ["11", "", "map", "binary", filename, "", name,
                             f"{name} test layer", "m", dtype, dimensions, record_layout,
                             missing, record_index, record_count]))


def write_registry(path: Path, rows: list[dict[str, str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(rows)


def write_params(path: Path, nx: int = 3, ny: int = 2, nlfp: int = 2) -> None:
    path.write_text(f"{nx}\n{ny}\n{nlfp}\n1.0\n-1.5\n1.5\n-1.0\n1.0\n", encoding="ascii")


class GenerateCtlTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.map_dir = self.root / "map"
        self.map_dir.mkdir()
        write_params(self.map_dir / "params.txt")
        self.registry = self.root / "map_variables.csv"

    def tearDown(self):
        self.tmp.cleanup()

    def run_generator(self, *, dry_run: bool = False, overwrite: bool = False):
        return generate_directory(self.map_dir, self.registry, dry_run=dry_run, overwrite=overwrite)

    def test_single_record_float_and_integer_coordinates_and_y_orientation(self):
        write_registry(self.registry, [row("elevtn.bin", "elevtn", "float32"), row("basin.bin", "basin", "int32")])
        (self.map_dir / "elevtn.bin").write_bytes(b"\0" * (3 * 2 * 4))
        (self.map_dir / "basin.bin").write_bytes(b"\0" * (3 * 2 * 4))
        result = self.run_generator()
        self.assertEqual(result.generated, 2)
        ctl = (self.map_dir / "elevtn.ctl").read_text()
        self.assertIn("options yrev little_endian", ctl)
        self.assertIn("xdef 3 linear -1 1", ctl)
        self.assertIn("ydef 2 linear -0.5 1", ctl)
        self.assertIn("elevtn 1 99", ctl)
        self.assertIn("elevtn test layer [m]", ctl)
        basin = (self.map_dir / "basin.ctl").read_text()
        self.assertIn("basin 1 -1,40,4,-1", basin)

    def test_independent_variables_and_multiple_layers(self):
        rows = [
            row("nextxy.bin", "nextx", "int32", record_index="1", record_count="2"),
            row("nextxy.bin", "nexty", "int32", record_layout="record 2", record_index="2", record_count="2"),
            row("fldhgt.bin", "fldhgt", "float32", dimensions="NX,NY,NLFP",
                record_layout="records 1..NLFP; record k is layer k", record_index="1", record_count="NLFP"),
        ]
        write_registry(self.registry, rows)
        (self.map_dir / "nextxy.bin").write_bytes(b"\0" * (3 * 2 * 2 * 4))
        (self.map_dir / "fldhgt.bin").write_bytes(b"\0" * (3 * 2 * 2 * 4))
        result = self.run_generator()
        self.assertEqual(result.generated, 2)
        nextxy = (self.map_dir / "nextxy.ctl").read_text()
        self.assertIn("vars 2", nextxy)
        self.assertLess(nextxy.index("nextx 1"), nextxy.index("nexty 1"))
        fldhgt = (self.map_dir / "fldhgt.ctl").read_text()
        self.assertIn("zdef 2 linear 1 1", fldhgt)
        self.assertIn("fldhgt 2 99", fldhgt)

    def test_high_resolution_uses_location_geometry_and_area_name(self):
        hires = self.map_dir / "1min"
        hires.mkdir()
        (hires / "location.txt").write_text(
            "2 narea\niarea carea west east south north nx ny csize\n"
            "1 otherarea 0 1 0 1 2 2 0.5\n"
            "2 testarea 10 12 20 21 4 2 0.5\n", encoding="ascii")
        filename = "1min/testarea.elevtn.bin"
        write_registry(self.registry, [row("<HIRES>/<AREA>.elevtn.bin", "elevtn_hires", "float32",
                                           "NX_HIRES,NY_HIRES (or NX_TILE,NY_TILE)")])
        (hires / "testarea.elevtn.bin").write_bytes(b"\0" * (4 * 2 * 4))
        result = generate_directory(hires, self.registry)
        self.assertEqual(result.generated, 1)
        ctl = (hires / "testarea.elevtn.ctl").read_text()
        self.assertIn("xdef 4 linear 10.25 0.5", ctl)
        self.assertIn("ydef 2 linear 20.25 0.5", ctl)
        self.assertNotIn("xdef 3 linear", ctl)

    def test_unknown_files_are_skipped_and_size_mismatch_is_not_written(self):
        write_registry(self.registry, [row("known.bin", "known", "float32")])
        (self.map_dir / "known.bin").write_bytes(b"bad")
        (self.map_dir / "mystery.bin").write_bytes(b"\0")
        result = self.run_generator()
        self.assertEqual((result.generated, result.skipped, result.failed), (0, 1, 1))
        self.assertFalse((self.map_dir / "known.ctl").exists())

    def test_input_matrix_uses_its_diminfo_descriptor_and_mixed_record_types(self):
        matrix = row(
            "inpmat_test-1deg.bin", "inpmat", "int32 + float32", "NX,NY,INPN",
            record_layout=("record 1: INPN x-index fields; record 2: INPN y-index fields; "
                           "record 3: INPN overlap-area fields (each record shape NX,NY,INPN)"),
            record_index="", record_count="",
            missing="Unused mapping slots are initialized to zero",
        )
        write_registry(self.registry, [matrix])
        (self.map_dir / "diminfo_test-1deg.txt").write_text(
            "3 !! nXX\n2 !! nYY\n2 !! floodplain layers\n360 !! input nXX\n"
            "180 !! input nYY\n4 !! input num\ninpmat_test-1deg.bin\n"
            "-1.5 !! west\n1.5 !! east\n1.0 !! north\n-1.0 !! south\n",
            encoding="ascii",
        )
        (self.map_dir / "inpmat_test-1deg.bin").write_bytes(b"\0" * (3 * 2 * 4 * 3 * 4))
        result = self.run_generator()
        self.assertEqual((result.generated, result.skipped, result.failed), (1, 0, 0))
        ctl = (self.map_dir / "inpmat_test-1deg.ctl").read_text()
        self.assertIn("undef 0", ctl)
        self.assertIn("zdef 4 linear 1 1", ctl)
        self.assertIn("vars 3", ctl)
        self.assertIn("inpx 4 -1,40,4,-1", ctl)
        self.assertIn("inpy 4 -1,40,4,-1", ctl)
        self.assertIn("inpa 4 99", ctl)

    def test_unknown_registry_dimensions_are_skipped_not_reported_as_grid_mismatch(self):
        write_registry(self.registry, [row("width.bin", "width", "float32", "dataset-dependent")])
        (self.map_dir / "width.bin").write_bytes(b"\0" * 24)
        result = self.run_generator()
        self.assertEqual((result.generated, result.skipped, result.failed), (0, 1, 0))

    def test_missing_metadata_is_skipped_and_overwrite_is_protected(self):
        write_registry(self.registry, [row("elevtn.bin", "elevtn", "float32", missing="")])
        (self.map_dir / "elevtn.bin").write_bytes(b"\0" * 24)
        result = self.run_generator()
        self.assertEqual(result.skipped, 1)
        self.assertFalse((self.map_dir / "elevtn.ctl").exists())
        write_registry(self.registry, [row("elevtn.bin", "elevtn", "float32")])
        self.assertEqual(self.run_generator(dry_run=True).generated, 1)
        self.assertFalse((self.map_dir / "elevtn.ctl").exists())
        self.assertEqual(self.run_generator().generated, 1)
        self.assertEqual(self.run_generator().skipped, 1)
        self.assertEqual(self.run_generator(overwrite=True).generated, 1)

    def test_missing_grid_metadata_skips_files(self):
        (self.map_dir / "elevtn.bin").write_bytes(b"\0" * 24)
        (self.map_dir / "params.txt").unlink()
        write_registry(self.registry, [row("elevtn.bin", "elevtn", "float32")])
        result = self.run_generator()
        self.assertEqual((result.generated, result.skipped, result.failed), (0, 1, 0))

    def test_csv_missing_convention_extracts_the_declared_grid_sentinel(self):
        self.assertEqual(missing_sentinel({"missing_value_convention": "-9999 for non-network cells and paths masked at the edge"}, "int32"), -9999)
        self.assertEqual(missing_sentinel({"missing_value_convention": "-9999 for cells where nextx is -9999"}, "int32"), -9999)
        with self.assertRaisesRegex(CtlError, "outside the range of int8"):
            missing_sentinel({"missing_value_convention": "-9999 fill value"}, "int8")

    def test_lsmask_ctl_uses_the_declared_minus9999_missing_value(self):
        write_registry(self.registry, [row(
            "lsmask.bin", "lsmask", "int32",
            missing="Missing value: -9999 for unfilled output",
        )])
        (self.map_dir / "lsmask.bin").write_bytes(b"\0" * 24)
        result = self.run_generator()
        self.assertEqual(result.generated, 1)
        self.assertIn("undef -9999", (self.map_dir / "lsmask.ctl").read_text())

    def test_lonlat_records_have_distinct_outlet_names_descriptions_and_units(self):
        rows = [
            row("lonlat.bin", "outlon", "float32", record_layout="record 1",
                record_index="1", record_count="2", missing="-9999 outside mapped river cells"),
            row("lonlat.bin", "outlat", "float32", record_layout="record 2",
                record_index="2", record_count="2", missing="-9999 outside mapped river cells"),
        ]
        rows[0].update(long_name="Unit-catchment outlet longitude", units="degrees_east")
        rows[1].update(long_name="Unit-catchment outlet latitude", units="degrees_north")
        write_registry(self.registry, rows)
        (self.map_dir / "lonlat.bin").write_bytes(b"\0" * (3 * 2 * 2 * 4))
        result = self.run_generator()
        self.assertEqual(result.generated, 1)
        ctl = (self.map_dir / "lonlat.ctl").read_text()
        self.assertIn("outlon 1 99 Unit-catchment outlet longitude [degrees_east]", ctl)
        self.assertIn("outlat 1 99 Unit-catchment outlet latitude [degrees_north]", ctl)

    def test_structured_two_record_layout_without_record_index(self):
        write_registry(self.registry, [row("downxy.bin", "downxy", "int32",
                                           record_layout="record 1 downx; record 2 downy",
                                           record_index="", record_count="")])
        (self.map_dir / "downxy.bin").write_bytes(b"\0" * 48)
        self.assertEqual(self.run_generator().generated, 1)
        ctl = (self.map_dir / "downxy.ctl").read_text()
        self.assertIn("vars 2", ctl)
        self.assertIn("downx 1 -1,40,4,-1", ctl)
        self.assertIn("downy 1 -1,40,4,-1", ctl)


if __name__ == "__main__":
    unittest.main()
