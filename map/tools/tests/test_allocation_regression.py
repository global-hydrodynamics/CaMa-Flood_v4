"""Legacy-to-v5 behavior regression for the map allocation programs."""

import csv
import hashlib
import json
import shutil
import subprocess
import tempfile
import unittest
from decimal import Decimal
from pathlib import Path


TOOLS = Path(__file__).resolve().parents[1]
REPO = TOOLS.parents[1]
SAMPLE = REPO / "map/mekong_15min"
DATA = REPO / "map/data"
FIXTURES = TOOLS / "tests/data/allocation"
GOLDEN = FIXTURES / "legacy_gfortran_14_2"
FLAGS = ["-O3", "-Wall", "-cpp", "-ffree-line-length-none", "-fimplicit-none", "-ftree-vectorize"]


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def in_domain_rows(source, additions):
    with source.open(encoding="utf-8-sig", newline="") as stream:
        rows = list(csv.reader(stream))
    header, values = rows[0], rows[1:]
    selected = []
    for row in values:
        try:
            lat, lon = float(row[1]), float(row[2])
        except (IndexError, ValueError):
            continue
        if 90 <= lon <= 110 and 5 <= lat <= 35:
            selected.append(row)
    selected.extend(additions(len(header)))
    return [header, *selected]


def write_csv(path, rows):
    with path.open("w", encoding="utf-8", newline="") as stream:
        csv.writer(stream, lineterminator="\n").writerows(rows)


def compile_program(compiler, source, output, extra_sources=()):
    subprocess.run(
        [compiler, *FLAGS, *(str(item) for item in extra_sources), str(source), "-o", str(output)],
        check=True,
        capture_output=True,
        text=True,
        cwd=output.parent,
    )


def output_rows(path):
    lines = path.read_text().splitlines()
    return lines[0], [line.split() for line in lines[1:]]


def assert_output_equivalent(test, expected_path, actual_path):
    expected_header, expected = output_rows(expected_path)
    actual_header, actual = output_rows(actual_path)
    test.assertEqual(actual_header, expected_header)
    test.assertEqual(len(actual), len(expected), f"{actual_path}: expected {len(expected)} rows, got {len(actual)}")
    for row_number, (want, got) in enumerate(zip(expected, actual), start=1):
        test.assertEqual(len(got), len(want), f"field count in row {row_number}")
        for field_number, (a, b) in enumerate(zip(want, got), start=1):
            try:
                da, db = Decimal(a), Decimal(b)
            except Exception:
                test.assertEqual(b, a, f"row {row_number}, field {field_number}")
                continue
            if not da.is_finite() or not db.is_finite():
                test.assertEqual(b, a, f"row {row_number}, field {field_number}")
                continue
            exponent = min(da.as_tuple().exponent, db.as_tuple().exponent)
            tolerance = Decimal(5).scaleb(exponent - 1)
            test.assertLessEqual(abs(db - da), tolerance,
                                 f"row {row_number}, field {field_number}: {a} != {b}")


@unittest.skipUnless(shutil.which("gfortran"), "gfortran is unavailable")
class AllocationRegressionTest(unittest.TestCase):
    def test_legacy_baseline_and_v5_outputs_match(self):
        manifest = json.loads((GOLDEN / "manifest.json").read_text(encoding="utf-8"))
        for source_name, expected_hash in manifest["legacy_sources"].items():
            self.assertEqual(sha256(REPO / "map/src/src_param" / source_name), expected_hash,
                             f"legacy baseline source changed: {source_name}")

        with tempfile.TemporaryDirectory(prefix="cmf-allocation-regression-") as temp_name:
            temp = Path(temp_name)
            map_root = temp / "map"
            script_dir = map_root / "script_params"
            bin_dir = temp / "bin"
            script_dir.mkdir(parents=True)
            bin_dir.mkdir()
            (map_root / "1min").symlink_to(SAMPLE / "1min", target_is_directory=True)
            map_inputs = [SAMPLE / name for name in (
                "params.txt", "uparea.bin", "ctmare.bin", "nextxy.bin", "elevtn.bin"
            )]
            hires_inputs = [SAMPLE / "1min" / f"1min.{name}.bin" for name in (
                "uparea", "catmxy", "downxy", "elevtn"
            )]
            sample_hashes = {path: sha256(path) for path in [*map_inputs, *hires_inputs]}
            for name in ("params.txt", "uparea.bin", "ctmare.bin", "nextxy.bin", "elevtn.bin"):
                (map_root / name).symlink_to(SAMPLE / name)

            compiler = shutil.which("gfortran")
            legacy_dir = REPO / "map/src/src_param"
            tools_dir = TOOLS / "code"
            module_object = temp / "mod_allocation_map_io.o"
            subprocess.run([compiler, *FLAGS, "-c", str(tools_dir / "mod_allocation_map_io.F90"),
                            "-o", str(module_object)], check=True, capture_output=True, text=True, cwd=temp)
            programs = {}
            for stem in ("flow_gauge", "dam", "level_gauge"):
                legacy_name = "allocate_" + stem + ".F90"
                legacy_exe = bin_dir / ("legacy_" + stem)
                v5_exe = bin_dir / ("v5_" + stem)
                compile_program(compiler, legacy_dir / legacy_name, legacy_exe)
                compile_program(compiler, tools_dir / legacy_name, v5_exe, [module_object])
                programs[stem] = (legacy_exe, v5_exe)

            grdc = temp / "grdc.csv"
            write_csv(grdc, in_domain_rows(DATA / "GRDC_allocated.csv", lambda n: [[
                "99999999", "0", "0", "1", *([""] * max(0, n - 4))
            ]]))
            grand = temp / "grand.csv"

            def grand_additions(width):
                pad = lambda values: values + [""] * max(0, width - len(values))
                return [
                    pad(["99999998", "0", "0", "1", "SyntheticOutside", "SyntheticRiver", "100", "2000"]),
                    pad(["99999997", "10", "10", "100", "SyntheticNegativeCapacity", "SyntheticRiver", "-1", "2000"]),
                ]

            write_csv(grand, in_domain_rows(DATA / "GRanD_allocated.csv", grand_additions))
            level = FIXTURES / "level_synthetic_mekong.csv"

            def run(program, executable, arguments, produced):
                for old in ("gauge_alloc.txt", "dam_alloc_river.txt", "dam_alloc_small.txt", "dam_alloc_error.txt"):
                    (script_dir / old).unlink(missing_ok=True)
                result = subprocess.run([str(executable), *map(str, arguments)], cwd=script_dir,
                                        capture_output=True, text=True, errors="replace")
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                return [script_dir / item for item in produced]

            comparisons = []
            legacy, v5 = programs["flow_gauge"]
            for mode, golden in (("multi", "flow_multi.txt"), ("single", "flow_single.txt")):
                old_file = run("flow", legacy, (grdc, mode), ("gauge_alloc.txt",))[0]
                old_snapshot = temp / f"legacy_{golden}"
                shutil.copyfile(old_file, old_snapshot)
                new_file = run("flow", v5, (grdc, mode, "1min"), ("gauge_alloc.txt",))[0]
                assert_output_equivalent(self, GOLDEN / golden, old_snapshot)
                assert_output_equivalent(self, GOLDEN / golden, new_file)

            legacy, v5 = programs["dam"]
            products = ("dam_alloc_river.txt", "dam_alloc_small.txt", "dam_alloc_error.txt")
            old_files = run("dam", legacy, (grand,), products)
            old_snapshots = []
            for index, old_file in enumerate(old_files):
                snapshot = temp / f"legacy_dam_{index}.txt"
                shutil.copyfile(old_file, snapshot)
                old_snapshots.append(snapshot)
            new_files = run("dam", v5, (grand, "1min"), products)
            for golden, old_file, new_file in zip(("dam_river.txt", "dam_small.txt", "dam_error.txt"), old_snapshots, new_files):
                assert_output_equivalent(self, GOLDEN / golden, old_file)
                assert_output_equivalent(self, GOLDEN / golden, new_file)

            legacy, v5 = programs["level_gauge"]
            old_file = run("level", legacy, (level,), ("gauge_alloc.txt",))[0]
            old_snapshot = temp / "legacy_level_gauge.txt"
            shutil.copyfile(old_file, old_snapshot)
            new_file = run("level", v5, (level, "1min"), ("gauge_alloc.txt",))[0]
            assert_output_equivalent(self, GOLDEN / "level_gauge.txt", old_snapshot)
            assert_output_equivalent(self, GOLDEN / "level_gauge.txt", new_file)

            wrapper_root = temp / "wrapper_workspace"
            wrapper_map = wrapper_root / "map"
            wrapper_scripts = wrapper_map / "script_params"
            wrapper_tools = wrapper_root / "tools/code"
            wrapper_data = wrapper_root / "data"
            wrapper_scripts.mkdir(parents=True)
            wrapper_tools.mkdir(parents=True)
            wrapper_data.mkdir()
            (wrapper_map / "1min").symlink_to(SAMPLE / "1min", target_is_directory=True)
            for name in ("params.txt", "uparea.bin", "ctmare.bin", "nextxy.bin", "elevtn.bin"):
                (wrapper_map / name).symlink_to(SAMPLE / name)
            for source_exe, name in ((programs["flow_gauge"][1], "allocate_flow_gauge"),
                                     (programs["dam"][1], "allocate_dam"),
                                     (programs["level_gauge"][1], "allocate_level_gauge")):
                (wrapper_tools / name).symlink_to(source_exe)
            for source, name in ((grdc, "testgrdc.csv"), (grand, "testgrand.csv"), (level, "testlevel.csv")):
                shutil.copyfile(source, wrapper_data / name)
            wrapper_inputs = {
                "t01-alloc_flow_gauge.sh": ('GRDC_allocated.csv', 'testgrdc.csv'),
                "t02-alloc_dams.sh": ('GRanD_allocated.csv', 'testgrand.csv'),
                "t03-alloc_level_gauge.sh": ('MLIT_LevelGauge_allocated.csv', 'testlevel.csv'),
            }
            for script_name, (default_name, fixture_name) in wrapper_inputs.items():
                script = TOOLS / "script_params" / script_name
                text = script.read_text(encoding="utf-8").replace(default_name, fixture_name)
                copied = wrapper_scripts / script_name
                copied.write_text(text, encoding="utf-8")
                copied.chmod(0o755)
                result = subprocess.run([str(copied)], cwd=wrapper_root, capture_output=True,
                                        text=True, errors="replace")
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            for golden, output in (("flow_multi.txt", "GRDC_alloc.txt"),
                                   ("dam_river.txt", "GRanD_river.txt"),
                                   ("dam_small.txt", "GRanD_small.txt"),
                                   ("dam_error.txt", "GRanD_error.txt"),
                                   ("level_gauge.txt", "MLIT_levelgauge.txt")):
                assert_output_equivalent(self, GOLDEN / golden, wrapper_map / output)

            missing_tag = subprocess.run([str(programs["flow_gauge"][1]), str(grdc), "multi"],
                                         cwd=script_dir, capture_output=True, text=True, errors="replace")
            self.assertNotEqual(missing_tag.returncode, 0)
            self.assertIn("provide HIRES_TAG", missing_tag.stdout + missing_tag.stderr)

            unsupported = map_root / "15sec"
            unsupported.mkdir()
            (unsupported / "location.txt").write_text("2 narea\n", encoding="ascii")
            tiled = subprocess.run([str(programs["flow_gauge"][1]), str(grdc), "multi", "15sec"],
                                   cwd=script_dir, capture_output=True, text=True, errors="replace")
            self.assertNotEqual(tiled.returncode, 0)
            self.assertIn("only one high-resolution area is supported", tiled.stdout + tiled.stderr)

            self.assertEqual(sample_hashes, {path: sha256(path) for path in sample_hashes})


if __name__ == "__main__":
    unittest.main()
