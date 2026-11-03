from __future__ import annotations

import csv
import os
import pty
import select
import shutil
import struct
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

from map.tools.convert_map_binary_to_netcdf import generate_conversion_list

ROOT = Path(__file__).resolve().parents[3]
SCRIPT_SOURCE = ROOT / "map/mekong_15min/script_bin2nc"
SCRIPT_TEMPLATE = ROOT / "map/tools/script_bin2nc"
CONVERTER_SOURCE = ROOT / "map/tools/convert_map_binary_to_netcdf.py"

FIELDS = [
    "category_id",
    "category",
    "binary_filename",
    "dtype",
    "dimensions",
    "record_index",
    "record_count",
]


def registry_row(
    filename: str,
    category_id: str,
    category: str,
    dtype: str = "float32",
    dimensions: str = "NX,NY",
    record_index: str = "1",
    record_count: str = "1",
) -> dict[str, str]:
    return dict(
        zip(
            FIELDS,
            [category_id, category, filename, dtype, dimensions, record_index, record_count],
        )
    )


def write_registry(path: Path, rows: list[dict[str, str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(rows)


def make_workflow_tree(root: Path, *, use_converter_stub: bool) -> tuple[Path, Path]:
    map_root = root / "map"
    script_dir = map_root / "mekong_15min/script_bin2nc"
    tools_dir = map_root / "tools"
    map_dir = map_root / "mekong_15min"
    script_dir.mkdir(parents=True)
    tools_dir.mkdir()
    (map_root / "map_variables.csv").write_text("unused by the shell fixture\n", encoding="utf-8")
    for name in ("s01-list_binary.sh", "s02-convert_netcdf.sh"):
        shutil.copyfile(SCRIPT_SOURCE / name, script_dir / name)
    if use_converter_stub:
        converter = tools_dir / "convert_map_binary_to_netcdf.py"
        converter.write_text(
            """#!/usr/bin/env python3
import argparse
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--map-dir')
parser.add_argument('--registry')
parser.add_argument('--input')
parser.add_argument('--output')
parser.add_argument('--force', action='store_true')
args = parser.parse_args()
if not Path(args.input).is_file():
    raise SystemExit('input missing')
Path(args.output).parent.mkdir(parents=True, exist_ok=True)
Path(args.output).write_text('force' if args.force else 'converted')
print('Converted fixture:', Path(args.input).name)
""",
            encoding="utf-8",
        )
    else:
        shutil.copyfile(CONVERTER_SOURCE, tools_dir / CONVERTER_SOURCE.name)
    map_dir.mkdir(exist_ok=True)
    (map_dir / "params.txt").write_text("2\n2\n1\n1\n0\n2\n0\n2\n", encoding="ascii")
    return script_dir, map_dir


def run_tty(script: Path, cwd: Path, response: bytes, prompt: bytes) -> str:
    master, slave = pty.openpty()
    process = subprocess.Popen(
        ["sh", str(script)],
        cwd=cwd,
        stdin=slave,
        stdout=slave,
        stderr=slave,
        close_fds=True,
    )
    os.close(slave)
    output = bytearray()
    deadline = time.monotonic() + 10
    try:
        while prompt not in output:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError(f"prompt not shown; output was: {output.decode(errors='replace')}")
            ready, _, _ = select.select([master], [], [], remaining)
            if not ready:
                continue
            output.extend(os.read(master, 4096))
        os.write(master, response)
        while process.poll() is None:
            ready, _, _ = select.select([master], [], [], 0.2)
            if ready:
                try:
                    chunk = os.read(master, 4096)
                except OSError:
                    break
                if not chunk:
                    break
                output.extend(chunk)
        while True:
            ready, _, _ = select.select([master], [], [], 0.1)
            if not ready:
                break
            try:
                chunk = os.read(master, 4096)
            except OSError:
                break
            if not chunk:
                break
            output.extend(chunk)
    finally:
        os.close(master)
    process.wait(timeout=10)
    if process.returncode != 0:
        raise AssertionError(f"process exited {process.returncode}: {output.decode(errors='replace')}")
    return output.decode(errors="replace")


class BinaryToNetCDFWorkflowTests(unittest.TestCase):
    def test_map_local_scripts_match_shared_templates_except_map_directory(self):
        for name in ("s01-list_binary.sh", "s02-convert_netcdf.sh"):
            template = (SCRIPT_TEMPLATE / name).read_text(encoding="utf-8")
            sample = (SCRIPT_SOURCE / name).read_text(encoding="utf-8")
            template = template.replace(
                'MAP_DIR="$SCRIPT_DIR/../../mekong_15min"  # Change this to select another map directory.',
                'MAP_DIR="$SCRIPT_DIR/.."  # Map root containing params.txt and map Binary files.',
            )
            self.assertEqual(sample, template)

    def test_candidate_list_deduplicates_and_sorts_by_category_then_registry_order(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            map_dir = root / "mekong"
            map_dir.mkdir()
            for filename in ("network.bin", "elevation.bin", "late.bin", "matrix.bin", "unregistered.bin"):
                (map_dir / filename).touch()
            hires_dir = map_dir / "1min"
            hires_dir.mkdir()
            (hires_dir / "tile.bin").touch()
            rows = [
                registry_row("late.bin", "21", "parameters"),
                registry_row("network.bin", "11", "river network", "int32", record_count="2"),
                registry_row("network.bin", "11", "river network", "int32", record_index="2", record_count="2"),
                registry_row("late.bin", "11", "river network"),
                registry_row("elevation.bin", "11", "river network"),
                registry_row("matrix.bin", "15", "input matrix", "int32", "NX,NY,INPN"),
            ]
            registry = root / "map_variables.csv"
            write_registry(registry, rows)
            output = root / "bin2nc_list.txt"

            with tempfile.TemporaryFile(mode="w+") as log:
                from contextlib import redirect_stdout

                with redirect_stdout(log):
                    count = generate_conversion_list(map_dir, registry, output)
                log.seek(0)
                messages = log.read()

            self.assertEqual(count, 3)
            text = output.read_text()
            self.assertLess(text.index("# [11] river network"), text.index("network.bin"))
            self.assertLess(text.index("network.bin"), text.index("late.bin"))
            self.assertLess(text.index("late.bin"), text.index("elevation.bin"))
            self.assertNotIn("matrix.bin\n", text)
            self.assertNotIn("tile.bin", text)
            self.assertIn("unregistered.bin is not registered", messages)
            self.assertIn("matrix.bin is registered but not supported", messages)

    def test_s01_preserves_existing_list_without_a_terminal_and_regenerates_after_confirmation(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            script_dir, map_dir = make_workflow_tree(root, use_converter_stub=False)
            registry = map_dir.parent / "map_variables.csv"
            write_registry(registry, [registry_row("elevtn.bin", "11", "default_rivmap")])
            (map_dir / "elevtn.bin").write_bytes(b"\0" * 16)
            list_file = root / "bin2nc_list.txt"
            list_file.write_text("# maintainer selection\n", encoding="utf-8")
            script = script_dir / "s01-list_binary.sh"

            result = subprocess.run(
                ["sh", str(script)], cwd=root, input="", text=True, capture_output=True, check=True
            )
            self.assertIn("non-interactive mode", result.stdout)
            self.assertEqual(list_file.read_text(), "# maintainer selection\n")

            transcript = run_tty(script, root, b"y\n", b"Regenerate it? [y/N]:")
            self.assertIn("Generated 1 conversion candidates", transcript)
            self.assertIn("elevtn.bin", list_file.read_text())
            self.assertFalse((map_dir / "netcdf/bin2nc_list.txt").exists())

    def test_s02_ignores_comments_skips_noninteractive_overwrite_and_accepts_force(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            script_dir, map_dir = make_workflow_tree(root, use_converter_stub=True)
            (map_dir / "first.bin").write_text("first")
            (map_dir / "second.bin").write_text("second")
            netcdf_dir = map_dir / "netcdf"
            netcdf_dir.mkdir()
            (root / "bin2nc_list.txt").write_text(
                "# select files\nfirst.bin\n#second.bin\n\n", encoding="ascii"
            )
            script = script_dir / "s02-convert_netcdf.sh"
            output = netcdf_dir / "first.nc"

            result = subprocess.run(
                ["sh", str(script)], cwd=root, text=True, input="", capture_output=True, check=True
            )
            self.assertIn("Converted fixture: first.bin", result.stdout)
            self.assertTrue(output.is_file())
            self.assertFalse((netcdf_dir / "second.nc").exists())

            output.write_text("keep")
            result = subprocess.run(
                ["sh", str(script)], cwd=root, text=True, input="", capture_output=True, check=True
            )
            self.assertIn("non-interactive mode", result.stdout)
            self.assertEqual(output.read_text(), "keep")

            subprocess.run(["sh", str(script), "--force"], cwd=root, check=True, capture_output=True)
            self.assertEqual(output.read_text(), "force")

    def test_s02_interactive_overwrite_reprompts_and_reads_input_separately_from_list(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            script_dir, map_dir = make_workflow_tree(root, use_converter_stub=True)
            (map_dir / "first.bin").write_text("first")
            netcdf_dir = map_dir / "netcdf"
            netcdf_dir.mkdir()
            (root / "bin2nc_list.txt").write_text("first.bin\n", encoding="ascii")
            output = netcdf_dir / "first.nc"
            output.write_text("old")

            transcript = run_tty(
                script_dir / "s02-convert_netcdf.sh",
                root,
                b"maybe\ny\n",
                b"Overwrite? [y/N]:",
            )
            self.assertIn("Please answer y or N", transcript)
            self.assertEqual(output.read_text(), "converted")

    def test_s02_rejects_missing_and_invalid_list_entries(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            script_dir, map_dir = make_workflow_tree(root, use_converter_stub=True)
            netcdf_dir = map_dir / "netcdf"
            netcdf_dir.mkdir()
            list_file = root / "bin2nc_list.txt"
            script = script_dir / "s02-convert_netcdf.sh"
            list_file.write_text("missing.bin\n", encoding="ascii")
            result = subprocess.run(["sh", str(script)], cwd=root, text=True, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("listed Binary file does not exist", result.stderr)

            list_file.write_text("../outside.bin\n", encoding="ascii")
            result = subprocess.run(["sh", str(script)], cwd=root, text=True, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("invalid or non-base filename", result.stderr)

    def test_s02_converts_real_multirecord_and_layered_rasters(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            script_dir, map_dir = make_workflow_tree(root, use_converter_stub=False)
            shutil.copyfile(ROOT / "map/map_variables.csv", map_dir.parent / "map_variables.csv")
            (map_dir / "params.txt").write_text("2\n2\n2\n1\n0\n2\n0\n2\n", encoding="ascii")
            (map_dir / "nextxy.bin").write_bytes(
                struct.pack("<8i", 2, 0, 2, -9999, 1, 2, 0, -9999)
            )
            (map_dir / "fldhgt.bin").write_bytes(
                struct.pack("<8f", 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
            )
            (map_dir / "elevtn.bin").write_bytes(struct.pack("<4f", 10.0, 20.0, 30.0, 40.0))
            netcdf_dir = map_dir / "netcdf"
            netcdf_dir.mkdir()
            (script_dir / "bin2nc_list.txt").write_text(
                "# selected files\nnextxy.bin\nfldhgt.bin\n#elevtn.bin\n", encoding="ascii"
            )

            result = subprocess.run(
                ["sh", str(script_dir / "s02-convert_netcdf.sh")],
                cwd=script_dir,
                text=True,
                capture_output=True,
            )
            if "NetCDF C library not found" in result.stderr:
                self.skipTest(result.stderr.strip())
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertTrue((netcdf_dir / "nextxy.nc").is_file())
            self.assertTrue((netcdf_dir / "fldhgt.nc").is_file())
            self.assertFalse((netcdf_dir / "elevtn.nc").exists())
            self.assertIn("Converted and verified nextx, nexty", result.stdout)
            self.assertIn("Converted and verified fldhgt", result.stdout)


if __name__ == "__main__":
    unittest.main()
