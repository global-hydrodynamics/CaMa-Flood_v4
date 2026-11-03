#!/usr/bin/env python3
"""Generate GrADS CTL files for CaMa-Flood Binary restart maps."""

from __future__ import annotations

import argparse
import csv
import math
import re
import struct
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path


class RestartCtlError(Exception):
    """An input or output condition prevents reliable CTL generation."""


@dataclass(frozen=True)
class MapGeometry:
    nx: int
    ny: int
    resolution: float
    west: float
    south: float


GRADS_DESCRIPTIONS = {
    "rivsto": "River storage",
    "fldsto": "Floodplain storage",
    "rivout_pre": "Previous river outflow",
    "fldout_pre": "Previous floodplain outflow",
    "rivdph_pre": "Previous river depth",
    "fldsto_pre": "Previous floodplain storage",
    "gdwsto": "Groundwater storage",
    "damsto": "Reservoir storage",
    "levsto": "Levee-side storage",
}


def strip_comment(line: str) -> str:
    """Remove a Fortran ! comment outside quoted strings."""
    quote = None
    for index, char in enumerate(line):
        if quote:
            if char == quote:
                quote = None
        elif char in "'\"":
            quote = char
        elif char == "!":
            return line[:index]
    return line


def read_namelist(path: Path) -> dict[str, str]:
    assignments = {}
    pattern = re.compile(r"^\s*([A-Za-z][A-Za-z0-9_]*)\s*=\s*(.*?)\s*,?\s*$")
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise RestartCtlError(f"Cannot read namelist {path}: {exc}") from exc

    for raw_line in lines:
        match = pattern.match(strip_comment(raw_line))
        if match:
            value = match.group(2).strip()
            if value.endswith("/"):
                value = value[:-1].rstrip()
            if len(value) >= 2 and value[0] in "'\"" and value[-1] == value[0]:
                quote = value[0]
                value = value[1:-1].replace(quote * 2, quote)
            assignments[match.group(1).upper()] = value
    return assignments


def logical(values: dict[str, str], key: str, default: bool = False) -> bool:
    value = values.get(key)
    if value is None:
        return default
    value = value.strip().upper()
    if value in {".TRUE.", "TRUE", "T"}:
        return True
    if value in {".FALSE.", "FALSE", "F"}:
        return False
    raise RestartCtlError(f"Namelist {key} must be a Fortran logical, got {value!r}.")


def required(values: dict[str, str], key: str, default: str | None = None) -> str:
    value = values.get(key, default)
    if value is None or not value.strip():
        raise RestartCtlError(f"Required namelist value {key} is missing.")
    return value.strip()


def find_params(run_dir: Path, values: dict[str, str]) -> Path:
    for key in ("CNEXTXY", "CGRAREA", "CDIMINFO"):
        value = values.get(key)
        if not value:
            continue
        path = Path(value)
        if not path.is_absolute():
            path = run_dir / path
        directory = path if path.is_dir() else path.parent
        for parent in (directory, *directory.parents):
            candidate = parent / "params.txt"
            if candidate.is_file():
                return candidate
    raise RestartCtlError("Could not locate map params.txt using CNEXTXY, CGRAREA, or CDIMINFO.")


def read_geometry(path: Path) -> MapGeometry:
    try:
        tokens = [line.split()[0] for line in path.read_text(encoding="utf-8").splitlines() if line.split()]
        nx, ny = int(tokens[0]), int(tokens[1])
        resolution = float(tokens[3])
        west, south = float(tokens[4]), float(tokens[6])
    except (OSError, IndexError, ValueError) as exc:
        raise RestartCtlError(f"Cannot read grid dimensions and bounds from {path}: {exc}") from exc
    if nx <= 0 or ny <= 0 or resolution <= 0:
        raise RestartCtlError(f"Grid dimensions and resolution must be positive in {path}.")
    return MapGeometry(nx, ny, resolution, west, south)


def read_registry(path: Path) -> list[dict[str, str]]:
    required_columns = {
        "name", "long_name", "units", "binary_dtype", "binary_order",
        "binary_file", "condition",
    }
    try:
        with path.open(newline="", encoding="utf-8") as stream:
            reader = csv.DictReader(stream)
            missing = required_columns - set(reader.fieldnames or [])
            if missing:
                raise RestartCtlError("Restart registry missing columns: " + ", ".join(sorted(missing)))
            rows = list(reader)
    except (OSError, csv.Error) as exc:
        raise RestartCtlError(f"Cannot read restart registry {path}: {exc}") from exc
    return rows


def selected_variables(rows: list[dict[str, str]], storage_only: bool, options: dict[str, bool]) -> list[dict[str, str]]:
    selected = []
    for row in rows:
        if row["binary_file"].strip() != "main .bin":
            continue
        condition = row["condition"].strip().lower()
        if condition == "always":
            include = True
        elif condition == "not lstoonly":
            include = not storage_only
        elif condition in options:
            include = options[condition]
        else:
            raise RestartCtlError(f"Unknown Restart condition {row['condition']!r} for {row['name']}.")
        if include:
            selected.append(row)
    try:
        return sorted(selected, key=lambda row: int(row["binary_order"]))
    except ValueError as exc:
        raise RestartCtlError("Main-file binary_order values in restart_variables.csv must be integers.") from exc


def restart_time(path: Path, prefix: str) -> datetime:
    stem = path.name
    if not stem.startswith(prefix) or not stem.endswith(".bin"):
        raise RestartCtlError(f"Expected a Binary restart named {prefix}<YYYYMMDDHH>.bin: {path.name}")
    stamp = stem[len(prefix):-4]
    if not re.fullmatch(r"\d{10}", stamp):
        raise RestartCtlError(f"Restart filename does not contain a YYYYMMDDHH timestamp: {path.name}")
    try:
        return datetime.strptime(stamp, "%Y%m%d%H")
    except ValueError as exc:
        raise RestartCtlError(f"Invalid restart timestamp in {path.name}: {exc}") from exc


def endian_name(byte_order: str) -> str:
    return "little_endian" if byte_order == "<" else "big_endian"


def write_float32_preview(source: Path, target: Path, cells: int, byte_order: str, record_count: int) -> None:
    """Convert a Float64 restart to a separate Float32 file for GrADS."""
    source_format = f"{byte_order}{cells}d"
    target_format = f"{byte_order}{cells}f"
    record_bytes = cells * 8
    temporary = target.with_name(target.name + ".tmp")
    try:
        with source.open("rb") as source_stream, temporary.open("wb") as target_stream:
            for _ in range(record_count):
                data = source_stream.read(record_bytes)
                values = struct.unpack(source_format, data)
                try:
                    target_stream.write(struct.pack(target_format, *values))
                except (OverflowError, struct.error) as exc:
                    raise RestartCtlError(f"Cannot convert Float64 values to GrADS Float32: {exc}") from exc
        temporary.replace(target)
    finally:
        if temporary.exists():
            temporary.unlink()


def write_ctl(
    ctl_path: Path,
    data_path: Path,
    variables: list[dict[str, str]],
    geometry: MapGeometry,
    timestamp: datetime,
    undef: str,
    endian: str,
    title: str,
) -> None:
    resolution = geometry.resolution
    x_start = geometry.west + resolution / 2
    y_start = geometry.south + resolution / 2
    month = ("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")[timestamp.month - 1]
    grads_date = f"{timestamp.hour:02d}Z{timestamp.day:02d}{month}{timestamp.year:04d}"
    lines = [
        f"DSET ^{data_path.name}",
        f"TITLE {title}",
        f"UNDEF {undef}",
        f"OPTIONS yrev {endian_name(endian)}",
        f"XDEF {geometry.nx} LINEAR {x_start:.10g} {resolution:.10g}",
        f"YDEF {geometry.ny} LINEAR {y_start:.10g} {resolution:.10g}",
        "ZDEF 1 LINEAR 1 1",
        f"TDEF 1 LINEAR {grads_date} 1hr",
        f"VARS {len(variables)}",
    ]
    for row in variables:
        units = " ".join(row["units"].split())
        description = GRADS_DESCRIPTIONS[row["name"]]
        description = f"{description} [{units}]"
        lines.append(f"{row['name']} 1 99 {description}")
    lines.append("ENDVARS")
    ctl_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def generate(args: argparse.Namespace) -> int:
    run_dir = args.run_dir.resolve()
    namelist_path = args.namelist.resolve() if args.namelist else run_dir / "input_cmf.nam"
    registry_path = args.registry.resolve()
    values = read_namelist(namelist_path)
    if logical(values, "LRESTCDF"):
        raise RestartCtlError("LRESTCDF=.TRUE.; this tool handles Binary Restart files only.")

    storage_only = logical(values, "LSTOONLY")
    options = {
        "lgdwdly": logical(values, "LGDWDLY"),
        "ldamout": logical(values, "LDAMOUT"),
        "llevee": logical(values, "LLEVEE"),
    }
    variables = selected_variables(read_registry(registry_path), storage_only, options)
    if not variables:
        raise RestartCtlError("No main-file Restart variables were selected from the registry.")

    geometry = read_geometry(find_params(run_dir, values))
    prefix = required(values, "CVNREST", "restart")
    suffix = required(values, "CSUFBIN", ".bin")
    if suffix != ".bin":
        raise RestartCtlError(f"Binary Restart suffix must be .bin, got {suffix!r}.")
    restart_dir = Path(required(values, "CRESTDIR", "./"))
    if not restart_dir.is_absolute():
        restart_dir = run_dir / restart_dir
    restart_paths = [args.restart.resolve()] if args.restart else sorted(restart_dir.glob(f"{prefix}??????????.bin"))
    if not restart_paths:
        raise RestartCtlError(f"No {prefix}<YYYYMMDDHH>.bin restart files found in {restart_dir}.")

    double_precision = logical(values, "LRESTDBL", True)
    endian = {"native": "<" if sys.byteorder == "little" else ">", "little": "<", "big": ">"}[args.endian]
    bytes_per_value = 8 if double_precision else 4
    expected_size = geometry.nx * geometry.ny * bytes_per_value * len(variables)
    undef = values.get("DMIS", "1.0E20").strip()
    try:
        if not math.isfinite(float(undef.replace("D", "E").replace("d", "e"))):
            raise ValueError
    except ValueError as exc:
        raise RestartCtlError(f"DMIS must be a finite numeric value, got {undef!r}.") from exc

    print(f"Run directory: {run_dir}")
    print(f"Map grid: {geometry.nx} x {geometry.ny}")
    print(f"Restart fields: {', '.join(row['name'] for row in variables)}")
    for restart in restart_paths:
        if not restart.is_file():
            raise RestartCtlError(f"Restart file not found: {restart}")
        timestamp = restart_time(restart, prefix)
        size = restart.stat().st_size
        if size != expected_size:
            raise RestartCtlError(
                f"{restart.name}: {size} bytes; expected {expected_size} bytes for "
                f"{len(variables)} fields of {geometry.nx} x {geometry.ny} Float{bytes_per_value * 8}."
            )

        data_path = restart
        title = f"CaMa-Flood Binary Restart ({timestamp:%Y-%m-%d %H}:00)"
        if double_precision:
            data_path = restart.with_name(restart.stem + ".grads32.bin")
            write_float32_preview(restart, data_path, geometry.nx * geometry.ny, endian, len(variables))
            title += " - Float32 visualization copy of Float64 data"
            print(f"[INFO] GrADS reads 32-bit floating-point binary; wrote visualization copy {data_path.name}.")
        ctl_path = restart.with_suffix(".ctl")
        write_ctl(ctl_path, data_path, variables, geometry, timestamp, undef, endian, title)
        print(f"[SUCCESS] {ctl_path}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Generate GrADS CTLs for CaMa-Flood Binary Restart files.")
    parser.add_argument("--run-dir", type=Path, required=True, help="Directory containing input_cmf.nam")
    parser.add_argument("--restart", type=Path, help="Generate a CTL for one specific Binary Restart")
    parser.add_argument("--namelist", type=Path, help="Namelist path (default: RUN_DIR/input_cmf.nam)")
    parser.add_argument(
        "--registry",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "src_main" / "restart_variables.csv",
        help="Restart variable registry (default: src_main/restart_variables.csv)",
    )
    parser.add_argument(
        "--endian", choices=("native", "little", "big"), default="native",
        help="Byte order of the direct-access restart (default: native host order).",
    )
    args = parser.parse_args()
    try:
        return generate(args)
    except RestartCtlError as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
