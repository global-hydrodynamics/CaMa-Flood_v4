#!/usr/bin/env python3
"""Generate GrADS CTL files for CaMa-Flood gridded binary outputs."""

from __future__ import annotations

import argparse
import csv
import math
import re
import sys
import tempfile
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path


class CtlError(Exception):
    """An input or output condition prevents reliable CTL generation."""


@dataclass(frozen=True)
class MapGeometry:
    nx: int
    ny: int
    resolution: float
    west: float
    east: float
    south: float
    north: float


@dataclass(frozen=True)
class RunConfig:
    start: datetime
    end: datetime
    output_hours: int
    names: list[str]
    tag: str
    output_dir: Path
    binary_output: bool
    vector_output: bool
    dt_seconds: int


def strip_fortran_comment(line: str) -> str:
    """Remove a Fortran ! comment, except when ! is inside a quoted string."""
    quote = None
    result = []
    i = 0
    while i < len(line):
        char = line[i]
        if quote:
            result.append(char)
            if char == quote:
                if i + 1 < len(line) and line[i + 1] == quote:
                    result.append(line[i + 1])
                    i += 1
                else:
                    quote = None
        elif char in "'\"":
            quote = char
            result.append(char)
        elif char == "!":
            break
        else:
            result.append(char)
        i += 1
    return "".join(result)


def parse_namelist_value(value: str) -> str:
    value = value.strip()
    if value.endswith("/"):
        value = value[:-1].rstrip()
    if value.endswith(","):
        value = value[:-1].rstrip()
    if len(value) >= 2 and value[0] in "'\"" and value[-1] == value[0]:
        quote = value[0]
        return value[1:-1].replace(quote + quote, quote)
    return value


def read_namelist(path: Path) -> dict[str, str]:
    """Read scalar assignments used by the standard CaMa-Flood namelist."""
    assignments: dict[str, str] = {}
    assignment = re.compile(r"^\s*([A-Za-z][A-Za-z0-9_]*)\s*=\s*(.*?)\s*,?\s*$")
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise CtlError(f"Cannot read namelist {path}: {exc}") from exc

    for raw_line in lines:
        line = strip_fortran_comment(raw_line)
        match = assignment.match(line)
        if match:
            assignments[match.group(1).upper()] = parse_namelist_value(match.group(2))
    return assignments


def required(assignments: dict[str, str], key: str) -> str:
    value = assignments.get(key)
    if value is None or not value.strip():
        raise CtlError(f"Required namelist value {key} is missing.")
    return value.strip()


def parse_logical(assignments: dict[str, str], key: str) -> bool:
    value = required(assignments, key).upper()
    if value in {".TRUE.", "TRUE", "T"}:
        return True
    if value in {".FALSE.", "FALSE", "F"}:
        return False
    raise CtlError(f"Namelist value {key} must be a Fortran logical, got {value!r}.")


def parse_datetime(assignments: dict[str, str], prefix: str) -> datetime:
    try:
        return datetime(
            int(required(assignments, prefix + "YEAR")),
            int(required(assignments, "SMON" if prefix == "S" else "EMON")),
            int(required(assignments, "SDAY" if prefix == "S" else "EDAY")),
            int(required(assignments, prefix + "HOUR")),
        )
    except ValueError as exc:
        raise CtlError(f"Invalid {prefix} start/end date in input_cmf.nam: {exc}") from exc


def read_run_config(run_dir: Path, namelist_path: Path) -> tuple[RunConfig, dict[str, str]]:
    values = read_namelist(namelist_path)
    start = parse_datetime(values, "S")
    end = parse_datetime(values, "E")
    if end <= start:
        raise CtlError("Simulation end time must be later than start time.")

    try:
        output_hours = int(required(values, "IFRQ_OUT"))
        dt_number = float(required(values, "DT").replace("D", "E").replace("d", "e"))
    except ValueError as exc:
        raise CtlError(f"IFRQ_OUT and DT must be numeric: {exc}") from exc
    if output_hours < 1 or output_hours > 24:
        raise CtlError(f"IFRQ_OUT must be between 1 and 24 hours, got {output_hours}.")
    if not math.isfinite(dt_number) or dt_number < 60 or not dt_number.is_integer():
        raise CtlError("DT must be a finite whole number of seconds and at least 60 seconds.")
    dt_seconds = int(dt_number)
    if dt_seconds % 60:
        raise CtlError("DT must be an integer number of minutes for the model's time counter.")

    names = [name.strip() for name in required(values, "CVARSOUT").split(",") if name.strip()]
    if not names:
        raise CtlError("CVARSOUT contains no output variables.")

    output_value = required(values, "COUTDIR")
    output_path = Path(output_value)
    if not output_path.is_absolute():
        output_path = run_dir / output_path

    config = RunConfig(
        start=start,
        end=end,
        output_hours=output_hours,
        names=names,
        tag=required(values, "COUTTAG"),
        output_dir=output_path.resolve(),
        binary_output=not parse_logical(values, "LOUTCDF"),
        vector_output=parse_logical(values, "LOUTVEC"),
        dt_seconds=dt_seconds,
    )
    return config, values


def find_params_file(run_dir: Path, values: dict[str, str]) -> Path:
    """Find map params.txt through the namelist's map and dimension paths."""
    candidates = []
    for key in ("CNEXTXY", "CGRAREA", "CDIMINFO"):
        raw = values.get(key)
        if raw:
            path = Path(raw)
            if not path.is_absolute():
                path = run_dir / path
            candidates.append(path.resolve())

    for candidate in candidates:
        directory = candidate if candidate.is_dir() else candidate.parent
        for parent in (directory, *directory.parents):
            params = parent / "params.txt"
            if params.is_file():
                return params
    raise CtlError(
        "Could not locate map params.txt from CNEXTXY, CGRAREA, or CDIMINFO in the namelist."
    )


def read_map_geometry(path: Path) -> MapGeometry:
    """Read NX, NY and map bounds from the standard eight-line params.txt."""
    try:
        tokens = [line.split()[0] for line in path.read_text(encoding="utf-8").splitlines() if line.split()]
    except OSError as exc:
        raise CtlError(f"Cannot read map metadata {path}: {exc}") from exc
    if len(tokens) < 8:
        raise CtlError(f"Expected at least 8 numeric records in {path}, found {len(tokens)}.")
    try:
        nx, ny = int(tokens[0]), int(tokens[1])
        geometry = MapGeometry(
            nx=nx,
            ny=ny,
            resolution=float(tokens[3]),
            west=float(tokens[4]),
            east=float(tokens[5]),
            south=float(tokens[6]),
            north=float(tokens[7]),
        )
    except ValueError as exc:
        raise CtlError(f"Invalid numeric field in {path}: {exc}") from exc
    if nx <= 0 or ny <= 0 or geometry.resolution <= 0:
        raise CtlError(f"Grid dimensions and resolution in {path} must be positive.")
    return geometry


def read_registry(path: Path) -> dict[str, dict[str, str]]:
    try:
        with path.open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
    except (OSError, csv.Error) as exc:
        raise CtlError(f"Cannot read output-variable registry {path}: {exc}") from exc
    if not rows:
        raise CtlError(f"Output-variable registry is empty: {path}")
    required_columns = {
        "name", "long_name", "units", "binary_dtype", "dimensions",
        "missing_value", "temporal_type", "status", "notes",
    }
    missing = sorted(required_columns - set(rows[0]))
    if missing:
        raise CtlError("Registry is missing required columns: " + ", ".join(missing))
    registry = {row["name"].strip(): row for row in rows if row.get("name", "").strip()}
    inherited_fields = (
        "long_name", "description", "units", "internal_variable", "internal_dtype",
        "binary_dtype", "netcdf_dtype", "dimensions", "temporal_type", "missing_value",
    )
    for name, row in registry.items():
        canonical_name = row.get("alias_of", "").strip()
        if not canonical_name:
            continue
        canonical = registry.get(canonical_name)
        if canonical is None:
            raise CtlError(f"Output alias {name!r} refers to missing canonical name {canonical_name!r}.")
        for field in inherited_fields:
            if not row.get(field, "").strip():
                row[field] = canonical.get(field, "")
    return registry


def output_times(config: RunConfig) -> list[datetime]:
    """Reproduce CMF_DRV_ADVANCE output timestamps from the source time loop."""
    elapsed_seconds = (config.end - config.start).total_seconds()
    nsteps = int(elapsed_seconds / config.dt_seconds)
    step_minutes = config.dt_seconds // 60
    times = []
    for step in range(1, nsteps + 1):
        time = config.start + timedelta(minutes=step * step_minutes)
        if time.hour % config.output_hours == 0 and time.minute == 0:
            times.append(time)
    if not times:
        return []
    gaps = {(right - left).total_seconds() for left, right in zip(times, times[1:])}
    if len(gaps) > 1:
        raise CtlError(
            "The Fortran output schedule is irregular for this IFRQ_OUT and start time; "
            "GrADS TDEF cannot represent it as a regular time axis."
        )
    return times


def grads_time(value: datetime) -> str:
    months = ("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    return f"{value.hour:02d}Z{value.day:02d}{months[value.month - 1]}{value.year:04d}"


def grads_increment(config: RunConfig, times: list[datetime]) -> str:
    if len(times) > 1:
        hours = int((times[1] - times[0]).total_seconds() // 3600)
    else:
        hours = config.output_hours
    if hours % 24 == 0:
        return f"{hours // 24}dy"
    return f"{hours}hr"


def eligible_variable(name: str, registry: dict[str, dict[str, str]]) -> tuple[dict[str, str] | None, str | None]:
    row = registry.get(name)
    if row is None:
        return None, "not present in output_variables.csv"
    if row["status"].strip().lower() != "confirmed":
        detail = row.get("notes", "").strip() or "registry status is not confirmed"
        return None, f"registry status={row['status']!r}: {detail}"
    if not row["long_name"].strip() or not row["units"].strip() or not row["missing_value"].strip():
        return None, "registry long_name, units, or missing_value is empty"
    if row["binary_dtype"].strip().lower() != "float32":
        return None, f"unsupported binary_dtype {row['binary_dtype']!r}; only float32 is implemented"
    if not row["dimensions"].strip().startswith("NX,NY;"):
        return None, f"unsupported dimensions {row['dimensions']!r}; expected an NX,NY map"
    try:
        undef = float(row["missing_value"])
    except ValueError:
        return None, f"invalid missing_value {row['missing_value']!r}"
    if not math.isfinite(undef):
        return None, f"missing_value must be finite, got {row['missing_value']!r}"
    return row, None


def record_size(row: dict[str, str], geometry: MapGeometry) -> int:
    if row["binary_dtype"].strip().lower() != "float32":
        raise CtlError(f"Unsupported binary dtype for {row['name']}.")
    return geometry.nx * geometry.ny * 4


def write_ctl(
    ctl_path: Path,
    binary_path: Path,
    row: dict[str, str],
    geometry: MapGeometry,
    times: list[datetime],
    config: RunConfig,
    endian: str,
) -> None:
    resolution = geometry.resolution
    x_start = geometry.west + resolution / 2.0
    y_start = geometry.south + resolution / 2.0
    options = "yrev " + endian
    title = f"CaMa-Flood output: {row['long_name'].strip()} ({row['name']})"
    title = " ".join(title.replace("\r", " ").replace("\n", " ").split())
    description = " ".join(row["long_name"].strip().replace("\r", " ").replace("\n", " ").split())
    units = " ".join(row["units"].strip().split())
    text = (
        f"DSET ^{binary_path.name}\n"
        f"TITLE {title}\n"
        f"UNDEF {row['missing_value'].strip()}\n"
        f"OPTIONS {options}\n"
        f"XDEF {geometry.nx} LINEAR {x_start:.10g} {resolution:.10g}\n"
        f"YDEF {geometry.ny} LINEAR {y_start:.10g} {resolution:.10g}\n"
        "ZDEF 1 LINEAR 1 1\n"
        f"TDEF {len(times)} LINEAR {grads_time(times[0])} {grads_increment(config, times)}\n"
        "VARS 1\n"
        f"{row['name']} 1 99 {description} [{units}]\n"
        "ENDVARS\n"
    )
    ctl_path.parent.mkdir(parents=True, exist_ok=True)
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", newline="\n", dir=ctl_path.parent,
            prefix=ctl_path.name + ".", suffix=".tmp", delete=False,
        ) as stream:
            temporary = Path(stream.name)
            stream.write(text)
        temporary.replace(ctl_path)
    finally:
        if temporary and temporary.exists():
            temporary.unlink()


def generate(args: argparse.Namespace) -> int:
    run_dir = args.run_dir.resolve()
    namelist_path = args.namelist.resolve() if args.namelist else run_dir / "input_cmf.nam"
    registry_path = args.registry.resolve()
    config, values = read_run_config(run_dir, namelist_path)
    geometry = read_map_geometry(find_params_file(run_dir, values))
    registry = read_registry(registry_path)

    if not config.binary_output:
        print("[SKIP] LOUTCDF=.TRUE.; this tool handles binary output only.")
        return 0
    if config.vector_output:
        print("[SKIP] LOUTVEC=.TRUE.; vector output is not supported by this tool.")
        return 0
    if values.get("LOUTINI", ".FALSE.").strip().upper() in {".TRUE.", "TRUE", "T"}:
        raise CtlError(
            "LOUTINI=.TRUE. is not supported for binary output: the current Fortran "
            "initial-output call advances IRECOUT although it skips binary writes."
        )

    times = output_times(config)
    if not times:
        print("[SKIP] No binary output timestamps are scheduled in this simulation period.")
        return 0

    endian = args.endian
    if endian == "native":
        endian = "little_endian" if sys.byteorder == "little" else "big_endian"
    expected_records = len(times)
    errors = 0
    generated = 0

    print(f"Run directory: {run_dir}")
    print(f"Map grid: {geometry.nx} x {geometry.ny}, resolution {geometry.resolution:g} degrees")
    print(f"Expected output records: {expected_records} ({grads_time(times[0])} to {grads_time(times[-1])})")

    for name in config.names:
        row, reason = eligible_variable(name, registry)
        if reason:
            print(f"[SKIP] {name}: {reason}")
            continue
        assert row is not None
        binary_path = config.output_dir / f"{name}{config.tag}.bin"
        ctl_path = binary_path.with_suffix(".ctl")
        record_bytes = record_size(row, geometry)
        record_count = expected_records

        if args.mode == "existing":
            if not binary_path.is_file():
                print(f"[SKIP] {name}: binary file not found: {binary_path}")
                continue
            size = binary_path.stat().st_size
            record_count, remainder = divmod(size, record_bytes)
            if remainder:
                print(
                    f"[ERROR] {name}: file size {size} is not a whole number of "
                    f"{record_bytes}-byte records; no CTL written."
                )
                errors += 1
                continue
            if record_count == 0:
                print(f"[SKIP] {name}: binary file contains no complete records.")
                continue
            if record_count > expected_records:
                print(
                    f"[ERROR] {name}: found {record_count} records, more than the "
                    f"{expected_records} scheduled by this namelist; no CTL written."
                )
                errors += 1
                continue
            if record_count < expected_records:
                print(
                    f"[INCOMPLETE] {name}: {record_count}/{expected_records} records; "
                    "writing a CTL for records currently present."
                )
            record_times = times[:record_count]
        else:
            record_times = times

        write_ctl(ctl_path, binary_path, row, geometry, record_times, config, endian)
        generated += 1
        mode_note = "planned" if args.mode == "planned" else f"{record_count} records"
        print(f"[SUCCESS] {ctl_path} ({mode_note})")

    print(f"CTL files generated: {generated}")
    return 1 if errors else 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate GrADS CTL files for CaMa-Flood gridded binary model outputs."
    )
    parser.add_argument("--run-dir", type=Path, required=True, help="Directory containing input_cmf.nam")
    parser.add_argument("--namelist", type=Path, help="Namelist path (default: RUN_DIR/input_cmf.nam)")
    parser.add_argument(
        "--registry", type=Path,
        default=Path(__file__).resolve().parents[1] / "variable_table" / "output_variables.csv",
        help="Output variable registry (default: variable_table/output_variables.csv)",
    )
    parser.add_argument("--mode", choices=("planned", "existing"), required=True)
    parser.add_argument(
        "--endian", choices=("native", "little", "big"), default="native",
        help="Binary byte order; native defaults to the Python host's byte order.",
    )
    args = parser.parse_args()
    if args.endian == "little":
        args.endian = "little_endian"
    elif args.endian == "big":
        args.endian = "big_endian"
    try:
        return generate(args)
    except CtlError as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
