#!/usr/bin/env python3
"""Generate GrADS CTL files from map_variables.csv and map-local grid metadata."""
from __future__ import annotations

import argparse
import csv
import fnmatch
import math
import re
import sys
from dataclasses import dataclass
from pathlib import Path


class CtlError(ValueError):
    """Invalid or incomplete metadata for CTL generation."""


class CtlValidationError(CtlError):
    """A registered file conflicts with its declared layout or actual size."""


@dataclass(frozen=True)
class Grid:
    nx: int
    ny: int
    west: float
    east: float
    south: float
    north: float
    spacing: float
    source: str
    high_resolution: bool = False


@dataclass(frozen=True)
class Area:
    iarea: int
    carea: str
    grid: Grid


@dataclass(frozen=True)
class Variable:
    name: str
    levels: int
    dtype: str
    description: str
    units: str
    record_index: int


SUPPORTED_TYPES = {
    # GrADS supports byte input as unsigned only; signed byte data keep their
    # original bit pattern and their Undef code is converted to 0..255 below.
    "int8": (1, "-1,40,1"),
    "int16": (2, "-1,40,2,-1"),
    "int32": (4, "-1,40,4"),
    "float32": (4, "99"),
}
DEFAULT_MISSING = {"int8": -9, "int16": -9999, "int32": -9999, "float32": -9999.0}
REQUIRED_COLUMNS = {
    "category_id", "binary_filename", "alternative_binary_filename", "name",
    "long_name", "units", "dtype", "dimensions", "record_layout",
    "record_index", "record_count", "missing_value", "special_values",
}
NAME_RE = re.compile(r"^[A-Za-z][A-Za-z0-9_]{0,14}$")
RECORD_RE = re.compile(r"^\s*record\s+(\d+)\s*(?::\s*|\s+)?([A-Za-z][A-Za-z0-9_]*)?\s*$", re.I)
LAYER_LAYOUT_RE = re.compile(r"^\s*records\s+1\.\.([A-Za-z0-9_]+)\s*;\s*record\s+k\s+is\s+layer\s+k\s*$", re.I)


def default_registry_path() -> Path:
    return Path(__file__).resolve().parents[2] / "map_variables.csv"


def read_registry(path: Path) -> list[dict[str, str]]:
    try:
        with path.open(newline="", encoding="utf-8") as stream:
            reader = csv.DictReader(stream)
            missing = REQUIRED_COLUMNS - set(reader.fieldnames or [])
            if missing:
                raise CtlError(f"Registry {path} lacks columns: {', '.join(sorted(missing))}")
            rows = list(reader)
    except OSError as exc:
        raise CtlError(f"Cannot read registry {path}: {exc}") from exc
    for line, row in enumerate(rows, 2):
        try:
            int(row["category_id"])
        except (ValueError, TypeError):
            raise CtlError(f"{path}:{line}: category_id must be an integer")
    return rows


def read_params(path: Path) -> Grid:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise CtlError(f"Cannot read base-map geometry {path}: {exc}") from exc
    values: list[float] = []
    for line in lines:
        value = line.split("!!", 1)[0].split("#", 1)[0].strip()
        if not value:
            continue
        try:
            values.append(float(value.split()[0]))
        except ValueError as exc:
            raise CtlError(f"Invalid numeric record in {path}: {line!r}") from exc
    if len(values) < 8:
        raise CtlError(f"{path} must provide NX, NY, NLFP, GSIZE, WEST, EAST, SOUTH, NORTH")
    nx, ny, nlfp = values[:3]
    if any(not value.is_integer() for value in (nx, ny, nlfp)) or min(nx, ny, nlfp) < 1:
        raise CtlError(f"NX, NY, and NLFP in {path} must be positive integers")
    return validate_grid(Grid(int(nx), int(ny), values[4], values[5], values[6], values[7], values[3], str(path)))


def validate_grid(grid: Grid) -> Grid:
    if grid.nx < 1 or grid.ny < 1:
        raise CtlError(f"grid dimensions in {grid.source} must be positive")
    if grid.spacing <= 0 or grid.east <= grid.west or grid.north <= grid.south:
        raise CtlError(f"Invalid grid spacing or extent in {grid.source}")
    if not math.isclose((grid.east-grid.west)/grid.nx, grid.spacing, rel_tol=1e-6, abs_tol=1e-8):
        raise CtlError(f"NX and GSIZE do not match WEST/EAST in {grid.source}")
    if not math.isclose((grid.north-grid.south)/grid.ny, grid.spacing, rel_tol=1e-6, abs_tol=1e-8):
        raise CtlError(f"NY and GSIZE do not match SOUTH/NORTH in {grid.source}")
    return grid


def read_location(path: Path) -> list[Area]:
    try:
        lines = [line.strip() for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]
    except OSError as exc:
        raise CtlError(f"Cannot read high-resolution geometry {path}: {exc}") from exc
    if len(lines) < 2:
        raise CtlError(f"{path} must contain an area count, a header, and area records")
    try:
        narea = int(lines[0].split()[0])
    except (ValueError, IndexError) as exc:
        raise CtlError(f"Invalid area count in {path}") from exc
    header = [item.lower() for item in lines[1].split()]
    required = ["iarea", "carea", "west", "east", "south", "north", "nx", "ny", "csize"]
    if header != required:
        raise CtlError(f"{path} header must contain: {' '.join(required)}")
    if len(lines) != narea + 2:
        raise CtlError(f"{path} declares {narea} areas but contains {len(lines)-2} records")
    areas: list[Area] = []
    for line_no, line in enumerate(lines[2:], 3):
        fields = line.split()
        if len(fields) != len(required):
            raise CtlError(f"{path}:{line_no}: expected {len(required)} fields, found {len(fields)}")
        try:
            iarea = int(fields[0])
            west, east, south, north = map(float, fields[2:6])
            nx, ny = int(fields[6]), int(fields[7])
            spacing = float(fields[8])
        except ValueError as exc:
            raise CtlError(f"{path}:{line_no}: invalid numeric location field") from exc
        grid = validate_grid(Grid(nx, ny, west, east, south, north, spacing, str(path), True))
        areas.append(Area(iarea, fields[1], grid))
    if len({area.carea for area in areas}) != len(areas):
        raise CtlError(f"{path} contains duplicate carea names")
    return areas


def registry_patterns(row: dict[str, str]) -> list[str]:
    result = []
    for key in ("binary_filename", "alternative_binary_filename"):
        value = (row.get(key) or "").strip()
        if value and value.lower() not in {"n/a", "na", "none"}:
            result.extend(item.strip() for item in value.split(";") if item.strip())
    return result


def pattern_glob(pattern: str) -> str:
    return pattern.replace("<HIRES>", "*").replace("<AREA>", "*")


def matching_rows(relpath: str, rows: list[dict[str, str]]) -> list[dict[str, str]]:
    found: list[dict[str, str]] = []
    for row in rows:
        if any(fnmatch.fnmatchcase(relpath, pattern_glob(pattern)) for pattern in registry_patterns(row)):
            found.append(row)
    return found


def select_area(relpath: str, target: Path, rows: list[dict[str, str]], areas: list[Area]) -> Area:
    matches: set[str] = set()
    for row in rows:
        for pattern in registry_patterns(row):
            if "<AREA>" not in pattern:
                continue
            for area in areas:
                concrete = pattern.replace("<HIRES>", target.name).replace("<AREA>", area.carea)
                if fnmatch.fnmatchcase(relpath, concrete):
                    matches.add(area.carea)
    if not matches and len(areas) == 1:
        return areas[0]
    if len(matches) != 1:
        raise CtlError("cannot select one location.txt area matching this binary filename")
    return next(area for area in areas if area.carea in matches)


def parse_count(value: str, grid: Grid) -> int | None:
    value = (value or "").strip()
    if not value:
        return None
    if value.isdigit():
        return int(value)
    if value.upper() == "NLFP" and not grid.high_resolution:
        # NLFP is record 3 of params.txt; read it directly from that file.
        try:
            tokens = [line.split("!!", 1)[0].split("#", 1)[0].split()[0] for line in Path(grid.source).read_text().splitlines() if line.split("!!", 1)[0].split("#", 1)[0].strip()]
            count = int(float(tokens[2]))
        except (OSError, ValueError, IndexError):
            raise CtlError(f"Cannot resolve NLFP from {grid.source}")
        if count < 1:
            raise CtlError(f"NLFP in {grid.source} must be positive")
        return count
    raise CtlError(f"unsupported symbolic record_count {value!r}")


def missing_sentinel(row: dict[str, str], dtype: str) -> tuple[int | float, bool]:
    """Return the original stored sentinel and whether a type fallback was used."""
    dtype = dtype.strip().lower()
    if dtype not in DEFAULT_MISSING:
        raise CtlError(f"unsupported dtype {dtype!r} for missing-value handling")
    raw = (row.get("missing_value") or "").strip()
    fallback = not raw
    if fallback:
        value: int | float = DEFAULT_MISSING[dtype]
    else:
        try:
            value = float(raw)
        except ValueError as exc:
            raise CtlValidationError(f"invalid numeric missing_value {raw!r}") from exc
    if not math.isfinite(value):
        raise CtlValidationError("missing-value sentinel must be finite")
    if dtype.startswith("int"):
        if not value.is_integer():
            raise CtlValidationError(f"integer file has non-integer missing value {value}")
        bits = SUPPORTED_TYPES[dtype][0] * 8
        if not -(2 ** (bits - 1)) <= value < 2 ** (bits - 1):
            raise CtlValidationError(f"missing value {value:g} is outside the range of {dtype}")
        value = int(value)
    if abs(value) > 3.4028235e38:
        raise CtlValidationError(f"missing value {value:g} is outside the range of float32")
    return value, fallback


def record_variables(rows: list[dict[str, str]], grid: Grid) -> tuple[list[Variable], int, int | float, bool]:
    if not rows:
        raise CtlError("no registry rows")
    types = {(row.get("dtype") or "").strip().lower() for row in rows}
    if len(types) != 1:
        raise CtlError("file has mixed or inconsistent dtype declarations")
    dtype = types.pop()
    if dtype not in SUPPORTED_TYPES:
        raise CtlError(f"unsupported dtype {dtype or '(blank)'}")
    dims = {(row.get("dimensions") or "").replace(" ", "").upper() for row in rows}
    allowed = {"NX_HIRES,NY_HIRES", "NX_HIRES,NY_HIRES(ORNX_TILE,NY_TILE)"} if grid.high_resolution else {"NX,NY", "NX,NY,NLFP"}
    if len(dims) != 1 or dims.pop() not in allowed:
        raise CtlError("dimensions are unknown, inconsistent, or unsupported for this grid")

    sentinel_info = [missing_sentinel(row, dtype) for row in rows]
    sentinels = {value for value, _ in sentinel_info}
    if len(sentinels) != 1:
        raise CtlValidationError("rows for one file specify inconsistent missing-value sentinels")
    sentinel = sentinels.pop()
    used_fallback = any(fallback for _, fallback in sentinel_info)

    declared_counts = {parse_count(row.get("record_count", ""), grid) for row in rows}
    declared_counts.discard(None)
    if len(declared_counts) > 1:
        raise CtlError("rows for one file specify inconsistent record_count")
    declared_count = next(iter(declared_counts), None)

    # A three-dimensional base-map variable with an explicit layer layout is one
    # GrADS variable whose levels occupy consecutive direct-access records.
    layered: list[Variable] = []
    for row in rows:
        match = LAYER_LAYOUT_RE.fullmatch((row.get("record_layout") or ""))
        if match:
            count = parse_count(match.group(1), grid)
            if count is None or (declared_count is not None and count != declared_count):
                raise CtlError("layer layout and record_count disagree")
            name = (row.get("name") or "").strip()
            if not NAME_RE.fullmatch(name):
                raise CtlError(f"invalid GrADS variable name {name!r}")
            if (row.get("dimensions") or "").replace(" ", "").upper() != "NX,NY,NLFP":
                raise CtlError("NLFP layer layout requires dimensions=NX,NY,NLFP")
            if (row.get("record_index") or "1").strip() != "1":
                raise CtlValidationError("layered variable must begin at record_index 1")
            layered.append(Variable(name, count, dtype, row.get("long_name", "").strip(), row.get("units", "").strip(), 1))
    if layered:
        if len(rows) != 1 or len(layered) != 1:
            raise CtlError("layered files must have one logical variable row")
        return layered, layered[0].levels, sentinel, used_fallback

    variables: list[Variable] = []
    for row in rows:
        count = parse_count(row.get("record_count", ""), grid)
        layout = (row.get("record_layout") or "").strip()
        row_index = (row.get("record_index") or "").strip()
        if row_index:
            try:
                index = int(row_index)
            except ValueError as exc:
                raise CtlError(f"invalid record_index {row_index!r}") from exc
            if index < 1:
                raise CtlError("record_index must be positive")
            name = (row.get("name") or "").strip()
            if not NAME_RE.fullmatch(name):
                raise CtlError(f"invalid GrADS variable name {name!r}")
            variables.append(Variable(name, 1, dtype, row.get("long_name", "").strip(), row.get("units", "").strip(), index))
            continue

        descriptors = [RECORD_RE.fullmatch(part) for part in layout.split(";")]
        if descriptors and all(descriptors) and len(descriptors) > 1 and all(match.group(2) for match in descriptors if match):
            for match in descriptors:
                assert match is not None
                index = int(match.group(1))
                name = match.group(2)
                if not NAME_RE.fullmatch(name):
                    raise CtlError(f"invalid GrADS variable name {name!r}")
                variables.append(Variable(name, 1, dtype, row.get("long_name", "").strip(), row.get("units", "").strip(), index))
            continue
        if re.fullmatch(r"record\s+1", layout, re.I):
            name = (row.get("name") or "").strip()
            if not NAME_RE.fullmatch(name):
                raise CtlError(f"invalid GrADS variable name {name!r}")
            variables.append(Variable(name, 1, dtype, row.get("long_name", "").strip(), row.get("units", "").strip(), 1))
            continue
        raise CtlError(f"record layout is not machine-readable: {layout or '(blank)'}")

    indices = [variable.record_index for variable in variables]
    inferred_count = max(indices, default=0)
    record_count = declared_count if declared_count is not None else inferred_count
    if record_count < 1 or sorted(indices) != list(range(1, record_count + 1)):
        raise CtlValidationError("record indices/layout do not describe every consecutive record")
    if record_count != len(variables):
        raise CtlValidationError("record count does not match independent variable records")
    variables.sort(key=lambda item: item.record_index)
    return variables, record_count, sentinel, used_fallback


def expected_dimensions(row: dict[str, str], grid: Grid) -> bool:
    dims = (row.get("dimensions") or "").replace(" ", "").upper()
    if grid.high_resolution:
        return dims.startswith("NX_HIRES,NY_HIRES") and "NLFP" not in dims
    return dims == "NX,NY" or dims == "NX,NY,NLFP"


def read_input_matrix_descriptor(binary_path: Path, grid: Grid,
                                 registry_row: dict[str, str]) -> tuple[list[Variable], int, int | float, bool]:
    """Read the companion text file written by region_generate_inpmat.F90."""
    stem = binary_path.stem
    candidates = [binary_path.with_suffix(".txt")]
    if stem.startswith("inpmat_"):
        candidates.append(binary_path.with_name(f"diminfo_{stem[len('inpmat_'): ]}.txt"))
    descriptor = next((path for path in candidates if path.is_file()), None)
    if descriptor is None:
        raise CtlError("input matrix needs its paired dimension description (.txt or diminfo_*.txt)")
    try:
        lines = [line.strip() for line in descriptor.read_text(encoding="utf-8").splitlines() if line.strip()]
    except OSError as exc:
        raise CtlError(f"Cannot read input-matrix description {descriptor}: {exc}") from exc
    if len(lines) < 11:
        raise CtlError(f"{descriptor} must contain 6 dimensions, binary filename, and 4 bounds")
    try:
        nxx, nyy, nlfp, nxin, nyin, inpn = [int(line.split()[0]) for line in lines[:6]]
        west, east, north, south = [float(line.split()[0]) for line in lines[7:11]]
    except (ValueError, IndexError) as exc:
        raise CtlError(f"{descriptor} has invalid dimension records") from exc
    if min(nxx, nyy, nlfp, nxin, nyin, inpn) < 1:
        raise CtlError(f"{descriptor} dimensions must be positive")
    if (nxx, nyy) != (grid.nx, grid.ny):
        raise CtlValidationError(
            f"input-matrix description grid {nxx}x{nyy} does not match params.txt {grid.nx}x{grid.ny}"
        )
    if Path(lines[6]).name != binary_path.name:
        raise CtlValidationError(
            f"{descriptor} describes {Path(lines[6]).name}, not {binary_path.name}"
        )
    if not all(math.isclose(a, b, rel_tol=1e-6, abs_tol=1e-6) for a, b in (
        (west, grid.west), (east, grid.east), (south, grid.south), (north, grid.north)
    )):
        raise CtlValidationError(f"{descriptor} geographic bounds do not match params.txt")
    sentinel, fallback = missing_sentinel(registry_row, "int32")
    return [
        Variable("inpx", inpn, "int32", f"Input-grid x index (INPN={inpn})", "", 1),
        Variable("inpy", inpn, "int32", f"Input-grid y index (INPN={inpn})", "", 2),
        Variable("inpa", inpn, "float32", f"Overlap area (INPN={inpn})", "m2", 3),
    ], 3, sentinel, fallback


def is_input_matrix(row: dict[str, str]) -> bool:
    return (
        (row.get("dimensions") or "").replace(" ", "").upper() == "NX,NY,INPN"
        and (row.get("dtype") or "").strip().lower() == "int32 + float32"
        and "x-index" in (row.get("record_layout") or "").lower()
        and "y-index" in (row.get("record_layout") or "").lower()
        and "overlap-area" in (row.get("record_layout") or "").lower()
    )


def ctl_text(path: Path, variables: list[Variable], grid: Grid, sentinel: int | float) -> str:
    dx = (grid.east - grid.west) / grid.nx
    dy = (grid.north - grid.south) / grid.ny
    if not math.isclose(dx, dy, rel_tol=1e-7, abs_tol=1e-9):
        raise CtlError("CTL generator supports square regular lon/lat cells only")
    zlevels = max(variable.levels for variable in variables)
    ctl_sentinel = sentinel & 0xFF if variables[0].dtype == "int8" else sentinel
    lines = [
        f"dset ^{path.name}", f"undef {ctl_sentinel:g}",
        f"title CaMa-Flood map file {path.name}",
        "options yrev little_endian",
        f"xdef {grid.nx} linear {grid.west + dx / 2:.12g} {dx:.12g}",
        f"ydef {grid.ny} linear {grid.south + dy / 2:.12g} {dy:.12g}",
        "tdef 1 linear 00Z01jan2000 1yr",
        f"zdef {zlevels} linear 1 1",
        f"vars {len(variables)}",
    ]
    for variable in variables:
        type_code = SUPPORTED_TYPES[variable.dtype][1]
        name = variable.name
        if grid.high_resolution and name.endswith("_hires"):
            name = name[:-len("_hires")]
        description = variable.description.replace("\n", " ").replace("\r", " ")
        units = variable.units.replace("\n", " ").replace("\r", " ")
        if units:
            description += f" [{units}]"
        lines.append(f"{name} {variable.levels} {type_code} {description}".rstrip())
    lines.append("ENDVARS")
    return "\n".join(lines) + "\n"


@dataclass
class Summary:
    generated: int = 0
    skipped: int = 0
    failed: int = 0
    fallback_files: int = 0


def generate_directory(map_dir: Path, registry_path: Path, *, dry_run: bool = False,
                       overwrite: bool = False) -> Summary:
    target = map_dir.resolve()
    summary = Summary()
    if not target.is_dir():
        print(f"FAILED {target}: target directory does not exist")
        summary.failed = 1
        return summary
    files = sorted(path for path in target.glob("*.bin") if path.is_file())
    if not files:
        print(f"No .bin files found directly under {target}")
        return summary
    try:
        rows = read_registry(registry_path)
    except CtlError as exc:
        for path in files:
            print(f"FAILED {path.name}: {exc}")
            summary.failed += 1
        return summary

    location_path = target / "location.txt"
    params_path = target / "params.txt"
    areas: list[Area] = []
    base_grid: Grid | None = None
    dataset_root = target
    if not location_path.is_file() and not params_path.is_file():
        for path in files:
            print(f"SKIP {path.name}: target directory has neither params.txt nor location.txt")
            summary.skipped += 1
        return summary
    try:
        if params_path.is_file():
            base_grid = read_params(params_path)
        elif location_path.is_file():
            areas = read_location(location_path)
            dataset_root = target.parent
        else:
            raise CtlError("no params.txt or location.txt in target directory")
    except CtlError as exc:
        for path in files:
            print(f"FAILED {path.name}: {exc}")
            summary.failed += 1
        return summary

    for path in files:
        relpath = path.relative_to(dataset_root).as_posix()
        matches = matching_rows(relpath, rows)
        if not matches:
            print(f"SKIP {path.name}: not registered in map_variables.csv")
            summary.skipped += 1
            continue
        try:
            if base_grid is not None:
                grid = base_grid
                if len(matches) == 1 and is_input_matrix(matches[0]):
                    variables, records, sentinel, used_fallback = read_input_matrix_descriptor(path, grid, matches[0])
                elif not all(expected_dimensions(row, grid) for row in matches):
                    declared_dims = (matches[0].get("dimensions") or "").replace(" ", "").upper()
                    if declared_dims not in {"NX,NY", "NX,NY,NLFP"}:
                        raise CtlError("registry dimensions are not a supported regular map grid")
                    raise CtlValidationError("registry dimensions do not match base-map params.txt geometry")
                else:
                    variables, records, sentinel, used_fallback = record_variables(matches, grid)
            else:
                area = select_area(relpath, target, matches, areas)
                grid = area.grid
                if not all(expected_dimensions(row, grid) for row in matches):
                    declared_dims = (matches[0].get("dimensions") or "").replace(" ", "").upper()
                    if not declared_dims.startswith("NX_HIRES,NY_HIRES"):
                        raise CtlError("registry dimensions are not a supported high-resolution grid")
                    raise CtlValidationError("registry dimensions do not match high-resolution location.txt geometry")
                variables, records, sentinel, used_fallback = record_variables(matches, grid)
            expected_size = sum(
                grid.nx * grid.ny * variable.levels * SUPPORTED_TYPES[variable.dtype][0]
                for variable in variables
            )
            actual_size = path.stat().st_size
            if actual_size != expected_size:
                raise CtlValidationError(f"binary size mismatch: expected {expected_size} bytes ({records} record(s), {grid.nx}x{grid.ny}, {variables[0].dtype}), found {actual_size}")
            try:
                text = ctl_text(path, variables, grid, sentinel)
            except CtlError as exc:
                raise CtlValidationError(str(exc)) from exc
            output = path.with_suffix(".ctl")
            if used_fallback:
                summary.fallback_files += 1
                print(f"WARNING {path.name}: missing_value is blank; using {variables[0].dtype} default {sentinel:g}")
            if output.exists() and not overwrite:
                print(f"SKIP {path.name}: {output.name} exists (use --overwrite)")
                summary.skipped += 1
                continue
            if dry_run:
                print(f"PLAN {path.name} -> {output.name} ({grid.nx}x{grid.ny}, {records} record(s), {grid.source})")
            else:
                output.write_text(text, encoding="utf-8")
                print(f"WROTE {output}")
            summary.generated += 1
        except CtlValidationError as exc:
            print(f"FAILED {path.name}: {exc}")
            summary.failed += 1
        except CtlError as exc:
            print(f"SKIP {path.name}: {exc}")
            summary.skipped += 1
        except OSError as exc:
            print(f"FAILED {path.name}: {exc}")
            summary.failed += 1
    return summary


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--map-dir", type=Path, required=True,
                        help="directory to scan directly for .bin files (map root or high-resolution subdirectory)")
    parser.add_argument("--metadata-csv", type=Path, default=default_registry_path(),
                        help="variable registry (default: map/map_variables.csv)")
    parser.add_argument("--dry-run", action="store_true", help="validate and list planned CTLs without writing")
    parser.add_argument("--overwrite", action="store_true", help="replace existing .ctl files")
    args = parser.parse_args(argv)
    summary = generate_directory(args.map_dir, args.metadata_csv, dry_run=args.dry_run, overwrite=args.overwrite)
    action = "planned" if args.dry_run else "generated"
    print(f"Summary: {action}={summary.generated}, skipped={summary.skipped}, failed={summary.failed}, fallback={summary.fallback_files}")
    return 1 if summary.failed else 0


if __name__ == "__main__":
    sys.exit(main())
