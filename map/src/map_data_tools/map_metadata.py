"""Shared metadata, configuration, and binary validation for map tools."""
from __future__ import annotations

import csv
import json
import math
from pathlib import Path
from typing import Any

import numpy as np

CORE_CATEGORY_ID = 11
CORE_FILES = {
    "nextxy.bin", "ctmare.bin", "elevtn.bin", "nxtdst.bin", "rivlen.bin", "fldhgt.bin"
}
CORE_VARIABLES = {"nextx", "nexty", "ctmare", "elevtn", "nxtdst", "rivlen", "fldhgt"}


class MapDataError(ValueError):
    """Raised for incomplete or incompatible map metadata or files."""


def default_registry_path() -> Path:
    return Path(__file__).resolve().parents[2] / "map_variables.csv"


def read_registry(path: Path | str | None = None) -> list[dict[str, str]]:
    registry = Path(path) if path is not None else default_registry_path()
    try:
        with registry.open(newline="", encoding="utf-8") as stream:
            reader = csv.DictReader(stream)
            required = {"category_id", "category", "binary_filename", "name", "dtype",
                        "dimensions", "record_index", "record_count", "long_name", "units",
                        "missing_value", "special_values", "vertical_datum"}
            missing = required - set(reader.fieldnames or [])
            if missing:
                raise MapDataError(f"Registry {registry} is missing columns: {', '.join(sorted(missing))}")
            rows = list(reader)
    except OSError as exc:
        raise MapDataError(f"Cannot read metadata CSV {registry}: {exc}") from exc
    for line, row in enumerate(rows, start=2):
        try:
            int(row["category_id"])
        except (TypeError, ValueError) as exc:
            raise MapDataError(f"{registry}:{line}: category_id must be an integer") from exc
    return rows


def read_core_specs(rows: list[dict[str, str]]) -> dict[str, list[dict[str, str]]]:
    grouped: dict[str, list[dict[str, str]]] = {}
    for row in rows:
        if int(row["category_id"]) == CORE_CATEGORY_ID:
            grouped.setdefault(row["binary_filename"], []).append(row)
    if set(grouped) != CORE_FILES:
        raise MapDataError(
            "Category 11 must define exactly the six core files; found: "
            + ", ".join(sorted(grouped))
        )
    names = {row["name"] for entries in grouped.values() for row in entries}
    if names != CORE_VARIABLES:
        raise MapDataError("Category 11 does not define the expected core variable names")
    for filename, entries in grouped.items():
        indices = [int(entry["record_index"]) for entry in entries]
        if len(indices) != len(set(indices)):
            raise MapDataError(f"Duplicate record_index in registry for {filename}")
        file_records = 2 if filename == "nextxy.bin" else ("NLFP" if filename == "fldhgt.bin" else 1)
        for entry in entries:
            if entry["dtype"] not in {"int32", "float32"}:
                raise MapDataError(f"Unsupported dtype {entry['dtype']!r} for {filename}")
            if not entry["long_name"].strip():
                raise MapDataError(f"Missing long_name for core variable {entry['name']}")
            expected_dtype = "int32" if entry["name"] in {"nextx", "nexty"} else "float32"
            expected_dims = "NX,NY,NLFP" if filename == "fldhgt.bin" else "NX,NY"
            expected_index = {"nextx": 1, "nexty": 2}.get(entry["name"], 1)
            if entry["dtype"] != expected_dtype or entry["dimensions"].replace(" ", "") != expected_dims:
                raise MapDataError(f"Unsupported core type/dimensions for {entry['name']} in registry")
            if int(entry["record_index"]) != expected_index:
                raise MapDataError(f"Unexpected record_index for {entry['name']} in {filename}")
            if entry["record_count"] != str(file_records):
                raise MapDataError(f"Unexpected record_count for {entry['name']} in {filename}; expected {file_records}")
    return grouped


def read_params(map_dir: Path | str) -> dict[str, float | int]:
    path = Path(map_dir) / "params.txt"
    values: list[float] = []
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise MapDataError(f"Cannot read {path}: {exc}") from exc
    for line in lines:
        body = line.split("!!", 1)[0].split("#", 1)[0].strip()
        if not body:
            continue
        try:
            values.append(float(body.split()[0]))
        except ValueError as exc:
            raise MapDataError(f"Invalid numeric record in {path}: {line!r}") from exc
    if len(values) < 8:
        raise MapDataError(f"{path} must provide NX, NY, NLFP, GSIZE, WEST, EAST, SOUTH, NORTH")
    nx, ny, nlfp = values[:3]
    if any(not value.is_integer() for value in (nx, ny, nlfp)) or min(nx, ny, nlfp) < 1:
        raise MapDataError(f"NX, NY, and NLFP in {path} must be positive integers")
    result: dict[str, float | int] = {
        "NX": int(nx), "NY": int(ny), "NLFP": int(nlfp), "GSIZE": values[3],
        "WEST": values[4], "EAST": values[5], "SOUTH": values[6], "NORTH": values[7]
    }
    if result["GSIZE"] <= 0 or result["EAST"] <= result["WEST"] or result["NORTH"] <= result["SOUTH"]:
        raise MapDataError(f"Invalid grid spacing or bounds in {path}")
    dx = (result["EAST"] - result["WEST"]) / result["NX"]
    dy = (result["NORTH"] - result["SOUTH"]) / result["NY"]
    for axis, step in (("x", dx), ("y", dy)):
        if not math.isclose(step, result["GSIZE"], rel_tol=1e-6, abs_tol=1e-7):
            raise MapDataError(
                f"{path} describes nonuniform/unsupported {axis} spacing: "
                f"derived {step:g}, GSIZE {result['GSIZE']:g}"
            )
    return result


def read_dataset_config(path: Path | str | None, map_dir: Path | str) -> dict[str, Any]:
    map_root = Path(map_dir).resolve()
    config_path = Path(path) if path is not None else map_root / "map_metadata.json"
    try:
        config = json.loads(config_path.read_text(encoding="utf-8"))
    except OSError as exc:
        raise MapDataError(f"Cannot read dataset config {config_path}: {exc}") from exc
    except json.JSONDecodeError as exc:
        raise MapDataError(f"Invalid JSON in {config_path}: {exc}") from exc
    required = {"schema_version", "dataset_id", "byte_order", "row_order", "coordinate_mode", "files", "missing_values"}
    missing = required - config.keys()
    if missing:
        raise MapDataError(f"{config_path} is missing keys: {', '.join(sorted(missing))}")
    if config["schema_version"] != 1:
        raise MapDataError(f"Unsupported map metadata schema_version: {config['schema_version']!r}")
    if config["byte_order"] not in {"little", "big"}:
        raise MapDataError("byte_order must explicitly be 'little' or 'big'")
    if config["row_order"] not in {"north_to_south", "south_to_north"}:
        raise MapDataError("row_order must explicitly be 'north_to_south' or 'south_to_north'")
    if config["coordinate_mode"] != "regular_lonlat_from_params":
        raise MapDataError("Only coordinate_mode='regular_lonlat_from_params' is supported")
    if not isinstance(config["dataset_id"], str) or not config["dataset_id"].strip():
        raise MapDataError("dataset_id must be a non-empty string")
    if not isinstance(config["files"], dict) or not isinstance(config["missing_values"], dict):
        raise MapDataError("files and missing_values must be JSON objects")
    for filename in CORE_FILES:
        if filename not in config["files"]:
            raise MapDataError(f"Dataset config must map a path for {filename}")
        if filename not in config["missing_values"]:
            raise MapDataError(f"Dataset config must explicitly define missing_values for {filename}")
        missing_value = config["missing_values"][filename]
        if isinstance(missing_value, bool) or not isinstance(missing_value, (int, float)) or not math.isfinite(missing_value):
            raise MapDataError(f"missing_values[{filename!r}] must be a finite numeric sentinel")
    config["_config_path"] = config_path.resolve()
    return config


def dataset_file(map_dir: Path | str, config: dict[str, Any], filename: str) -> Path:
    value = config["files"][filename]
    if not isinstance(value, str) or not value:
        raise MapDataError(f"files[{filename!r}] must be a non-empty path")
    path = Path(value)
    return (path if path.is_absolute() else Path(map_dir) / path).resolve()


def core_layout(groups: dict[str, list[dict[str, str]]], params: dict[str, float | int]) -> dict[str, tuple[np.dtype, int]]:
    layout: dict[str, tuple[np.dtype, int]] = {}
    for filename, entries in groups.items():
        types = {entry["dtype"] for entry in entries}
        if len(types) != 1:
            raise MapDataError(f"Inconsistent dtype declarations for {filename}")
        dtype_name = types.pop()
        records = int(params["NLFP"]) if filename == "fldhgt.bin" else max(int(e["record_index"]) for e in entries)
        layout[filename] = (np.dtype("i4" if dtype_name == "int32" else "f4"), records)
    return layout


def validate_binary_sizes(map_dir: Path | str, groups: dict[str, list[dict[str, str]]],
                          params: dict[str, float | int], config: dict[str, Any]) -> dict[str, tuple[np.dtype, int]]:
    map_root = Path(map_dir)
    layout = core_layout(groups, params)
    nx, ny = int(params["NX"]), int(params["NY"])
    for filename, (dtype, record_count) in layout.items():
        path = dataset_file(map_root, config, filename)
        if not path.is_file():
            raise MapDataError(f"Missing binary map file: {path}")
        expected = nx * ny * record_count * dtype.itemsize
        actual = path.stat().st_size
        if actual != expected:
            raise MapDataError(
                f"Binary size mismatch for {path}: expected {expected} bytes "
                f"({record_count} records of {nx}x{ny} {dtype}), found {actual} bytes"
            )
    return layout


def load_core_data(map_dir: Path | str, groups: dict[str, list[dict[str, str]]],
                   params: dict[str, float | int], config: dict[str, Any]) -> dict[str, np.ndarray]:
    map_root = Path(map_dir)
    layout = validate_binary_sizes(map_root, groups, params, config)
    arrays: dict[str, np.ndarray] = {}
    nx, ny = int(params["NX"]), int(params["NY"])
    for filename, (native_dtype, record_count) in layout.items():
        path = dataset_file(map_root, config, filename)
        dtype = native_dtype.newbyteorder("<" if config["byte_order"] == "little" else ">")
        flat = np.fromfile(path, dtype=dtype)
        # Direct-access Fortran arrays are (NX, NY): x is contiguous. A C-order
        # array shaped (record, NY, NX) has the same byte sequence.
        arrays[filename] = flat.reshape(record_count, ny, nx).copy()
    variables: dict[str, np.ndarray] = {}
    for filename, entries in groups.items():
        records = arrays[filename]
        for entry in entries:
            index = int(entry["record_index"]) - 1
            if filename == "fldhgt.bin":
                variables[entry["name"]] = records
            else:
                variables[entry["name"]] = records[index]
    return variables


def grid_coordinates(params: dict[str, float | int], config: dict[str, Any]) -> tuple[np.ndarray, np.ndarray]:
    nx, ny = int(params["NX"]), int(params["NY"])
    dx = (float(params["EAST"]) - float(params["WEST"])) / nx
    dy = (float(params["NORTH"]) - float(params["SOUTH"])) / ny
    lon = float(params["WEST"]) + (np.arange(nx, dtype=np.float64) + 0.5) * dx
    if config["row_order"] == "north_to_south":
        lat = float(params["NORTH"]) - (np.arange(ny, dtype=np.float64) + 0.5) * dy
    else:
        lat = float(params["SOUTH"]) + (np.arange(ny, dtype=np.float64) + 0.5) * dy
    return lon, lat


def validate_missing_value(filename: str, dtype_name: str, config: dict[str, Any]) -> int | float:
    value = config["missing_values"][filename]
    if dtype_name == "int32" and (not isinstance(value, int) or isinstance(value, bool)):
        raise MapDataError(f"Missing sentinel for integer map {filename} must be an integer")
    if dtype_name == "float32" and not math.isfinite(float(value)):
        raise MapDataError(f"Missing sentinel for {filename} must be finite")
    return value


def check_overwrite(paths: list[Path], overwrite: bool) -> None:
    existing = [str(path) for path in paths if path.exists()]
    if existing and not overwrite:
        raise MapDataError("Refusing to overwrite existing output(s): " + ", ".join(existing) + ". Pass --overwrite to replace them.")
