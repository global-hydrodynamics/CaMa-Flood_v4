#!/usr/bin/env python3
"""Validate common map-variable metadata and binary product grouping."""
from __future__ import annotations

import csv
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REGISTRY = ROOT / "map" / "map_variables.csv"
INTEGER_RANGES = {"int8": (-128, 127), "int16": (-32768, 32767),
                  "int32": (-2147483648, 2147483647)}
FLOAT_TYPES = {"float32"}
PRODUCT_KINDS = {"binary_map", "binary_mult", "text_meta", "text_table"}


def main() -> None:
    with REGISTRY.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        fields = set(reader.fieldnames or [])
        required = {
            "binary_filename", "name", "product_kind", "dtype",
            "missing_value", "special_values",
        }
        missing = required - fields
        if missing:
            raise SystemExit(f"Registry lacks required fields: {', '.join(sorted(missing))}")
        if "missing_value_convention" in fields:
            raise SystemExit("Legacy free-text column missing_value_convention is still present")
        rows = list(reader)

    for line, row in enumerate(rows, start=2):
        kind = (row["product_kind"] or "").strip()
        if kind not in PRODUCT_KINDS:
            raise SystemExit(
                f"line {line}: product_kind must be one of "
                f"{', '.join(sorted(PRODUCT_KINDS))}, got {kind!r}"
            )

    binary_files: dict[str, list[dict[str, str]]] = {}
    for row in rows:
        if row["product_kind"] in {"binary_map", "binary_mult"}:
            binary_files.setdefault(row["binary_filename"], []).append(row)
    for filename, file_rows in binary_files.items():
        kinds = {row["product_kind"] for row in file_rows}
        if len(kinds) != 1:
            raise SystemExit(f"{filename}: all variable rows must use one product_kind")
        if len(file_rows) > 1 and kinds != {"binary_mult"}:
            raise SystemExit(f"{filename}: multi-variable files must use product_kind=binary_mult")
        has_3d_array = False
        for row in file_rows:
            dimensions = (row["dimensions"] or "").split("(", 1)[0].replace(" ", "")
            rank = dimensions.count(",") + 1 if dimensions else 0
            if rank > 2 and row["product_kind"] != "binary_mult":
                raise SystemExit(f"{filename}/{row['name']}: arrays above 2-D must use binary_mult")
            has_3d_array = has_3d_array or rank > 2
            if "/" in (row["name"] or ""):
                raise SystemExit(f"{filename}: name must contain one variable short-name per row")
        if kinds == {"binary_mult"} and len(file_rows) == 1 and not has_3d_array:
            raise SystemExit(f"{filename}: binary_mult requires multiple variable rows or a multidimensional array")

    explicit = 0
    blanks = 0
    seen = set()
    for line, row in enumerate(rows, start=2):
        identity = (row["binary_filename"], row["name"])
        if identity in seen:
            raise SystemExit(f"line {line}: duplicate binary/name entry {identity}")
        seen.add(identity)
        raw = (row["missing_value"] or "").strip()
        dtype = (row["dtype"] or "").strip().lower()
        if not raw:
            blanks += 1
            continue
        explicit += 1
        try:
            value = float(raw)
        except ValueError:
            raise SystemExit(f"line {line}: missing_value must contain one numeric value, got {raw!r}")
        if not math.isfinite(value):
            raise SystemExit(f"line {line}: missing_value must be finite")
        if dtype in INTEGER_RANGES:
            if not value.is_integer() or not INTEGER_RANGES[dtype][0] <= value <= INTEGER_RANGES[dtype][1]:
                raise SystemExit(f"line {line}: missing_value {raw} is invalid for {dtype}")
        elif dtype in FLOAT_TYPES:
            if abs(value) > 3.4028235e38:
                raise SystemExit(f"line {line}: missing_value {raw} is out of float32 range")
    print(f"Map registry valid: {len(rows)} entries; {explicit} numeric missing values; {blanks} unresolved/blank values.")


if __name__ == "__main__":
    main()
