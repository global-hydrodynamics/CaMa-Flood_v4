#!/usr/bin/env python3
"""Validate the common map-variable registry's missing/special-value fields."""
from __future__ import annotations

import csv
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REGISTRY = ROOT / "map" / "map_variables.csv"
INTEGER_RANGES = {"int8": (-128, 127), "int16": (-32768, 32767),
                  "int32": (-2147483648, 2147483647)}
FLOAT_TYPES = {"float32"}


def main() -> None:
    with REGISTRY.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        fields = set(reader.fieldnames or [])
        required = {"binary_filename", "name", "dtype", "missing_value", "special_values"}
        missing = required - fields
        if missing:
            raise SystemExit(f"Registry lacks required fields: {', '.join(sorted(missing))}")
        if "missing_value_convention" in fields:
            raise SystemExit("Legacy free-text column missing_value_convention is still present")
        rows = list(reader)

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
