#!/usr/bin/env python3
"""Copy an exact regional float32 window from daily ELSE_GPCC binary grids."""

import argparse
from datetime import date, timedelta
import math
from pathlib import Path
import re
import struct
import sys

FLOAT_BYTES = 4
SOURCE_NX = 360
SOURCE_NY = 180
OUTPUT_NX = 20
OUTPUT_NY = 30
SOURCE_PATTERN = "Roff____{date}.one"
OUTPUT_PATTERN = "Roff____{date}.one"


def read_ctl(path):
    """Read the small subset of GrADS CTL metadata used by the extractor."""
    metadata = {}
    options = set()
    with path.open(encoding="utf-8") as stream:
        for raw_line in stream:
            parts = raw_line.strip().split()
            if not parts:
                continue
            key = parts[0].lower()
            if key in {"xdef", "ydef"}:
                if len(parts) != 5 or parts[2].lower() != "linear":
                    raise ValueError(f"{path}: {key} must be a 1-D linear axis")
                metadata[key] = (int(parts[1]), float(parts[3]), float(parts[4]))
            elif key == "undef":
                metadata[key] = float(parts[1])
            elif key == "tdef":
                if len(parts) != 5 or parts[2].lower() != "linear":
                    raise ValueError(f"{path}: tdef must be linear")
                metadata[key] = (int(parts[1]), parts[3].lower(), parts[4].lower())
            elif key == "dset":
                metadata[key] = parts[1]
            elif key == "options":
                options.update(item.lower() for item in parts[1:])
    required = {"xdef", "ydef", "undef"}
    if required - metadata.keys():
        raise ValueError(f"{path}: missing CTL fields: {sorted(required - metadata.keys())}")
    metadata["options"] = options
    return metadata


def aligned_index(value, origin, spacing, label):
    index = round((value - origin) / spacing)
    if not math.isclose(origin + index * spacing, value, abs_tol=1e-8):
        raise ValueError(f"{label}={value} does not align with the source grid")
    return index


def date_range(start, end):
    current = start
    while current <= end:
        yield current
        current += timedelta(days=1)


def check_finite_values(data, undef, source_name):
    for (value,) in struct.iter_unpack("<f", data):
        if value != undef and not math.isfinite(value):
            raise ValueError(f"{source_name}: found unexpected NaN or infinity")


def write_ctl(path, start, end, west, south, nx, ny, resolution, undef):
    count = (end - start).days + 1
    ctl_text = (
        f"dset ^runoff/Roff____%y4%m2%d2.one\n"
        f"undef {undef:g}\n"
        f"title ELSE_GPCC daily runoff, Mekong 1-degree sample\n"
        f"options yrev little_endian template\n"
        f"xdef {nx} linear {west + resolution / 2:g} {resolution:g}\n"
        f"ydef {ny} linear {south + resolution / 2:g} {resolution:g}\n"
        f"tdef {count} linear 00Z{start.day:02d}{('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')[start.month - 1]}{start.year:04d} 1dy\n"
        f"zdef 1 linear 1 1\n"
        f"vars 1\n"
        f"var 0 99 ** runoff [mm/day]\n"
        f"ENDVARS\n"
    )
    path.write_text(ctl_text, encoding="utf-8")

    generated = read_ctl(path)
    expected_count = count
    expected_tdef = (
        expected_count,
        f"00z{start.day:02d}{('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')[start.month - 1]}{start.year:04d}",
        "1dy",
    )
    if generated.get("dset") != "^runoff/Roff____%y4%m2%d2.one":
        raise ValueError(f"{path}: generated dset does not match daily output names")
    if generated.get("xdef", (None, None, None))[0] != nx or not math.isclose(generated["xdef"][1], west + resolution / 2):
        raise ValueError(f"{path}: generated xdef does not match the requested longitude grid")
    if generated.get("ydef", (None, None, None))[0] != ny or not math.isclose(generated["ydef"][1], south + resolution / 2):
        raise ValueError(f"{path}: generated ydef does not match the requested latitude grid")
    if generated.get("tdef") != expected_tdef:
        raise ValueError(f"{path}: generated tdef does not match the inclusive daily dates")
    if not math.isclose(generated["undef"], undef) or not {"yrev", "little_endian", "template"}.issubset(generated["options"]):
        raise ValueError(f"{path}: generated undef or binary options are inconsistent")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, required=True)
    parser.add_argument("--source-ctl", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--west", type=float, required=True)
    parser.add_argument("--east", type=float, required=True)
    parser.add_argument("--south", type=float, required=True)
    parser.add_argument("--north", type=float, required=True)
    parser.add_argument("--start", type=date.fromisoformat, required=True)
    parser.add_argument("--end", type=date.fromisoformat, required=True)
    args = parser.parse_args()

    source_dir = args.source_dir.resolve()
    source_ctl = args.source_ctl.resolve()
    output_dir = args.output_dir.resolve()
    if not source_dir.is_dir():
        raise ValueError(f"source directory not found: {source_dir}")
    if not source_ctl.is_file():
        raise ValueError(f"source CTL not found: {source_ctl}")
    if source_dir == output_dir or source_dir in output_dir.parents:
        raise ValueError("output directory must not be inside the read-only source directory")
    if args.start > args.end:
        raise ValueError("start date must be on or before end date")
    if not (args.west < args.east and args.south < args.north):
        raise ValueError("invalid geographic domain")

    ctl = read_ctl(source_ctl)
    nx, x_first_center, dx = ctl["xdef"]
    ny, y_first_center, dy = ctl["ydef"]
    undef = ctl["undef"]
    options = ctl["options"]
    if (nx, ny) != (SOURCE_NX, SOURCE_NY):
        raise ValueError(f"source CTL grid is {nx}x{ny}; expected {SOURCE_NX}x{SOURCE_NY}")
    if dx <= 0 or dy <= 0 or not math.isclose(dx, 1.0) or not math.isclose(dy, 1.0):
        raise ValueError("source CTL must describe a regular 1-degree grid")
    if "little_endian" not in options or "yrev" not in options:
        raise ValueError("source CTL must declare little_endian and yrev")
    if not math.isclose(undef, -999.0):
        raise ValueError(f"source CTL undef={undef:g}; expected the source value -999")

    # xdef/ydef describe grid centers. yrev means the source rows run north-to-south.
    source_west = x_first_center - dx / 2
    source_south = y_first_center - dy / 2
    source_north = y_first_center + (ny - 1) * dy + dy / 2
    x_start = aligned_index(args.west, source_west, dx, "west")
    x_end = aligned_index(args.east, source_west, dx, "east")
    y_start = aligned_index(source_north - args.north, 0.0, dy, "north offset")
    y_end = aligned_index(source_north - args.south, 0.0, dy, "south offset")
    if (x_start, x_end, y_start, y_end) != (270, 290, 55, 85):
        raise ValueError(
            "domain does not resolve to expected source window columns 270:290, rows 55:85"
        )
    if (x_end - x_start, y_end - y_start) != (OUTPUT_NX, OUTPUT_NY):
        raise ValueError("requested grid dimensions are not 20x30")
    if args.west < source_west or args.east > source_west + nx * dx:
        raise ValueError("longitude domain falls outside the source grid")
    if args.south < source_south or args.north > source_north:
        raise ValueError("latitude domain falls outside the source grid")

    dates = list(date_range(args.start, args.end))
    expected_count = (args.end - args.start).days + 1
    if len(dates) != expected_count:
        raise ValueError("date sequence contains a duplicate or missing date")
    source_size = nx * ny * FLOAT_BYTES
    output_size = OUTPUT_NX * OUTPUT_NY * FLOAT_BYTES
    source_files = [source_dir / SOURCE_PATTERN.format(date=d.strftime("%Y%m%d")) for d in dates]
    missing = [path.name for path in source_files if not path.is_file()]
    if missing:
        preview = ", ".join(missing[:5])
        raise FileNotFoundError(f"{len(missing)} source date files are missing: {preview}")
    wrong_sizes = [f"{path.name}={path.stat().st_size}" for path in source_files if path.stat().st_size != source_size]
    if wrong_sizes:
        raise ValueError(f"source file size mismatch (expected {source_size} bytes): {wrong_sizes[:5]}")

    output_dir.mkdir(parents=True, exist_ok=True)
    runoff_dir = output_dir / "runoff"
    runoff_dir.mkdir(parents=True, exist_ok=True)
    expected_names = {OUTPUT_PATTERN.format(date=d.strftime("%Y%m%d")) for d in dates}
    existing_names = {path.name for path in runoff_dir.glob("Roff____*.one")}
    unexpected = sorted(existing_names - expected_names)
    if unexpected:
        raise ValueError(f"output directory contains dates outside requested period: {unexpected[:5]}")

    row_bytes = nx * FLOAT_BYTES
    crop_bytes = (x_end - x_start) * FLOAT_BYTES
    print(f"Source grid: {nx}x{ny}, 1 degree, N-to-S rows, undef={undef:g}")
    print(f"Window: columns {x_start}:{x_end}, rows {y_start}:{y_end}; output {OUTPUT_NX}x{OUTPUT_NY}")
    print(f"Dates: {args.start.isoformat()} through {args.end.isoformat()} ({expected_count} daily files)")
    print(f"Each output: {output_size} bytes; total: {output_size * expected_count} bytes")

    for index, (day, source_path) in enumerate(zip(dates, source_files), start=1):
        source_data = source_path.read_bytes()
        output_data = b"".join(
            source_data[row * row_bytes + x_start * FLOAT_BYTES:
                        row * row_bytes + x_end * FLOAT_BYTES]
            for row in range(y_start, y_end)
        )
        if len(output_data) != output_size:
            raise ValueError(f"{source_path.name}: extracted {len(output_data)} bytes, expected {output_size}")
        check_finite_values(output_data, undef, source_path.name)

        target_path = runoff_dir / OUTPUT_PATTERN.format(date=day.strftime("%Y%m%d"))
        temporary_path = target_path.with_suffix(target_path.suffix + ".tmp")
        temporary_path.write_bytes(output_data)
        temporary_path.replace(target_path)
        if target_path.stat().st_size != output_size or target_path.read_bytes() != output_data:
            raise ValueError(f"{target_path.name}: output size or exact source-window comparison failed")
        if index % 365 == 0 or index == expected_count:
            print(f"[PROGRESS] {index}/{expected_count}: {day.isoformat()}")

    actual_names = {path.name for path in runoff_dir.glob("Roff____*.one")}
    if actual_names != expected_names:
        raise ValueError(f"output date set mismatch: expected {len(expected_names)}, found {len(actual_names)}")
    wrong_output_sizes = [path.name for path in runoff_dir.glob("Roff____*.one") if path.stat().st_size != output_size]
    if wrong_output_sizes:
        raise ValueError(f"output size mismatch: {wrong_output_sizes[:5]}")
    ctl_path = output_dir / "runoff.ctl"
    write_ctl(ctl_path, args.start, args.end, args.west, args.south,
              OUTPUT_NX, OUTPUT_NY, dx, undef)
    print(f"Validated {len(actual_names)} files ({output_size * len(actual_names)} bytes total).")
    print(f"Exact float32 byte windows, source dates, and output sizes verified; wrote {ctl_path}.")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as error:
        print(f"[ERROR] {error}", file=sys.stderr)
        raise SystemExit(1)
