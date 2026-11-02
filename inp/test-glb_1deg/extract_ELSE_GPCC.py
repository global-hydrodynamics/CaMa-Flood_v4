#!/usr/bin/env python3
"""Copy and validate the legacy two-year global ELSE_GPCC runoff sample."""

import argparse
from datetime import date, timedelta
from pathlib import Path
import shutil
import sys

NX = 360
NY = 180
FLOAT_BYTES = 4
UNDEF = -999.0
SOURCE_PATTERN = "Roff____{date}.one"


def read_ctl(path):
    fields = {}
    options = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        parts = raw_line.strip().split()
        if not parts:
            continue
        key = parts[0].lower()
        if key in {"xdef", "ydef"}:
            if len(parts) != 5 or parts[2].lower() != "linear":
                raise ValueError(f"{path}: {key} must use a linear axis")
            fields[key] = (int(parts[1]), float(parts[3]), float(parts[4]))
        elif key == "undef":
            fields["undef"] = float(parts[1])
        elif key == "tdef":
            fields["tdef"] = tuple(parts[1:])
        elif key == "options":
            options.update(value.lower() for value in parts[1:])
    required = {"xdef", "ydef", "undef"}
    if required - fields.keys():
        raise ValueError(f"{path}: missing CTL fields {sorted(required - fields.keys())}")
    fields["options"] = options
    return fields


def dates_inclusive(start, end):
    current = start
    while current <= end:
        yield current
        current += timedelta(days=1)


def ctl_text(start, end):
    count = (end - start).days + 1
    months = ("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    return (
        "dset ^Roff/Roff____%y4%m2%d2.one\n"
        f"undef {UNDEF:g}\n"
        "title ELSE_GPCC global daily runoff, 1-degree test sample\n"
        "options yrev little_endian template\n"
        "xdef 360 linear -179.5 1.0\n"
        "ydef 180 linear -89.5 1.0\n"
        f"tdef {count} linear 00Z{start.day:02d}{months[start.month - 1]}{start.year:04d} 1dy\n"
        "zdef 1 linear 1 1\n"
        "vars 1\n"
        "var 0 99 ** runoff [mm/day]\n"
        "ENDVARS\n"
    )


def validate_ctl(path, start, end):
    ctl = read_ctl(path)
    if ctl["xdef"] != (NX, -179.5, 1.0) or ctl["ydef"] != (NY, -89.5, 1.0):
        raise ValueError(f"{path}: output grid does not match global 1-degree geometry")
    if ctl["undef"] != UNDEF or not {"yrev", "little_endian", "template"}.issubset(ctl["options"]):
        raise ValueError(f"{path}: output missing-value or binary options are incorrect")
    expected_count = (end - start).days + 1
    expected_start = f"00Z{start.day:02d}{('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')[start.month - 1]}{start.year:04d}"
    if ctl.get("tdef") != (str(expected_count), "linear", expected_start, "1dy"):
        raise ValueError(f"{path}: output time axis is inconsistent with the selected dates")
    text = path.read_text(encoding="utf-8").lower()
    if "runoff [mm/day]" not in text or "dset ^roff/roff____%y4%m2%d2.one" not in text:
        raise ValueError(f"{path}: output variable units or filename template is incorrect")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, required=True)
    parser.add_argument("--source-ctl", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--start", type=date.fromisoformat, required=True)
    parser.add_argument("--end", type=date.fromisoformat, required=True)
    args = parser.parse_args()

    source_dir = args.source_dir.resolve(strict=True)
    source_ctl = args.source_ctl.resolve(strict=True)
    output_dir = args.output_dir.resolve()
    runoff_dir = output_dir / "Roff"
    if not source_dir.is_dir() or not source_ctl.is_file():
        raise ValueError("source directory or CTL is not available")
    output_resolved = runoff_dir.resolve()
    if source_dir == output_resolved or source_dir in output_resolved.parents:
        raise ValueError("output Roff directory must not be inside the source data")
    if args.start > args.end:
        raise ValueError("start date must not follow end date")

    source_ctl_info = read_ctl(source_ctl)
    if source_ctl_info["xdef"] != (NX, -179.5, 1.0) or source_ctl_info["ydef"] != (NY, -89.5, 1.0):
        raise ValueError("source CTL does not describe the expected global 360x180 1-degree grid")
    if source_ctl_info["undef"] != UNDEF:
        raise ValueError(f"source CTL undef is {source_ctl_info['undef']}; expected {UNDEF:g}")
    if not {"yrev", "little_endian"}.issubset(source_ctl_info["options"]):
        raise ValueError("source CTL must declare north-to-south little-endian binary data")
    source_tdef = source_ctl_info.get("tdef")
    if source_tdef != ("7671", "linear", "00Z01jan1981", "1dy"):
        print(f"[INFO] Source CTL tdef is {source_tdef}; requested dates will be verified against filenames.")
    else:
        print("[INFO] Source CTL tdef (7,671 records from 1981) does not describe all available filenames; using dated files.")

    dates = list(dates_inclusive(args.start, args.end))
    names = {SOURCE_PATTERN.format(date=day.strftime("%Y%m%d")) for day in dates}
    existing = {path.name for path in runoff_dir.glob("Roff____*.one")} if runoff_dir.exists() else set()
    unexpected = sorted(existing - names)
    if unexpected:
        raise ValueError(f"output contains dates outside the selected period: {unexpected[:5]}")

    expected_size = NX * NY * FLOAT_BYTES
    source_files = [source_dir / SOURCE_PATTERN.format(date=day.strftime("%Y%m%d")) for day in dates]
    missing = [path.name for path in source_files if not path.is_file()]
    if missing:
        raise FileNotFoundError(f"missing {len(missing)} source files; first: {missing[0]}")
    wrong_sizes = [(path.name, path.stat().st_size) for path in source_files if path.stat().st_size != expected_size]
    if wrong_sizes:
        raise ValueError(f"source files must be {expected_size} bytes: {wrong_sizes[:5]}")

    source_state = {path: (path.stat().st_size, path.stat().st_mtime_ns) for path in source_files}
    runoff_dir.mkdir(parents=True, exist_ok=True)
    print(f"Source grid: {NX}x{NY}, 1 degree; yrev, little-endian float32; undef={UNDEF:g}")
    print(f"Period: {args.start.isoformat()} through {args.end.isoformat()} ({len(dates)} daily files)")
    print(f"Each file: {expected_size} bytes; total: {expected_size * len(dates)} bytes")

    for index, (day, source_path) in enumerate(zip(dates, source_files), start=1):
        target = runoff_dir / source_path.name
        temporary = target.with_suffix(target.suffix + ".tmp")
        shutil.copyfile(source_path, temporary)
        if temporary.stat().st_size != expected_size or temporary.read_bytes() != source_path.read_bytes():
            temporary.unlink(missing_ok=True)
            raise ValueError(f"{source_path.name}: copied values differ from source or file size is wrong")
        temporary.replace(target)
        if index % 100 == 0 or index == len(dates):
            print(f"[PROGRESS] {index}/{len(dates)}: {day.isoformat()}")

    actual = {path.name for path in runoff_dir.glob("Roff____*.one")}
    if actual != names:
        raise ValueError(f"output date set mismatch: expected {len(names)}, found {len(actual)}")
    if any((runoff_dir / name).stat().st_size != expected_size for name in names):
        raise ValueError("one or more output files have an incorrect size")
    if source_state != {path: (path.stat().st_size, path.stat().st_mtime_ns) for path in source_files}:
        raise ValueError("source files changed during extraction")

    ctl_path = output_dir / "runoff.ctl"
    ctl_path.write_text(ctl_text(args.start, args.end), encoding="ascii")
    validate_ctl(ctl_path, args.start, args.end)
    print(f"[SUCCESS] Validated {len(actual)} source-identical files; wrote {ctl_path}.")
    print("[INFO] No temporal or spatial transformation was applied; source mm/day values and N-to-S order are preserved.")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as error:
        print(f"[ERROR] {error}", file=sys.stderr)
        raise SystemExit(1)
