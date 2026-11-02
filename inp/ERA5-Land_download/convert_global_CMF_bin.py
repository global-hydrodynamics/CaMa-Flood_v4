#!/usr/bin/env python3
"""Convert global ERA5-Land monthly NetCDF in bounded monthly chunks.

This orchestrates the existing ERA5-Land binary converter without changing
its accumulation, unit-conversion, or timestamp behavior.
"""
from __future__ import annotations

import argparse
import calendar
from datetime import date, timedelta
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--start", required=True, help="Inclusive date, YYYY-MM-DD")
    parser.add_argument("--end", required=True, help="Inclusive date, YYYY-MM-DD")
    parser.add_argument("--indir", type=Path, required=True, help="Directory of monthly source NetCDF")
    parser.add_argument("--outdir", type=Path, required=True, help="Destination for daily binary files and runoff.ctl")
    parser.add_argument("--input-variable", default="runoff")
    parser.add_argument("--binary-prefix", default="runoff_")
    parser.add_argument("--binary-suffix", default=".bin")
    parser.add_argument("--undef", type=float, default=1.0e20)
    parser.add_argument("--endian", choices=["little", "big"], default="little")
    parser.add_argument("--overwrite", action="store_true", help="Replace existing daily outputs")
    parser.add_argument("--dry-run", action="store_true", help="List monthly conversions without reading data")
    return parser.parse_args()


def months(start: date, end: date):
    current = date(start.year, start.month, 1)
    last = date(end.year, end.month, 1)
    while current <= last:
        month_end = date(current.year, current.month, calendar.monthrange(current.year, current.month)[1])
        yield max(current, start), min(month_end, end)
        current = date(current.year + (current.month == 12), current.month % 12 + 1, 1)


def grads_time(day: date) -> str:
    month = ("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")[day.month - 1]
    return f"00Z{day.day:02d}{month}{day.year:04d}"


def rewrite_ctl(source: Path, destination: Path, *, prefix: str, suffix: str, start: date, records: int) -> tuple[int, int]:
    lines = source.read_text(encoding="utf-8").splitlines()
    output = []
    seen_tdef = False
    nx = ny = 0
    for line in lines:
        if line.lstrip().lower().startswith("dset"):
            output.append(f"dset  ^./{prefix}%y4%m2%d2{suffix}")
        elif line.lstrip().lower().startswith("xdef"):
            nx = int(line.split()[1])
            output.append(line)
        elif line.lstrip().lower().startswith("ydef"):
            ny = int(line.split()[1])
            output.append(line)
        elif line.lstrip().lower().startswith("tdef"):
            match = re.fullmatch(r"\s*tdef\s+\S+\s+linear\s+\S+\s+(.+?)\s*", line, re.I)
            if not match:
                raise ValueError(f"Cannot parse converter CTL tdef line: {line}")
            output.append(f"tdef  {records:5d} linear  {grads_time(start)} {match.group(1)}")
            seen_tdef = True
        else:
            output.append(line)
    if not seen_tdef:
        raise ValueError(f"No tdef found in {source}")
    if nx <= 0 or ny <= 0:
        raise ValueError(f"Cannot determine grid dimensions from {source}")
    destination.write_text("\n".join(output) + "\n", encoding="utf-8")
    return nx, ny


def main() -> int:
    args = parse_args()
    start = date.fromisoformat(args.start)
    end = date.fromisoformat(args.end)
    if end < start:
        raise ValueError("--end must be on or after --start")
    if not args.indir.is_dir():
        raise FileNotFoundError(f"Input directory not found: {args.indir}")
    converter = Path(__file__).with_name("convert_CMF_bin.py")
    chunks = list(months(start, end))
    missing = [args.indir / f"ERA5-Land_runoff_{a:%Y%m}.nc" for a, _ in chunks
               if not (args.indir / f"ERA5-Land_runoff_{a:%Y%m}.nc").is_file()]
    if missing:
        raise FileNotFoundError("Missing monthly NetCDF input(s): " + ", ".join(map(str, missing)))
    args.outdir.mkdir(parents=True, exist_ok=True)
    expected = [args.outdir / f"{args.binary_prefix}{(start + timedelta(days=i)):%Y%m%d}{args.binary_suffix}"
                for i in range((end - start).days + 1)]
    existing = [p for p in expected if p.exists()]
    if existing and not args.overwrite and not args.dry_run:
        raise FileExistsError(f"{len(existing)} output file(s) already exist; use --overwrite to replace them")
    if args.dry_run:
        for chunk_start, chunk_end in chunks:
            print(f"[DRY-RUN] {chunk_start}..{chunk_end}: {args.indir}/ERA5-Land_runoff_{chunk_start:%Y%m}.nc")
        print(f"[DRY-RUN] Expected output daily files: {len(expected)}")
        return 0

    template_ctl = None
    for chunk_start, chunk_end in chunks:
        print(f"[STEP] Convert {chunk_start} through {chunk_end}", flush=True)
        with tempfile.TemporaryDirectory(prefix=".era5land-global-", dir=args.outdir) as temp:
            tempdir = Path(temp)
            cmd = [
                sys.executable, str(converter), "--start", chunk_start.isoformat(), "--end", chunk_end.isoformat(),
                "--indir", str(args.indir.resolve()), "--outdir", str(tempdir),
                "--input-file-unit", "monthly", "--input-pattern", "ERA5-Land_runoff_%Y%m.nc",
                "--input-variable", args.input_variable, "--timestep-hours", "24", "--output-timestep-hours", "24",
                "--binary-prefix", args.binary_prefix, "--binary-suffix", args.binary_suffix,
                "--ctl-file", str(tempdir / "runoff.ctl"), "--undef", str(args.undef), "--endian", args.endian,
            ]
            subprocess.run(cmd, check=True)
            chunk_files = sorted(tempdir.glob(f"{args.binary_prefix}*{args.binary_suffix}"))
            expected_days = (chunk_end - chunk_start).days + 1
            if len(chunk_files) != expected_days:
                raise RuntimeError(f"{chunk_start:%Y-%m}: expected {expected_days} daily files, found {len(chunk_files)}")
            ctl = tempdir / "runoff.ctl"
            if not ctl.is_file():
                raise RuntimeError(f"Converter did not write {ctl}")
            template_ctl = ctl.read_text(encoding="utf-8")
            for path in chunk_files:
                target = args.outdir / path.name
                if target.exists() and not args.overwrite:
                    raise FileExistsError(f"Refusing to replace {target}")
                shutil.move(str(path), str(target))

    if template_ctl is None:
        raise RuntimeError("No monthly chunks were converted")
    # Reuse the converter's actual grid, variable, endian, and unit metadata.
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", suffix=".ctl", delete=False) as tmp:
        tmp.write(template_ctl)
        tmp_path = Path(tmp.name)
    try:
        nx, ny = rewrite_ctl(tmp_path, args.outdir / "runoff.ctl", prefix=args.binary_prefix,
                             suffix=args.binary_suffix, start=start, records=len(expected))
    finally:
        tmp_path.unlink(missing_ok=True)
    for path in expected:
        expected_size = nx * ny * 4
        if not path.is_file() or path.stat().st_size != expected_size:
            actual_size = path.stat().st_size if path.exists() else "missing"
            raise RuntimeError(f"Output size mismatch for {path}: expected {expected_size}, got {actual_size}")
    print(f"[SUCCESS] Wrote {len(expected)} daily files and {args.outdir / 'runoff.ctl'}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        raise SystemExit(1)
