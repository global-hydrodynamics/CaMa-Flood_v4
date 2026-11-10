#!/usr/bin/env python3
"""Create and validate the tutorial-local +3 m river-depth map."""

from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path
import tempfile

import netCDF4
import numpy as np

SCRIPT_DIR = Path(__file__).resolve().parent
SOURCE = SCRIPT_DIR.parents[2] / "map/mekong_15min/netcdf/rivhgt.nc"
OUTPUT = SCRIPT_DIR / "map_modify/rivhgt_deep.nc"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def build_map(source_path: Path, output_path: Path, force: bool = False) -> dict[str, object]:
    if not source_path.is_file():
        raise FileNotFoundError(f"Shared source map not found: {source_path}")
    if output_path.exists() and not force:
        raise FileExistsError(f"Output already exists (use --force to replace): {output_path}")

    source_hash_before = sha256(source_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with netCDF4.Dataset(source_path, "r") as src:
        if "rivhgt" not in src.variables:
            raise ValueError(f"Missing variable 'rivhgt' in {source_path}")
        source_var = src.variables["rivhgt"]
        values = np.asarray(source_var[:])
        fill = getattr(source_var, "_FillValue", getattr(source_var, "missing_value", None))
        if values.dtype != np.dtype("float32"):
            raise ValueError(f"Expected Float32 rivhgt; found {values.dtype}")
        if fill is None:
            raise ValueError("Source rivhgt has no _FillValue or missing_value")
        valid = np.isfinite(values) & (values != np.float32(fill))
        missing_value = getattr(source_var, "missing_value", fill)
        if missing_value is not None:
            valid &= values != np.float32(missing_value)
        if not valid.any():
            raise ValueError("Source rivhgt contains no valid cells")

        modified = values.copy()
        modified[valid] = (modified[valid] + np.float32(3.0)).astype(np.float32)
        fd, tmp_name = tempfile.mkstemp(prefix=".rivhgt_deep.", suffix=".nc", dir=output_path.parent)
        os.close(fd)
        tmp_path = Path(tmp_name)
        try:
            with netCDF4.Dataset(tmp_path, "w", format="NETCDF4") as dst:
                for name, dim in src.dimensions.items():
                    dst.createDimension(name, None if dim.isunlimited() else len(dim))
                for name in src.ncattrs():
                    dst.setncattr(name, src.getncattr(name))
                for name, var in src.variables.items():
                    options = {}
                    if name == "rivhgt" and "_FillValue" in var.ncattrs():
                        options["fill_value"] = var.getncattr("_FillValue")
                    target = dst.createVariable(name, var.datatype, var.dimensions, **options)
                    for attr in var.ncattrs():
                        if attr != "_FillValue":
                            target.setncattr(attr, var.getncattr(attr))
                    target[:] = modified if name == "rivhgt" else var[:]
            with netCDF4.Dataset(tmp_path, "r") as check:
                out = check.variables["rivhgt"][:]
                out_mask = np.ma.getmaskarray(out)
                raw_out = np.asarray(out.filled(fill), dtype=np.float32)
                if out.shape != values.shape or out.dtype != values.dtype:
                    raise ValueError("Output rivhgt dimensions or dtype changed")
                if not np.array_equal(out_mask, ~valid):
                    raise ValueError("Output missing mask differs from source")
                delta = raw_out[valid] - values[valid]
                if not np.allclose(delta, np.float32(3.0), rtol=0.0, atol=1e-6):
                    raise ValueError("Output does not contain the intended +3 m change")
                for coord in ("lon", "lat"):
                    if coord in src.variables:
                        if not np.array_equal(check.variables[coord][:], src.variables[coord][:]):
                            raise ValueError(f"Coordinate {coord} changed")
            if sha256(source_path) != source_hash_before:
                raise RuntimeError("Shared source checksum changed during map preparation")
            os.replace(tmp_path, output_path)
        finally:
            if tmp_path.exists():
                tmp_path.unlink()

    return {
        "source_sha256": source_hash_before,
        "output_sha256": sha256(output_path),
        "valid_cells": int(valid.sum()),
        "missing_cells": int(valid.size - valid.sum()),
        "shape": tuple(int(n) for n in values.shape),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=SOURCE)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    parser.add_argument("--force", action="store_true", help="Replace the tutorial-local output file")
    args = parser.parse_args()
    result = build_map(args.source.resolve(), args.output.resolve(), args.force)
    print(f"[SUCCESS] Wrote {args.output.resolve()}")
    print(f"[SUCCESS] Shape={result['shape']}; valid={result['valid_cells']}; missing={result['missing_cells']}")
    print(f"[CHECK] Source SHA-256: {result['source_sha256']}")
    print(f"[CHECK] Output SHA-256: {result['output_sha256']}")
    print("[CHECK] +3.0 m on valid cells; missing mask, grid, and dtype preserved.")


if __name__ == "__main__":
    main()
