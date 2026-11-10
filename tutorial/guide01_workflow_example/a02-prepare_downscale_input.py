#!/usr/bin/env python3
"""Read one NetCDF event and prepare the coarse Float32 input for the legacy downscaler."""

from __future__ import annotations

import argparse
import csv
import os
from pathlib import Path
import tempfile

import numpy as np
import pandas as pd
import xarray as xr

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[2]
EVENT_CSV = ROOT / "analysis/event_selection.csv"
OUTPUT_DIR = ROOT / "out/downscaling"


def selected_event(path: Path) -> pd.Timestamp:
    with path.open(newline="") as stream:
        row = next(csv.DictReader(stream))
    status = row.get("selection_status", "")
    if not status.startswith(("provisional", "reviewed")):
        raise ValueError("Event-selection record is not marked as provisional or reviewed")
    return pd.Timestamp(row["time_coordinate_interval_end"])


def prepare_input(rivdph_path: Path, rivhgt_path: Path, event_time: str | pd.Timestamp,
                  output_path: Path) -> dict[str, object]:
    target = pd.Timestamp(event_time).to_datetime64()
    with xr.open_dataset(rivdph_path, decode_times=True, mask_and_scale=True) as output_ds, \
         xr.open_dataset(rivhgt_path, decode_times=False, mask_and_scale=True) as map_ds:
        if "rivdph" not in output_ds or "rivhgt" not in map_ds:
            raise ValueError("Required NetCDF variables rivdph/rivhgt were not found")
        water_depth = output_ds["rivdph"]
        channel_depth = map_ds["rivhgt"]
        if water_depth.attrs.get("units") != "m" or channel_depth.attrs.get("units") != "m":
            raise ValueError("rivdph and rivhgt must both declare units='m'")
        if not np.array_equal(water_depth.lat.values, channel_depth.lat.values) or \
           not np.array_equal(water_depth.lon.values, channel_depth.lon.values):
            raise ValueError("rivdph output grid does not match the selected rivhgt map")
        times = output_ds.time.values
        matches = np.flatnonzero(times == target)
        if len(matches) != 1:
            raise ValueError(f"Requested event {pd.Timestamp(target)} matched {len(matches)} time records")
        index = int(matches[0])
        q = np.asarray(water_depth.isel(time=index).values, dtype=np.float64)
        h = np.asarray(channel_depth.values, dtype=np.float64)
        valid = np.isfinite(q) & np.isfinite(h)
        for da, values in ((water_depth, q), (channel_depth, h)):
            fill = da.attrs.get("_FillValue", da.encoding.get("_FillValue"))
            missing = da.attrs.get("missing_value")
            if fill is not None:
                valid &= values != float(fill)
            if missing is not None:
                valid &= values != float(missing)
        if not valid.any():
            raise ValueError("No overlapping valid cells in event rivdph and rivhgt")
        flddph = np.full(q.shape, np.float32(1.0e20), dtype=np.float32)
        flddph[valid] = np.maximum(q[valid] - h[valid], 0.0).astype(np.float32)
        if not np.array_equal(flddph.shape, q.shape):
            raise ValueError("Derived field dimensions changed")

        output_path.parent.mkdir(parents=True, exist_ok=True)
        fd, temp_name = tempfile.mkstemp(prefix=f".{output_path.name}.", dir=output_path.parent)
        os.close(fd)
        temp_path = Path(temp_name)
        try:
            # C order for (lat,lon) writes x/longitude fastest, matching the legacy direct-access grid.
            flddph.astype("<f4", copy=False).tofile(temp_path)
            if temp_path.stat().st_size != q.size * 4:
                raise ValueError("Prepared Binary byte size does not match one Float32 map record")
            os.replace(temp_path, output_path)
        finally:
            if temp_path.exists():
                temp_path.unlink()

    return {"time_index_zero_based": index, "timestamp": str(pd.Timestamp(target)),
            "valid_cells": int(valid.sum()), "missing_cells": int(valid.size - valid.sum()),
            "shape": tuple(int(n) for n in q.shape), "minimum_flddph_m": float(flddph[valid].min()),
            "maximum_flddph_m": float(flddph[valid].max()), "output": str(output_path)}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", choices=("control", "deep_channel"), required=True)
    parser.add_argument("--event-csv", type=Path, default=EVENT_CSV)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    event = selected_event(args.event_csv)
    case_dir = ROOT / "out" / args.case
    rivhgt = (REPO / "map/mekong_15min/netcdf/rivhgt.nc" if args.case == "control"
              else ROOT / "map_modify/rivhgt_deep.nc")
    destination = args.output or OUTPUT_DIR / args.case / "input" / f"flddph{event.strftime('%Y%m%d')}.bin"
    result = prepare_input(case_dir / "o_rivdph2000.nc", rivhgt, event, destination)
    print(f"[SUCCESS] Derived flddph=max(rivdph-rivhgt,0) for {result['timestamp']}")
    print(f"[CHECK] time index={result['time_index_zero_based']}; shape={result['shape']}; "
          f"valid={result['valid_cells']}; missing={result['missing_cells']}; "
          f"range={result['minimum_flddph_m']:.6g}..{result['maximum_flddph_m']:.6g} m")
    print(f"[SUCCESS] Wrote one Float32 map record: {result['output']}")


if __name__ == "__main__":
    main()
