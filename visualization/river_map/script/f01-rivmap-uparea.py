#!/usr/bin/env python3
"""Plot the Mekong map's upstream drainage area on its native grid."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm, Normalize
from netCDF4 import Dataset


def read_params(path: Path) -> tuple[int, int, float, float, float, float, float]:
    """Read nx, ny, resolution, and map-edge coordinates from params.txt."""
    try:
        values = [float(line.split()[0]) for line in path.read_text().splitlines()
                  if line.split() and not line.lstrip().startswith("#")]
    except (OSError, ValueError, IndexError) as exc:
        raise ValueError(f"Cannot read map geometry from {path}: {exc}") from exc
    if len(values) < 8:
        raise ValueError(f"Expected at least eight parameter values in {path}")
    nx, ny = int(values[0]), int(values[1])
    return nx, ny, values[3], values[4], values[5], values[6], values[7]


def verify_coordinates(lon: np.ndarray, lat: np.ndarray,
                       geometry: tuple[int, int, float, float, float, float, float]) -> None:
    nx, ny, resolution, west, east, south, north = geometry
    if len(lon) != nx or len(lat) != ny:
        raise ValueError(
            f"Coordinate dimensions {len(lon)}x{len(lat)} do not match params.txt {nx}x{ny}"
        )
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("Expected ascending longitude and descending latitude coordinates")
    expected_lon = west + (np.arange(nx) + 0.5) * resolution
    expected_lat = north - (np.arange(ny) + 0.5) * resolution
    if not np.allclose(lon, expected_lon, rtol=0, atol=1e-5):
        raise ValueError("NetCDF longitude centers do not match params.txt geometry")
    if not np.allclose(lat, expected_lat, rtol=0, atol=1e-5):
        raise ValueError("NetCDF latitude centers do not match params.txt geometry")
    if not np.isclose(east, west + nx * resolution) or not np.isclose(
            north, south + ny * resolution):
        raise ValueError("params.txt bounds, grid dimensions, and resolution are inconsistent")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--uparea", type=Path, required=True, help="Input uparea.nc")
    parser.add_argument("--params", type=Path, required=True, help="Map params.txt")
    parser.add_argument("--west", type=float, required=True, help="Displayed western edge")
    parser.add_argument("--east", type=float, required=True, help="Displayed eastern edge")
    parser.add_argument("--south", type=float, required=True, help="Displayed southern edge")
    parser.add_argument("--north", type=float, required=True, help="Displayed northern edge")
    parser.add_argument("--cmap", default="YlGnBu", help="Matplotlib sequential colormap")
    parser.add_argument("--scale", choices=("log", "linear"), default="log")
    parser.add_argument("--dpi", type=int, default=180)
    parser.add_argument("--output", type=Path, required=True, help="Output PNG path")
    args = parser.parse_args()
    if args.west >= args.east or args.south >= args.north:
        parser.error("map extent must have west < east and south < north")
    if args.dpi < 1:
        parser.error("--dpi must be positive")
    return args


def main() -> None:
    args = parse_args()
    geometry = read_params(args.params)
    nx, ny, _, map_west, map_east, map_south, map_north = geometry
    if (args.west < map_west or args.east > map_east or
            args.south < map_south or args.north > map_north):
        raise ValueError("Requested display extent must lie within the map bounds")

    with Dataset(args.uparea) as source:
        for name in ("lon", "lat", "uparea"):
            if name not in source.variables:
                raise ValueError(f"{args.uparea} is missing the required {name!r} variable")
        lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
        variable = source.variables["uparea"]
        if variable.dimensions != ("lat", "lon") or variable.shape != (ny, nx):
            raise ValueError(
                f"uparea dimensions/shape {variable.dimensions}/{variable.shape} "
                f"do not match expected ('lat', 'lon')/({ny}, {nx})"
            )
        units = getattr(variable, "units", "")
        if units != "m2":
            raise ValueError(f"Expected uparea units 'm2', found {units!r}")
        values = np.ma.asarray(variable[:], dtype=np.float64).filled(np.nan)

    verify_coordinates(lon, lat, geometry)
    valid = np.isfinite(values) & (values > 0)
    if not np.any(valid):
        raise ValueError("uparea.nc contains no finite positive drainage-area values")

    # Convert the physical values, but derive limits from the full map before zooming.
    area_km2 = values / 1_000_000.0
    valid_values = area_km2[valid]
    vmin, vmax = float(valid_values.min()), float(valid_values.max())
    if args.scale == "log":
        norm = LogNorm(vmin=vmin, vmax=vmax)
    else:
        norm = Normalize(vmin=vmin, vmax=vmax)

    fig, ax = plt.subplots(figsize=(8.5, 8.0))
    image = ax.imshow(
        np.ma.masked_where(~valid, area_km2), origin="upper",
        extent=(map_west, map_east, map_south, map_north),
        cmap=args.cmap, norm=norm, interpolation="nearest",
    )
    ax.set_xlim(args.west, args.east)
    ax.set_ylim(args.south, args.north)
    ax.set_xlabel("Longitude (°E)")
    ax.set_ylabel("Latitude (°N)")
    ax.set_title("Upstream drainage area on the 15-minute grid")
    mid_lat = (args.south + args.north) / 2.0
    ax.set_aspect(1.0 / np.cos(np.deg2rad(mid_lat)))
    fig.colorbar(image, ax=ax, label="Upstream drainage area (km²)")
    fig.tight_layout()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.output, dpi=args.dpi, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote {args.output}")
    print(f"Map grid: {nx} x {ny} at {geometry[2]:g}°; "
          f"normalization: {args.scale} ({vmin:g}–{vmax:g} km², full map)")
    print(f"Displayed extent: {args.west:g}–{args.east:g}°E, "
          f"{args.south:g}–{args.north:g}°N")


if __name__ == "__main__":
    main()
