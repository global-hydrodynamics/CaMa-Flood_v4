"""Shared map and station-link loading/drawing helpers for Figure p01-f02."""

from __future__ import annotations

import argparse
import csv
import math
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from matplotlib.ticker import MultipleLocator
from netCDF4 import Dataset
import numpy as np


def parse_args(description: str) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("--uparea", required=True, type=Path)
    parser.add_argument("--params", required=True, type=Path)
    parser.add_argument("--stations", required=True, type=Path)
    parser.add_argument("--west", required=True, type=float)
    parser.add_argument("--east", required=True, type=float)
    parser.add_argument("--south", required=True, type=float)
    parser.add_argument("--north", required=True, type=float)
    parser.add_argument("--dpi", default=200, type=int)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    if not (args.west < args.east and args.south < args.north):
        parser.error("map extent must have west < east and south < north")
    if args.dpi < 1:
        parser.error("--dpi must be positive")
    return args


def read_params(path: Path) -> tuple[int, int, float, float, float, float, float]:
    values = [float(line.split()[0]) for line in path.read_text().splitlines()
              if line.split() and not line.lstrip().startswith("#")]
    if len(values) < 8:
        raise ValueError(f"Expected at least eight geometry values in {path}")
    return (int(values[0]), int(values[1]), values[3],
            values[4], values[5], values[6], values[7])


def read_map_and_links(args: argparse.Namespace):
    """Read map geometry, uparea, and preselected station-to-cell links."""
    nx, ny, resolution, west, east, south, north = read_params(args.params)
    if (args.west < west or args.east > east or
            args.south < south or args.north > north):
        raise ValueError("display extent must be contained within params.txt map bounds")

    with Dataset(args.uparea) as data:
        lon = np.asarray(data.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(data.variables["lat"][:], dtype=np.float64)
        variable = data.variables["uparea"]
        if variable.dimensions != ("lat", "lon") or variable.shape != (ny, nx):
            raise ValueError("uparea dimensions do not match params.txt geometry")
        if getattr(variable, "units", "") != "m2":
            raise ValueError("expected uparea units 'm2'")
        uparea = np.ma.asarray(variable[:], dtype=np.float64).filled(np.nan)

    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("expected longitude increasing and latitude decreasing")
    if not np.allclose(lon, west + (np.arange(nx) + 0.5) * resolution, atol=1e-5, rtol=0):
        raise ValueError("uparea longitude centers do not match params.txt")
    if not np.allclose(lat, north - (np.arange(ny) + 0.5) * resolution, atol=1e-5, rtol=0):
        raise ValueError("uparea latitude centers do not match params.txt")

    stations = []
    with args.stations.open(newline="", encoding="utf-8-sig") as stream:
        for row in csv.DictReader(stream):
            station_lon = float(row["station_lon_e"])
            station_lat = float(row["station_lat_n"])
            ix, iy = int(row["ix"]), int(row["iy"])
            expected_ix = math.floor((station_lon - west) / resolution) + 1
            expected_iy = math.floor((north - station_lat) / resolution) + 1
            if (ix, iy) != (expected_ix, expected_iy):
                raise ValueError(
                    f"{row['station_name']} cell ({ix},{iy}) does not match "
                    f"the cell containing its station coordinates ({expected_ix},{expected_iy})"
                )
            if not (1 <= ix <= nx and 1 <= iy <= ny):
                raise ValueError(f"{row['station_name']} maps outside the model grid")
            area_km2 = float(uparea[iy - 1, ix - 1]) / 1_000_000.0
            if not np.isfinite(area_km2) or area_km2 <= 0:
                raise ValueError(f"{row['station_name']} has invalid linked-cell uparea")
            stations.append({
                "station_id": row["station_id"],
                "name": row["station_name"],
                "river": row["river_name"],
                "station_lon": station_lon,
                "station_lat": station_lat,
                "ix": ix,
                "iy": iy,
                "cell_lon": west + (ix - 0.5) * resolution,
                "cell_lat": north - (iy - 0.5) * resolution,
                "area_km2": area_km2,
            })
    if not stations:
        raise ValueError(f"No station links found in {args.stations}")
    return lon, lat, uparea, stations, (west, east, south, north)


def draw_background(ax, lon: np.ndarray, lat: np.ndarray, uparea: np.ndarray,
                    map_bounds: tuple[float, float, float, float]) -> None:
    """Add a light, full-map log-scaled drainage-area raster for context."""
    from matplotlib.colors import LogNorm

    valid = np.isfinite(uparea) & (uparea > 0)
    image = ax.imshow(
        np.ma.masked_where(~valid, uparea / 1_000_000.0),
        origin="upper",
        extent=map_bounds,
        cmap="YlGnBu",
        norm=LogNorm(vmin=float(np.nanmin(uparea[valid]) / 1_000_000.0),
                     vmax=float(np.nanmax(uparea[valid]) / 1_000_000.0)),
        interpolation="nearest",
        alpha=0.32,
        zorder=0,
    )
    image.cmap.set_bad("white")


def set_map_axes(ax, args: argparse.Namespace) -> None:
    mid_lat = (args.south + args.north) / 2.0
    ax.set_xlim(args.west, args.east)
    ax.set_ylim(args.south, args.north)
    ax.set_aspect(1.0 / np.cos(np.deg2rad(mid_lat)))
    ax.set_xlabel("Longitude (°E)")
    ax.set_ylabel("Latitude (°N)")
    ax.xaxis.set_major_locator(MultipleLocator(1.0))
    ax.yaxis.set_major_locator(MultipleLocator(2.0))
    ax.grid(color="white", linewidth=0.45, alpha=0.8, zorder=1)


def draw_station_and_cell_markers(ax, stations: list[dict],
                                  label_offsets: dict[str, tuple[int, int]]) -> None:
    for station in stations:
        ax.scatter(station["station_lon"], station["station_lat"],
                   marker="o", s=8, facecolor="white", edgecolor="#762a83",
                   linewidth=1.0, zorder=5)
        ax.scatter(station["cell_lon"], station["cell_lat"],
                   marker="s", s=8, facecolor="#fdae61", edgecolor="#303030",
                   linewidth=0.5, zorder=6)
        offset = label_offsets.get(station["station_id"], (6, 6))
        ax.annotate(station["name"], (station["station_lon"], station["station_lat"]),
                    xytext=offset, textcoords="offset points", fontsize=7.2,
                    ha="left" if offset[0] >= 0 else "right",
                    va="bottom" if offset[1] >= 0 else "top",
                    color="#222222", zorder=8,
                    bbox={"facecolor": "white", "edgecolor": "none",
                          "alpha": 0.75, "pad": 1.2})


def add_basic_legend(ax) -> None:
    handles = [
        Line2D([], [], marker="o", linestyle="none", markersize=4,
               markerfacecolor="white", markeredgecolor="#762a83",
               label="Station coordinate"),
        Line2D([], [], marker="s", linestyle="none", markersize=4,
               markerfacecolor="#fdae61", markeredgecolor="#303030",
               label="15-minute cell center"),
        Line2D([], [], color="#4d4d4d", linewidth=1.1,
               marker=">", markersize=4, label="Station-to-cell link"),
        Patch(facecolor="#c6dfb8", alpha=0.65,
              label="Faint background: uparea raster (log scale)"),
    ]
    ax.legend(handles=handles, loc="lower left", fontsize=7, framealpha=0.92)


def save_panel(fig, output: Path, dpi: int = 200) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(output, dpi=dpi, facecolor="white")
    plt.close(fig)
    print(f"Wrote {output}")
