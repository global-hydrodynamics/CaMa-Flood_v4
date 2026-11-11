"""Read and classify explicit downstream links in the Mekong river map."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
from netCDF4 import Dataset
from matplotlib.patches import FancyArrowPatch

from river_map_common import read_map_raster


@dataclass(frozen=True)
class RiverNetwork:
    """Grid-center coordinates and valid one-based downstream connections."""

    lon: np.ndarray
    lat: np.ndarray
    uparea: np.ndarray
    source_rows: np.ndarray
    source_cols: np.ndarray
    dest_rows: np.ndarray
    dest_cols: np.ndarray
    flexible: np.ndarray
    out_of_map: int


def read_river_network(uparea_path: Path, nextxy_path: Path, geometry) -> RiverNetwork:
    """Read explicit nextxy links and validate their grid and river endpoints."""
    lon, lat, uparea_data = read_map_raster(uparea_path, "uparea", "m2", geometry)
    uparea = uparea_data.filled(np.nan)

    with Dataset(nextxy_path) as source:
        for name in ("lon", "lat", "nextx", "nexty"):
            if name not in source.variables:
                raise ValueError(f"{nextxy_path} is missing required variable {name!r}")
        nextx_var = source.variables["nextx"]
        nexty_var = source.variables["nexty"]
        expected_shape = uparea.shape
        for name, variable in (("nextx", nextx_var), ("nexty", nexty_var)):
            if variable.dimensions != ("lat", "lon") or variable.shape != expected_shape:
                raise ValueError(
                    f"{nextxy_path}:{name} dimensions/shape {variable.dimensions}/"
                    f"{variable.shape} do not match ('lat', 'lon')/{expected_shape}"
                )
        nextx = np.ma.asarray(nextx_var[:]).filled(-9999).astype(np.int64)
        nexty = np.ma.asarray(nexty_var[:]).filled(-9999).astype(np.int64)
        netcdf_lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        netcdf_lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
    if not (np.allclose(lon, netcdf_lon, rtol=0, atol=1e-5) and
            np.allclose(lat, netcdf_lat, rtol=0, atol=1e-5)):
        raise ValueError("nextxy coordinates do not match the uparea map grid")

    river = np.isfinite(uparea) & (uparea > 0)
    rows, cols = np.indices(uparea.shape)
    positive_x = nextx > 0
    positive_y = nexty > 0
    if np.any(river & (positive_x != positive_y)):
        raise ValueError("nextxy contains a river cell with only one positive destination index")

    positive = river & positive_x & positive_y
    in_bounds = ((nextx >= 1) & (nextx <= len(lon)) &
                 (nexty >= 1) & (nexty <= len(lat)))
    out_of_map = int(np.count_nonzero(positive & ~in_bounds))
    valid = positive & in_bounds
    source_rows, source_cols = np.where(valid)
    dest_cols = nextx[valid] - 1
    dest_rows = nexty[valid] - 1
    if np.any(~river[dest_rows, dest_cols]):
        raise ValueError("nextxy points to a destination without valid uparea")

    flexible = np.maximum(
        np.abs(nextx[valid] - (cols[valid] + 1)),
        np.abs(nexty[valid] - (rows[valid] + 1)),
    ) > 1
    return RiverNetwork(
        lon, lat, uparea, source_rows, source_cols, dest_rows, dest_cols,
        flexible, out_of_map,
    )


def count_segments_intersecting_extent(network: RiverNetwork, extent) -> tuple[int, int, int]:
    """Count visible links without filtering the full link set used for plotting."""
    west, east, south, north = extent
    visible = 0
    visible_flexible = 0
    for sr, sc, dr, dc, flexible in zip(
        network.source_rows, network.source_cols, network.dest_rows,
        network.dest_cols, network.flexible,
    ):
        x0, y0 = network.lon[sc], network.lat[sr]
        x1, y1 = network.lon[dc], network.lat[dr]
        dx, dy = x1 - x0, y1 - y0
        lower, upper = 0.0, 1.0
        for p, q in ((-dx, x0 - west), (dx, east - x0),
                     (-dy, y0 - south), (dy, north - y0)):
            if p == 0:
                if q < 0:
                    upper = -1.0
                    break
                continue
            ratio = q / p
            if p < 0:
                lower = max(lower, ratio)
            else:
                upper = min(upper, ratio)
        if lower <= upper:
            visible += 1
            visible_flexible += int(flexible)
    return visible, visible - visible_flexible, visible_flexible


def draw_flexible_arrows(ax, network: RiverNetwork, colors, widths,
                         alpha: float = 0.98, zorder: int = 5):
    """Draw nonlocal nextxy links as curved arrows between exact cell centers."""
    patches = []
    flexible_indices = np.flatnonzero(network.flexible)
    colors = np.broadcast_to(colors, (len(flexible_indices), 4)) if isinstance(
        colors, np.ndarray
    ) else [colors] * len(flexible_indices)
    widths = np.broadcast_to(widths, (len(flexible_indices),))
    for index, color, width in zip(flexible_indices, colors, widths):
        source = (network.lon[network.source_cols[index]],
                  network.lat[network.source_rows[index]])
        destination = (network.lon[network.dest_cols[index]],
                      network.lat[network.dest_rows[index]])
        arrow = FancyArrowPatch(
            source, destination, arrowstyle="-|>", connectionstyle="arc3,rad=0.16",
            mutation_scale=8.5, linewidth=float(width), color=color, alpha=alpha,
            shrinkA=0, shrinkB=0, zorder=zorder,
        )
        ax.add_patch(arrow)
        patches.append(arrow)
    return patches
