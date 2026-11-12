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
                         alpha: float = 0.98, zorder: int = 5, head_sizes=8.5, curvature=0.16,
                         show_heads=True):
    """Draw curved links between exact cell centers, with optional arrowheads."""
    patches = []
    flexible_indices = np.flatnonzero(network.flexible)
    colors = np.broadcast_to(colors, (len(flexible_indices), 4)) if isinstance(
        colors, np.ndarray
    ) else [colors] * len(flexible_indices)
    widths = np.broadcast_to(widths, (len(flexible_indices),))
    head_sizes = np.broadcast_to(head_sizes, (len(flexible_indices),))
    for index, color, width, head_size in zip(flexible_indices, colors, widths, head_sizes):
        source = (network.lon[network.source_cols[index]],
                  network.lat[network.source_rows[index]])
        destination = (network.lon[network.dest_cols[index]],
                      network.lat[network.dest_rows[index]])
        arrow = FancyArrowPatch(
            source, destination, arrowstyle="-|>" if show_heads else "-", connectionstyle=f"arc3,rad={curvature}",
            mutation_scale=float(head_size), linewidth=float(width), color=color, alpha=alpha,
            shrinkA=0, shrinkB=0, zorder=zorder,
        )
        ax.add_patch(arrow)
        patches.append(arrow)
    return patches


def draw_area_arrows(ax, network: RiverNetwork, area_km2, norm, *, cmap=None, show_heads=True):
    """Draw all links using six log-area shaft-width classes in physical inches.

    Black links emphasize thickness; colored arrows use a narrower width
    range so the palette stays legible. Full-map normalization is independent
    of the view. Grouping quivers gives actual variable shaft widths.
    """
    scaled = np.clip(np.asarray(norm(area_km2)), 0, 1)
    levels = np.minimum((scaled * 6).astype(int), 5)
    widths = 0.008 + (0.024 if cmap is None else 0.014) * levels / 5
    colors = np.tile([0., 0., 0., 1.], (len(area_km2), 1)) if cmap is None else cmap(norm(area_km2))
    source_lon = network.lon[network.source_cols]
    source_lat = network.lat[network.source_rows]
    delta_lon = network.lon[network.dest_cols] - source_lon
    delta_lat = network.lat[network.dest_rows] - source_lat
    rendered = 0
    # Narrow rivers first, so broad main-channel arrows remain visible.
    for level in range(6):
        selected = (~network.flexible) & (levels == level)
        if not np.any(selected):
            continue
        shaft_width = float(widths[selected][0])
        # Black b arrows keep a's 0.050-inch head width/length and 0.045-inch
        # head-axis length, independently of the area-scaled shaft thickness.
        if not show_heads:
            headwidth, headlength, headaxislength = 1.0, 0.0, 0.0
        elif cmap is None:
            headwidth, headlength, headaxislength = (
                0.050 / shaft_width, 0.050 / shaft_width, 0.045 / shaft_width,
            )
        else:
            headwidth, headlength, headaxislength = 3.5, 4.0, 3.5
        arrows = ax.quiver(
            source_lon[selected], source_lat[selected], delta_lon[selected], delta_lat[selected],
            angles="xy", scale_units="xy", scale=1, pivot="tail", units="inches",
            width=shaft_width, headwidth=headwidth, headlength=headlength,
            headaxislength=headaxislength, color=colors[selected], zorder=4 + level * 0.1,
        )
        rendered += arrows.U.size
    flexible = network.flexible
    patches = draw_flexible_arrows(
        ax, network, colors[flexible], widths[flexible] * 72,
        alpha=1.0, zorder=5, head_sizes=7.0 if cmap is None else 5 + 200 * widths[flexible],
        show_heads=show_heads,
    )
    rendered += len(patches)
    if rendered != len(network.source_rows):
        raise RuntimeError(f"Rendered {rendered} arrows for {len(network.source_rows)} links")
    return rendered, widths
