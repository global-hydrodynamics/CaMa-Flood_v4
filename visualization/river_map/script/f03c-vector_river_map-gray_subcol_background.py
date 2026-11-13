#!/usr/bin/env python3
"""Plot outlet-based nextxy links over a dark grayscale subbasin background."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.collections import LineCollection
from matplotlib.colors import BoundaryNorm, ListedColormap, LogNorm
from netCDF4 import Dataset
from matplotlib.ticker import FuncFormatter, LogLocator
from cmf_colormap import get_cmap

from river_map_common import (
    add_aligned_colorbar,
    create_map_axes,
    read_geometry,
    read_map_raster,
    save_panel,
)


AREA_WIDTHS = (0.014, 0.018, 0.024, 0.032)


def read_subcol(path: Path, geometry):
    """Read one little-endian int32 Fortran-order subcol raster."""
    nx, ny = geometry[:2]
    expected_bytes = nx * ny * 4
    if path.stat().st_size != expected_bytes:
        raise ValueError(
            f"{path} must contain exactly {expected_bytes} bytes for {nx}x{ny} int32 cells"
        )
    values = np.fromfile(path, dtype="<i4").reshape((ny, nx))
    valid = ((values == -9999) | (values == 0) | ((values >= 1) & (values <= 10)))
    if not np.all(valid):
        raise ValueError(f"{path} has values outside -9999, 0, or display indices 1–10")
    return values


def read_network(uparea_path: Path, nextxy_path: Path, lonlat_path: Path,
                 params_path: Path):
    """Read map inputs and validate nextxy-linked outlet-to-outlet segments."""
    geometry = read_geometry(params_path)
    lon, lat, uparea_data = read_map_raster(uparea_path, "uparea", "m2", geometry)
    uparea = uparea_data.filled(np.nan)
    expected_shape = uparea.shape

    with Dataset(nextxy_path) as data:
        for name in ("lon", "lat", "nextx", "nexty"):
            if name not in data.variables:
                raise ValueError(f"{nextxy_path} is missing required variable {name!r}")
        next_lon = np.asarray(data.variables["lon"][:], dtype=np.float64)
        next_lat = np.asarray(data.variables["lat"][:], dtype=np.float64)
        nextx_var = data.variables["nextx"]
        nexty_var = data.variables["nexty"]
        if not (np.issubdtype(nextx_var.dtype, np.integer) and
                np.issubdtype(nexty_var.dtype, np.integer)):
            raise ValueError(f"{nextxy_path}:nextx/nexty must contain integer indices")
        nextx_dimensions, nextx_shape = nextx_var.dimensions, nextx_var.shape
        nexty_dimensions, nexty_shape = nexty_var.dimensions, nexty_var.shape
        nextx = np.ma.asarray(nextx_var[:]).filled(-9999).astype(np.int64)
        nexty = np.ma.asarray(nexty_var[:]).filled(-9999).astype(np.int64)

    with Dataset(lonlat_path) as data:
        for name in ("lon", "lat", "outlon", "outlat"):
            if name not in data.variables:
                raise ValueError(f"{lonlat_path} is missing required variable {name!r}")
        outlet_lon_var = data.variables["outlon"]
        outlet_lat_var = data.variables["outlat"]
        outlet_lon_dimensions, outlet_lon_shape = outlet_lon_var.dimensions, outlet_lon_var.shape
        outlet_lat_dimensions, outlet_lat_shape = outlet_lat_var.dimensions, outlet_lat_var.shape
        outlon = np.ma.asarray(outlet_lon_var[:]).filled(np.nan).astype(np.float64)
        outlat = np.ma.asarray(outlet_lat_var[:]).filled(np.nan).astype(np.float64)
        outlet_lon = np.asarray(data.variables["lon"][:], dtype=np.float64)
        outlet_lat = np.asarray(data.variables["lat"][:], dtype=np.float64)
        if getattr(outlet_lon_var, "units", "") != "degrees_east":
            raise ValueError(f"{lonlat_path}:outlon must use degrees_east")
        if getattr(outlet_lat_var, "units", "") != "degrees_north":
            raise ValueError(f"{lonlat_path}:outlat must use degrees_north")

    if (next_lon.shape != lon.shape or next_lat.shape != lat.shape or
            outlet_lon.shape != lon.shape or outlet_lat.shape != lat.shape):
        raise ValueError("NetCDF lon/lat coordinate dimensions do not match the uparea grid")

    for name, dimensions, shape in (
        ("nextx", nextx_dimensions, nextx_shape), ("nexty", nexty_dimensions, nexty_shape),
        ("outlon", outlet_lon_dimensions, outlet_lon_shape),
        ("outlat", outlet_lat_dimensions, outlet_lat_shape),
    ):
        if dimensions != ("lat", "lon") or shape != expected_shape:
            raise ValueError(
                f"{name} dimensions/shape {dimensions}/{shape} "
                f"do not match the uparea grid ('lat', 'lon')/{expected_shape}"
            )
    for name, grid_lon, grid_lat in (
        ("nextxy", next_lon, next_lat), ("lonlat", outlet_lon, outlet_lat),
    ):
        if not (np.allclose(lon, grid_lon, rtol=0, atol=1e-5) and
                np.allclose(lat, grid_lat, rtol=0, atol=1e-5)):
            raise ValueError(f"{name} coordinates do not match the uparea grid")

    river_cells = np.isfinite(uparea) & (uparea > 0)
    positive_x = nextx > 0
    positive_y = nexty > 0
    if np.any(river_cells & (positive_x != positive_y)):
        raise ValueError("nextxy has a river cell with only one positive destination index")

    candidate = river_cells & positive_x & positive_y
    nx, ny = len(lon), len(lat)
    in_bounds = ((nextx >= 1) & (nextx <= nx) & (nexty >= 1) & (nexty <= ny))
    out_of_map = int(np.count_nonzero(candidate & ~in_bounds))
    valid = candidate & in_bounds
    source_rows, source_cols = np.where(valid)
    destination_cols = nextx[valid] - 1
    destination_rows = nexty[valid] - 1

    destination_is_river = river_cells[destination_rows, destination_cols]
    if np.any(~destination_is_river):
        raise ValueError("nextxy points to a destination without valid upstream area")

    outlet_valid = (
        np.isfinite(outlon) & np.isfinite(outlat) &
        (outlon != -9999) & (outlat != -9999) &
        (outlon >= -180) & (outlon <= 360) &
        (outlat >= -90) & (outlat <= 90)
    )
    endpoints_valid = (outlet_valid[source_rows, source_cols] &
                       outlet_valid[destination_rows, destination_cols])
    invalid_outlets = int(np.count_nonzero(~endpoints_valid))
    source_rows = source_rows[endpoints_valid]
    source_cols = source_cols[endpoints_valid]
    destination_rows = destination_rows[endpoints_valid]
    destination_cols = destination_cols[endpoints_valid]

    source_lon = outlon[source_rows, source_cols]
    source_lat = outlat[source_rows, source_cols]
    destination_lon = outlon[destination_rows, destination_cols]
    destination_lat = outlat[destination_rows, destination_cols]
    source_area_km2 = uparea[source_rows, source_cols] / 1_000_000.0
    if not len(source_rows):
        raise ValueError("No valid nextxy links with geographic outlet coordinates were found")
    if np.any((source_lon == destination_lon) & (source_lat == destination_lat)):
        raise ValueError("A valid nextxy link has identical source and destination outlets")

    expected_source = np.column_stack((outlon[source_rows, source_cols],
                                       outlat[source_rows, source_cols]))
    expected_destination = np.column_stack((outlon[destination_rows, destination_cols],
                                            outlat[destination_rows, destination_cols]))
    segments = np.stack((expected_source, expected_destination), axis=1)
    if not (np.array_equal(segments[:, 0, 0], source_lon) and
            np.array_equal(segments[:, 0, 1], source_lat) and
            np.array_equal(segments[:, 1, 0], destination_lon) and
            np.array_equal(segments[:, 1, 1], destination_lat)):
        raise ValueError("Network segment coordinates do not match nextxy outlet endpoints")

    log_area = np.log10(source_area_km2)
    quartile_limits = np.quantile(log_area, (0.25, 0.5, 0.75))
    area_classes = np.digitize(log_area, quartile_limits)
    return (geometry, source_lon, source_lat, destination_lon, destination_lat,
            source_area_km2, area_classes, quartile_limits, out_of_map, invalid_outlets)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--uparea", type=Path, required=True)
    parser.add_argument("--nextxy", type=Path, required=True)
    parser.add_argument("--lonlat", type=Path, required=True)
    parser.add_argument("--subcol", type=Path, required=True)
    parser.add_argument("--params", type=Path, required=True)
    parser.add_argument("--west", type=float, required=True)
    parser.add_argument("--east", type=float, required=True)
    parser.add_argument("--south", type=float, required=True)
    parser.add_argument("--north", type=float, required=True)
    parser.add_argument("--figure-width", type=float, default=0.0)
    parser.add_argument("--figure-height", type=float, default=8.0)
    parser.add_argument("--dpi", type=int, default=200)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.figure_width < 0 or args.figure_height < 0 or args.dpi < 1:
        parser.error("figure dimensions must be non-negative and DPI must be positive")
    return args


def main() -> None:
    args = parse_args()
    (geometry, source_lon, source_lat, destination_lon, destination_lat,
     source_area_km2, area_classes, quartile_limits, out_of_map,
     invalid_outlets) = read_network(args.uparea, args.nextxy, args.lonlat, args.params)
    subcol = read_subcol(args.subcol, geometry)
    _, _, _, west, east, south, north = geometry

    title = "p01-f03c Vector River Map: Gray Subcol Background"
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north, title,
    )
    fig.patch.set_facecolor("white")
    ax.set_facecolor("white")
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")

    gray_levels = [0.50, *np.linspace(0.45, 0.10, 10)]
    subcol_cmap = ListedColormap(
        [(level, level, level) for level in gray_levels], name="subcol_gray",
    )
    subcol_cmap.set_bad("white")
    subcol_values = np.ma.masked_where((subcol < 0) | (subcol > 10), subcol)
    ax.imshow(
        subcol_values, origin="upper", extent=(west, east, south, north),
        cmap=subcol_cmap, norm=BoundaryNorm(np.arange(-0.5, 11.5), subcol_cmap.N),
        interpolation="nearest", zorder=0,
    )

    area_norm = LogNorm(vmin=float(source_area_km2.min()),
                        vmax=float(source_area_km2.max()))
    area_cmap = get_cmap("shade_rainbow")
    segment_start = np.column_stack((source_lon, source_lat))
    segment_end = np.column_stack((destination_lon, destination_lat))
    segments = np.stack((segment_start, segment_end), axis=1)
    draw_order = np.argsort(source_area_km2, kind="stable")
    segments = segments[draw_order]
    ordered_areas = source_area_km2[draw_order]
    line_widths = np.asarray(AREA_WIDTHS)[area_classes[draw_order]] * 72.0
    links = LineCollection(
        segments,
        colors=area_cmap(area_norm(ordered_areas)),
        linewidths=line_widths,
        alpha=0.95,
        zorder=2,
    )
    ax.add_collection(links)
    rendered = len(links.get_segments())
    if rendered != len(source_lon):
        raise RuntimeError(f"Rendered {rendered} line segments for {len(source_lon)} valid links")
    if not np.array_equal(links.get_segments(), segments):
        raise ValueError("A plotted line segment does not match its nextxy outlet endpoints")

    colorbar = add_aligned_colorbar(
        fig, ax, plt.cm.ScalarMappable(norm=area_norm, cmap=area_cmap),
        "Source-cell upstream drainage area (km²)", orientation="vertical",
    )
    colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=18)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()

    save_panel(
        fig, args.output, args.dpi,
        figure_width=args.figure_width, figure_height=args.figure_height,
    )
    plt.close(fig)

    area_cutoffs = np.power(10.0, quartile_limits)
    print(f"Wrote {args.output}: {rendered} arrowless outlet link segments, "
          "drawn from smaller to larger source-cell upstream area; "
          f"full-map line-width quartile cutoffs={area_cutoffs.tolist()} km²; "
          f"shade-rainbow source-area range={source_area_km2.min():g}–"
          f"{source_area_km2.max():g} km²; "
          f"out-of-map nextxy={out_of_map}; invalid outlet links={invalid_outlets}.")


if __name__ == "__main__":
    main()
