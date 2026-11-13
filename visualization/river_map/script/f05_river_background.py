#!/usr/bin/env python3
"""Draw a shared outlet-based river network over one 1-minute raster layer."""

from __future__ import annotations

import argparse
import importlib.util
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.collections import LineCollection
from matplotlib.colors import LogNorm
from matplotlib.lines import Line2D
from matplotlib.ticker import LogFormatterMathtext, LogLocator
from netCDF4 import Dataset

from cmf_colormap import get_cmap
from river_map_common import read_geometry, read_map_raster
from river_network_common import read_river_network
from river_network_layout import create_network_axes, save_network_panel

_f03b_path = Path(__file__).with_name("f03b-vector_river_map-extended_network.py")
_f03b_spec = importlib.util.spec_from_file_location("f03b_network_helpers", _f03b_path)
_f03b = importlib.util.module_from_spec(_f03b_spec)
_f03b_spec.loader.exec_module(_f03b)
draw_directed_links = _f03b.draw_directed_links
read_bifurcations = _f03b.read_bifurcations
read_catchment_mapping = _f03b.read_catchment_mapping


PANEL = {
    "gray_elevtn": ("a", "Gray Elevation", "gray", False),
    "gray_slope": ("b", "Gray Slope", "gray", False),
    "gray_flddif": ("c", "Gray Floodplain Difference", "gray", False),
    "terrain_elevtn": ("d", "Terrain Elevation", "pale_terrain", True),
    "terrain_slope": ("e", "Terrain Slope", "pale_terrain", True),
    "terrain_flddif": ("f", "Terrain Floodplain Difference", "pale_terrain", True),
}
EXTENT = (99.0, 109.0, 9.0, 19.0)
AREA_WIDTHS = np.linspace(0.012, 0.036, 6)
BIFURCATION_COLORS = ("#F4A582", "#D6604D", "#B2182B", "#FF0000")


def read_high_resolution_grid(location_path: Path, geometry):
    """Read and validate the 1-minute grid geometry from location.txt."""
    lines = location_path.read_text().splitlines()
    if len(lines) < 3:
        raise ValueError(f"{location_path} does not contain a 1-minute grid descriptor")
    fields = lines[2].split()
    if len(fields) < 9 or fields[1] != "1min":
        raise ValueError(f"{location_path} does not describe a 1-minute grid")
    west, east, south, north = map(float, fields[2:6])
    nx, ny = map(int, fields[6:8])
    resolution = float(fields[8])
    coarse_nx, coarse_ny, coarse_res, cw, ce, cs, cn = geometry
    if (not np.isclose(resolution, 1 / 60) or nx != coarse_nx * 15 or
            ny != coarse_ny * 15 or not np.isclose(east - west, nx * resolution) or
            not np.isclose(north - south, ny * resolution) or
            not np.allclose((west, east, south, north), (cw, ce, cs, cn)) or
            not np.isclose(coarse_res, 0.25)):
        raise ValueError(f"{location_path} is inconsistent with params.txt")
    return nx, ny, resolution, west, east, south, north


def read_float_raster(path: Path, nx: int, ny: int):
    """Read one little-endian float32, Fortran-order high-resolution raster."""
    expected_bytes = nx * ny * 4
    if path.stat().st_size != expected_bytes:
        raise ValueError(f"{path} has {path.stat().st_size} bytes; expected {expected_bytes}")
    values = np.fromfile(path, dtype="<f4").reshape((ny, nx))
    return np.ma.masked_where(~np.isfinite(values) | (values == -9999), values)


def maximum_neighbor_elevation_difference(elevation: np.ma.MaskedArray):
    """Return maximum absolute elevation difference to a valid 8-neighbor [m]."""
    values = np.asarray(elevation.data, dtype=np.float64)
    valid = ~np.ma.getmaskarray(elevation)
    result = np.full(values.shape, np.nan, dtype=np.float64)
    height, width = values.shape
    for row_offset in (-1, 0, 1):
        for col_offset in (-1, 0, 1):
            if row_offset == 0 and col_offset == 0:
                continue
            source_rows = slice(max(0, row_offset), min(height, height + row_offset))
            source_cols = slice(max(0, col_offset), min(width, width + col_offset))
            neighbor_rows = slice(max(0, -row_offset), min(height, height - row_offset))
            neighbor_cols = slice(max(0, -col_offset), min(width, width - col_offset))
            source_valid = valid[source_rows, source_cols]
            neighbor_valid = valid[neighbor_rows, neighbor_cols]
            differences = np.abs(
                values[source_rows, source_cols] - values[neighbor_rows, neighbor_cols]
            )
            target = result[source_rows, source_cols]
            usable = source_valid & neighbor_valid
            target[usable] = np.fmax(target[usable], differences[usable])
    return np.ma.masked_invalid(result)


def crop_high_resolution(values, grid, extent):
    """Crop a north-first raster to cell centers intersecting the map extent."""
    nx, ny, resolution, west, east, south, north = grid
    display_west, display_east, display_south, display_north = extent
    col0 = max(0, int(np.floor((display_west - west) / resolution)))
    col1 = min(nx, int(np.ceil((display_east - west) / resolution)))
    row0 = max(0, int(np.floor((north - display_north) / resolution)))
    row1 = min(ny, int(np.ceil((north - display_south) / resolution)))
    cropped = values[row0:row1, col0:col1]
    cropped_west = west + col0 * resolution
    cropped_north = north - row0 * resolution
    cropped_extent = (
        cropped_west, cropped_west + cropped.shape[1] * resolution,
        cropped_north - cropped.shape[0] * resolution, cropped_north,
    )
    return cropped, cropped_extent


def grayscale_background(values, fill_mask=None, fill_value=0.5, palette="gray"):
    """Apply the shared percentile/sqrt stretch and selected background palette."""
    valid_values = np.asarray(values.compressed(), dtype=np.float64)
    if valid_values.size == 0:
        raise ValueError("High-resolution background has no valid values")
    lower, upper = np.percentile(valid_values, (2, 98))
    if not upper > lower:
        upper = lower + 1.0
    normalized = np.clip((values - lower) / (upper - lower), 0, 1)
    # Higher source values remain lighter; gray intensity stays below 0.58.
    intensity = 0.16 + 0.42 * np.ma.sqrt(normalized)
    if fill_mask is not None:
        displayed = np.asarray(intensity.filled(1.0))
        displayed[fill_mask] = fill_value
        intensity = np.ma.array(displayed, mask=np.ma.getmaskarray(intensity) & ~fill_mask)
    if palette == "pale_terrain":
        # Expand the existing display stretch across the full palette. A gentle
        # gamma below one makes its pink high end visible without changing data
        # limits, masks, or the preceding percentile/square-root normalization.
        palette_position = np.clip((intensity - 0.16) / 0.42, 0, 1)
        intensity = np.ma.power(palette_position, 0.4)
    cmap = (plt.get_cmap("gray") if palette == "gray" else get_cmap(palette)).copy()
    cmap.set_bad("white")
    return intensity, cmap, float(lower), float(upper)


def read_outlets(path: Path, network):
    """Read geographic outlet coordinates aligned with the model grid."""
    with Dataset(path) as source:
        for name in ("lon", "lat", "outlon", "outlat"):
            if name not in source.variables:
                raise ValueError(f"{path} is missing variable {name!r}")
        lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
        outlon_var, outlat_var = source.variables["outlon"], source.variables["outlat"]
        outlon = np.ma.asarray(outlon_var[:], dtype=np.float64).filled(np.nan)
        outlat = np.ma.asarray(outlat_var[:], dtype=np.float64).filled(np.nan)
        if (outlon_var.dimensions != ("lat", "lon") or
                outlat_var.dimensions != ("lat", "lon") or
                outlon.shape != network.uparea.shape or outlat.shape != network.uparea.shape or
                getattr(outlon_var, "units", "") != "degrees_east" or
                getattr(outlat_var, "units", "") != "degrees_north"):
            raise ValueError(f"{path}: outlon/outlat are not aligned with the map grid")
    if not (np.allclose(lon, network.lon, rtol=0, atol=1e-5) and
            np.allclose(lat, network.lat, rtol=0, atol=1e-5)):
        raise ValueError(f"{path} coordinates do not match nextxy/uparea")
    return outlon, outlat


def main_segments(network, outlon, outlat):
    """Resolve valid nextxy links to exact source and destination outlets."""
    sr, sc, dr, dc = (network.source_rows, network.source_cols,
                      network.dest_rows, network.dest_cols)
    valid = (
        np.isfinite(outlon[sr, sc]) & np.isfinite(outlat[sr, sc]) &
        np.isfinite(outlon[dr, dc]) & np.isfinite(outlat[dr, dc]) &
        (outlon[sr, sc] >= -180) & (outlon[sr, sc] <= 360) &
        (outlon[dr, dc] >= -180) & (outlon[dr, dc] <= 360) &
        (outlat[sr, sc] >= -90) & (outlat[sr, sc] <= 90) &
        (outlat[dr, dc] >= -90) & (outlat[dr, dc] <= 90) &
        (outlon[sr, sc] != -9999) & (outlat[sr, sc] != -9999) &
        (outlon[dr, dc] != -9999) & (outlat[dr, dc] != -9999)
    )
    segments = np.stack((
        np.column_stack((outlon[sr[valid], sc[valid]], outlat[sr[valid], sc[valid]])),
        np.column_stack((outlon[dr[valid], dc[valid]], outlat[dr[valid], dc[valid]])),
    ), axis=1)
    area_km2 = network.uparea[sr[valid], sc[valid]] / 1e6
    if not len(segments):
        raise ValueError("No valid nextxy links with outlet coordinates")
    return segments, area_km2


def headwater_guides(network, outlon, outlat, catmxy_path, location_path, geometry):
    """Reuse the f03b headwater-centroid-to-outlet guide construction."""
    river_cells = np.isfinite(network.uparea) & (network.uparea > 0)
    centroid_lon, centroid_lat, pixel_counts, _ = read_catchment_mapping(
        catmxy_path, location_path, geometry, river_cells,
    )
    incoming = np.bincount(
        network.dest_rows * network.uparea.shape[1] + network.dest_cols,
        minlength=network.uparea.size,
    ).reshape(network.uparea.shape)
    headwaters = np.flatnonzero((river_cells & (incoming == 0)).ravel())
    valid = (
        (pixel_counts[headwaters] > 0) &
        np.isfinite(outlon.ravel()[headwaters]) & np.isfinite(outlat.ravel()[headwaters]) &
        (outlon.ravel()[headwaters] >= -180) & (outlon.ravel()[headwaters] <= 360) &
        (outlat.ravel()[headwaters] >= -90) & (outlat.ravel()[headwaters] <= 90) &
        (outlon.ravel()[headwaters] != -9999) & (outlat.ravel()[headwaters] != -9999)
    )
    ids = headwaters[valid]
    return np.stack((
        np.column_stack((centroid_lon[ids], centroid_lat[ids])),
        np.column_stack((outlon.ravel()[ids], outlat.ravel()[ids])),
    ), axis=1)


def add_network_legend(ax, black_network):
    main_color = "black" if black_network else "#2866A5"
    bifurcation_color = "black" if black_network else BIFURCATION_COLORS[-1]
    guide_color = "black" if black_network else "#777777"
    main_label = "Main channel (upstream area)"
    bifurcation_width = 0.024 * 72 if black_network else 1.5
    guide_width = 0.85 if black_network else 0.8
    handles = [
        Line2D([], [], color=main_color, linewidth=1.5, label=main_label),
        Line2D([], [], color=bifurcation_color, linewidth=bifurcation_width,
               label="Channel bifurcation"),
        Line2D([], [], color=guide_color, linewidth=guide_width,
               alpha=1.0 if black_network else 0.7,
               label="Headwater guide"),
    ]
    return handles


def render(args):
    option = args.option
    suffix, description, background_palette, black_network = PANEL[option]
    geometry = read_geometry(args.params)
    high_grid = read_high_resolution_grid(args.location, geometry)
    highres = read_float_raster(args.background, high_grid[0], high_grid[1])
    fill_mask = None
    if option in ("gray_slope", "terrain_slope"):
        highres = maximum_neighbor_elevation_difference(highres)
    elif option in ("gray_flddif", "terrain_flddif"):
        elevation = read_float_raster(args.elevtn, high_grid[0], high_grid[1])
        fill_mask = np.ma.getmaskarray(highres) & ~np.ma.getmaskarray(elevation)
    display, background_cmap, gray_min, gray_max = grayscale_background(
        highres, fill_mask, palette=background_palette,
    )
    display, image_extent = crop_high_resolution(display, high_grid, EXTENT)

    network = read_river_network(args.uparea, args.nextxy, geometry)
    outlon, outlat = read_outlets(args.lonlat, network)
    links, area_km2 = main_segments(network, outlon, outlat)
    bifurcations, bif_width, bif_invalid = read_bifurcations(
        args.bifprm, outlon, outlat,
    )
    channel = bif_width > 0
    if not channel.any():
        raise ValueError("No positive-width channel bifurcations were found")
    guides = headwater_guides(
        network, outlon, outlat, args.catmxy, args.location, geometry,
    )

    fig, ax, cax = create_network_axes(
        geometry, args,
        f"p01-f05{suffix} High-resolution Background:\n{description}",
        show_colorbar=not black_network, reserve_colorbar=True, right_margin=0.55,
    )
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    # Reserve the same bottom strip for the shared network legend in every panel.
    fig_width, fig_height = fig.get_size_inches()
    map_pos, bar_pos = ax.get_position(), cax.get_position()
    reserve = 0.38
    ax.set_position((map_pos.x0 + reserve / (2 * fig_width),
                     map_pos.y0 + reserve / fig_height,
                     map_pos.width - reserve / fig_width,
                     map_pos.height - reserve / fig_height))
    cax.set_position((bar_pos.x0, bar_pos.y0 + reserve / fig_height,
                      bar_pos.width, bar_pos.height - reserve / fig_height))
    fig.patch.set_facecolor("white")
    ax.set_facecolor("white")
    ax.imshow(display, origin="upper", extent=image_extent, cmap=background_cmap,
              vmin=0, vmax=1, interpolation="nearest", zorder=0)
    ax.add_collection(LineCollection(
        guides, colors="black" if black_network else "#777777",
        linewidths=0.85, alpha=1.0 if black_network else 0.70, zorder=1,
        clip_on=True,
    ))

    area_cmap = get_cmap("shade_rainbow")
    area_limits = (float(np.nanmin(area_km2)), float(np.nanmax(area_km2)))
    if area_limits[0] <= 0:
        raise ValueError("Valid source-cell upstream areas must be positive")
    if area_limits[0] == area_limits[1]:
        area_limits = (area_limits[0] / 1.01, area_limits[1] * 1.01)
    norm = LogNorm(*area_limits)
    scaled = np.clip(norm(area_km2), 0, 1)
    area_classes = np.minimum((scaled * len(AREA_WIDTHS)).astype(int), len(AREA_WIDTHS) - 1)
    for level, width in enumerate(AREA_WIDTHS):
        selected = area_classes == level
        if selected.any():
            draw_directed_links(
                ax, links[selected], "black" if black_network else area_cmap(norm(area_km2[selected])), width,
                0.01, 1.0 if black_network else 0.96, 3 + level * 0.1,
            )

    if black_network:
        draw_directed_links(
            ax, bifurcations[channel], "black", 0.024, 0.01,
            1.0, 5, marker_position="midpoint",
        )
    else:
        channel_log_width = np.log10(bif_width[channel])
        width_bins = np.digitize(
            channel_log_width, np.quantile(channel_log_width, (0.25, 0.5, 0.75)),
        )
        channel_ids = np.flatnonzero(channel)
        for group, color in enumerate(BIFURCATION_COLORS):
            selected_ids = channel_ids[width_bins == group]
            if selected_ids.size:
                draw_directed_links(
                    ax, bifurcations[selected_ids], color, 0.024, 0.01,
                    0.96, 5, marker_position="midpoint",
                )

    valid_outlets = (
        np.isfinite(outlon) & np.isfinite(outlat) & (outlon != -9999) &
        (outlat != -9999) & (outlon >= -180) & (outlon <= 360) &
        (outlat >= -90) & (outlat <= 90)
    )
    outlet_points = np.unique(np.column_stack((outlon[valid_outlets], outlat[valid_outlets])), axis=0)
    ax.scatter(outlet_points[:, 0], outlet_points[:, 1], s=5, facecolors="white",
               edgecolors="black", linewidths=0.35, zorder=7)

    if not black_network:
        sm = plt.cm.ScalarMappable(norm=norm, cmap=area_cmap)
        sm.set_array([])
        colorbar = fig.colorbar(sm, cax=cax, orientation="vertical")
        colorbar.set_label("Upstream area [km²]")
        colorbar.locator = LogLocator(base=10, subs=(1, 2, 5))
        colorbar.formatter = LogFormatterMathtext(base=10)
        colorbar.update_ticks()
    legend = fig.legend(
        handles=add_network_legend(ax, black_network), loc="lower center",
        bbox_to_anchor=((map_pos.x0 + map_pos.width / 2), 0.012),
        bbox_transform=fig.transFigure, ncols=3, fontsize=7,
        framealpha=0.94, borderpad=0.4, labelspacing=0.25,
        columnspacing=0.8, handlelength=1.7,
    )
    legend.set_zorder(10)
    save_network_panel(fig, ax, cax, args, embed_map_layout=True)
    plt.close(fig)
    print(
        f"Wrote {args.output}; background display limits: {gray_min:g}–{gray_max:g}; "
        f"valid main links: {len(links)}; positive-width channel bifurcations: "
        f"{int(channel.sum())}; headwater guides: {len(guides)}; "
        f"invalid bifurcation indices/widths/outlets: "
        f"{bif_invalid['indices']}/{bif_invalid['widths']}/{bif_invalid['outlets']}."
    )
    if option in ("gray_slope", "terrain_slope"):
        print("Slope display is the maximum absolute elevation difference to a valid "
              "8-neighbor, in metres; no distance normalization is applied.")


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--option", choices=PANEL, required=True)
    parser.add_argument("--background", type=Path, required=True)
    parser.add_argument("--elevtn", type=Path,
                        help="Elevation mask used to identify land without flddif values")
    parser.add_argument("--uparea", type=Path, required=True)
    parser.add_argument("--nextxy", type=Path, required=True)
    parser.add_argument("--lonlat", type=Path, required=True)
    parser.add_argument("--bifprm", type=Path, required=True)
    parser.add_argument("--catmxy", type=Path, required=True)
    parser.add_argument("--location", type=Path, required=True)
    parser.add_argument("--params", type=Path, required=True)
    parser.add_argument("--west", type=float, default=EXTENT[0])
    parser.add_argument("--east", type=float, default=EXTENT[1])
    parser.add_argument("--south", type=float, default=EXTENT[2])
    parser.add_argument("--north", type=float, default=EXTENT[3])
    parser.add_argument("--figure-width", type=float, default=0.0)
    parser.add_argument("--figure-height", type=float, default=8.0)
    parser.add_argument("--max-width", type=float, default=0.0)
    parser.add_argument("--max-height", type=float, default=0.0)
    parser.add_argument("--dpi", type=int, default=200)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.option in ("gray_flddif", "terrain_flddif") and args.elevtn is None:
        parser.error("--elevtn is required for gray_flddif to distinguish land from sea")
    args.extent = (args.west, args.east, args.south, args.north)
    return args


if __name__ == "__main__":
    render(parse_args())
