#!/usr/bin/env python3
"""Plot 1-minute catchments with outlet-based main and channel-bifurcation links."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import LinearSegmentedColormap, ListedColormap, LogNorm
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch, Patch
from netCDF4 import Dataset
import numpy as np

from river_map_common import (
    add_map_arguments,
    read_geometry,
    read_map_raster,
    save_panel,
)
from f04_layout import add_f04_legend, create_f04_layout, format_f04_colorbar
from river_network_common import read_river_network


MAIN_COLORS = ("#7FA6D6", "#4F80BF", "#285A9B", "#103A70")
MAIN_WIDTHS = (0.8, 1.15, 1.65, 2.2)
CHANNEL_COLORS = ("#F4A582", "#D6604D", "#B2182B", "#67001F")
CHANNEL_WIDTHS = (0.7, 0.95, 1.2, 1.5)
EXTENT = (104.5, 107.0, 9.5, 12.0)


def read_catchment_mapping(path: Path, location_path: Path, geometry,
                           river_cells: np.ndarray):
    """Read 1-minute coarse-cell assignments and identify unassigned land."""
    nx, ny, _, west, east, south, north = geometry
    lines = location_path.read_text(encoding="utf-8").splitlines()
    if len(lines) < 3:
        raise ValueError(f"{location_path} does not contain the 1-minute grid descriptor")
    fields = lines[2].split()
    if len(fields) < 9 or fields[1] != "1min":
        raise ValueError(f"{location_path} does not describe a 1-minute grid")
    hires_west, hires_east, hires_south, hires_north = map(float, fields[2:6])
    nxhi, nyhi = map(int, fields[6:8])
    hires_resolution = float(fields[8])
    if (not np.isclose(hires_resolution, 1 / 60) or nxhi != nx * 15 or nyhi != ny * 15 or
            not np.allclose((hires_west, hires_east, hires_south, hires_north),
                            (west, east, south, north))):
        raise ValueError(f"{location_path} grid geometry does not match params.txt")

    values = np.fromfile(path, dtype="<i2")
    expected_values = 2 * nxhi * nyhi
    if values.size != expected_values:
        raise ValueError(f"{path} contains {values.size} values; expected {expected_values}")
    catmx, catmy = values.reshape(2, nyhi, nxhi)

    valid_x = (catmx >= 1) & (catmx <= nx)
    valid_y = (catmy >= 1) & (catmy <= ny)
    if np.any(valid_x != valid_y):
        raise ValueError(f"{path} contains pixels with only one valid coarse-cell index")
    assigned = valid_x & valid_y
    rows, cols = np.where(assigned)
    maps_to_river = np.zeros(assigned.shape, dtype=bool)
    maps_to_river[rows, cols] = river_cells[catmy[rows, cols] - 1, catmx[rows, cols] - 1]
    if np.any(assigned & ~maps_to_river):
        raise ValueError(f"{path} assigns a high-resolution pixel to a non-river map cell")
    valid = assigned & maps_to_river
    unassigned_land = (catmx == -999) & (catmy == -999)

    metadata = {
        "nx": nxhi, "ny": nyhi, "resolution": hires_resolution,
        "west": hires_west, "east": hires_east,
        "south": hires_south, "north": hires_north,
        "unassigned_land": int(np.count_nonzero(unassigned_land)),
    }
    return catmx, catmy, valid, unassigned_land, metadata


def crop_catchment_data(catmx, catmy, valid, unassigned_land, uparea,
                        geometry, hires, extent):
    """Map assigned fine pixels to coarse uparea and crop to the display extent."""
    nx, ny, _, _, _, _, _ = geometry
    nxhi, nyhi = hires["nx"], hires["ny"]
    hires_resolution = hires["resolution"]
    hires_west, hires_north = hires["west"], hires["north"]
    xmin, xmax, ymin, ymax = extent
    col0 = max(0, int((xmin - hires_west) / hires_resolution) - 1)
    col1 = min(nxhi, int(np.ceil((xmax - hires_west) / hires_resolution)) + 1)
    row0 = max(0, int((hires_north - ymax) / hires_resolution) - 1)
    row1 = min(nyhi, int(np.ceil((hires_north - ymin) / hires_resolution)) + 1)

    subx, suby = catmx[row0:row1, col0:col1], catmy[row0:row1, col0:col1]
    subvalid = valid[row0:row1, col0:col1]
    sub_unassigned_land = unassigned_land[row0:row1, col0:col1]
    if np.any((subx == -999) != (suby == -999)):
        raise ValueError("catmxy has mismatched unassigned-pixel flags")

    catchment_area = np.full(subvalid.shape, np.nan, dtype=np.float64)
    rows, cols = np.where(subvalid)
    catchment_area[rows, cols] = uparea[suby[rows, cols] - 1,
                                       subx[rows, cols] - 1] / 1e6
    x_edges = hires_west + np.arange(col0, col1 + 1) * hires_resolution
    y_edges = hires_north - np.arange(row0, row1 + 1) * hires_resolution
    return (subx, suby, subvalid, sub_unassigned_land,
            catchment_area, x_edges, y_edges)


def read_visual_categories(path: Path, hires, extent):
    """Read signed-int8 1-minute FLOW visual classes in Fortran raster order."""
    values = np.fromfile(path, dtype=np.int8)
    expected = hires["nx"] * hires["ny"]
    if values.size != expected:
        raise ValueError(f"{path} contains {values.size} values; expected {expected}")
    visual = values.reshape(hires["ny"], hires["nx"])
    xmin, xmax, ymin, ymax = extent
    resolution = hires["resolution"]
    col0 = max(0, int((xmin - hires["west"]) / resolution) - 1)
    col1 = min(hires["nx"], int(np.ceil((xmax - hires["west"]) / resolution)) + 1)
    row0 = max(0, int((hires["north"] - ymax) / resolution) - 1)
    row1 = min(hires["ny"], int(np.ceil((hires["north"] - ymin) / resolution)) + 1)
    return visual[row0:row1, col0:col1]


def catchment_boundaries(catmx, catmy, valid, x_edges, y_edges):
    """Return pixel edges where neighboring valid catmxy assignments differ."""
    changed_x = (valid[:, :-1] & valid[:, 1:] &
                 ((catmx[:, :-1] != catmx[:, 1:]) | (catmy[:, :-1] != catmy[:, 1:])))
    rows, cols = np.where(changed_x)
    vertical = [((x_edges[col + 1], y_edges[row]),
                 (x_edges[col + 1], y_edges[row + 1])) for row, col in zip(rows, cols)]
    changed_y = (valid[:-1, :] & valid[1:, :] &
                 ((catmx[:-1, :] != catmx[1:, :]) | (catmy[:-1, :] != catmy[1:, :])))
    rows, cols = np.where(changed_y)
    horizontal = [((x_edges[col], y_edges[row + 1]),
                   (x_edges[col + 1], y_edges[row + 1])) for row, col in zip(rows, cols)]
    return vertical + horizontal


def compute_headwater_guides(geometry, river_cells, destination_rows,
                             destination_cols, catmx, catmy, catvalid,
                             outlon, outlat, hires):
    """Build centroid-to-outlet guides for river cells with no incoming links."""
    nx, ny = geometry[:2]
    ncell = nx * ny
    incoming_count = np.bincount(
        destination_rows * nx + destination_cols, minlength=ncell
    ).reshape(ny, nx)
    headwater_ids = np.flatnonzero((river_cells & (incoming_count == 0)).ravel())

    # Each valid fine pixel contributes its geographic center to its coarse cell.
    pixel_rows, pixel_cols = np.nonzero(catvalid)
    labels = ((catmy[catvalid].astype(np.int64) - 1) * nx
              + catmx[catvalid].astype(np.int64) - 1)
    pixel_counts = np.bincount(labels, minlength=ncell)
    pixel_lon = hires["west"] + (pixel_cols + 0.5) * hires["resolution"]
    pixel_lat = hires["north"] - (pixel_rows + 0.5) * hires["resolution"]
    longitude_sums = np.bincount(labels, weights=pixel_lon, minlength=ncell)
    latitude_sums = np.bincount(labels, weights=pixel_lat, minlength=ncell)
    centroid_lon = np.full(ncell, np.nan)
    centroid_lat = np.full(ncell, np.nan)
    np.divide(longitude_sums, pixel_counts, out=centroid_lon, where=pixel_counts > 0)
    np.divide(latitude_sums, pixel_counts, out=centroid_lat, where=pixel_counts > 0)

    valid_centroid = pixel_counts[headwater_ids] > 0
    valid_outlet = (
        np.isfinite(outlon.ravel()[headwater_ids]) &
        np.isfinite(outlat.ravel()[headwater_ids]) &
        (outlon.ravel()[headwater_ids] != -9999) &
        (outlat.ravel()[headwater_ids] != -9999)
    )
    guide_ids = headwater_ids[valid_centroid & valid_outlet]
    segments = np.stack((
        np.column_stack((centroid_lon[guide_ids], centroid_lat[guide_ids])),
        np.column_stack((outlon.ravel()[guide_ids], outlat.ravel()[guide_ids])),
    ), axis=1)
    if not np.all(np.isfinite(segments)):
        raise ValueError("A headwater guide has a non-finite centroid or outlet")

    return segments, {
        "detected": int(headwater_ids.size),
        "valid_centroids": int(np.count_nonzero(valid_centroid)),
        "plotted": int(guide_ids.size),
    }


def segments_intersect_extent(segments, extent):
    """Select link bounding boxes overlapping the displayed lon/lat rectangle."""
    west, east, south, north = extent
    start, end = segments[:, 0, :], segments[:, 1, :]
    return ((np.maximum(start[:, 0], end[:, 0]) >= west) &
            (np.minimum(start[:, 0], end[:, 0]) <= east) &
            (np.maximum(start[:, 1], end[:, 1]) >= south) &
            (np.minimum(start[:, 1], end[:, 1]) <= north))


def add_directional_arrow(ax, segment, color, linewidth, *,
                          start_fraction=0.58, end_fraction=0.73, size=6.5,
                          zorder=7):
    """Overlay a short arrowhead along one stored directed segment."""
    start, end = np.asarray(segment[0]), np.asarray(segment[1])
    arrow_start = start + start_fraction * (end - start)
    arrow_end = start + end_fraction * (end - start)
    ax.add_patch(FancyArrowPatch(
        arrow_start, arrow_end, arrowstyle="-|>", mutation_scale=size,
        linewidth=linewidth, color=color, alpha=0.95, zorder=zorder,
        shrinkA=0, shrinkB=0,
    ))


def read_outlets(path: Path, network):
    """Read geographic outlet positions and verify their coarse grid."""
    with Dataset(path) as source:
        for name in ("lon", "lat", "outlon", "outlat"):
            if name not in source.variables:
                raise ValueError(f"{path} is missing required variable {name!r}")
        outlon_var, outlat_var = source.variables["outlon"], source.variables["outlat"]
        if (outlon_var.dimensions != ("lat", "lon") or
                outlat_var.dimensions != ("lat", "lon") or
                outlon_var.shape != network.uparea.shape or
                outlat_var.shape != network.uparea.shape):
            raise ValueError(f"{path} outlet coordinates do not match the coarse map grid")
        if (getattr(outlon_var, "units", "") != "degrees_east" or
                getattr(outlat_var, "units", "") != "degrees_north"):
            raise ValueError(f"{path} outlet coordinate units are not geographic degrees")
        outlon = np.ma.asarray(outlon_var[:], dtype=np.float64).filled(np.nan)
        outlat = np.ma.asarray(outlat_var[:], dtype=np.float64).filled(np.nan)
        lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
    if not (np.allclose(network.lon, lon, rtol=0, atol=1e-5) and
            np.allclose(network.lat, lat, rtol=0, atol=1e-5)):
        raise ValueError(f"{path} grid coordinates do not match the uparea map")
    return outlon, outlat


def read_channel_bifurcations(path: Path, outlon, outlat):
    """Read positive-first-layer-width paths and resolve outlet endpoints."""
    required = ("ix", "iy", "jx", "jy", "width", "end_lon", "end_lat")
    with Dataset(path) as source:
        for name in required:
            if name not in source.variables:
                raise ValueError(f"{path} is missing required variable {name!r}")
        indices = {name: np.asarray(source.variables[name][:], dtype=np.int64)
                   for name in ("ix", "iy", "jx", "jy")}
        widths_var = source.variables["width"]
        if widths_var.dimensions != ("layer", "path") or getattr(widths_var, "units", "") != "m":
            raise ValueError(f"{path}:width must use (layer, path) dimensions and metres")
        widths = np.ma.asarray(widths_var[:], dtype=np.float64).filled(np.nan)
        end_lon = np.ma.asarray(source.variables["end_lon"][:], dtype=np.float64).filled(np.nan)
        end_lat = np.ma.asarray(source.variables["end_lat"][:], dtype=np.float64).filled(np.nan)

    nx, ny = outlon.shape[1], outlon.shape[0]
    ix, iy, jx, jy = (indices[name] for name in ("ix", "iy", "jx", "jy"))
    valid = ((ix >= 1) & (ix <= nx) & (jx >= 1) & (jx <= nx) &
             (iy >= 1) & (iy <= ny) & (jy >= 1) & (jy <= ny) &
             np.all(np.isfinite(widths), axis=0) & np.all(widths >= 0, axis=0))
    ids = np.flatnonzero(valid)
    source_rows, source_cols = iy[ids] - 1, ix[ids] - 1
    dest_rows, dest_cols = jy[ids] - 1, jx[ids] - 1
    x0, y0 = outlon[source_rows, source_cols], outlat[source_rows, source_cols]
    x1, y1 = outlon[dest_rows, dest_cols], outlat[dest_rows, dest_cols]
    good = (np.isfinite(x0) & np.isfinite(y0) & np.isfinite(x1) & np.isfinite(y1) &
            (x0 != -9999) & (y0 != -9999) & (x1 != -9999) & (y1 != -9999) &
            (x0 >= -180) & (x0 <= 360) & (x1 >= -180) & (x1 <= 360) &
            (y0 >= -90) & (y0 <= 90) & (y1 >= -90) & (y1 <= 90))
    ids, x0, y0, x1, y1 = ids[good], x0[good], y0[good], x1[good], y1[good]
    if not (np.allclose(x1, end_lon[ids], rtol=0, atol=1e-5) and
            np.allclose(y1, end_lat[ids], rtol=0, atol=1e-5)):
        raise ValueError(f"{path} destination indices do not match stored outlet coordinates")

    segments = np.stack((np.column_stack((x0, y0)),
                         np.column_stack((x1, y1))), axis=1)
    channel = widths[0, ids] > 0
    ids, segments = ids[channel], segments[channel]
    first_layer_width = widths[0, ids]

    # Duplicate endpoint pairs are a single channel connection in the reference figure.
    unique = []
    seen = set()
    for index, path_id in enumerate(ids):
        key = (int(ix[path_id]), int(iy[path_id]), int(jx[path_id]), int(jy[path_id]))
        if key not in seen:
            seen.add(key)
            unique.append(index)
    unique = np.asarray(unique, dtype=int)
    return segments[unique], first_layer_width[unique]


def main(default_option: str) -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--uparea", type=Path, required=True)
    parser.add_argument("--nextxy", type=Path, required=True)
    parser.add_argument("--lonlat", type=Path, required=True)
    parser.add_argument("--bifprm", type=Path, required=True)
    parser.add_argument("--catmxy", type=Path, required=True)
    parser.add_argument("--location", type=Path, required=True)
    parser.add_argument("--visual", type=Path)
    parser.add_argument("--option", choices=("outlet_uparea", "channel_pixel"),
                        default=default_option)
    add_map_arguments(parser)
    args = parser.parse_args()
    extent = (args.west, args.east, args.south, args.north)
    if args.option == "channel_pixel" and args.visual is None:
        parser.error("--visual is required when --option channel_pixel is selected")
    if extent != EXTENT:
        raise ValueError(f"p01-f04 extent is fixed at {EXTENT}")

    geometry = read_geometry(args.params)
    _, _, uparea_data = read_map_raster(args.uparea, "uparea", "m2", geometry)
    network = read_river_network(args.uparea, args.nextxy, geometry)
    outlon, outlat = read_outlets(args.lonlat, network)
    bif_segments, bif_width = read_channel_bifurcations(args.bifprm, outlon, outlat)

    river_cells = np.isfinite(network.uparea) & (network.uparea > 0)
    catmx, catmy, catvalid, unassigned_land, hires = read_catchment_mapping(
        args.catmxy, args.location, geometry, river_cells,
    )
    uparea = uparea_data.filled(np.nan)
    (subx, suby, subvalid, sub_unassigned_land, catchment_area,
     x_edges, y_edges) = crop_catchment_data(
        catmx, catmy, catvalid, unassigned_land, uparea, geometry, hires, extent,
    )
    visual_crop = (read_visual_categories(args.visual, hires, extent)
                   if args.option == "channel_pixel" else None)
    if visual_crop is not None and visual_crop.shape != subvalid.shape:
        raise ValueError("visual and catmxy crops do not have matching grid dimensions")
    headwater_guides, headwater_stats = compute_headwater_guides(
        geometry, river_cells, network.dest_rows, network.dest_cols,
        catmx, catmy, catvalid, outlon, outlat, hires,
    )

    panel_id = "p01-f04a" if args.option == "outlet_uparea" else "p01-f04b"
    method = ("Upstream Area at Unit Catchment" if args.option == "outlet_uparea"
              else "Channel Pixel")
    fig, ax, colorbar_ax, legend_ax = create_f04_layout(
        geometry, extent, f"{panel_id} River Map Catchment: {method}",
    )
    fig.patch.set_facecolor("white")
    ax.set_facecolor("white")

    catchment_cmap = LinearSegmentedColormap.from_list(
        "catchment_area_light_green",
        ["#FFFFD4", "#D9F0A3", "#78C679", "#31A354"],
    )
    valid_areas = catchment_area[np.isfinite(catchment_area) & (catchment_area > 0)]
    mesh = ax.pcolormesh(
        x_edges, y_edges, np.ma.masked_invalid(catchment_area),
        cmap=catchment_cmap,
        norm=LogNorm(vmin=float(valid_areas.min()), vmax=float(valid_areas.max())),
        shading="flat", rasterized=True, zorder=1,
    )
    unassigned_layer = np.ma.masked_where(
        ~sub_unassigned_land, np.ones(sub_unassigned_land.shape, dtype=np.uint8))
    ax.pcolormesh(
        x_edges, y_edges, unassigned_layer,
        cmap=ListedColormap(["#D9D9D9"]), vmin=0.5, vmax=1.5,
        shading="flat", rasterized=True, zorder=1.1,
    )

    pixel_legend = []
    if visual_crop is not None:
        for category, color, label in (
            (10, "#83C6ED", "Main channel pixel (visual = 10)"),
            (7, "#C7A6DD", "Small stream pixel (visual = 7)"),
        ):
            pixel_mask = subvalid & (visual_crop == category)
            pixel_layer = np.ma.masked_where(
                ~pixel_mask, np.ones(pixel_mask.shape, dtype=np.uint8),
            )
            ax.pcolormesh(
                x_edges, y_edges, pixel_layer,
                cmap=ListedColormap([color]), vmin=0.5, vmax=1.5,
                alpha=0.78, shading="flat", rasterized=True, zorder=1.4,
            )
            pixel_legend.append(Patch(facecolor=color, edgecolor="none", label=label))

    fine_edges = catchment_boundaries(subx, suby, subvalid, x_edges, y_edges)
    if fine_edges:
        ax.add_collection(LineCollection(
            fine_edges, colors="#202020", linewidths=0.38,
            alpha=0.72, zorder=2,
        ))

    nx, ny, resolution, west, east, south, north = geometry
    coarse_x_edges = west + np.arange(nx + 1) * resolution
    coarse_y_edges = north - np.arange(ny + 1) * resolution
    ax.vlines(coarse_x_edges, extent[2], extent[3], colors="#777777",
              linewidth=0.42, alpha=0.42, zorder=2.5)
    ax.hlines(coarse_y_edges, extent[0], extent[1], colors="#777777",
              linewidth=0.42, alpha=0.42, zorder=2.5)

    visible_guides = headwater_guides[
        segments_intersect_extent(headwater_guides, extent)]
    if len(visible_guides):
        ax.add_collection(LineCollection(
        visible_guides, colors="#888888", linewidths=1.0,
            alpha=0.65, zorder=2.8,
        ))

    valid_outlet = (river_cells & np.isfinite(outlon) & np.isfinite(outlat) &
                    (outlon >= extent[0]) & (outlon <= extent[1]) &
                    (outlat >= extent[2]) & (outlat <= extent[3]))
    ax.scatter(outlon[valid_outlet], outlat[valid_outlet], marker="s", s=14,
               facecolors="white", edgecolors="#263238", linewidths=0.4,
               zorder=4)

    source_lon = outlon[network.source_rows, network.source_cols]
    source_lat = outlat[network.source_rows, network.source_cols]
    dest_lon = outlon[network.dest_rows, network.dest_cols]
    dest_lat = outlat[network.dest_rows, network.dest_cols]
    main_valid = (np.isfinite(source_lon) & np.isfinite(source_lat) &
                  np.isfinite(dest_lon) & np.isfinite(dest_lat) &
                  (source_lon != -9999) & (source_lat != -9999) &
                  (dest_lon != -9999) & (dest_lat != -9999))
    main_segments = np.stack((
        np.column_stack((source_lon[main_valid], source_lat[main_valid])),
        np.column_stack((dest_lon[main_valid], dest_lat[main_valid])),
    ), axis=1)
    main_area = network.uparea[network.source_rows[main_valid],
                               network.source_cols[main_valid]] / 1e6
    visible_main = segments_intersect_extent(main_segments, extent)
    main_segments, main_area = main_segments[visible_main], main_area[visible_main]
    main_classes = np.digitize(
        np.log10(main_area), np.quantile(np.log10(main_area), [0.25, 0.5, 0.75]))
    main_arrow_count = 0
    for group, (color, width) in enumerate(zip(MAIN_COLORS, MAIN_WIDTHS)):
        group_mask = main_classes == group
        group_segments = main_segments[group_mask]
        if not len(group_segments):
            continue
        ax.add_collection(LineCollection(
            group_segments, colors=color, linewidths=width,
            alpha=0.92, zorder=3 + group * 0.1,
        ))
        lengths = np.sqrt(
            ((group_segments[:, 1, 0] - group_segments[:, 0, 0]) *
             np.cos(np.deg2rad((group_segments[:, 1, 1] + group_segments[:, 0, 1]) / 2))) ** 2 +
            (group_segments[:, 1, 1] - group_segments[:, 0, 1]) ** 2
        )
        for segment in group_segments[lengths >= 0.20]:
            add_directional_arrow(ax, segment, color, width, zorder=4)
            main_arrow_count += 1

    visible_bif = segments_intersect_extent(bif_segments, extent)
    bif_segments, bif_width = bif_segments[visible_bif], bif_width[visible_bif]
    if not len(bif_segments):
        raise ValueError("No positive-width channel bifurcations intersect the requested extent")
    bif_classes = np.digitize(
        np.log10(bif_width), np.quantile(np.log10(bif_width), [0.25, 0.5, 0.75]))
    channel_arrow_count = 0
    for group, (color, width) in enumerate(zip(CHANNEL_COLORS, CHANNEL_WIDTHS)):
        group_segments = bif_segments[bif_classes == group]
        if not len(group_segments):
            continue
        ax.add_collection(LineCollection(
            group_segments, colors=color, linewidths=width,
            alpha=0.98, zorder=6,
        ))
        for segment in group_segments:
            add_directional_arrow(
                ax, segment, color, width, start_fraction=0.57,
                end_fraction=0.74, size=5.5, zorder=7,
            )
            channel_arrow_count += 1

    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.grid(alpha=0.12, linewidth=0.4, zorder=2.6)
    format_f04_colorbar(fig, colorbar_ax, mesh)

    legend_handles = [
        Line2D([], [], marker="s", markerfacecolor="white",
               markeredgecolor="#263238", linestyle="None",
               label="Unit-catchment outlet"),
        Line2D([], [], color="#202020", linewidth=0.7,
               label="Unit-catchment boundary"),
        Patch(facecolor="#D9D9D9", edgecolor="none",
              label="Unassigned land pixel (catmxy = -999)"),
        Line2D([], [], color=MAIN_COLORS[2], linewidth=1.7,
               label="Main channel (upstream area at unit-catchment)"),
        Line2D([], [], color=CHANNEL_COLORS[2], linewidth=1.3,
               label="Channel bifurcation (path-width colors)"),
        Line2D([], [], color="#888888", linewidth=0.9, alpha=0.65,
               label="Headwater catchment guide"),
    ] + pixel_legend
    add_f04_legend(legend_ax, legend_handles)

    save_panel(fig, args.output, args.dpi, args.max_width, args.max_height,
               args.figure_width, args.figure_height)
    plt.close(fig)
    distinct_catchments = len(set(zip(subx[subvalid].tolist(), suby[subvalid].tolist())))
    print(f"Wrote {args.output}; extent={extent}; "
          f"{int(subvalid.sum())} mapped pixels, {distinct_catchments} catchments, "
          f"{int(sub_unassigned_land.sum())} unassigned land pixels, "
          f"{len(fine_edges)} boundaries, {len(visible_guides)} headwater guides, "
          f"{len(main_segments)} main links/{main_arrow_count} arrows, "
          f"{len(bif_segments)} channel bifurcations/{channel_arrow_count} arrows; "
          f"{int(np.count_nonzero(subvalid & (visual_crop == 10))) if visual_crop is not None else 0} main-channel pixels, "
          f"{int(np.count_nonzero(subvalid & (visual_crop == 7))) if visual_crop is not None else 0} small-stream pixels; "
          "no observation-site data or markers")


if __name__ == "__main__":
    main("outlet_uparea")
