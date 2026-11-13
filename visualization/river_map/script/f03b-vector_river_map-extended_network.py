#!/usr/bin/env python3
"""Plot main channels, bifurcations, and headwater catchment guides."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.collections import LineCollection
from matplotlib.colors import BoundaryNorm, ListedColormap
from matplotlib.lines import Line2D
from netCDF4 import Dataset

from river_map_common import create_map_axes, read_geometry, read_map_raster, save_panel
from river_network_common import read_river_network


AREA_COLORS = ("#7FA6D6", "#4F80BF", "#285A9B", "#103A70")
AREA_WIDTHS = (0.014, 0.018, 0.024, 0.032)
BIFURCATION_COLORS = ("#F4A582", "#D6604D", "#B2182B", "#FF0000")
OVERLAND_COLOR = "#76B77B"


def read_bifurcations(path: Path, outlon: np.ndarray, outlat: np.ndarray):
    """Resolve bifprm endpoint indices to valid geographic outlet coordinates."""
    required = ("ix", "iy", "jx", "jy", "width", "end_lon", "end_lat")
    with Dataset(path) as source:
        for name in required:
            if name not in source.variables:
                raise ValueError(f"{path} is missing required variable {name!r}")

        endpoints = {}
        for name in ("ix", "iy", "jx", "jy"):
            variable = source.variables[name]
            if variable.dimensions != ("path",) or not np.issubdtype(variable.dtype, np.integer):
                raise ValueError(f"{path}:{name} must be an integer path variable")
            endpoints[name] = np.asarray(variable[:], dtype=np.int64)

        width_variable = source.variables["width"]
        if (width_variable.dimensions != ("layer", "path") or
                getattr(width_variable, "units", "") != "m"):
            raise ValueError(f"{path}:width must have (layer, path) dimensions and units m")
        widths = np.ma.asarray(width_variable[:], dtype=np.float64).filled(np.nan)
        if widths.ndim != 2 or widths.shape[1] != len(endpoints["ix"]):
            raise ValueError(f"{path}:width dimensions do not match bifurcation paths")

        end_lon = np.ma.asarray(source.variables["end_lon"][:]).filled(np.nan)
        end_lat = np.ma.asarray(source.variables["end_lat"][:]).filled(np.nan)

    nx = outlon.shape[1]
    ny = outlon.shape[0]
    ix, iy, jx, jy = (endpoints[name] for name in ("ix", "iy", "jx", "jy"))
    valid_indices = ((ix >= 1) & (ix <= nx) & (jx >= 1) & (jx <= nx) &
                     (iy >= 1) & (iy <= ny) & (jy >= 1) & (jy <= ny))
    valid_width = np.all(np.isfinite(widths), axis=0) & np.all(widths >= 0, axis=0)
    valid = valid_indices & valid_width

    path_ids = np.flatnonzero(valid)
    source_rows, source_cols = iy[path_ids] - 1, ix[path_ids] - 1
    destination_rows, destination_cols = jy[path_ids] - 1, jx[path_ids] - 1
    source_lon = outlon[source_rows, source_cols]
    source_lat = outlat[source_rows, source_cols]
    destination_lon = outlon[destination_rows, destination_cols]
    destination_lat = outlat[destination_rows, destination_cols]
    valid_outlets = (
        np.isfinite(source_lon) & np.isfinite(source_lat) &
        np.isfinite(destination_lon) & np.isfinite(destination_lat) &
        (source_lon != -9999) & (source_lat != -9999) &
        (destination_lon != -9999) & (destination_lat != -9999) &
        (source_lon >= -180) & (source_lon <= 360) &
        (destination_lon >= -180) & (destination_lon <= 360) &
        (source_lat >= -90) & (source_lat <= 90) &
        (destination_lat >= -90) & (destination_lat <= 90)
    )
    path_ids = path_ids[valid_outlets]
    source_lon, source_lat = source_lon[valid_outlets], source_lat[valid_outlets]
    destination_lon = destination_lon[valid_outlets]
    destination_lat = destination_lat[valid_outlets]
    first_layer_width = widths[0, path_ids]

    if not (np.allclose(destination_lon, end_lon[path_ids], rtol=0, atol=1e-5) and
            np.allclose(destination_lat, end_lat[path_ids], rtol=0, atol=1e-5)):
        raise ValueError(f"{path}: destination indices do not match stored outlet coordinates")

    segments = np.stack((np.column_stack((source_lon, source_lat)),
                         np.column_stack((destination_lon, destination_lat))), axis=1)
    invalid = {
        "indices": int(np.count_nonzero(~valid_indices)),
        "widths": int(np.count_nonzero(valid_indices & ~valid_width)),
        "outlets": int(np.count_nonzero(valid & ~valid_outlets)),
    }
    return segments, first_layer_width, invalid


def read_catchment_mapping(path: Path, location_path: Path, geometry,
                           river_cells: np.ndarray):
    """Read 1-minute catchment assignments and calculate pixel-centre centroids."""
    nx, ny, _, _, _, _, _ = geometry
    _, _, _, map_west, map_east, map_south, map_north = geometry
    location_lines = location_path.read_text().splitlines()
    if len(location_lines) < 3:
        raise ValueError(f"{location_path} does not contain a grid descriptor")
    fields = location_lines[2].split()
    if len(fields) < 9 or fields[1] != "1min":
        raise ValueError(f"{location_path} does not describe the expected 1-minute grid")
    west, east, south, north = map(float, fields[2:6])
    nxhi, nyhi = map(int, fields[6:8])
    resolution = float(fields[8])
    if (not np.isclose(resolution, 1.0 / 60.0) or
            nxhi != nx * 15 or nyhi != ny * 15 or
            not np.isclose(east, west + nxhi * resolution) or
            not np.isclose(north, south + nyhi * resolution) or
            not np.allclose((west, east, south, north),
                            (map_west, map_east, map_south, map_north))):
        raise ValueError(f"{location_path} has inconsistent 1-minute grid geometry")
    expected_size = 2 * nxhi * nyhi
    values = np.fromfile(path, dtype="<i2")
    if values.size != expected_size:
        raise ValueError(f"{path} has {values.size} values; expected {expected_size}")
    catmx, catmy = values.reshape(2, nyhi, nxhi)

    valid_x = (catmx >= 1) & (catmx <= nx)
    valid_y = (catmy >= 1) & (catmy <= ny)
    paired_assignment = valid_x & valid_y
    partial_assignment = valid_x != valid_y
    map_rows, map_cols = np.where(paired_assignment)
    maps_to_river = np.zeros(paired_assignment.shape, dtype=bool)
    maps_to_river[map_rows, map_cols] = river_cells[
        catmy[map_rows, map_cols] - 1, catmx[map_rows, map_cols] - 1
    ]
    valid = paired_assignment & maps_to_river

    rows, cols = np.nonzero(valid)
    labels = ((catmy[valid].astype(np.int64) - 1) * nx
              + catmx[valid].astype(np.int64) - 1)
    ncell = nx * ny
    pixel_counts = np.bincount(labels, minlength=ncell)
    pixel_lon = west + (cols + 0.5) * resolution
    pixel_lat = north - (rows + 0.5) * resolution
    longitude_sums = np.bincount(labels, weights=pixel_lon, minlength=ncell)
    latitude_sums = np.bincount(labels, weights=pixel_lat, minlength=ncell)
    centroid_lon = np.full(ncell, np.nan)
    centroid_lat = np.full(ncell, np.nan)
    np.divide(longitude_sums, pixel_counts, out=centroid_lon, where=pixel_counts > 0)
    np.divide(latitude_sums, pixel_counts, out=centroid_lat, where=pixel_counts > 0)

    metadata = {
        "nx": nxhi, "ny": nyhi, "resolution": resolution,
        "west": west, "east": east, "south": south, "north": north,
        "partial_assignments": int(np.count_nonzero(partial_assignment)),
        "nonriver_assignments": int(np.count_nonzero(paired_assignment & ~maps_to_river)),
        "unassigned": int(np.count_nonzero(~paired_assignment)),
    }
    return centroid_lon, centroid_lat, pixel_counts, metadata


def segments_intersect_extent(segments: np.ndarray, extent) -> np.ndarray:
    """Return exact segment/rectangle intersections using Liang-Barsky clipping."""
    west, east, south, north = extent
    visible = np.zeros(len(segments), dtype=bool)
    for index, (start, end) in enumerate(segments):
        x0, y0 = start
        dx, dy = end - start
        lower, upper = 0.0, 1.0
        for p, q in ((-dx, x0 - west), (dx, east - x0),
                     (-dy, y0 - south), (dy, north - y0)):
            if p == 0:
                if q < 0:
                    upper = -1.0
                    break
                continue
            fraction = q / p
            if p < 0:
                lower = max(lower, fraction)
            else:
                upper = min(upper, fraction)
        visible[index] = lower <= upper
    return visible


def read_inputs(args):
    """Read and validate coarse-map, outlet, bifurcation, and catmxy inputs."""
    geometry = read_geometry(args.params)
    network = read_river_network(args.uparea, args.nextxy, geometry)
    lon, lat, bsncol_data = read_map_raster(args.bsncol, "bsncol", "1", geometry)
    bsncol = bsncol_data.filled(-9999)

    with Dataset(args.lonlat) as source:
        for name in ("lon", "lat", "outlon", "outlat"):
            if name not in source.variables:
                raise ValueError(f"{args.lonlat} is missing required variable {name!r}")
        outlon_var, outlat_var = source.variables["outlon"], source.variables["outlat"]
        if (outlon_var.dimensions != ("lat", "lon") or
                outlat_var.dimensions != ("lat", "lon") or
                outlon_var.shape != network.uparea.shape or
                outlat_var.shape != network.uparea.shape or
                getattr(outlon_var, "units", "") != "degrees_east" or
                getattr(outlat_var, "units", "") != "degrees_north"):
            raise ValueError(f"{args.lonlat}:outlon/outlat are not aligned with the map grid")
        map_lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        map_lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
        outlon = np.ma.asarray(outlon_var[:], dtype=np.float64).filled(np.nan)
        outlat = np.ma.asarray(outlat_var[:], dtype=np.float64).filled(np.nan)
    if not (np.allclose(lon, map_lon, rtol=0, atol=1e-5) and
            np.allclose(lat, map_lat, rtol=0, atol=1e-5)):
        raise ValueError("lonlat outlet coordinates do not match the map grid")

    bif_segments, bif_width, bif_invalid = read_bifurcations(
        args.bifprm, outlon, outlat)
    river_cells = np.isfinite(network.uparea) & (network.uparea > 0)
    centroid_lon, centroid_lat, pixel_counts, hires = read_catchment_mapping(
        args.catmxy, args.location, geometry, river_cells)

    incoming = np.bincount(
        network.dest_rows * network.uparea.shape[1] + network.dest_cols,
        minlength=network.uparea.size,
    ).reshape(network.uparea.shape)
    headwater_ids = np.flatnonzero((river_cells & (incoming == 0)).ravel())
    headwater_rows, headwater_cols = np.unravel_index(headwater_ids, river_cells.shape)
    nx, ny = network.uparea.shape[1], network.uparea.shape[0]
    touches_edge = (
        (headwater_rows == 0) | (headwater_rows == ny - 1) |
        (headwater_cols == 0) | (headwater_cols == nx - 1)
    )
    valid_guides = (
        (pixel_counts[headwater_ids] > 0) &
        np.isfinite(outlon.ravel()[headwater_ids]) &
        np.isfinite(outlat.ravel()[headwater_ids]) &
        (outlon.ravel()[headwater_ids] != -9999) &
        (outlat.ravel()[headwater_ids] != -9999) &
        (outlon.ravel()[headwater_ids] >= -180) &
        (outlon.ravel()[headwater_ids] <= 360) &
        (outlat.ravel()[headwater_ids] >= -90) &
        (outlat.ravel()[headwater_ids] <= 90)
    )
    guide_ids = headwater_ids[valid_guides]
    guide_segments = np.stack((
        np.column_stack((centroid_lon[guide_ids], centroid_lat[guide_ids])),
        np.column_stack((outlon.ravel()[guide_ids], outlat.ravel()[guide_ids])),
    ), axis=1)
    if not np.all(np.isfinite(guide_segments)):
        raise ValueError("A headwater guide has a non-finite centroid or outlet")
    if not (np.array_equal(guide_segments[:, 1, 0], outlon.ravel()[guide_ids]) and
            np.array_equal(guide_segments[:, 1, 1], outlat.ravel()[guide_ids])):
        raise ValueError("A headwater guide does not terminate at its assigned cell outlet")

    main_source_lon = outlon[network.source_rows, network.source_cols]
    main_source_lat = outlat[network.source_rows, network.source_cols]
    main_destination_lon = outlon[network.dest_rows, network.dest_cols]
    main_destination_lat = outlat[network.dest_rows, network.dest_cols]
    valid_main = (
        np.isfinite(main_source_lon) & np.isfinite(main_source_lat) &
        np.isfinite(main_destination_lon) & np.isfinite(main_destination_lat) &
        (main_source_lon != -9999) & (main_source_lat != -9999) &
        (main_destination_lon != -9999) & (main_destination_lat != -9999) &
        (main_source_lon >= -180) & (main_source_lon <= 360) &
        (main_destination_lon >= -180) & (main_destination_lon <= 360) &
        (main_source_lat >= -90) & (main_source_lat <= 90) &
        (main_destination_lat >= -90) & (main_destination_lat <= 90)
    )
    main_segments = np.stack((
        np.column_stack((main_source_lon[valid_main], main_source_lat[valid_main])),
        np.column_stack((main_destination_lon[valid_main], main_destination_lat[valid_main])),
    ), axis=1)
    area = network.uparea[network.source_rows[valid_main],
                          network.source_cols[valid_main]] / 1e6
    if not len(main_segments):
        raise ValueError("No valid main-channel links with outlet coordinates were found")

    stats = {
        "main_invalid_outlets": int(np.count_nonzero(~valid_main)),
        "bif_invalid": bif_invalid,
        "headwater_candidates": int(headwater_ids.size),
        "headwater_edge_candidates": int(np.count_nonzero(touches_edge)),
        "headwater_guides": int(len(guide_segments)),
        "headwater_unplotted": int(headwater_ids.size - len(guide_segments)),
        "main_visible": int(segments_intersect_extent(main_segments, args.extent).sum()),
        "bif_visible": int(segments_intersect_extent(bif_segments, args.extent).sum()),
        "guides_visible": int(segments_intersect_extent(guide_segments, args.extent).sum()),
        "catmxy": hires,
    }
    return (geometry, network, outlon, outlat, bsncol, main_segments, area,
            bif_segments, bif_width, guide_segments, stats)


def add_legend(ax):
    """Identify the network features shown in the extended map."""
    handles = [
        Line2D([], [], color=AREA_COLORS[2], linewidth=AREA_WIDTHS[2] * 72,
               label="Main channel"),
        Line2D([], [], color=BIFURCATION_COLORS[-1], linewidth=1.1,
               label="Channel bifurcation"),
        Line2D([], [], color=OVERLAND_COLOR, linewidth=1.1,
               label="Overland bifurcation"),
        Line2D([], [], color="#888888", linewidth=0.7, alpha=0.65,
               label="Headwater catchment centroid to outlet"),
    ]
    legend = ax.legend(handles=handles, loc="lower left", ncols=2,
                       fontsize=7, framealpha=0.95, borderpad=0.5,
                       labelspacing=0.3, columnspacing=0.8, handlelength=1.7)
    legend.set_zorder(10)


def draw_directed_links(ax, segments, color, line_width, arrow_width, alpha, zorder):
    """Draw full links with a small direction arrow centered on each link."""
    ax.add_collection(LineCollection(
        segments, colors=color, linewidths=line_width * 72,
        alpha=alpha, zorder=zorder, clip_on=True,
    ))

    starts, ends = segments[:, 0, :], segments[:, 1, :]
    midpoints = (starts + ends) / 2.0
    arrow_vectors = (ends - starts) * 0.34
    arrows = ax.quiver(
        midpoints[:, 0] - arrow_vectors[:, 0] / 2.0,
        midpoints[:, 1] - arrow_vectors[:, 1] / 2.0,
        arrow_vectors[:, 0], arrow_vectors[:, 1],
        angles="xy", scale_units="xy", scale=1, pivot="tip", units="inches",
        minlength=0.0, width=arrow_width, headwidth=5.0, headlength=5.0,
        headaxislength=4.5, color=color, alpha=alpha, zorder=zorder + 0.1,
    )
    if arrows.U.size != len(segments):
        raise RuntimeError(f"Rendered {arrows.U.size} direction arrows for {len(segments)} links")


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--uparea", type=Path, required=True)
    parser.add_argument("--nextxy", type=Path, required=True)
    parser.add_argument("--lonlat", type=Path, required=True)
    parser.add_argument("--bsncol", type=Path, required=True)
    parser.add_argument("--bifprm", type=Path, required=True)
    parser.add_argument("--params", type=Path, required=True)
    parser.add_argument("--catmxy", type=Path, required=True)
    parser.add_argument("--location", type=Path, required=True)
    parser.add_argument("--west", type=float, default=102.0)
    parser.add_argument("--east", type=float, default=108.0)
    parser.add_argument("--south", type=float, default=9.0)
    parser.add_argument("--north", type=float, default=15.0)
    parser.add_argument("--figure-width", type=float, default=0.0)
    parser.add_argument("--figure-height", type=float, default=8.0)
    parser.add_argument("--dpi", type=int, default=200)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.figure_width < 0 or args.figure_height < 0 or args.dpi < 1:
        parser.error("figure dimensions must be non-negative and DPI must be positive")
    if not (args.west < args.east and args.south < args.north):
        parser.error("map extent must have west < east and south < north")
    args.extent = (args.west, args.east, args.south, args.north)
    return args


def main():
    args = parse_args()
    (geometry, network, outlon, outlat, bsncol, main_segments, main_area,
     bif_segments, bif_width, guide_segments, stats) = read_inputs(args)
    _, _, _, west, east, south, north = geometry

    main_log_area = np.log10(main_area)
    area_limits = np.quantile(main_log_area, (0.25, 0.5, 0.75))
    area_classes = np.digitize(main_log_area, area_limits)
    channel = bif_width > 0
    overland = ~channel
    if not np.any(channel):
        raise ValueError("bifprm contains no channel bifurcations with positive first-layer width")
    log_width = np.log10(bif_width[channel])
    bif_width_limits = np.quantile(log_width, (0.25, 0.5, 0.75))
    channel_classes = np.full(len(bif_width), -1, dtype=int)
    channel_classes[channel] = np.digitize(log_width, bif_width_limits)

    title = "p01-f03b Vector River Map: Extended Network"
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north, title,
    )
    fig.patch.set_facecolor("white")
    ax.set_facecolor("white")

    basin_cmap = ListedColormap(
        ["#DCE8E1", "#E8E3D7", "#DDE6EF", "#EDE0E5", "#E4E8D5"]
    )
    basin_cmap.set_bad("white")
    basin_norm = BoundaryNorm(np.arange(0.5, 6.5, 1), basin_cmap.N)
    basin_values = np.ma.masked_where((bsncol < 1) | (bsncol > 5), bsncol)
    ax.imshow(basin_values, origin="upper", extent=(west, east, south, north),
              cmap=basin_cmap, norm=basin_norm, interpolation="nearest",
              alpha=0.72, zorder=0)
    resolution = geometry[2]
    x_edges = west + np.arange(geometry[0] + 1) * resolution
    y_edges = north - np.arange(geometry[1] + 1) * resolution
    ax.vlines(x_edges, south, north, colors="#777777", linewidth=0.32,
              alpha=0.42, zorder=1)
    ax.hlines(y_edges, west, east, colors="#777777", linewidth=0.32,
              alpha=0.42, zorder=1)
    ax.add_collection(LineCollection(
        guide_segments, colors="#888888", linewidths=0.62,
        linestyles="solid", alpha=0.62, zorder=1.5, clip_on=True,
    ))

    source_rows = network.source_rows
    source_cols = network.source_cols
    dest_rows = network.dest_rows
    dest_cols = network.dest_cols
    main_count = 0
    for area_class, (color, width) in enumerate(zip(AREA_COLORS, AREA_WIDTHS)):
        selected = area_classes == area_class
        if not np.any(selected):
            continue
        segments = main_segments[selected]
        draw_directed_links(
            ax, segments, color, width, 0.01, 0.95, 2 + area_class * 0.1,
        )
        main_count += len(segments)

    if main_count != len(main_segments):
        raise RuntimeError(f"Rendered {main_count} main arrows for {len(main_segments)} valid links")

    bif_count = 0
    for width_class, color in enumerate(BIFURCATION_COLORS):
        selected = channel & (channel_classes == width_class)
        if not np.any(selected):
            continue
        segments = bif_segments[selected]
        draw_directed_links(
            ax, segments, color, 0.02, 0.01, 0.96, 4,
        )
        bif_count += len(segments)
    overland_segments = bif_segments[overland]
    draw_directed_links(
        ax, overland_segments, OVERLAND_COLOR, 0.005, 0.007, 0.72, 3,
    )
    if bif_count != int(channel.sum()) or len(overland_segments) != int(overland.sum()):
        raise RuntimeError("Not all valid bifurcation connections were rendered")

    add_legend(ax)
    save_panel(fig, args.output, args.dpi,
               figure_width=args.figure_width, figure_height=args.figure_height)
    plt.close(fig)

    print(
        f"Wrote {args.output}: main links {len(main_segments)} full-map / "
        f"{stats['main_visible']} extent; channel bifurcations {int(channel.sum())} / "
        f"{int(segments_intersect_extent(bif_segments[channel], args.extent).sum())}; "
        f"overland bifurcations {int(overland.sum())} / "
        f"{int(segments_intersect_extent(bif_segments[overland], args.extent).sum())}; "
        f"headwater guides {len(guide_segments)} / {stats['guides_visible']}; "
        f"map-edge headwater candidates {stats['headwater_edge_candidates']}."
    )
    print(
        f"Excluded: main links without valid outlet coordinates="
        f"{stats['main_invalid_outlets']}; invalid bifurcation indices/widths/outlets="
        f"{stats['bif_invalid']['indices']}/{stats['bif_invalid']['widths']}/"
        f"{stats['bif_invalid']['outlets']}; headwater candidates without valid guides="
        f"{stats['headwater_unplotted']}; catmxy unassigned pixels="
        f"{stats['catmxy']['unassigned']}."
    )
    if stats["headwater_edge_candidates"]:
        print(
            "Note: regional-map edge cells without incoming links are candidates, "
            "not necessarily physical headwaters."
        )


if __name__ == "__main__":
    main()
