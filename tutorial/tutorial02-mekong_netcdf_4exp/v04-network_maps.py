#!/usr/bin/env python3
"""Create four map figures explaining the Tutorial 02 river network."""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import BoundaryNorm, LinearSegmentedColormap, ListedColormap, LogNorm
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch, Patch
from netCDF4 import Dataset
import numpy as np


# User settings: map extents and output resolution for the four figures.
ROOT = Path(__file__).resolve().parents[2]
MAP_ROOT = ROOT / "map" / "mekong_15min"
NETCDF_DIR = MAP_ROOT / "netcdf"
HIRES_DIR = MAP_ROOT / "1min"
FIGURE_DIR = Path(__file__).resolve().parent / "figure"
FULL_EXTENT = (90.0, 110.0, 5.0, 35.0)
BASIN_EXTENT = (99.5, 108.2, 8.5, 18.5)
PANEL_C_EXTENT = (102.0, 108.0, 8.0, 15.0)
DELTA_EXTENT = (104.5, 106.8, 9.4, 11.8)
FIGURE_DPI = 240

STATIONS = [
    {"name": "Vientiane", "cell": (51, 69), "station": (102.613, 17.929)},
    {"name": "Ubon Ratchathani", "cell": (60, 80), "station": (104.863, 15.221)},
    {"name": "Stung Treng", "cell": (64, 86), "station": (105.946, 13.537)},
    {"name": "Prek Kdam", "cell": (60, 93), "station": (104.804, 11.821)},
    {"name": "My Thuan", "cell": (64, 99), "station": (105.9, 10.273)},
    {"name": "Can Tho", "cell": (64, 100), "station": (105.79775, 10.058)},
]
LABEL_OFFSETS = {
    "Vientiane": (-52, 7),
    "Ubon Ratchathani": (-18, 9),
    "Stung Treng": (8, 7),
    "Prek Kdam": (-47, -13),
    "My Thuan": (7, 9),
    "Can Tho": (7, -14),
}


def read_map_data():
    """Read model-grid attributes, downstream links, outlets, and bifurcations."""
    with Dataset(NETCDF_DIR / "uparea.nc") as data:
        lon = np.asarray(data.variables["lon"][:])
        lat = np.asarray(data.variables["lat"][:])
        uparea = np.ma.filled(data.variables["uparea"][:], np.nan).astype(float)
    with Dataset(NETCDF_DIR / "nextxy_noedge.nc") as data:
        nextx = np.ma.filled(data.variables["nextx"][:], -9999).astype(int)
        nexty = np.ma.filled(data.variables["nexty"][:], -9999).astype(int)
    with Dataset(NETCDF_DIR / "lonlat.nc") as data:
        outlon = np.ma.filled(data.variables["outlon"][:], -9999).astype(float)
        outlat = np.ma.filled(data.variables["outlat"][:], -9999).astype(float)
    with Dataset(NETCDF_DIR / "bsncol.nc") as data:
        bsncol = np.ma.asarray(data.variables["bsncol"][:])
    with Dataset(NETCDF_DIR / "bifprm.nc") as data:
        bif = {name: np.asarray(data.variables[name][:]) for name in
               ("pth_id", "ix", "iy", "jx", "jy", "width",
                "start_lon", "start_lat", "end_lon", "end_lat")}

    shape = (len(lat), len(lon))
    if uparea.shape != shape or nextx.shape != shape or nexty.shape != shape:
        raise ValueError("uparea and nextxy arrays do not match the coordinate grid")
    if outlon.shape != shape or outlat.shape != shape or bsncol.shape != shape:
        raise ValueError("outlet and basin-color arrays do not match the coordinate grid")
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("expected longitude increasing and latitude north-to-south")

    return lon, lat, uparea, nextx, nexty, outlon, outlat, bsncol, bif


def read_grid_geometry():
    """Read coarse and high-resolution grid bounds from map descriptors."""
    values = [line.split()[0] for line in (MAP_ROOT / "params.txt").read_text().splitlines()]
    nx, ny = int(values[0]), int(values[1])
    resolution = float(values[3])
    west, east, south, north = map(float, values[4:8])

    fields = (HIRES_DIR / "location.txt").read_text().splitlines()[2].split()
    hires_west, hires_east, hires_south, hires_north = map(float, fields[2:6])
    hires_nx, hires_ny = map(int, fields[6:8])
    hires_resolution = float(fields[8])
    return (nx, ny, resolution, west, east, south, north,
            hires_nx, hires_ny, hires_resolution,
            hires_west, hires_east, hires_south, hires_north)


def read_catchment_mapping(geometry, river_cells):
    """Read the two int16 catmxy records and their assigned coarse cell values."""
    nx, ny, _, _, _, _, _, nxhi, nyhi, _, _, _, _, _ = geometry
    values = np.fromfile(HIRES_DIR / "1min.catmxy.bin", dtype="<i2")
    if values.size != 2 * nxhi * nyhi:
        raise ValueError(f"catmxy has {values.size} values; expected {2 * nxhi * nyhi}")
    catmx, catmy = values.reshape(2, nyhi, nxhi)
    valid = ((catmx >= 1) & (catmx <= nx) & (catmy >= 1) & (catmy <= ny))
    maps_to_river = np.zeros(valid.shape, dtype=bool)
    rows, cols = np.where(valid)
    maps_to_river[rows, cols] = river_cells[catmy[rows, cols] - 1, catmx[rows, cols] - 1]
    if np.any(valid & ~maps_to_river):
        raise ValueError("catmxy assigns high-resolution pixels to non-river map cells")
    return catmx, catmy, valid


def map_station_pixels(stations, geometry, catmx, catmy, valid):
    """Resolve each geographic point through the documented catmxy index rule."""
    (_, _, _, _, _, _, _, nxhi, nyhi, hires_resolution,
     hires_west, _, _, hires_north) = geometry
    for station in stations:
        lon, lat = station["station"]
        col = int((lon - hires_west) / hires_resolution)
        row = int((hires_north - lat) / hires_resolution)
        if not (0 <= col < nxhi and 0 <= row < nyhi and valid[row, col]):
            raise ValueError(f"{station['name']}: location has no valid catmxy assignment")
        mapped_cell = (int(catmx[row, col]), int(catmy[row, col]))
        if mapped_cell != station["cell"]:
            raise ValueError(f"{station['name']}: catmxy maps to {mapped_cell}, expected {station['cell']}")
        station["pixel"] = (col, row)


def downstream_links(lon, lat, uparea, nextx, nexty):
    """Return valid one-based nextxy links and count malformed/out-of-map links."""
    river = np.isfinite(uparea) & (uparea > 0)
    rows, cols = np.indices(uparea.shape)
    positive = river & (nextx > 0) & (nexty > 0)
    in_bounds = ((nextx >= 1) & (nextx <= len(lon)) &
                 (nexty >= 1) & (nexty <= len(lat)))
    invalid = int(np.count_nonzero(positive & ~in_bounds))
    if np.any(river & ((nextx > 0) != (nexty > 0))):
        raise ValueError("nextxy contains a source with only one positive destination index")
    mask = positive & in_bounds
    source_rows, source_cols = np.where(mask)
    dest_cols = nextx[mask] - 1
    dest_rows = nexty[mask] - 1
    if np.any(~river[dest_rows, dest_cols]):
        raise ValueError("nextxy points to a destination without valid upstream area")
    nonlocal_mask = np.maximum(np.abs(nextx[mask] - (cols[mask] + 1)),
                               np.abs(nexty[mask] - (rows[mask] + 1))) > 1
    segments = np.stack(
        (np.column_stack((lon[source_cols], lat[source_rows])),
         np.column_stack((lon[dest_cols], lat[dest_rows]))), axis=1)
    return river, mask, source_rows, source_cols, dest_rows, dest_cols, segments, nonlocal_mask, invalid


def validate_outlets_and_bifurcations(stations, geometry, catmx, catmy, valid,
                                      outlon, outlat, bif):
    """Check selected outlet pixels and bifurcation table endpoints against map data."""
    (nx, ny, _, _, _, _, _, nxhi, nyhi, hires_resolution,
     hires_west, hires_east, hires_south, hires_north) = geometry
    for station in stations:
        ix, iy = station["cell"]
        x, y = outlon[iy - 1, ix - 1], outlat[iy - 1, ix - 1]
        col = int((x - hires_west) / hires_resolution)
        row = int((hires_north - y) / hires_resolution)
        if not (0 <= col < nxhi and 0 <= row < nyhi and valid[row, col]):
            raise ValueError(f"{station['name']}: outlet has no valid high-resolution catchment pixel")
        if (int(catmx[row, col]), int(catmy[row, col])) != (ix, iy):
            raise ValueError(f"{station['name']}: outlet pixel maps to a different coarse cell")
        station["outlet"] = (x, y)

    ix, iy, jx, jy = (bif[name].astype(int) for name in ("ix", "iy", "jx", "jy"))
    valid_endpoints = ((ix >= 1) & (ix <= nx) & (jx >= 1) & (jx <= nx) &
                       (iy >= 1) & (iy <= ny) & (jy >= 1) & (jy <= ny))
    invalid_bifurcations = int(np.count_nonzero(~valid_endpoints))
    if invalid_bifurcations:
        raise ValueError(f"bifprm.nc has {invalid_bifurcations} out-of-map pathway endpoints")
    return invalid_bifurcations


def geographic_aspect(ax, south, north):
    """Approximate local lon/lat distances with a latitude-adjusted aspect."""
    mid_lat = np.deg2rad((south + north) / 2)
    ax.set_aspect(1 / np.cos(mid_lat))


def add_station_markers(ax, stations, lon, lat, outlon, outlat,
                        connect_to="center", show_centers=True,
                        show_outlets=True, labels=True):
    """Show station, grid-center, and outlet locations with selected connectors."""
    connector_segments = []
    for station in stations:
        ix, iy = station["cell"]
        center = (lon[ix - 1], lat[iy - 1])
        outlet = (outlon[iy - 1, ix - 1], outlat[iy - 1, ix - 1])
        target = center if connect_to == "center" else outlet
        connector_segments.append((station["station"], target))

    ax.add_collection(LineCollection(connector_segments, colors="#B8B8B8",
                                     linewidths=0.65, alpha=0.85, zorder=6))
    for station in stations:
        ix, iy = station["cell"]
        ax.scatter(*station["station"], marker="x", color="#a00065", s=27,
                   linewidths=1.2, zorder=9)
        if show_centers:
            ax.scatter(lon[ix - 1], lat[iy - 1], marker="o", facecolors="white",
                       edgecolors="#222222", s=28, linewidths=0.8, zorder=8)
        if show_outlets:
            ax.scatter(outlon[iy - 1, ix - 1], outlat[iy - 1, ix - 1], marker="s",
                       facecolors="#ffcc55", edgecolors="#222222", s=25,
                       linewidths=0.7, zorder=8)
        if labels:
            ax.annotate(station["name"], station["station"],
                        xytext=LABEL_OFFSETS[station["name"]],
                        textcoords="offset points", fontsize=8, zorder=10)


def add_common_legend(ax, include_network=False, show_outlets=True):
    """Add station symbols first, followed by any displayed network lines."""
    handles = [
        Line2D([], [], color="#a00065", marker="x", linestyle="None",
               label="Station coordinate"),
        Line2D([], [], marker="o", markerfacecolor="white", markeredgecolor="#222222",
               linestyle="None", label="15-minute grid-cell center"),
    ]
    if show_outlets:
        handles.append(Line2D([], [], marker="s", markerfacecolor="#ffcc55",
                              markeredgecolor="#222222", linestyle="None",
                              label="Unit-catchment outlet"))
    handles.append(Line2D([], [], color="#B8B8B8", linewidth=0.7,
                          label="Station-to-grid-center link"))
    if include_network:
        handles.extend([
            Line2D([], [], color="#647d91", linewidth=1.2,
                   label="Local downstream link (≤8-neighbor)"),
            Line2D([], [], color="#f28e2b", linewidth=1.5,
                   label="Flexible link (>8-neighbor)"),
        ])
    ax.legend(handles=handles, loc="lower left", fontsize=7, framealpha=0.9)


def add_directional_arrow(ax, segment, color, linewidth=0.8,
                          start_fraction=0.58, end_fraction=0.73, size=6.5,
                          alpha=0.95, zorder=7):
    """Overlay a small point-sized arrowhead along a stored directed segment."""
    start, end = np.asarray(segment[0]), np.asarray(segment[1])
    arrow_start = start + start_fraction * (end - start)
    arrow_end = start + end_fraction * (end - start)
    ax.add_patch(FancyArrowPatch(arrow_start, arrow_end, arrowstyle="-|>",
                                 mutation_scale=size, linewidth=linewidth,
                                 color=color, alpha=alpha, zorder=zorder,
                                 shrinkA=0, shrinkB=0))


def format_geographic_axes(ax, extent, geometry):
    """Set shared geographic limits, labels, and aspect ratio."""
    _, _, _, west, east, south, north, *_ = geometry
    xmin, xmax, ymin, ymax = extent
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    ax.set_xlabel("Longitude (degrees east)")
    ax.set_ylabel("Latitude (degrees north)")
    geographic_aspect(ax, ymin, ymax)
    ax.grid(alpha=0.12, linewidth=0.4)


def save_figure(fig, name):
    path = FIGURE_DIR / name
    fig.savefig(path, dpi=FIGURE_DPI, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote {path}")


def plot_uparea(lon, lat, uparea, stations, outlon, outlat, geometry):
    """Panel (a): show upstream drainage area on the model's regular grid."""
    river = np.isfinite(uparea) & (uparea > 0)
    data = np.ma.masked_where(~river, uparea / 1e6)
    fig, ax = plt.subplots(figsize=(8.5, 11.5))
    west, east, south, north = geometry[3:7]
    image = ax.imshow(data, origin="upper", extent=(west, east, south, north),
                      cmap="YlGnBu", norm=LogNorm(vmin=float(data.min()), vmax=float(data.max())),
                      interpolation="nearest", alpha=0.95)
    add_station_markers(ax, stations, lon, lat, outlon, outlat,
                        connect_to="center", show_outlets=False)
    add_common_legend(ax, show_outlets=False)
    format_geographic_axes(ax, FULL_EXTENT, geometry)
    fig.colorbar(image, ax=ax, shrink=0.72, label="Upstream drainage area (km²)")
    ax.set_title("(a) Upstream drainage area on the 15-minute grid")
    save_figure(fig, "station01_uparea.png")


def plot_grid_network(lon, lat, uparea, links, stations, outlon, outlat, geometry):
    """Panel (b): show nextxy links between regular computational cell centers."""
    _, _, rows, cols, dest_rows, dest_cols, segments, nonlocal_mask, _ = links
    fig, ax = plt.subplots(figsize=(8.1, 8.4))
    west, east, south, north = geometry[3:7]
    image = ax.imshow(np.ma.masked_where(~np.isfinite(uparea) | (uparea <= 0), uparea / 1e6),
                      origin="upper", extent=(west, east, south, north), cmap="YlGnBu",
                      norm=LogNorm(vmin=float(np.nanmin(uparea[uparea > 0]) / 1e6),
                                   vmax=float(np.nanmax(uparea) / 1e6)),
                      interpolation="nearest", alpha=0.17)
    # Draw each nextxy link as an arrow whose tip reaches its destination center.
    local = ~nonlocal_mask
    source_lon, source_lat = lon[cols], lat[rows]
    delta_lon = lon[dest_cols] - source_lon
    delta_lat = lat[dest_rows] - source_lat
    local_arrows = ax.quiver(
        source_lon[local], source_lat[local], delta_lon[local], delta_lat[local],
        angles="xy", scale_units="xy", scale=1, pivot="tail", units="inches",
        width=0.007, headwidth=7.5, headlength=6.5, headaxislength=5.8,
        color="#4778B5", alpha=0.9, zorder=3)
    flexible_arrows = ax.quiver(
        source_lon[nonlocal_mask], source_lat[nonlocal_mask],
        delta_lon[nonlocal_mask], delta_lat[nonlocal_mask],
        angles="xy", scale_units="xy", scale=1, pivot="tail", units="inches",
        width=0.010, headwidth=7.5, headlength=6.5, headaxislength=5.8,
        color="#F28E2B", alpha=0.98, zorder=5)
    arrow_count = local_arrows.U.size + flexible_arrows.U.size
    if arrow_count != len(segments):
        raise RuntimeError("Panel (b) did not render one arrow per valid nextxy link")

    add_station_markers(ax, stations, lon, lat, outlon, outlat,
                        connect_to="center", show_outlets=False)
    add_common_legend(ax, include_network=True, show_outlets=False)
    format_geographic_axes(ax, BASIN_EXTENT, geometry)
    ax.set_title("(b) Explicit downstream links between grid-cell centers")
    print(f"Panel (b): {len(segments)} valid downstream links and "
          f"{arrow_count} directed arrows rendered ({local_arrows.U.size} local, "
          f"{flexible_arrows.U.size} flexible).")
    save_figure(fig, "station02_grid_network.png")


def plot_outlet_network(lon, lat, uparea, links, stations, outlon, outlat,
                        bsncol, bif, geometry):
    """Panel (c): show outlet connectivity, basin colors, and bifurcations."""
    _, _, rows, cols, dest_rows, dest_cols, _, _, _ = links
    main_segments = np.stack(
        (np.column_stack((outlon[rows, cols], outlat[rows, cols])),
         np.column_stack((outlon[dest_rows, dest_cols], outlat[dest_rows, dest_cols]))), axis=1)
    main_delta = main_segments[:, 1, :] - main_segments[:, 0, :]
    source_lon = outlon[rows, cols]
    source_lat = outlat[rows, cols]
    destination_lon = outlon[dest_rows, dest_cols]
    destination_lat = outlat[dest_rows, dest_cols]
    source_area = uparea[rows, cols]
    if not (np.array_equal(main_segments[:, 0, 0], source_lon) and
            np.array_equal(main_segments[:, 0, 1], source_lat) and
            np.array_equal(main_segments[:, 1, 0], destination_lon) and
            np.array_equal(main_segments[:, 1, 1], destination_lat)):
        raise ValueError("Main-channel arrows do not match latlon outlet coordinates")

    bif_source_lon = outlon[bif["iy"].astype(int) - 1, bif["ix"].astype(int) - 1]
    bif_source_lat = outlat[bif["iy"].astype(int) - 1, bif["ix"].astype(int) - 1]
    bif_destination_lon = outlon[bif["jy"].astype(int) - 1, bif["jx"].astype(int) - 1]
    bif_destination_lat = outlat[bif["jy"].astype(int) - 1, bif["jx"].astype(int) - 1]
    bif_segments = np.stack(
        (np.column_stack((bif_source_lon, bif_source_lat)),
         np.column_stack((bif_destination_lon, bif_destination_lat))), axis=1)
    bif_delta = bif_segments[:, 1, :] - bif_segments[:, 0, :]
    channel = bif["width"][0, :] > 0
    overland = ~channel
    source_error = max(
        float(np.max(np.abs(bif_segments[:, 0, 0] - bif_source_lon))),
        float(np.max(np.abs(bif_segments[:, 0, 1] - bif_source_lat)))
    )
    destination_error = max(
        float(np.max(np.abs(bif_segments[:, 1, 0] - bif_destination_lon))),
        float(np.max(np.abs(bif_segments[:, 1, 1] - bif_destination_lat)))
    )
    if source_error != 0.0 or destination_error != 0.0:
        raise ValueError("Bifurcation endpoints do not match cell outlet coordinates")

    fig, ax = plt.subplots(figsize=(8.5, 9.2))
    west, east, south, north = geometry[3:7]
    resolution = geometry[2]
    x_edges = west + np.arange(geometry[0] + 1) * resolution
    y_edges = north - np.arange(geometry[1] + 1) * resolution
    basin_cmap = ListedColormap(
        ["#DCE8E1", "#E8E3D7", "#DDE6EF", "#EDE0E5", "#E4E8D5"])
    basin_cmap.set_bad("white")
    basin_norm = BoundaryNorm(np.arange(0.5, 6.5, 1), basin_cmap.N)
    basin_values = np.ma.masked_where((bsncol < 1) | (bsncol > 5), bsncol)
    ax.imshow(basin_values, origin="upper", extent=(west, east, south, north),
              cmap=basin_cmap, norm=basin_norm, interpolation="nearest",
              alpha=0.72, zorder=0)
    ax.vlines(x_edges, south, north, colors="#777777", linewidth=0.32,
              alpha=0.42, zorder=1)
    ax.hlines(y_edges, west, east, colors="#777777", linewidth=0.32,
              alpha=0.42, zorder=1)
    area_groups = np.digitize(
        np.log10(source_area), np.quantile(np.log10(source_area), [0.25, 0.5, 0.75]))
    main_widths = (0.0035, 0.0050, 0.0070, 0.0095)
    main_colors = ("#7FA6D6", "#4F80BF", "#285A9B", "#103A70")
    main_arrows = []
    main_group_masks = []
    for group, (width, color) in enumerate(zip(main_widths, main_colors)):
        group_mask = area_groups == group
        main_group_masks.append(group_mask)
        main_arrows.append(ax.quiver(
            main_segments[group_mask, 0, 0], main_segments[group_mask, 0, 1],
            main_delta[group_mask, 0], main_delta[group_mask, 1],
            angles="xy", scale_units="xy", scale=1, pivot="tail", units="inches",
            minlength=0.0, width=width, headwidth=7.5, headlength=6.5,
            headaxislength=5.8, color=color, alpha=0.92, zorder=2))
    channel_arrows = ax.quiver(
        bif_segments[channel, 0, 0], bif_segments[channel, 0, 1],
        bif_delta[channel, 0], bif_delta[channel, 1], angles="xy",
        scale_units="xy", scale=1, pivot="tail", units="inches", minlength=0.0,
        width=0.007,
        headwidth=7.5, headlength=6.5, headaxislength=5.8,
        color="#CF3030", alpha=0.96, zorder=4)
    overland_arrows = ax.quiver(
        bif_segments[overland, 0, 0], bif_segments[overland, 0, 1],
        bif_delta[overland, 0], bif_delta[overland, 1], angles="xy",
        scale_units="xy", scale=1, pivot="tail", units="inches", minlength=0.0,
        width=0.005,
        headwidth=7.5, headlength=6.5, headaxislength=5.8,
        color="#76B77B", alpha=0.72, zorder=3)
    if (sum(arrows.U.size for arrows in main_arrows) != len(main_segments) or
            channel_arrows.U.size != int(channel.sum()) or
            overland_arrows.U.size != int(overland.sum())):
        raise RuntimeError("Panel (c) did not render every network connection")
    main_origin_error = max(max(
        float(np.max(np.abs(arrows.X - source_lon[group_mask]))),
        float(np.max(np.abs(arrows.Y - source_lat[group_mask])))
    ) for arrows, group_mask in zip(main_arrows, main_group_masks))
    channel_origin_error = max(
        float(np.max(np.abs(channel_arrows.X - bif_source_lon[channel]))),
        float(np.max(np.abs(channel_arrows.Y - bif_source_lat[channel])))
    )
    overland_origin_error = max(
        float(np.max(np.abs(overland_arrows.X - bif_source_lon[overland]))),
        float(np.max(np.abs(overland_arrows.Y - bif_source_lat[overland])))
    )
    main_destination_error = max(max(
        float(np.max(np.abs(arrows.X + arrows.U - destination_lon[group_mask]))),
        float(np.max(np.abs(arrows.Y + arrows.V - destination_lat[group_mask])))
    ) for arrows, group_mask in zip(main_arrows, main_group_masks))
    channel_destination_error = max(
        float(np.max(np.abs(channel_arrows.X + channel_arrows.U -
                            bif_destination_lon[channel]))),
        float(np.max(np.abs(channel_arrows.Y + channel_arrows.V -
                            bif_destination_lat[channel])))
    )
    overland_destination_error = max(
        float(np.max(np.abs(overland_arrows.X + overland_arrows.U -
                            bif_destination_lon[overland]))),
        float(np.max(np.abs(overland_arrows.Y + overland_arrows.V -
                            bif_destination_lat[overland])))
    )
    if max(main_origin_error, channel_origin_error, overland_origin_error,
           main_destination_error, channel_destination_error,
           overland_destination_error) > 1e-12:
        raise ValueError("A rendered arrow endpoint does not match a cell outlet")

    add_station_markers(ax, stations, lon, lat, outlon, outlat,
                        connect_to="outlet", show_centers=False,
                        show_outlets=True)
    handles = [
        Line2D([], [], color="#a00065", marker="x", linestyle="None",
               label="Station coordinate"),
        Line2D([], [], marker="s", markerfacecolor="#ffcc55", markeredgecolor="#222222",
               linestyle="None", label="Unit-catchment outlet"),
        Line2D([], [], color="#B8B8B8", linewidth=0.7,
               label="Station-to-outlet link"),
        Line2D([], [], color=main_colors[0], linewidth=0.8,
               label="Main channel (lower uparea)"),
        Line2D([], [], color=main_colors[1], linewidth=1.2,
               label="Main channel (moderate uparea)"),
        Line2D([], [], color=main_colors[2], linewidth=1.8,
               label="Main channel (higher uparea)"),
        Line2D([], [], color=main_colors[3], linewidth=2.4,
               label="Main channel (highest uparea)"),
        Line2D([], [], color="#CF3030", linewidth=1.5, label="Channel bifurcation"),
        Line2D([], [], color="#76B77B", linewidth=1.0, label="Overland bifurcation"),
        Patch(facecolor="#DDE6EF", edgecolor="none", label="Basin color class (bsncol.nc)"),
        Line2D([], [], color="#777777", linewidth=0.5, label="15-minute grid boundary"),
    ]
    ax.legend(handles=handles, loc="lower left", fontsize=7, framealpha=0.94)
    format_geographic_axes(ax, PANEL_C_EXTENT, geometry)
    ax.set_title("(c) Geographic outlet network on the computational grid")
    save_figure(fig, "station03_outlet_bifurcation.png")
    print(f"Panel (c): {len(main_segments)} main-channel arrows; "
          f"{channel_arrows.U.size} channel and {overland_arrows.U.size} overland "
          f"bifurcation arrows. Maximum endpoint-to-latlon outlet differences: "
          f"main {max(main_origin_error, main_destination_error):.2g}°, "
          f"channel {max(channel_origin_error, channel_destination_error):.2g}°, "
          f"overland {max(overland_origin_error, overland_destination_error):.2g}°.")


def boundary_segments(catmx, catmy, valid, x_edges, y_edges):
    """Build exact pixel-edge segments where neighboring valid catmxy cells differ."""
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


def segments_intersect_extent(segments, extent):
    """Select segments whose endpoint bounding box intersects a lon/lat extent."""
    xmin, xmax, ymin, ymax = extent
    start, end = segments[:, 0, :], segments[:, 1, :]
    return ((np.maximum(start[:, 0], end[:, 0]) >= xmin) &
            (np.minimum(start[:, 0], end[:, 0]) <= xmax) &
            (np.maximum(start[:, 1], end[:, 1]) >= ymin) &
            (np.minimum(start[:, 1], end[:, 1]) <= ymax))


def plot_delta_catchments(lon, lat, uparea, nextx, nexty, outlon, outlat,
                          geometry, catmx, catmy, valid, stations, bif):
    """Panel (d): relate high-resolution catchments, outlets, and delta links."""
    (_, _, _, _, _, _, _, nxhi, nyhi, hires_resolution,
     hires_west, _, _, hires_north) = geometry
    xmin, xmax, ymin, ymax = DELTA_EXTENT
    col0 = max(0, int((xmin - hires_west) / hires_resolution) - 1)
    col1 = min(nxhi, int(np.ceil((xmax - hires_west) / hires_resolution)) + 1)
    row0 = max(0, int((hires_north - ymax) / hires_resolution) - 1)
    row1 = min(nyhi, int(np.ceil((hires_north - ymin) / hires_resolution)) + 1)
    subx, suby = catmx[row0:row1, col0:col1], catmy[row0:row1, col0:col1]
    subvalid = valid[row0:row1, col0:col1]
    unassigned_land = (subx == -999) & (suby == -999)
    if np.any((subx == -999) != (suby == -999)):
        raise ValueError("catmxy has mismatched unassigned-pixel flags")
    catchment_area = np.full(subvalid.shape, np.nan, dtype=float)
    rows, cols = np.where(subvalid)
    catchment_area[rows, cols] = uparea[suby[rows, cols] - 1, subx[rows, cols] - 1] / 1e6

    hires_x_edges = hires_west + np.arange(col0, col1 + 1) * hires_resolution
    hires_y_edges = hires_north - np.arange(row0, row1 + 1) * hires_resolution
    catchment_edges = boundary_segments(
        subx, suby, subvalid, hires_x_edges, hires_y_edges)
    fig, ax = plt.subplots(figsize=(10.0, 11.0))
    catchment_cmap = LinearSegmentedColormap.from_list(
        "catchment_area_light_green",
        ["#FFFFD4", "#D9F0A3", "#78C679", "#31A354"])
    mesh = ax.pcolormesh(
        hires_x_edges, hires_y_edges, np.ma.masked_invalid(catchment_area),
        cmap=catchment_cmap,
        norm=LogNorm(vmin=float(np.nanmin(catchment_area)),
                     vmax=float(np.nanmax(catchment_area))),
        shading="flat", zorder=1)
    unassigned_layer = np.ma.masked_where(
        ~unassigned_land, np.ones(unassigned_land.shape, dtype=np.uint8))
    ax.pcolormesh(
        hires_x_edges, hires_y_edges, unassigned_layer,
        cmap=ListedColormap(["#D9D9D9"]), vmin=0.5, vmax=1.5,
        shading="flat", zorder=1.1)
    ax.add_collection(LineCollection(catchment_edges, colors="#202020",
                                     linewidths=0.38, alpha=0.72, zorder=2))

    # Coarse cell edges show the grid that the irregular fine pixels are assigned to.
    west, east, south, north = geometry[3:7]
    x_edges = west + np.arange(geometry[0] + 1) * geometry[2]
    y_edges = north - np.arange(geometry[1] + 1) * geometry[2]
    ax.vlines(x_edges, ymin, ymax, colors="#777777", linewidth=0.42,
              alpha=0.42, zorder=2.5)
    ax.hlines(y_edges, xmin, xmax, colors="#777777", linewidth=0.42,
              alpha=0.42, zorder=2.5)

    river = np.isfinite(uparea) & (uparea > 0)
    outlet_rows, outlet_cols = np.where(
        river & (outlon >= xmin) & (outlon <= xmax) &
        (outlat >= ymin) & (outlat <= ymax))
    ax.scatter(outlon[outlet_rows, outlet_cols], outlat[outlet_rows, outlet_cols],
               marker="s", s=14, facecolors="white", edgecolors="#263238",
               linewidths=0.4, zorder=4)

    link_mask = (river & (nextx > 0) & (nexty > 0) &
                 (nextx <= len(lon)) & (nexty <= len(lat)))
    link_rows, link_cols = np.where(link_mask)
    link_dest_cols = nextx[link_mask] - 1
    link_dest_rows = nexty[link_mask] - 1
    main_source_lon = outlon[link_rows, link_cols]
    main_source_lat = outlat[link_rows, link_cols]
    main_destination_lon = outlon[link_dest_rows, link_dest_cols]
    main_destination_lat = outlat[link_dest_rows, link_dest_cols]
    main_segments = np.stack(
        (np.column_stack((main_source_lon, main_source_lat)),
         np.column_stack((main_destination_lon, main_destination_lat))), axis=1)
    visible_links = segments_intersect_extent(main_segments, DELTA_EXTENT)
    visible_segments = main_segments[visible_links]
    visible_source_area = uparea[link_rows[visible_links], link_cols[visible_links]]
    visible_lengths = np.sqrt(
        ((visible_segments[:, 1, 0] - visible_segments[:, 0, 0]) *
         np.cos(np.deg2rad((visible_segments[:, 1, 1] + visible_segments[:, 0, 1]) / 2))) ** 2 +
        (visible_segments[:, 1, 1] - visible_segments[:, 0, 1]) ** 2)
    main_widths = (0.55, 0.85, 1.25, 1.8)
    main_colors = ("#7FA6D6", "#4F80BF", "#285A9B", "#103A70")
    visible_area_groups = np.digitize(
        np.log10(visible_source_area),
        np.quantile(np.log10(visible_source_area), [0.25, 0.5, 0.75]))
    for group, (width, color) in enumerate(zip(main_widths, main_colors)):
        group_mask = visible_area_groups == group
        ax.add_collection(LineCollection(
            visible_segments[group_mask], colors=color, linewidths=width,
            alpha=0.92, zorder=3))
        arrow_mask = group_mask & (visible_lengths >= 0.20)
        for segment in visible_segments[arrow_mask]:
            add_directional_arrow(ax, segment, color, linewidth=width,
                                  size=6.5, zorder=4)

    # Resolve bifurcation endpoint indices to geographic cell outlets.
    bif_source_lon = outlon[bif["iy"].astype(int) - 1, bif["ix"].astype(int) - 1]
    bif_source_lat = outlat[bif["iy"].astype(int) - 1, bif["ix"].astype(int) - 1]
    bif_destination_lon = outlon[bif["jy"].astype(int) - 1, bif["jx"].astype(int) - 1]
    bif_destination_lat = outlat[bif["jy"].astype(int) - 1, bif["jx"].astype(int) - 1]
    bif_segments = np.stack(
        (np.column_stack((bif_source_lon, bif_source_lat)),
         np.column_stack((bif_destination_lon, bif_destination_lat))), axis=1)
    channel = bif["width"][0, :] > 0
    intersects = segments_intersect_extent(bif_segments, DELTA_EXTENT)
    candidates = np.where(channel & intersects)[0]
    unique_indices = []
    seen = set()
    for index in candidates:
        key = (int(bif["ix"][index]), int(bif["iy"][index]),
               int(bif["jx"][index]), int(bif["jy"][index]))
        if key not in seen:
            seen.add(key)
            unique_indices.append(index)
    channel_indices = np.asarray(unique_indices, dtype=int)
    delta_bif_segments = bif_segments[channel_indices]
    delta_bif_width = bif["width"][0, channel_indices]
    bif_width_groups = np.digitize(
        np.log10(delta_bif_width),
        np.quantile(np.log10(delta_bif_width), [0.25, 0.5, 0.75]))
    bif_line_widths = (1.0, 1.5, 2.2, 3.0)
    bif_colors = ("#F4A582", "#D6604D", "#B2182B", "#67001F")
    for group, (line_width, color) in enumerate(zip(bif_line_widths, bif_colors)):
        group_mask = bif_width_groups == group
        ax.add_collection(LineCollection(
            delta_bif_segments[group_mask], colors=color,
            linewidths=line_width, alpha=0.98, zorder=6))
        for segment in delta_bif_segments[group_mask]:
            add_directional_arrow(ax, segment, color, linewidth=line_width,
                                  start_fraction=0.57, end_fraction=0.74,
                                  size=7.0, zorder=7)

    visible_stations = [station for station in stations
                        if xmin <= station["station"][0] <= xmax and
                        ymin <= station["station"][1] <= ymax]
    add_station_markers(ax, visible_stations, lon, lat, outlon, outlat,
                        connect_to="outlet", show_centers=False,
                        show_outlets=True)

    ax.legend(handles=[
        Line2D([], [], color="#a00065", marker="x", linestyle="None",
               label="Station coordinate"),
        Line2D([], [], marker="s", markerfacecolor="#ffcc55", markeredgecolor="#111111",
               linestyle="None", label="Unit-catchment outlet"),
        Line2D([], [], color="#B8B8B8", linewidth=0.7,
               label="Station-to-outlet link"),
        Line2D([], [], color="#202020", linewidth=0.7, label="Unit-catchment boundary"),
        Patch(facecolor="#D9D9D9", edgecolor="none",
              label="Unassigned land pixel (catmxy = -999)"),
        Line2D([], [], color=main_colors[0], linewidth=0.8,
               label="Main channel (lower uparea)"),
        Line2D([], [], color=main_colors[1], linewidth=1.2,
               label="Main channel (moderate uparea)"),
        Line2D([], [], color=main_colors[2], linewidth=1.8,
               label="Main channel (higher uparea)"),
        Line2D([], [], color=main_colors[3], linewidth=2.4,
               label="Main channel (highest uparea)"),
        Line2D([], [], color=bif_colors[0], linewidth=1.0,
               label="Channel bifurcation (narrower)"),
        Line2D([], [], color=bif_colors[1], linewidth=1.5,
               label="Channel bifurcation (moderate)"),
        Line2D([], [], color=bif_colors[2], linewidth=2.2,
               label="Channel bifurcation (wider)"),
        Line2D([], [], color=bif_colors[3], linewidth=3.0,
               label="Channel bifurcation (widest)"),
        Line2D([], [], color="#777777", linewidth=0.6, label="15-minute grid boundary"),
    ], loc="upper center", bbox_to_anchor=(0.5, -0.22), ncol=3,
        fontsize=6.5, framealpha=0.94, columnspacing=1.15, handlelength=2.0)
    fig.subplots_adjust(bottom=0.28)
    format_geographic_axes(ax, DELTA_EXTENT, geometry)
    fig.colorbar(mesh, ax=ax, shrink=0.8,
                 label="Mapped coarse-grid upstream drainage area (km²)")
    ax.set_title("(d) 1-minute catchments, outlets, and channel bifurcations")
    save_figure(fig, "station04_delta_catchments.png")
    distinct = len(set(zip(subx[subvalid].tolist(), suby[subvalid].tolist())))
    print(f"Panel (d): {int(subvalid.sum())} mapped pixels, {distinct} catchments, "
          f"{int(unassigned_land.sum())} unassigned land pixels, "
          f"{len(catchment_edges)} boundaries, {len(channel_indices)} channel "
          f"bifurcation segments (IDs {bif['pth_id'][channel_indices].tolist()}), "
          f"and {len(visible_segments)} visible nextxy links.")


def main():
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    geometry = read_grid_geometry()
    lon, lat, uparea, nextx, nexty, outlon, outlat, bsncol, bif = read_map_data()
    river, mask, rows, cols, dest_rows, dest_cols, segments, nonlocal_mask, invalid = \
        downstream_links(lon, lat, uparea, nextx, nexty)
    catmx, catmy, catvalid = read_catchment_mapping(geometry, river)
    map_station_pixels(STATIONS, geometry, catmx, catmy, catvalid)
    invalid_bifurcations = validate_outlets_and_bifurcations(
        STATIONS, geometry, catmx, catmy, catvalid, outlon, outlat, bif)

    # Confirm that My Thuan's mapped cell and Can Tho use distinct nextxy reaches.
    my_thuan = next(station for station in STATIONS if station["name"] == "My Thuan")
    can_tho = next(station for station in STATIONS if station["name"] == "Can Tho")
    my_x, my_y = my_thuan["cell"]
    can_x, can_y = can_tho["cell"]
    my_downstream = (int(nextx[my_y - 1, my_x - 1]), int(nexty[my_y - 1, my_x - 1]))
    can_downstream = (int(nextx[can_y - 1, can_x - 1]), int(nexty[can_y - 1, can_x - 1]))
    if my_thuan["cell"] == can_tho["cell"] or my_downstream == can_downstream:
        raise ValueError("My Thuan and Can Tho do not resolve to distinct network reaches")
    bif_sources = np.column_stack((bif["ix"], bif["iy"]))
    bif_destinations = np.column_stack((bif["jx"], bif["jy"]))
    can_tho_paths = np.where(np.all(bif_sources == can_tho["cell"], axis=1))[0]
    my_thuan_incoming = np.where(np.all(bif_destinations == my_thuan["cell"], axis=1))[0]
    if can_tho_paths.size != 2 or my_thuan_incoming.size == 0:
        raise ValueError("selected delta cells do not match their configured bifurcation links")
    print(f"Delta cells: My Thuan {my_thuan['cell']} -> {my_downstream}; Can Tho "
          f"{can_tho['cell']} -> {can_downstream}; bifurcation paths at Can Tho: "
          f"{bif['pth_id'][can_tho_paths].tolist()}; paths entering My Thuan: "
          f"{bif['pth_id'][my_thuan_incoming].tolist()}.")

    # Ensure the displayed NetCDF connectivity matches the distributed Binary map.
    nx, ny = geometry[:2]
    binary_nextxy = np.fromfile(MAP_ROOT / "nextxy_noedge.bin", dtype="<i4")
    if binary_nextxy.size != 2 * nx * ny:
        raise ValueError("nextxy_noedge.bin has an unexpected size")
    if not (np.array_equal(binary_nextxy.reshape(2, ny, nx)[0], nextx) and
            np.array_equal(binary_nextxy.reshape(2, ny, nx)[1], nexty)):
        raise ValueError("NetCDF nextxy_noedge does not match the Binary map")
    binary_uparea = np.fromfile(MAP_ROOT / "uparea.bin", dtype="<f4")
    binary_lonlat = np.fromfile(MAP_ROOT / "lonlat.bin", dtype="<f4")
    if binary_uparea.size != nx * ny or binary_lonlat.size != 2 * nx * ny:
        raise ValueError("uparea.bin or lonlat.bin has an unexpected size")
    if not np.array_equal(binary_uparea.reshape(ny, nx),
                          np.nan_to_num(uparea, nan=-9999.0)):
        raise ValueError("NetCDF uparea does not match the Binary map")
    binary_lonlat = binary_lonlat.reshape(2, ny, nx)
    if not (np.array_equal(binary_lonlat[0], outlon) and
            np.array_equal(binary_lonlat[1], outlat)):
        raise ValueError("NetCDF lonlat does not match the Binary map")

    print(f"Grid: {nx} x {ny}; valid river cells: {int(river.sum())}; "
          f"valid nextxy connections: {len(segments)}; nonlocal connections: "
          f"{int(nonlocal_mask.sum())}; invalid destinations: {invalid}.")
    print(f"Bifurcation endpoints: {len(bif['pth_id'])}; invalid endpoints: "
          f"{invalid_bifurcations}; channel (width layer 1 > 0): "
          f"{int(np.count_nonzero(bif['width'][0, :] > 0))}; overland: "
          f"{int(np.count_nonzero(bif['width'][0, :] <= 0))}.")
    print(f"High-resolution catmxy: {geometry[7]} x {geometry[8]}; mapped pixels: "
          f"{int(catvalid.sum())}; distinct mapped cells: "
          f"{len(set(zip(catmx[catvalid].tolist(), catmy[catvalid].tolist())))}.")

    plot_uparea(lon, lat, uparea, STATIONS, outlon, outlat, geometry)
    plot_grid_network(lon, lat, uparea,
                      (river, mask, rows, cols, dest_rows, dest_cols,
                       segments, nonlocal_mask, invalid),
                      STATIONS, outlon, outlat, geometry)
    plot_outlet_network(lon, lat, uparea,
                        (river, mask, rows, cols, dest_rows, dest_cols,
                         segments, nonlocal_mask, invalid),
                        STATIONS, outlon, outlat, bsncol, bif, geometry)
    plot_delta_catchments(lon, lat, uparea, nextx, nexty, outlon, outlat,
                          geometry, catmx, catmy, catvalid, STATIONS, bif)


if __name__ == "__main__":
    main()
