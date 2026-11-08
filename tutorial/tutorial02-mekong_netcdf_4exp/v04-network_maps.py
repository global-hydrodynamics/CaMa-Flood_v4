#!/usr/bin/env python3
"""Create four map figures explaining the Tutorial 02 river network."""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import LogNorm
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch
from netCDF4 import Dataset
import numpy as np


# User settings: map extents and output resolution for the four figures.
ROOT = Path(__file__).resolve().parents[2]
MAP_ROOT = ROOT / "map" / "mekong_15min"
NETCDF_DIR = MAP_ROOT / "netcdf"
HIRES_DIR = MAP_ROOT / "1min"
FIGURE_DIR = Path(__file__).resolve().parent / "figure"
BASIN_EXTENT = (99.5, 108.2, 8.5, 18.5)
DELTA_EXTENT = (105.55, 106.72, 9.48, 10.58)
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
    with Dataset(NETCDF_DIR / "bifprm.nc") as data:
        bif = {name: np.asarray(data.variables[name][:]) for name in
               ("pth_id", "ix", "iy", "jx", "jy", "width")}

    shape = (len(lat), len(lon))
    if uparea.shape != shape or nextx.shape != shape or nexty.shape != shape:
        raise ValueError("uparea and nextxy arrays do not match the coordinate grid")
    if outlon.shape != shape or outlat.shape != shape:
        raise ValueError("lonlat outlet arrays do not match the coordinate grid")
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("expected longitude increasing and latitude north-to-south")

    return lon, lat, uparea, nextx, nexty, outlon, outlat, bif


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


def add_station_markers(ax, stations, lon, lat, outlon, outlat, labels=True):
    """Show station references, coarse grid centers, and mapped outlets distinctly."""
    for station in stations:
        ix, iy = station["cell"]
        ax.scatter(*station["station"], marker="x", color="#a00065", s=27,
                   linewidths=1.2, zorder=8)
        ax.scatter(lon[ix - 1], lat[iy - 1], marker="o", facecolors="white",
                   edgecolors="#222222", s=28, linewidths=0.8, zorder=7)
        ax.scatter(outlon[iy - 1, ix - 1], outlat[iy - 1, ix - 1], marker="s",
                   facecolors="#ffcc55", edgecolors="#222222", s=25,
                   linewidths=0.7, zorder=7)
        if labels:
            ax.annotate(station["name"], station["station"],
                        xytext=LABEL_OFFSETS[station["name"]],
                        textcoords="offset points", fontsize=8, zorder=9)


def add_common_legend(ax, include_network=False):
    """Add compact station-coordinate symbol keys to Figures 1–3."""
    handles = [
        Line2D([], [], color="#a00065", marker="x", linestyle="None",
               label="Station coordinate"),
        Line2D([], [], marker="o", markerfacecolor="white", markeredgecolor="#222222",
               linestyle="None", label="15-minute grid-cell center"),
        Line2D([], [], marker="s", markerfacecolor="#ffcc55", markeredgecolor="#222222",
               linestyle="None", label="Unit-catchment outlet"),
    ]
    if include_network:
        handles.extend([
            Line2D([], [], color="#647d91", linewidth=1.2, label="Main channel"),
            Line2D([], [], color="#f28e2b", linewidth=1.5,
                   label="Flexible link (>8-neighbor)"),
        ])
    ax.legend(handles=handles, loc="lower left", fontsize=7, framealpha=0.9)


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
    fig, ax = plt.subplots(figsize=(8.1, 8.4))
    west, east, south, north = geometry[3:7]
    image = ax.imshow(data, origin="upper", extent=(west, east, south, north),
                      cmap="YlGnBu", norm=LogNorm(vmin=float(data.min()), vmax=float(data.max())),
                      interpolation="nearest", alpha=0.95)
    add_station_markers(ax, stations, lon, lat, outlon, outlat)
    add_common_legend(ax)
    format_geographic_axes(ax, BASIN_EXTENT, geometry)
    fig.colorbar(image, ax=ax, shrink=0.72, label="Upstream drainage area (km²)")
    ax.set_title("(a) Upstream drainage area on the 15-minute grid")
    save_figure(fig, "station01_uparea.png")


def plot_grid_network(lon, lat, uparea, links, stations, outlon, outlat, geometry):
    """Panel (b): show nextxy links between regular computational cell centers."""
    _, _, rows, cols, dest_rows, dest_cols, segments, nonlocal_mask, _ = links
    source_area = uparea[rows, cols] / 1e6
    ordinary = segments[~nonlocal_mask]
    flexible = segments[nonlocal_mask]
    fig, ax = plt.subplots(figsize=(8.1, 8.4))
    west, east, south, north = geometry[3:7]
    image = ax.imshow(np.ma.masked_where(~np.isfinite(uparea) | (uparea <= 0), uparea / 1e6),
                      origin="upper", extent=(west, east, south, north), cmap="YlGnBu",
                      norm=LogNorm(vmin=float(np.nanmin(uparea[uparea > 0]) / 1e6),
                                   vmax=float(np.nanmax(uparea) / 1e6)),
                      interpolation="nearest", alpha=0.17)
    collection = LineCollection(ordinary, cmap="Blues", norm=LogNorm(
        vmin=float(source_area.min()), vmax=float(source_area.max())),
        linewidths=0.28, alpha=0.62, zorder=3)
    collection.set_array(source_area[~nonlocal_mask])
    ax.add_collection(collection)
    ax.add_collection(LineCollection(flexible, colors="#f28e2b", linewidths=1.05,
                                     alpha=0.95, zorder=5))
    add_station_markers(ax, stations, lon, lat, outlon, outlat)
    add_common_legend(ax, include_network=True)
    format_geographic_axes(ax, BASIN_EXTENT, geometry)
    ax.set_title("(b) Explicit downstream links between grid-cell centers")
    save_figure(fig, "station02_grid_network.png")


def plot_outlet_network(lon, lat, uparea, links, stations, outlon, outlat, bif, geometry):
    """Panel (c): show the geographic outlet network and bifurcation pathways."""
    _, _, rows, cols, dest_rows, dest_cols, _, _, _ = links
    main_segments = np.stack(
        (np.column_stack((outlon[rows, cols], outlat[rows, cols])),
         np.column_stack((outlon[dest_rows, dest_cols], outlat[dest_rows, dest_cols]))), axis=1)
    bif_ix, bif_iy, bif_jx, bif_jy = (bif[name].astype(int) for name in ("ix", "iy", "jx", "jy"))
    start = np.column_stack((outlon[bif_iy - 1, bif_ix - 1], outlat[bif_iy - 1, bif_ix - 1]))
    end = np.column_stack((outlon[bif_jy - 1, bif_jx - 1], outlat[bif_jy - 1, bif_jx - 1]))
    bif_segments = np.stack((start, end), axis=1)
    channel = bif["width"][0, :] > 0
    overland = ~channel

    fig, ax = plt.subplots(figsize=(8.1, 8.4))
    west, east, south, north = geometry[3:7]
    grid_lon = np.arange(west, east + geometry[2] / 2, geometry[2])
    grid_lat = np.arange(south, north + geometry[2] / 2, geometry[2])
    ax.vlines(grid_lon, south, north, colors="#777777", linewidth=0.32, alpha=0.42, zorder=1)
    ax.hlines(grid_lat, west, east, colors="#777777", linewidth=0.32, alpha=0.42, zorder=1)
    ax.add_collection(LineCollection(main_segments, colors="#2459a6", linewidths=0.55,
                                     alpha=0.62, zorder=2))
    ax.add_collection(LineCollection(bif_segments[overland], colors="#76b77b",
                                     linewidths=0.65, alpha=0.78, zorder=3))
    ax.add_collection(LineCollection(bif_segments[channel], colors="#cf3030",
                                     linewidths=1.15, alpha=0.95, zorder=4))
    add_station_markers(ax, stations, lon, lat, outlon, outlat)
    handles = [
        Line2D([], [], color="#2459a6", label="Main channel (nextxy)"),
        Line2D([], [], color="#cf3030", linewidth=1.5, label="Channel bifurcation"),
        Line2D([], [], color="#76b77b", label="Overland bifurcation"),
        Line2D([], [], color="#777777", linewidth=0.5, label="15-minute grid boundary"),
        Line2D([], [], color="#a00065", marker="x", linestyle="None", label="Station coordinate"),
        Line2D([], [], marker="o", markerfacecolor="white", markeredgecolor="#222222",
               linestyle="None", label="Grid-cell center"),
        Line2D([], [], marker="s", markerfacecolor="#ffcc55", markeredgecolor="#222222",
               linestyle="None", label="Unit-catchment outlet"),
    ]
    ax.legend(handles=handles, loc="lower left", fontsize=7, framealpha=0.92)
    format_geographic_axes(ax, BASIN_EXTENT, geometry)
    ax.set_title("(c) Geographic outlet network on the computational grid")
    save_figure(fig, "station03_outlet_bifurcation.png")
    print(f"Panel (c): {len(main_segments)} main links; {int(channel.sum())} channel and "
          f"{int(overland.sum())} overland bifurcation paths; endpoint-only bifurcation geometry.")


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


def plot_delta_catchments(lon, lat, uparea, nextx, nexty, outlon, outlat,
                          geometry, catmx, catmy, valid, stations):
    """Panel (d): map high-resolution pixels to catchments and their outlets."""
    (_, _, _, _, _, _, _, nxhi, nyhi, hires_resolution,
     hires_west, _, _, hires_north) = geometry
    xmin, xmax, ymin, ymax = DELTA_EXTENT
    col0 = max(0, int((xmin - hires_west) / hires_resolution) - 1)
    col1 = min(nxhi, int(np.ceil((xmax - hires_west) / hires_resolution)) + 1)
    row0 = max(0, int((hires_north - ymax) / hires_resolution) - 1)
    row1 = min(nyhi, int(np.ceil((hires_north - ymin) / hires_resolution)) + 1)
    subx, suby = catmx[row0:row1, col0:col1], catmy[row0:row1, col0:col1]
    subvalid = valid[row0:row1, col0:col1]
    catchment_area = np.full(subvalid.shape, np.nan, dtype=float)
    rows, cols = np.where(subvalid)
    catchment_area[rows, cols] = uparea[suby[rows, cols] - 1, subx[rows, cols] - 1] / 1e6

    x_edges = hires_west + np.arange(col0, col1 + 1) * hires_resolution
    y_edges = hires_north - np.arange(row0, row1 + 1) * hires_resolution
    segments = boundary_segments(subx, suby, subvalid, x_edges, y_edges)
    fig, ax = plt.subplots(figsize=(9.0, 7.7))
    mesh = ax.pcolormesh(x_edges, y_edges, np.ma.masked_invalid(catchment_area),
                         cmap="YlGn", norm=LogNorm(vmin=float(np.nanmin(catchment_area)),
                                                     vmax=float(np.nanmax(catchment_area)),
                                                     clip=True), shading="flat", zorder=1)
    ax.add_collection(LineCollection(segments, colors="black", linewidths=0.5,
                                     alpha=0.9, zorder=3))

    # Show coarse grid boundaries to connect the pixel catchments to model cells.
    west, east, south, north = geometry[3:7]
    grid_lon = np.arange(west, east + geometry[2] / 2, geometry[2])
    grid_lat = np.arange(south, north + geometry[2] / 2, geometry[2])
    ax.vlines(grid_lon, ymin, ymax, colors="#777777", linewidth=0.45,
              alpha=0.45, zorder=2)
    ax.hlines(grid_lat, xmin, xmax, colors="#777777", linewidth=0.45,
              alpha=0.45, zorder=2)

    river = np.isfinite(uparea) & (uparea > 0)
    rows, cols = np.where(river)
    valid_outlet = ((outlon[rows, cols] > xmin) & (outlon[rows, cols] < xmax) &
                    (outlat[rows, cols] > ymin) & (outlat[rows, cols] < ymax))
    outlet_rows, outlet_cols = rows[valid_outlet], cols[valid_outlet]
    ax.scatter(outlon[outlet_rows, outlet_cols], outlat[outlet_rows, outlet_cols],
               marker="s", s=17, facecolors="white", edgecolors="#263238",
               linewidths=0.45, zorder=5)

    river = np.isfinite(uparea) & (uparea > 0)
    link_mask = (river & (nextx > 0) & (nexty > 0) &
                 (nextx <= len(lon)) & (nexty <= len(lat)))
    link_rows, link_cols = np.where(link_mask)
    link_dest_cols = nextx[link_mask] - 1
    link_dest_rows = nexty[link_mask] - 1
    inside_source = ((outlon[link_rows, link_cols] >= xmin) &
                     (outlon[link_rows, link_cols] <= xmax) &
                     (outlat[link_rows, link_cols] >= ymin) &
                     (outlat[link_rows, link_cols] <= ymax))
    inside_dest = ((outlon[link_dest_rows, link_dest_cols] >= xmin) &
                   (outlon[link_dest_rows, link_dest_cols] <= xmax) &
                   (outlat[link_dest_rows, link_dest_cols] >= ymin) &
                   (outlat[link_dest_rows, link_dest_cols] <= ymax))
    visible = inside_source | inside_dest
    for row, col, dest_row, dest_col in zip(
            link_rows[visible], link_cols[visible],
            link_dest_rows[visible], link_dest_cols[visible]):
        start = (outlon[row, col], outlat[row, col])
        end = (outlon[dest_row, dest_col], outlat[dest_row, dest_col])
        ax.add_patch(FancyArrowPatch(start, end, arrowstyle="-|>", mutation_scale=6.5,
                                     color="#2459a6", linewidth=0.75, alpha=0.8,
                                     zorder=4, shrinkA=1, shrinkB=1))

    for station in stations:
        if not (xmin <= station["station"][0] <= xmax and
                ymin <= station["station"][1] <= ymax):
            continue
        ix, iy = station["cell"]
        ax.scatter(*station["station"], marker="x", color="#a00065", s=34,
                   linewidths=1.3, zorder=8)
        ax.scatter(lon[ix - 1], lat[iy - 1], marker="o", facecolors="none",
                   edgecolors="#111111", s=28, linewidths=0.9, zorder=7)
        ax.scatter(outlon[iy - 1, ix - 1], outlat[iy - 1, ix - 1], marker="s",
                   facecolors="#ffcc55", edgecolors="#111111", s=31,
                   linewidths=0.8, zorder=8)
        ax.annotate(station["name"], station["station"],
                    xytext=LABEL_OFFSETS[station["name"]],
                    textcoords="offset points", fontsize=8, zorder=9)

    ax.legend(handles=[
        Line2D([], [], color="black", linewidth=0.8, label="Unit-catchment boundary"),
        Line2D([], [], color="#2459a6", marker=r"$\rightarrow$", linewidth=0.8,
               label="Downstream outlet link (nextxy)"),
        Line2D([], [], marker="s", markerfacecolor="white", markeredgecolor="#263238",
               linestyle="None", label="Unit-catchment outlet"),
        Line2D([], [], marker="x", color="#a00065", linestyle="None",
               label="Station coordinate"),
        Line2D([], [], marker="o", markerfacecolor="none", markeredgecolor="#111111",
               linestyle="None", label="Model-grid center"),
    ], loc="lower left", fontsize=7, framealpha=0.92)
    format_geographic_axes(ax, DELTA_EXTENT, geometry)
    fig.colorbar(mesh, ax=ax, shrink=0.82,
                 label="Mapped coarse-grid upstream drainage area (km²)")
    ax.set_title("(d) 1-minute pixels assigned to 15-minute unit catchments")
    save_figure(fig, "station04_delta_catchments.png")
    distinct = len(set(zip(subx[subvalid].tolist(), suby[subvalid].tolist())))
    print(f"Panel (d): {int(subvalid.sum())} mapped pixels, {distinct} catchments, "
          f"{len(segments)} categorical boundaries, {len(outlet_rows)} outlet points "
          f"and {int(visible.sum())} downstream links in view.")


def main():
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    geometry = read_grid_geometry()
    lon, lat, uparea, nextx, nexty, outlon, outlat, bif = read_map_data()
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
                        STATIONS, outlon, outlat, bif, geometry)
    plot_delta_catchments(lon, lat, uparea, nextx, nexty, outlon, outlat,
                          geometry, catmx, catmy, catvalid, STATIONS)


if __name__ == "__main__":
    main()
