#!/usr/bin/env python3
"""Plot 2001 annual maximum absolute discharge on grid and river network."""
from datetime import datetime, timedelta
import importlib.util
from pathlib import Path
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.collections import LineCollection
from matplotlib.colors import BoundaryNorm, LinearSegmentedColormap, LogNorm, ListedColormap
from matplotlib.lines import Line2D
from matplotlib.ticker import FuncFormatter
from netCDF4 import Dataset, num2date
import numpy as np

try:
    from cmf_colormap import get_cmap
except ModuleNotFoundError as exc:
    if exc.name != "cmf_colormap":
        raise
    repository_root = Path(__file__).resolve().parents[2]
    relative_script = Path(__file__).resolve().relative_to(repository_root)
    print(
        "[ERROR] Python package 'cmf_colormap' is missing from this environment.\n\n"
        f"Current Python:\n  {sys.executable}\n\n"
        f"CaMa-Flood repository:\n  {repository_root}\n\n"
        "Option 1: Use the recommended CaMa-Flood environment\n\n"
        f"  cd {repository_root}\n"
        "  ./s02-python_setup.sh\n\n"
        "  Then run with the repository-local Python:\n"
        f"  .venv/bin/python {relative_script}\n\n"
        "Option 2: Keep your existing Python environment\n\n"
        f"  cd {repository_root}\n"
        "  python -m pip install --no-deps ./visualization/cmf_colormap\n\n"
        f"  Then rerun with the same Python:\n  python {relative_script}\n\n"
        "Note: --no-deps does not install missing dependencies.\n"
        "No environment changes were made by this diagnostic.",
        file=sys.stderr,
    )
    raise SystemExit(1) from None


# User settings: compare daily-mean discharge for the complete 2001 calendar year.
TUTORIAL_DIR = Path(__file__).resolve().parent
ROOT = Path(__file__).resolve().parents[2]
RUN_DIR = TUTORIAL_DIR / "out"
FIGURE_DIR = TUTORIAL_DIR / "figure"
YEAR = 2001
EXPERIMENTS = {
    "mek_default": "Default",
    "mek_nobif": "No Bifurcation",
    "mek_kine": "Kinematic Wave",
    "mek_nofld": "No Floodplain",
}
DIFFERENCE_PAIRS = (
    ("mek_default", "mek_nobif"),
    ("mek_nobif", "mek_kine"),
    ("mek_kine", "mek_nofld"),
)
DIFFERENCE_IMPACTS = (
    "Impact of bifurcation scheme",
    "Impact of local inertial equation",
    "Impact of floodplain activation",
)
COLOR_MIN = 1.0  # Positive values below this use the colorbar's explicit under-range.
ZERO_COLOR = "#D9D9D9"
UNDER_COLOR = "#4B4B4B"
DIFFERENCE_LEVELS = np.array([
    -10000, -3000, -1000, -300, -100, -30, -10,
    10, 30, 100, 300, 1000, 3000, 10000,
], dtype=float)
ABSOLUTE_TICKS = np.array([
    1, 2, 5, 10, 20, 50, 100, 200, 500, 1000,
    2000, 5000, 10000, 20000, 50000,
], dtype=float)

# Load the same map reader and Figure 1c extent used to construct the network view.
NETWORK_SCRIPT = TUTORIAL_DIR / "v01-network_maps.py"
NETWORK_SPEC = importlib.util.spec_from_file_location("tutorial02_network_maps", NETWORK_SCRIPT)
if NETWORK_SPEC is None or NETWORK_SPEC.loader is None:
    raise RuntimeError(f"Cannot load Figure 1 network helpers: {NETWORK_SCRIPT}")
NETWORK_MAPS = importlib.util.module_from_spec(NETWORK_SPEC)
NETWORK_SPEC.loader.exec_module(NETWORK_MAPS)
ELEVATION_FILE = NETWORK_MAPS.MAP_ROOT / "1min_nc" / "1min.elevtn.nc"
ELEVATION_EXTENT = NETWORK_MAPS.PANEL_C_EXTENT


def represented_daily_intervals(time_variable, path):
    """Validate a complete daily time axis and return its represented dates."""
    dates = num2date(
        time_variable[:], time_variable.units,
        getattr(time_variable, "calendar", "standard"),
        only_use_cftime_datetimes=False,
    )
    expected_count = (datetime(YEAR + 1, 1, 1) - datetime(YEAR, 1, 1)).days
    if len(dates) != expected_count:
        raise ValueError(f"{path}: expected {expected_count} daily records, found {len(dates)}")

    represented = []
    for index, date in enumerate(dates):
        if (date.hour, date.minute, date.second) != (0, 0, 0):
            raise ValueError(f"{path}: record {index + 1} is not stamped at 00:00")
        represented.append(date - timedelta(days=1))
    expected = [datetime(YEAR, 1, 1) + timedelta(days=index)
                for index in range(expected_count)]
    if [(d.year, d.month, d.day) for d in represented] != [
            (d.year, d.month, d.day) for d in expected]:
        raise ValueError(f"{path}: time coordinate does not cover every day of {YEAR}")
    return time_variable[:].copy()


def annual_max_abs_grid(path, expected_lon, expected_lat):
    """Read daily-mean outflw and return cellwise max_t |Q(t)| for 2001."""
    with Dataset(path) as data:
        variable = data.variables["outflw"]
        if variable.dimensions != ("time", "lat", "lon"):
            raise ValueError(f"{path}: unexpected outflw dimensions {variable.dimensions}")
        if getattr(variable, "units", "") != "m3/s":
            raise ValueError(f"{path}: outflw units must be m3/s")
        if getattr(variable, "cell_methods", "") != "time: mean":
            raise ValueError(f"{path}: outflw is not documented as a time mean")
        times = represented_daily_intervals(data.variables["time"], path)
        lon = np.asarray(data.variables["lon"][:])
        lat = np.asarray(data.variables["lat"][:])
        if not np.array_equal(lon, expected_lon) or not np.array_equal(lat, expected_lat):
            raise ValueError(f"{path}: output coordinates differ from the map")
        flow = np.ma.masked_invalid(np.ma.asarray(variable[:], dtype=float))
        maximum = np.ma.max(np.ma.abs(flow), axis=0)
        return np.asarray(np.ma.filled(maximum, np.nan)), times


def annual_max_abs_paths(path, bif, expected_times):
    """Read channel-layer pthflw and match each record to bifprm by pth_id."""
    with Dataset(path) as data:
        variable = data.variables["pthflw"]
        path_ids_variable = data.variables["pth_id"]
        path_dim = path_ids_variable.dimensions[0]
        layer_dims = [dimension for dimension in variable.dimensions
                      if dimension not in ("time", path_dim)]
        if len(variable.dimensions) != 3 or len(layer_dims) != 1:
            raise ValueError(f"{path}: unexpected pthflw dimensions {variable.dimensions}")
        layer_dim = layer_dims[0]
        if variable.dimensions != ("time", layer_dim, path_dim):
            raise ValueError(f"{path}: cannot identify time/layer/path order in {variable.dimensions}")
        if getattr(variable, "units", "") != "m3/s":
            raise ValueError(f"{path}: pthflw units must be m3/s")
        if getattr(variable, "cell_methods", "") != "time: mean":
            raise ValueError(f"{path}: pthflw is not documented as a time mean")
        times = represented_daily_intervals(data.variables["time"], path)
        if not np.array_equal(times, expected_times):
            raise ValueError(f"{path}: pthflw and outflw time coordinates are not aligned")

        path_ids = np.asarray(path_ids_variable[:], dtype=int)
        if path_ids.ndim != 1 or len(np.unique(path_ids)) != len(path_ids):
            raise ValueError(f"{path}: pth_id values are not unique one-dimensional IDs")
        map_ids = np.asarray(bif["pth_id"], dtype=int)
        if set(path_ids.tolist()) != set(map_ids.tolist()):
            raise ValueError(f"{path}: pth_id values do not match map bifprm.nc")

        for name in ("ix", "iy", "jx", "jy"):
            output_endpoint = np.asarray(data.variables[name][:], dtype=int)
            map_endpoint = np.asarray(bif[name], dtype=int)
            map_by_id = {int(path_id): int(value)
                         for path_id, value in zip(map_ids, map_endpoint)}
            if any(map_by_id[int(path_id)] != int(value)
                   for path_id, value in zip(path_ids, output_endpoint)):
                raise ValueError(f"{path}: {name} endpoints do not match map bifprm.nc by pth_id")

        flow = np.ma.masked_invalid(np.ma.asarray(variable[:, 0, :], dtype=float))
        channel_by_id = {
            int(path_id): bool(width > 0)
            for path_id, width in zip(map_ids, np.asarray(bif["width"])[0, :])
        }
        channel = np.asarray([channel_by_id[int(path_id)] for path_id in path_ids])
        if np.any(np.ma.getmaskarray(flow[:, channel])):
            raise ValueError(f"{path}: channel-layer pthflw has missing records; no fill value is defined")
        maximum = np.ma.max(np.ma.abs(flow), axis=0)
        return path_ids, np.asarray(np.ma.filled(maximum, np.nan)), channel


def colored_segments(ax, segments, values, cmap, norm, zero_color,
                     linewidth, linestyle="solid", zorder=2):
    """Draw positive magnitudes with the common scale and exact zeros in gray."""
    finite = np.isfinite(values)
    zero = finite & (values == 0)
    positive = finite & (values > 0)
    if np.any(zero):
        ax.add_collection(LineCollection(
            segments[zero], colors=zero_color, linewidths=linewidth,
            linestyles=linestyle, zorder=zorder))
    if np.any(positive):
        ax.add_collection(LineCollection(
            segments[positive], colors=cmap(norm(values[positive])),
            linewidths=linewidth, linestyles=linestyle, zorder=zorder))
    return values[positive]


def read_elevation_background(path, extent):
    """Read and validate a cropped 1-minute elevation raster for map context."""
    with Dataset(path) as data:
        variable = data.variables["elevtn"]
        if variable.dimensions != ("lat", "lon"):
            raise ValueError(f"{path}: unexpected elevtn dimensions {variable.dimensions}")
        if getattr(variable, "units", "") != "m":
            raise ValueError(f"{path}: elevtn units must be metres")

        lon = np.asarray(data.variables["lon"][:])
        lat = np.asarray(data.variables["lat"][:])
        if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
            raise ValueError(f"{path}: expected longitude increasing and latitude decreasing")
        west, east, south, north = extent
        col = np.flatnonzero((lon >= west) & (lon <= east))
        row = np.flatnonzero((lat >= south) & (lat <= north))
        if not len(col) or not len(row):
            raise ValueError(f"{path}: no elevation pixels intersect {extent}")

        col_slice = slice(col[0], col[-1] + 1)
        row_slice = slice(row[0], row[-1] + 1)
        elevation = np.ma.masked_invalid(np.ma.asarray(variable[row_slice, col_slice], dtype=float))
        subset_lon = lon[col_slice]
        subset_lat = lat[row_slice]
        dx = float(np.median(np.diff(lon)))
        dy = abs(float(np.median(np.diff(lat))))
        image_extent = (
            subset_lon[0] - dx / 2,
            subset_lon[-1] + dx / 2,
            subset_lat[-1] - dy / 2,
            subset_lat[0] + dy / 2,
        )

    valid = np.asarray(elevation.compressed())
    if not valid.size:
        raise ValueError(f"{path}: cropped elevtn contains no valid pixels")
    low, high = np.percentile(valid, (2, 98))
    if high <= low:
        raise ValueError(f"{path}: cropped elevtn has no usable elevation range")
    # log1p spreads the low-elevation range so lowlands remain visible beside
    # much higher terrain, while keeping zero elevation well-defined.
    display_elevation = np.log1p(np.ma.maximum(elevation, 0))
    display_min, display_max = np.log1p((low, high))
    return (display_elevation, image_extent, float(display_min), float(display_max),
            float(low), float(high))


def main():
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)

    (lon, lat, uparea, nextx, nexty, outlon, outlat, _bsncol,
     bif) = NETWORK_MAPS.read_map_data()
    river_cells = np.isfinite(uparea) & (uparea > 0)
    links = NETWORK_MAPS.downstream_links(lon, lat, uparea, nextx, nexty)
    _, _, source_rows, source_cols, dest_rows, dest_cols, _, _, invalid = links
    if invalid:
        raise ValueError(f"The Figure 1c network has {invalid} out-of-map links")
    main_segments = np.stack((
        np.column_stack((outlon[source_rows, source_cols], outlat[source_rows, source_cols])),
        np.column_stack((outlon[dest_rows, dest_cols], outlat[dest_rows, dest_cols])),
    ), axis=1)
    if not np.all(np.isfinite(main_segments)) or np.any(np.abs(main_segments) > 360):
        raise ValueError("Figure 1c main-network outlet coordinates are invalid")
    # Draw small contributing areas first so downstream main channels remain visible.
    main_segment_order = np.argsort(uparea[source_rows, source_cols], kind="stable")
    main_segments_by_area = main_segments[main_segment_order]

    annual_grids = {}
    annual_times = {}
    all_positive = []
    for experiment in EXPERIMENTS:
        path = RUN_DIR / experiment / f"o_outflw{YEAR}.nc"
        maximum, times = annual_max_abs_grid(path, lon, lat)
        annual_grids[experiment] = maximum
        annual_times[experiment] = times
        valid = river_cells & np.isfinite(maximum)
        if not np.any(valid):
            raise ValueError(f"{path}: no valid river-cell outflw values")
        all_positive.extend(maximum[valid & (maximum > 0)].tolist())

    reference_times = annual_times["mek_default"]
    for experiment, times in annual_times.items():
        if not np.array_equal(times, reference_times):
            raise ValueError(f"{experiment}: 2001 daily records are not aligned with Default")

    default_path = RUN_DIR / "mek_default" / f"o_pthflw{YEAR}.nc"
    path_ids, path_maximum, channel_paths = annual_max_abs_paths(
        default_path, bif, reference_times)
    if not np.any(channel_paths):
        raise ValueError(f"{default_path}: no channel bifurcation paths are available")

    map_id_index = {int(path_id): index
                    for index, path_id in enumerate(np.asarray(bif["pth_id"], dtype=int))}
    bif_source_lon = np.asarray([
        outlon[int(bif["iy"][map_id_index[int(path_id)]]) - 1,
               int(bif["ix"][map_id_index[int(path_id)]]) - 1]
        for path_id in path_ids
    ])
    bif_source_lat = np.asarray([
        outlat[int(bif["iy"][map_id_index[int(path_id)]]) - 1,
               int(bif["ix"][map_id_index[int(path_id)]]) - 1]
        for path_id in path_ids
    ])
    bif_dest_lon = np.asarray([
        outlon[int(bif["jy"][map_id_index[int(path_id)]]) - 1,
               int(bif["jx"][map_id_index[int(path_id)]]) - 1]
        for path_id in path_ids
    ])
    bif_dest_lat = np.asarray([
        outlat[int(bif["jy"][map_id_index[int(path_id)]]) - 1,
               int(bif["jx"][map_id_index[int(path_id)]]) - 1]
        for path_id in path_ids
    ])
    bif_segments = np.stack((
        np.column_stack((bif_source_lon, bif_source_lat)),
        np.column_stack((bif_dest_lon, bif_dest_lat)),
    ), axis=1)
    if not np.all(np.isfinite(bif_segments)) or np.any(np.abs(bif_segments) > 360):
        raise ValueError("Bifurcation-path outlet coordinates are invalid")

    (elevation, elevation_image_extent, elevation_min, elevation_max,
     elevation_raw_min, elevation_raw_max) = read_elevation_background(
        ELEVATION_FILE, ELEVATION_EXTENT)

    main_maxima = {}
    for experiment, maximum in annual_grids.items():
        values = maximum[source_rows, source_cols]
        main_maxima[experiment] = values
        all_positive.extend(values[np.isfinite(values) & (values > 0)].tolist())
    all_positive.extend(path_maximum[channel_paths & np.isfinite(path_maximum) &
                                    (path_maximum > 0)].tolist())
    if not all_positive:
        raise ValueError("No positive 2001 discharge magnitudes are available to plot")

    vmax = float(np.max(all_positive))
    if vmax <= COLOR_MIN:
        raise ValueError(f"The shared maximum {vmax:g} m3/s does not exceed vmin={COLOR_MIN:g}")
    norm = LogNorm(vmin=COLOR_MIN, vmax=vmax, clip=False)
    cmap = get_cmap("shade_rainbow").copy()
    cmap.set_under(UNDER_COLOR)
    cmap.set_bad("white")

    differences = {
        (left, right): main_maxima[left] - main_maxima[right]
        for left, right in DIFFERENCE_PAIRS
    }
    difference_max = max(float(np.nanmax(np.abs(values)))
                         for values in differences.values())
    if not np.isfinite(difference_max) or difference_max <= 0:
        raise ValueError("No nonzero main-channel differences from Default are available")
    # A 256-entry copy keeps BoundaryNorm's integer color indices in range.
    difference_cmap = get_cmap("diverge_rainbow").resampled(256)
    difference_cmap.set_bad("white")
    difference_norm = BoundaryNorm(
        DIFFERENCE_LEVELS, ncolors=difference_cmap.N, clip=False)

    elevation_cmap = LinearSegmentedColormap.from_list(
        "tutorial02_soft_elevation", ["#4D4D4D", "#A0A0A0"])
    elevation_cmap.set_bad((1, 1, 1, 0))

    fig, axes = plt.subplots(3, 4, figsize=(20, 14.5))
    for column, (experiment, label) in enumerate(EXPERIMENTS.items()):
        raster_ax = axes[0, column]
        maximum = annual_grids[experiment]
        raster_valid = river_cells & np.isfinite(maximum)
        zero = raster_valid & (maximum == 0)
        if np.any(zero):
            raster_ax.pcolormesh(
                lon, lat, np.ma.masked_where(~zero, np.ones(maximum.shape)),
                shading="nearest", cmap=ListedColormap([ZERO_COLOR]),
                vmin=0.5, vmax=1.5, zorder=0)
        raster_values = np.ma.masked_where(~raster_valid | (maximum <= 0), maximum)
        raster_ax.pcolormesh(lon, lat, raster_values, shading="nearest",
                             cmap=cmap, norm=norm, zorder=1)
        raster_ax.set_title(label)
        raster_ax.set_xlim(98, 109.5)
        raster_ax.set_ylim(8.5, 22)
        raster_ax.set_aspect(1 / np.cos(np.deg2rad(11.5)))
        raster_ax.set_xlabel("Longitude (°E)")
        if column == 0:
            raster_ax.set_ylabel("Latitude (°N)")

        network_ax = axes[1, column]
        network_ax.set_facecolor("#FAFAFA")
        network_ax.imshow(
            elevation, extent=elevation_image_extent, origin="upper",
            cmap=elevation_cmap, vmin=elevation_min, vmax=elevation_max,
            interpolation="nearest", alpha=0.45, zorder=0,
        )
        if experiment == "mek_default":
            channel_positive = channel_paths & np.isfinite(path_maximum)
            bif_positive = colored_segments(
                network_ax, bif_segments[channel_positive],
                path_maximum[channel_positive], cmap, norm, ZERO_COLOR,
                linewidth=1.8, linestyle=(0, (2.5, 1.5)), zorder=2)
        else:
            bif_positive = np.empty(0)
        main_positive = colored_segments(
            network_ax, main_segments_by_area,
            main_maxima[experiment][main_segment_order], cmap, norm,
            ZERO_COLOR, linewidth=1.5, zorder=3)
        network_ax.set_xlim(*NETWORK_MAPS.PANEL_C_EXTENT[:2])
        network_ax.set_ylim(*NETWORK_MAPS.PANEL_C_EXTENT[2:])
        network_ax.set_xlabel("Longitude (°E)")
        if column == 0:
            network_ax.set_ylabel("Latitude (°N)")
        network_ax.set_aspect(1 / np.cos(np.deg2rad(11.5)))

        axes[2, column].set_axis_off()

        print(f"{experiment}: {np.count_nonzero(np.isfinite(main_maxima[experiment]))} "
              f"main connections; {main_positive.size} positive; "
              f"{int(np.count_nonzero(np.isfinite(main_maxima[experiment]) & (main_maxima[experiment] == 0)))} zero; "
              f"{bif_positive.size} positive channel bifurcation paths")

    for panel, (left, right) in enumerate(DIFFERENCE_PAIRS):
        difference_ax = axes[2, panel]
        difference_ax.set_axis_on()
        difference_ax.set_facecolor("#FAFAFA")
        difference_ax.imshow(
            elevation, extent=elevation_image_extent, origin="upper",
            cmap=elevation_cmap, vmin=elevation_min, vmax=elevation_max,
            interpolation="nearest", alpha=0.45, zorder=0,
        )
        values = differences[(left, right)][main_segment_order]
        finite = np.isfinite(values)
        if np.any(finite):
            difference_ax.add_collection(LineCollection(
                main_segments_by_area[finite], array=values[finite],
                cmap=difference_cmap, norm=difference_norm,
                linewidths=1.2, zorder=3,
            ))
        # Default's bifurcation paths are positional references only.
        if np.any(channel_paths):
            difference_ax.add_collection(LineCollection(
                bif_segments[channel_paths], colors="#666666",
                linewidths=0.45, linestyles=(0, (2.2, 1.8)),
                alpha=0.70, zorder=2,
            ))
        left_label = EXPERIMENTS[left]
        right_label = EXPERIMENTS[right]
        difference_ax.set_title(
            f"{DIFFERENCE_IMPACTS[panel]}\n{left_label} − {right_label}",
            fontsize=9,
        )
        difference_ax.set_xlim(*NETWORK_MAPS.PANEL_C_EXTENT[:2])
        difference_ax.set_ylim(*NETWORK_MAPS.PANEL_C_EXTENT[2:])
        difference_ax.set_xlabel("Longitude (°E)")
        if panel == 0:
            difference_ax.set_ylabel("Latitude (°N)")
        difference_ax.set_aspect(1 / np.cos(np.deg2rad(11.5)))

    fig.suptitle("Mekong annual maximum absolute daily-mean discharge (2001)", y=0.98)
    fig.text(0.015, 0.82, "Computational-grid raster", rotation=90,
             va="center", ha="center", fontsize=10)
    fig.text(0.015, 0.50, "Geographic river network", rotation=90,
             va="center", ha="center", fontsize=10)
    fig.text(0.015, 0.19, "Sequential scheme difference", rotation=90,
             va="center", ha="center", fontsize=10)

    handles = [
        Line2D([], [], color="#333333", linewidth=1.0,
               label="Main-channel connection (all experiments)"),
        Line2D([], [], color="#333333", linewidth=1.0,
               linestyle=(0, (2.5, 1.5)),
               label="Channel bifurcation (Default only)"),
    ]
    fig.legend(handles=handles, loc="lower center", ncol=2,
               bbox_to_anchor=(0.49, 0.040), frameon=False, fontsize=8)
    fig.text(
        0.5, 0.005,
        "Absolute rows show max_t |daily-mean Q|; maxima need not be simultaneous. "
        "Difference row compares adjacent configurations: Default − No Bifurcation, "
        "No Bifurcation − Kinematic Wave, and Kinematic Wave − No Floodplain; "
        "values beyond ±10,000 "
        "m³/s use the end colors; "
        "gray dashed bifurcations are positional references only.",
        ha="center", va="bottom", fontsize=8,
    )

    fig.subplots_adjust(left=0.055, right=0.90, top=0.94, bottom=0.13,
                        wspace=0.08, hspace=0.25)
    # Center each difference map between the two configuration panels it compares.
    for panel in range(len(DIFFERENCE_PAIRS)):
        left_box = axes[1, panel].get_position()
        right_box = axes[1, panel + 1].get_position()
        difference_box = axes[2, panel].get_position()
        center = (left_box.x1 + right_box.x0) / 2
        width = left_box.width * 0.88
        axes[2, panel].set_position([
            center - width / 2, difference_box.y0, width, difference_box.height,
        ])
    axes[2, 3].set_axis_off()
    row_colorbars = [
        ("Annual maximum absolute daily-mean discharge (m³/s)",
         ScalarMappable(norm=norm, cmap=cmap), "min"),
        ("Annual maximum absolute daily-mean discharge (m³/s)",
         ScalarMappable(norm=norm, cmap=cmap), "min"),
        ("Difference in annual maximum absolute discharge (m³/s)",
         ScalarMappable(norm=difference_norm, cmap=difference_cmap), "both"),
    ]
    fig.canvas.draw()
    for row, (label, mappable, extend) in enumerate(row_colorbars):
        bounds = axes[row, 1].get_position()
        color_ax = fig.add_axes([
            0.925, bounds.y0 + 0.08 * bounds.height,
            0.014, 0.84 * bounds.height,
        ])
        colorbar = fig.colorbar(mappable, cax=color_ax, extend=extend)
        if row == 2:
            colorbar.set_ticks(DIFFERENCE_LEVELS)
        else:
            ticks = ABSOLUTE_TICKS[ABSOLUTE_TICKS < vmax]
            colorbar.set_ticks(ticks)
            colorbar.ax.yaxis.set_major_formatter(
                FuncFormatter(lambda value, _position: f"{value:,.0f}"))
        colorbar.set_label(label, fontsize=8)
        colorbar.ax.tick_params(labelsize=7)

    output = FIGURE_DIR / "f04-annual_max_discharge.png"
    fig.savefig(output, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"Daily mean time axis: {len(reference_times)} complete records for {YEAR}; "
          f"outflw and pthflw are aligned.")
    print(f"Shared absolute logarithmic scale: positive values below {COLOR_MIN:g} m3/s "
          f"use the under-range; vmax={vmax:.3g} m3/s.")
    print(f"Difference scale: symmetric log-like bins from {DIFFERENCE_LEVELS[0]:g} to "
          f"{DIFFERENCE_LEVELS[-1]:g} m3/s (values outside use end colors); "
          f"observed range={-difference_max:.3g} to {difference_max:.3g} m3/s; "
          f"1-minute elevation uses log1p after clipping at "
          f"the 2nd/98th percentiles ({elevation_raw_min:.3g}, "
          f"{elevation_raw_max:.3g} m).")
    print(f"Wrote {output}")


if __name__ == "__main__":
    main()
