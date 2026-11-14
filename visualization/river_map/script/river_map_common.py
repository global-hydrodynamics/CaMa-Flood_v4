"""Shared map-grid reading and figure helpers for the river-map panels."""

from __future__ import annotations

import math
import os
from pathlib import Path
import tempfile

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.axes import Axes
from matplotlib.figure import Figure
from matplotlib.ticker import FuncFormatter, LogLocator, MultipleLocator
from mpl_toolkits.axes_grid1 import make_axes_locatable
from netCDF4 import Dataset
from PIL import Image


def add_map_arguments(parser) -> None:
    """Add settings shared by the independent panel scripts."""
    parser.add_argument("--params", type=Path, required=True, help="Map params.txt")
    parser.add_argument("--west", type=float, required=True, help="Displayed western edge")
    parser.add_argument("--east", type=float, required=True, help="Displayed eastern edge")
    parser.add_argument("--south", type=float, required=True, help="Displayed southern edge")
    parser.add_argument("--north", type=float, required=True, help="Displayed northern edge")
    parser.add_argument("--figure-width", type=float, default=0.0,
                        help="Panel width in inches; 0 chooses it automatically")
    parser.add_argument("--figure-height", type=float, default=0.0,
                        help="Panel height in inches; 0 chooses it automatically")
    # Retain the original post-render maximum-size options for older callers.
    parser.add_argument("--max-width", type=float, default=0.0,
                        help="Legacy maximum panel width in inches")
    parser.add_argument("--max-height", type=float, default=0.0,
                        help="Legacy maximum panel height in inches")
    parser.add_argument("--dpi", type=int, default=200)
    parser.add_argument("--output", type=Path, required=True, help="Output panel PNG")


def read_geometry(path: Path) -> tuple[int, int, float, float, float, float, float]:
    """Read grid dimensions, resolution, and map-edge coordinates."""
    try:
        values = [float(line.split()[0]) for line in path.read_text().splitlines()
                  if line.split() and not line.lstrip().startswith("#")]
    except (OSError, ValueError, IndexError) as exc:
        raise ValueError(f"Cannot read map geometry from {path}: {exc}") from exc
    if len(values) < 8:
        raise ValueError(f"Expected at least eight parameter values in {path}")
    return (int(values[0]), int(values[1]), values[3], values[4], values[5],
            values[6], values[7])


def read_map_raster(path: Path, variable_name: str, expected_units: str,
                    geometry: tuple[int, int, float, float, float, float, float]
                    ) -> tuple[np.ndarray, np.ndarray, np.ma.MaskedArray]:
    """Read one 2-D NetCDF map raster and verify its grid against params.txt."""
    nx, ny, resolution, west, east, south, north = geometry
    with Dataset(path) as source:
        for name in ("lon", "lat", variable_name):
            if name not in source.variables:
                raise ValueError(f"{path} is missing the required {name!r} variable")

        lon = np.asarray(source.variables["lon"][:], dtype=np.float64)
        lat = np.asarray(source.variables["lat"][:], dtype=np.float64)
        variable = source.variables[variable_name]
        if variable.dimensions != ("lat", "lon") or variable.shape != (ny, nx):
            raise ValueError(
                f"{path}:{variable_name} dimensions/shape {variable.dimensions}/"
                f"{variable.shape} do not match ('lat', 'lon')/({ny}, {nx})"
            )
        units = getattr(variable, "units", "")
        if units != expected_units:
            raise ValueError(
                f"Expected {variable_name} units {expected_units!r}, found {units!r}"
            )
        values = np.ma.asarray(variable[:], dtype=np.float64)

    if len(lon) != nx or len(lat) != ny:
        raise ValueError(f"Coordinate dimensions {len(lon)}x{len(lat)} do not match {nx}x{ny}")
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("Expected ascending longitude and descending latitude coordinates")
    expected_lon = west + (np.arange(nx) + 0.5) * resolution
    expected_lat = north - (np.arange(ny) + 0.5) * resolution
    if not np.allclose(lon, expected_lon, rtol=0, atol=1e-5):
        raise ValueError("NetCDF longitude centers do not match params.txt geometry")
    if not np.allclose(lat, expected_lat, rtol=0, atol=1e-5):
        raise ValueError("NetCDF latitude centers do not match params.txt geometry")
    if (not np.isclose(east, west + nx * resolution) or
            not np.isclose(north, south + ny * resolution)):
        raise ValueError("params.txt bounds, dimensions, and resolution are inconsistent")
    return lon, lat, values


def validate_extent(west: float, east: float, south: float, north: float,
                    geometry: tuple[int, int, float, float, float, float, float]) -> None:
    """Check that a requested display extent is ordered and lies in the map."""
    _, _, _, map_west, map_east, map_south, map_north = geometry
    if west >= east or south >= north:
        raise ValueError("map extent must have west < east and south < north")
    if (west < map_west or east > map_east or south < map_south or north > map_north):
        raise ValueError("Requested display extent must lie within the map bounds")


def create_map_axes(geometry, west: float, east: float, south: float, north: float,
                    title: str) -> tuple[Figure, Axes]:
    """Create consistently oriented map axes with geographic labels and ticks."""
    validate_extent(west, east, south, north, geometry)
    _, _, _, map_west, map_east, map_south, map_north = geometry
    midpoint_lat = (south + north) / 2.0
    geographic_ratio = (east - west) * math.cos(math.radians(midpoint_lat)) / (north - south)
    height = float(np.clip(4.5 + 0.12 * (north - south), 5.5, 8.0))
    figure_ratio = float(np.clip(0.48 + 0.60 * geographic_ratio, 0.65, 1.8))
    fig, ax = plt.subplots(figsize=(height * figure_ratio, height))
    ax.set_xlim(west, east)
    ax.set_ylim(south, north)
    ax.set_xlabel("Longitude (°E)")
    ax.set_ylabel("Latitude (°N)")
    span = max(east - west, north - south)
    tick_interval = 10.0 if span >= 40 else 5.0 if span >= 15 else 2.0
    if span < 6:
        tick_interval = 1.0 if span >= 3 else 0.5
    ax.xaxis.set_major_locator(MultipleLocator(tick_interval))
    ax.yaxis.set_major_locator(MultipleLocator(tick_interval))
    ax.set_title(title)
    ax.set_aspect(1.0 / math.cos(math.radians(midpoint_lat)))
    ax._river_map_extent = (map_west, map_east, map_south, map_north)
    return fig, ax


def draw_raster(ax: Axes, values, geometry, cmap, norm):
    """Draw map values in the native grid orientation."""
    _, _, _, west, east, south, north = geometry
    return ax.imshow(
        values, origin="upper", extent=(west, east, south, north),
        cmap=cmap, norm=norm, interpolation="nearest", aspect=ax.get_aspect(),
    )


def add_aligned_colorbar(fig: Figure, ax: Axes, mappable, label: str,
                         orientation: str = "vertical", **kwargs):
    """Add a colorbar aligned to the plotting axes, excluding figure margins."""
    divider = make_axes_locatable(ax)
    if orientation == "vertical":
        cax = divider.append_axes("right", size="5%", pad=0.08)
    elif orientation == "horizontal":
        cax = divider.append_axes("bottom", size="5%", pad=0.55)
    else:
        raise ValueError("orientation must be 'vertical' or 'horizontal'")
    colorbar = fig.colorbar(mappable, cax=cax, orientation=orientation, **kwargs)
    colorbar.set_label(label)
    cax._river_map_colorbar_parent = ax
    cax._river_map_colorbar_orientation = orientation
    cax._river_map_colorbar_pad = 0.08 if orientation == "vertical" else 0.55
    return colorbar


def add_log_colorbar(fig: Figure, ax: Axes, image, label: str) -> None:
    """Add a readable, axes-aligned colorbar for a logarithmic map."""
    colorbar = add_aligned_colorbar(fig, ax, image, label)
    colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=18)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()


def _fit_unconstrained_figure_dimension(fig: Figure, width: float,
                                        height: float) -> None:
    """Expand an automatic dimension when decorations constrain a map axes."""
    map_axes = [ax for ax in fig.axes if hasattr(ax, "_river_map_extent")]
    if not map_axes:
        return

    initial_width, initial_height = fig.get_size_inches()
    target_boxes = {}
    for ax in map_axes:
        original = ax.get_position(original=True)
        target_boxes[ax] = (original.width * initial_width,
                            original.height * initial_height)

    for _ in range(8):
        fig.canvas.draw()
        _align_colorbar_axes(fig)
        fig.canvas.draw()
        renderer = fig.canvas.get_renderer()
        figure_width, figure_height = fig.get_size_inches()
        width_change = 1.0
        height_change = 1.0
        for ax in map_axes:
            target_ratio = 1.0 / (ax.get_data_ratio() * float(ax.get_aspect()))
            original_width, original_height = target_boxes[ax]
            target_width = min(original_width, target_ratio * original_height)
            target_height = min(original_height, original_width / target_ratio)
            axes_box = ax.get_window_extent(renderer)
            axes_width = axes_box.width / fig.dpi
            axes_height = axes_box.height / fig.dpi
            for other in fig.axes:
                if other is ax:
                    continue
                colorbar_position = other.get_position()
                if (colorbar_position.height > 2 * colorbar_position.width and
                        width == 0 and axes_width < target_width * 0.999):
                    width_change = max(width_change, target_width / axes_width)
                elif (colorbar_position.width > 2 * colorbar_position.height and
                      height == 0 and axes_height < target_height * 0.999):
                    height_change = max(height_change, target_height / axes_height)

        # Include titles and labels that extend beyond the canvas.
        tight_bbox = fig.get_tightbbox(renderer)
        if width == 0:
            overflow = max(0.0, -tight_bbox.x0) + max(0.0, tight_bbox.x1 - figure_width)
            if overflow > 0.01:
                width_change = max(width_change,
                                   (figure_width + overflow + 0.02) / figure_width)
        if height == 0:
            overflow = max(0.0, -tight_bbox.y0) + max(0.0, tight_bbox.y1 - figure_height)
            if overflow > 0.01:
                height_change = max(height_change,
                                    (figure_height + overflow + 0.02) / figure_height)

        if width_change <= 1.001 and height_change <= 1.001:
            break
        fig.set_size_inches(figure_width * width_change,
                            figure_height * height_change, forward=True)


def _align_colorbar_axes(fig: Figure) -> None:
    """Align attached colorbar axes to the map axes' active plotting area."""
    figure_width, figure_height = fig.get_size_inches()
    for cax in fig.axes:
        parent = getattr(cax, "_river_map_colorbar_parent", None)
        if parent is None:
            continue
        position = cax.get_position()
        if not hasattr(cax, "_river_map_colorbar_thickness"):
            orientation = cax._river_map_colorbar_orientation
            thickness = (position.width * figure_width if orientation == "vertical"
                         else position.height * figure_height)
            cax._river_map_colorbar_thickness = thickness
            cax.set_axes_locator(None)

        map_position = parent.get_position()
        orientation = cax._river_map_colorbar_orientation
        pad = cax._river_map_colorbar_pad
        thickness = cax._river_map_colorbar_thickness
        if orientation == "vertical":
            cax.set_position((map_position.x1 + pad / figure_width,
                              map_position.y0, thickness / figure_width,
                              map_position.height))
        else:
            cax.set_position((map_position.x0,
                              map_position.y0 - pad / figure_height -
                              thickness / figure_height,
                              map_position.width, thickness / figure_height))


def save_panel(fig: Figure, output: Path, dpi: int,
               max_width: float = 0.0, max_height: float = 0.0,
               figure_width: float = 0.0, figure_height: float = 0.0
               ) -> tuple[int, int]:
    """Render at the requested physical size, or honor legacy max bounds."""
    output.parent.mkdir(parents=True, exist_ok=True)
    dimensions = (figure_width, figure_height, max_width, max_height)
    if not all(math.isfinite(value) and value >= 0 for value in dimensions):
        raise ValueError("figure dimensions and maximum bounds must be finite and non-negative")
    if dpi < 1:
        raise ValueError("DPI must be positive")
    if (figure_width > 0 or figure_height > 0) and (max_width > 0 or max_height > 0):
        raise ValueError("new figure dimensions cannot be combined with legacy max bounds")

    if (figure_width > 0 or figure_height > 0 or
            (max_width == 0 and max_height == 0)):
        current_width, current_height = fig.get_size_inches()
        if figure_height > 0 and figure_width == 0:
            target_height = figure_height
            target_width = current_width * target_height / current_height
        elif figure_width > 0 and figure_height == 0:
            target_width = figure_width
            target_height = current_height * target_width / current_width
        elif figure_width > 0 or figure_height > 0:
            target_width = figure_width or current_width
            target_height = figure_height or current_height
        if figure_width > 0 or figure_height > 0:
            fig.set_size_inches(target_width, target_height, forward=True)
        _fit_unconstrained_figure_dimension(fig, figure_width, figure_height)
        fig.savefig(output, dpi=dpi, format="png")
        with Image.open(output) as image:
            return image.size

    with tempfile.TemporaryDirectory(prefix=".river-map-panel-", dir=output.parent) as tmp:
        reference = Path(tmp) / "reference.png"
        resized = Path(tmp) / "resized.png"
        fig.savefig(reference, dpi=dpi, format="png")
        with Image.open(reference) as source:
            source.load()
            max_pixels_w = math.floor(max_width * dpi) if max_width > 0 else source.width
            max_pixels_h = math.floor(max_height * dpi) if max_height > 0 else source.height
            scale = min(1.0, max_pixels_w / source.width, max_pixels_h / source.height)
            size = (max(1, math.floor(source.width * scale)),
                    max(1, math.floor(source.height * scale)))
            if size == source.size:
                source.save(resized, format="PNG", dpi=(dpi, dpi))
            else:
                image = source.resize(size, Image.Resampling.LANCZOS)
                image.save(resized, format="PNG", dpi=(dpi, dpi))
                image.close()
        os.replace(resized, output)
    return size
