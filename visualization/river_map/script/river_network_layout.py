"""Fixed-canvas virtual-colorbar layout shared by raster and line river-map panels."""

from __future__ import annotations

import json

import matplotlib.pyplot as plt

from river_map_common import create_map_axes, save_panel


# Reference margins in inches: room for geographic ticks, titles, and the
# longest logarithmic colorbar ticks plus its vertical label in either panel.
LEFT = 0.72
BOTTOM = 0.62
TOP = 0.48
COLORBAR_GAP = 0.18
COLORBAR_WIDTH = 0.18
RIGHT = 1.05


def create_network_axes(geometry, args, title, *, show_colorbar):
    """Reserve identical map and colorbar rectangles, including hidden space."""
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north, title,
    )
    default_height = args.max_height if args.max_height > 0 else fig.get_figheight()
    width = args.figure_width
    height = args.figure_height
    aspect = (args.east - args.west) / (args.north - args.south)
    horizontal_furniture = LEFT + COLORBAR_GAP + COLORBAR_WIDTH + RIGHT
    vertical_furniture = BOTTOM + TOP

    if width == 0 and height == 0:
        height = default_height
    if height > 0 and width == 0:
        map_height = height - vertical_furniture
        map_width = map_height * aspect
        width = horizontal_furniture + map_width
    elif width > 0 and height == 0:
        map_width = width - horizontal_furniture
        map_height = map_width / aspect
        height = vertical_furniture + map_height
    else:
        available_width = width - horizontal_furniture
        available_height = height - vertical_furniture
        if available_width <= 0 or available_height <= 0:
            raise ValueError("Panel dimensions are too small for map labels and colorbar space")
        map_width = min(available_width, available_height * aspect)
        map_height = map_width / aspect

    if map_width <= 0 or map_height <= 0:
        raise ValueError("Panel dimensions must leave positive space for the map")

    # Keep equal-degree map scaling and center the map when both dimensions constrain it.
    x_offset = LEFT + max(0.0, (width - horizontal_furniture - map_width) / 2.0)
    y_offset = BOTTOM + max(0.0, (height - vertical_furniture - map_height) / 2.0)
    fig.set_size_inches(width, height)
    fig.set_layout_engine(None)
    ax.set_position((x_offset / width, y_offset / height,
                     map_width / width, map_height / height))
    ax.set_aspect(1.0)
    cax = fig.add_axes((
        (x_offset + map_width + COLORBAR_GAP) / width, y_offset / height,
        COLORBAR_WIDTH / width, map_height / height,
    ))
    cax.set_visible(show_colorbar)
    return fig, ax, cax


def save_network_panel(fig, ax, cax, args):
    """Save the full canvas and report axes geometry in exported PNG pixels."""
    # Explicit axes must not be moved or cropped by user Matplotlib settings.
    fig.set_layout_engine(None)
    with plt.rc_context({"savefig.bbox": None}):
        size = save_panel(
            fig, args.output, args.dpi, args.max_width, args.max_height,
            args.figure_width, args.figure_height,
        )
    fig.canvas.draw()

    def pixel_bounds(axes):
        # Matplotlib truncates the reference canvas to whole pixels. Use
        # renderer coordinates, then apply the actual raster resize factors;
        # multiplying normalized positions by the truncated canvas is inexact.
        reference_width, reference_height = (
            int(inches * args.dpi) for inches in fig.get_size_inches()
        )
        sx = args.dpi / fig.dpi * size[0] / reference_width
        sy = args.dpi / fig.dpi * size[1] / reference_height
        x, y, width, height = axes.get_window_extent().bounds
        return [round(v, 6) for v in (x * sx, y * sy, width * sx, height * sy)]

    print("Panel layout: " + json.dumps({
        "canvas_px": size, "map_xywh_px": pixel_bounds(ax),
        "colorbar_xywh_px": pixel_bounds(cax), "colorbar_visible": cax.get_visible(),
    }))
    return size
