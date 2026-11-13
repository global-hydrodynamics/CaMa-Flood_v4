"""Fixed-canvas virtual-colorbar layout shared by raster and line river-map panels."""

from __future__ import annotations

import json
import os
import tempfile

import matplotlib.pyplot as plt
from PIL import Image, PngImagePlugin

from river_map_common import create_map_axes, save_panel


# Reference margins in inches: room for geographic ticks, titles, and the
# longest logarithmic colorbar ticks plus its vertical label in either panel.
LEFT = 0.72
BOTTOM = 0.62
TOP = 0.48
COLORBAR_GAP = 0.18
COLORBAR_WIDTH = 0.18
RIGHT = 1.05
RIGHT_WITHOUT_COLORBAR = 0.24


def create_network_axes(geometry, args, title, *, show_colorbar,
                        reserve_colorbar=True, right_margin=None):
    """Reserve identical map and colorbar rectangles, including hidden space."""
    if show_colorbar and not reserve_colorbar:
        raise ValueError("a visible colorbar requires reserved colorbar space")
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north, title,
    )
    default_height = args.max_height if args.max_height > 0 else fig.get_figheight()
    width = args.figure_width
    height = args.figure_height
    aspect = (args.east - args.west) / (args.north - args.south)
    outer_right = (RIGHT if reserve_colorbar else RIGHT_WITHOUT_COLORBAR)
    if right_margin is not None:
        if right_margin < 0:
            raise ValueError("right margin must be non-negative")
        outer_right = right_margin
    horizontal_furniture = LEFT + outer_right
    if reserve_colorbar:
        horizontal_furniture += COLORBAR_GAP + COLORBAR_WIDTH
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
    if reserve_colorbar:
        cax = fig.add_axes((
            (x_offset + map_width + COLORBAR_GAP) / width, y_offset / height,
            COLORBAR_WIDTH / width, map_height / height,
        ))
    else:
        cax = fig.add_axes((
            (x_offset + map_width) / width, y_offset / height,
            0, map_height / height,
        ))
    cax.set_visible(show_colorbar)
    return fig, ax, cax


def save_network_panel(fig, ax, cax, args, *, embed_map_layout=False):
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

    map_bounds = pixel_bounds(ax)
    layout = {
        "canvas_px": size, "map_xywh_px": map_bounds,
        "colorbar_xywh_px": pixel_bounds(cax), "colorbar_visible": cax.get_visible(),
    }
    if embed_map_layout:
        extent = [float(value) for value in (*ax.get_xlim(), *ax.get_ylim())]
        image_bounds = [map_bounds[0], size[1] - map_bounds[1] - map_bounds[3],
                        map_bounds[2], map_bounds[3]]
        colorbar_bounds = layout["colorbar_xywh_px"]
        image_colorbar_bounds = [
            colorbar_bounds[0], size[1] - colorbar_bounds[1] - colorbar_bounds[3],
            colorbar_bounds[2], colorbar_bounds[3],
        ]
        metadata = PngImagePlugin.PngInfo()
        metadata.add_text("cama_map_layout", json.dumps({
            "bounds_px": image_bounds, "extent": extent,
            "colorbar_bounds_px": image_colorbar_bounds,
            "colorbar_visible": cax.get_visible(),
        }))
        with tempfile.NamedTemporaryFile(
                prefix=f".{args.output.stem}-", suffix=".png",
                dir=args.output.parent, delete=False) as temporary:
            temporary_path = temporary.name
        try:
            with Image.open(args.output) as image:
                image.save(temporary_path, format="PNG", pnginfo=metadata,
                           dpi=(args.dpi, args.dpi))
            os.replace(temporary_path, args.output)
        finally:
            if os.path.exists(temporary_path):
                os.unlink(temporary_path)
    print("Panel layout: " + json.dumps(layout))
    return size
