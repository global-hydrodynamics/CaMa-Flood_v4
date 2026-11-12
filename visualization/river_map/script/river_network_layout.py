"""Fixed-canvas virtual-colorbar layout shared by the two p01-f02 panels."""

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
    """Reserve identical map and colorbar rectangles, including hidden space.

    Height-only sizing uses the requested total height, with automatic width
    derived from the map aspect and reserved margins. A width limit scales the
    complete reference layout proportionally, as for other river-map panels.
    """
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north, title,
    )
    height = args.max_height if args.max_height > 0 else fig.get_figheight()
    if height <= BOTTOM + TOP:
        raise ValueError(f"Panel height must exceed {BOTTOM + TOP:g} inches")
    map_height = height - BOTTOM - TOP
    # Preserve the equal-degree aspect used by the existing background imshow.
    # imshow sets aspect='equal' after create_map_axes in both original panels.
    map_width = map_height * (args.east - args.west) / (args.north - args.south)
    width = LEFT + map_width + COLORBAR_GAP + COLORBAR_WIDTH + RIGHT
    fig.set_size_inches(width, height)
    fig.set_layout_engine(None)
    ax.set_position((LEFT / width, BOTTOM / height, map_width / width, map_height / height))
    ax.set_aspect(1.0)
    cax = fig.add_axes((
        (LEFT + map_width + COLORBAR_GAP) / width, BOTTOM / height,
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
            fig, args.output, args.dpi, args.max_width, 0,
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
