#!/usr/bin/env python3
"""Display original basin IDs with discrete colors, without replacing the IDs."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import BoundaryNorm, ListedColormap

from river_map_common import add_map_arguments, draw_raster, read_geometry, read_map_raster
from river_network_layout import (
    create_network_axes as create_panel_axes,
    save_network_panel as save_fixed_panel,
)


def draw_basin_legend(fig, cax, image, categories):
    """Expand low categorical IDs using legend geometry only, not LogNorm.

    IDs 1–20 each occupy one display slot. Each subsequent interval ending
    at a representative ID occupies one slot too, compressing its individual
    color swatches. The map's raw IDs, BoundaryNorm and palette are reused.
    """
    minimum, maximum = int(categories.min()), int(categories.max())
    representatives = np.array([30, 40, 50, 70, 100, 150, 200, 300])
    upper_ids = np.unique(np.r_[representatives, maximum])
    upper_ids = upper_ids[(upper_ids > 20) & (upper_ids >= minimum) & (upper_ids <= maximum)]
    low_ids = np.arange(minimum, min(20, maximum) + 1)
    # Piecewise-linear display coordinates at integer category boundaries.
    # No logarithm or scientific scaling is applied to the raster or IDs.
    knots = np.r_[minimum - 0.5, low_ids + 0.5, upper_ids + 0.5]
    boundaries = np.arange(minimum - 0.5, maximum + 1.5)
    display_edges = np.interp(boundaries, knots, np.arange(len(knots)))
    cax.pcolormesh(
        [0, 1], display_edges, np.arange(minimum, maximum + 1)[:, None],
        cmap=image.get_cmap(), norm=image.norm, shading="flat", rasterized=True,
    )
    ticks = np.unique(np.r_[categories[categories <= 20], upper_ids])
    ticks = ticks[np.isin(ticks, categories)]
    positions = np.interp(ticks, knots, np.arange(len(knots)))
    cax.set_ylim(display_edges[0], display_edges[-1])
    cax.set_xlim(0, 1)
    cax.set_xticks([])
    cax.set_yticks(positions, labels=[str(value) for value in ticks])
    cax.yaxis.tick_right()
    cax.yaxis.set_label_position("right")
    cax.set_ylabel("Basin ID (categorical; display spacing)")
    cax.minorticks_off()
    # Keep all twenty low-ID labels separate at smaller configured heights.
    height_inches = fig.get_figheight() * cax.get_position().height
    gap = float(np.diff(positions).min()) if len(positions) > 1 else 1.0
    font_size = min(9.0, 0.85 * 72 * height_inches * gap / np.ptp(display_edges))
    cax.tick_params(axis="y", labelsize=font_size)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="Original basin.nc")
    parser.add_argument("--cmap", default="tab20", help="Qualitative palette repeated across raw IDs")
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    _, _, raw = read_map_raster(args.input, "basin", "1", geometry)
    values = raw.filled(np.nan)
    valid = np.isfinite(values) & (values > 0)
    categories = np.unique(values[valid])
    if categories.size == 0 or np.any(categories != np.floor(categories)):
        raise ValueError("basin.nc must contain positive integer basin IDs")
    categories = categories.astype(int)
    minimum, maximum = int(categories.min()), int(categories.max())
    palette = plt.get_cmap(args.cmap)
    # Map the original integer values to discrete bins; only display colors
    # repeat. The raster values are neither renumbered nor replaced by bsncol.
    colors = palette(np.arange(maximum - minimum + 1) % palette.N)
    cmap = ListedColormap(colors, name=f"{args.cmap}_raw_basin_ids")
    cmap.set_bad("white")
    boundaries = np.arange(minimum - 0.5, maximum + 1.5)
    norm = BoundaryNorm(boundaries, cmap.N)

    fig, ax, cax = create_panel_axes(
        geometry, args, "p01-f01b Raster River Map:\nBasin", show_colorbar=True,
    )
    image = draw_raster(ax, np.ma.masked_where(~valid, values), geometry, cmap, norm)
    draw_basin_legend(fig, cax, image, categories)
    save_fixed_panel(fig, ax, cax, args)
    plt.close(fig)
    print(f"Wrote {args.output}; {len(categories)} original basin IDs: {minimum}–{maximum}")


if __name__ == "__main__":
    main()
