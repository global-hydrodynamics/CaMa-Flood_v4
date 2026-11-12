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
    ticks = categories[np.linspace(0, len(categories) - 1, min(6, len(categories)), dtype=int)]
    colorbar = fig.colorbar(image, cax=cax, ticks=ticks, spacing="uniform")
    colorbar.set_label("Basin ID (categorical; colors repeat)")
    colorbar.minorticks_off()
    save_fixed_panel(fig, ax, cax, args)
    plt.close(fig)
    print(f"Wrote {args.output}; {len(categories)} original basin IDs: {minimum}–{maximum}")


if __name__ == "__main__":
    main()
