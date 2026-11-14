#!/usr/bin/env python3
"""Plot the precomputed basin display-color indices."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import BoundaryNorm, ListedColormap

from river_map_common import (
    add_map_arguments,
    draw_raster,
    read_geometry,
    read_map_raster,
)
from river_network_layout import (
    create_network_axes as create_panel_axes,
    save_network_panel as save_fixed_panel,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="bsncol.nc")
    parser.add_argument("--cmap", default="tab10", help="Qualitative discrete palette")
    parser.add_argument("--white-mix", type=float, default=0.60,
                        help="Fraction of opaque white mixed into each basin color (0 to 1)")
    add_map_arguments(parser)
    args = parser.parse_args()
    if not 0 <= args.white_mix < 1:
        parser.error("--white-mix must be at least 0 and less than 1")

    geometry = read_geometry(args.params)
    _, _, raw = read_map_raster(args.input, "bsncol", "1", geometry)
    values = raw.filled(np.nan)
    valid = np.isfinite(values) & (values >= 1) & (values <= 10)
    categories = np.unique(values[valid]).astype(int)
    if categories.size == 0:
        raise ValueError("bsncol.nc contains no valid display-color indices")

    palette = plt.get_cmap(args.cmap)
    colors = palette(np.arange(10))
    colors[:, :3] = (1 - args.white_mix) * colors[:, :3] + args.white_mix
    cmap = ListedColormap(colors, name=f"{args.cmap}_pastel_basin_indices")
    cmap.set_bad("white")
    norm = BoundaryNorm(np.arange(0.5, 10.6, 1.0), cmap.N)
    fig, ax, cax = create_panel_axes(
        geometry, args, "p01-f01c Raster River Map:\nBasin Color", show_colorbar=True,
    )
    image = draw_raster(ax, np.ma.masked_where(~valid, values), geometry, cmap, norm)
    colorbar = fig.colorbar(
        image, cax=cax, boundaries=np.arange(0.5, 10.6, 1.0),
        ticks=categories, spacing="uniform",
    )
    colorbar.set_label("Display-color index (not basin ID)")
    save_fixed_panel(fig, ax, cax, args)
    plt.close(fig)
    print(f"Wrote {args.output}; display-color indices present: {', '.join(map(str, categories))}")


if __name__ == "__main__":
    main()
