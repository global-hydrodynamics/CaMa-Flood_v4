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
    add_aligned_colorbar,
    create_map_axes,
    draw_raster,
    read_geometry,
    read_map_raster,
    save_panel,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="bsncol.nc")
    parser.add_argument("--cmap", default="tab10", help="Qualitative discrete palette")
    add_map_arguments(parser)
    args = parser.parse_args()

    geometry = read_geometry(args.params)
    _, _, raw = read_map_raster(args.input, "bsncol", "1", geometry)
    values = raw.filled(np.nan)
    valid = np.isfinite(values) & (values >= 1) & (values <= 10)
    categories = np.unique(values[valid]).astype(int)
    if categories.size == 0:
        raise ValueError("bsncol.nc contains no valid display-color indices")

    palette = plt.get_cmap(args.cmap)
    colors = palette(np.arange(10))
    cmap = ListedColormap(colors, name=f"{args.cmap}_basin_indices")
    cmap.set_bad("white")
    norm = BoundaryNorm(np.arange(0.5, 10.6, 1.0), cmap.N)
    fig, ax = create_map_axes(
        geometry, args.west, args.east, args.south, args.north,
        "p01-f01b Raster River Map: Basin Color",
    )
    image = draw_raster(ax, np.ma.masked_where(~valid, values), geometry, cmap, norm)
    colorbar = add_aligned_colorbar(
        fig, ax, image, "", boundaries=np.arange(0.5, 10.6, 1.0),
        ticks=categories, spacing="uniform",
    )
    colorbar.set_label("Display-color index (not basin ID)")
    fig.tight_layout()
    save_panel(fig, args.output, args.dpi, args.max_width, args.max_height)
    plt.close(fig)
    print(f"Wrote {args.output}; display-color indices present: {', '.join(map(str, categories))}")


if __name__ == "__main__":
    main()
