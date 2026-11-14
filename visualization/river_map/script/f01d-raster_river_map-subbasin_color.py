#!/usr/bin/env python3
"""Plot subbasin colors with bsncol network context and color-class boundaries."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import BoundaryNorm, ListedColormap
from matplotlib.collections import LineCollection

from river_map_common import add_map_arguments, draw_raster, read_geometry, read_map_raster
from river_network_layout import (
    create_network_axes as create_panel_axes,
    save_network_panel as save_fixed_panel,
)


def read_subbasin_colors(path, geometry):
    """Read one little-endian int32 Fortran (NX, NY) record, x varying fastest."""
    nx, ny = geometry[:2]
    expected_bytes = nx * ny * 4
    if path.stat().st_size != expected_bytes:
        raise ValueError(f"{path} must contain exactly {expected_bytes} bytes for {nx}x{ny} int32 cells")
    # Fortran's first index (x) is contiguous: C-order (NY, NX) gives north-to-
    # south rows, matching origin='upper'. Do not transpose or flip the map.
    values = np.fromfile(path, dtype="<i4").reshape(ny, nx)
    valid = (values >= 1) & (values <= 10)
    if np.any(~(valid | (values == -9999) | (values == 0))):
        raise ValueError(f"{path} has invalid display indices; expected 1–10, -9999, or unassigned 0")
    return np.ma.masked_where(~valid, values)


def draw_bsncol_class_boundaries(ax, bsncol, geometry):
    """Draw shared cell edges where adjacent valid bsncol display classes differ."""
    _, _, resolution, west, _, _, north = geometry
    classes = bsncol.filled(0)
    valid = np.isfinite(classes) & (classes >= 1) & (classes <= 10)
    row, col = np.where(valid[:, :-1] & valid[:, 1:] &
                        (classes[:, :-1] != classes[:, 1:]))
    x, y = west + (col + 1) * resolution, north - row * resolution
    vertical = np.stack((np.column_stack((x, y)), np.column_stack((x, y - resolution))), axis=1)
    row, col = np.where(valid[:-1] & valid[1:] & (classes[:-1] != classes[1:]))
    x, y = west + col * resolution, north - (row + 1) * resolution
    horizontal = np.stack((np.column_stack((x, y)), np.column_stack((x + resolution, y))), axis=1)
    # Categorical display classes must not be interpolated as numeric contours.
    lines = LineCollection(np.concatenate((vertical, horizontal)), colors="black", linewidths=0.35, zorder=3)
    ax.add_collection(lines)
    return len(vertical) + len(horizontal)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="Prepared subcol-sample.bin")
    parser.add_argument("--bsncol", type=Path, required=True,
                        help="Basin display-color index NetCDF raster")
    parser.add_argument("--cmap", default="tab10", help="Qualitative discrete palette, as in Basin Color")
    parser.add_argument("--white-mix", type=float, default=0.60,
                        help="Fraction of opaque white mixed into each display color (0 to 1)")
    add_map_arguments(parser)
    args = parser.parse_args()
    if not 0 <= args.white_mix < 1:
        parser.error("--white-mix must be at least 0 and less than 1")

    geometry = read_geometry(args.params)
    subcol = read_subbasin_colors(args.input, geometry)
    _, _, bsncol = read_map_raster(args.bsncol, "bsncol", "1", geometry)
    bsncol_values = bsncol.filled(0)
    bsncol_valid = (np.isfinite(bsncol_values) & (bsncol_values >= 1) &
                    (bsncol_values <= 10))
    if np.any(~(bsncol_valid | np.ma.getmaskarray(bsncol))):
        raise ValueError(f"{args.bsncol}: bsncol display indices must be 1–10 or missing")

    # bsncol defines network coverage; it does not provide unique basin IDs.
    values = np.ma.masked_where(~bsncol_valid, subcol)
    categories = np.unique(values.compressed())
    if categories.size == 0:
        raise ValueError(f"{args.input} contains no assigned subbasin display-color indices")

    # Keep exactly the Basin Color index-to-color mapping, including white mix.
    colors = plt.get_cmap(args.cmap)(np.arange(10))
    colors[:, :3] = (1 - args.white_mix) * colors[:, :3] + args.white_mix
    cmap = ListedColormap(colors, name=f"{args.cmap}_pastel_subbasin_indices")
    # Let masked zero/missing cells reveal the gray unassigned-cell underlay;
    # cells outside valid bsncol remain white because the axes background is white.
    cmap.set_bad((1, 1, 1, 0))
    norm = BoundaryNorm(np.arange(0.5, 10.6, 1.0), cmap.N)
    fig, ax, cax = create_panel_axes(
        geometry, args, "p01-f01d Raster River Map:\nSubbasin Color", show_colorbar=True,
    )
    unassigned = bsncol_valid & np.ma.getmaskarray(subcol)
    unassigned |= bsncol_valid & (subcol.filled(0) == 0)
    mask_color = "#b3b3b3"
    draw_raster(ax, np.ma.masked_where(~unassigned, np.ones(unassigned.shape)),
                geometry, ListedColormap([mask_color]), None)
    image = draw_raster(ax, values, geometry, cmap, norm)
    boundary_count = draw_bsncol_class_boundaries(ax, bsncol, geometry)
    # Only present categories appear on the colorbar; values keep the same
    # palette lookup as f01c even if categories are absent or nonconsecutive.
    legend_edges = np.r_[categories[0] - 0.5,
                         (categories[:-1] + categories[1:]) / 2,
                         categories[-1] + 0.5]
    colorbar = fig.colorbar(image, cax=cax, boundaries=legend_edges, values=categories,
                           ticks=categories, spacing="uniform")
    colorbar.set_label("Subbasin display-color index (not subbasin ID)")
    colorbar.minorticks_off()
    save_fixed_panel(fig, ax, cax, args)
    plt.close(fig)
    print(f"Wrote {args.output}; subbasin display-color indices: {', '.join(map(str, categories))}; "
          f"{boundary_count} bsncol class-boundary edges; "
          f"{np.count_nonzero(unassigned)} unassigned river cells")


if __name__ == "__main__":
    main()
