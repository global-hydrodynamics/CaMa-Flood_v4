#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Visualize LEVFRC & LEVHGT with land/sea background from UPAREA.

- Land/sea mask from UPAREA:
      uparea > 0      → land (gray)
      uparea == -9999 → sea (black)

- LEVFRC:
      2×2 expansion (thickening)
      Overlapping pixels choose larger UPAREA (main-stem priority)
      Discrete colormap

- LEVHGT:
      No thickening (physical height)
      Smooth colormap (continuous)

Plot domain (cropped):
    lon: -130 to 180
    lat:  -60 to  75

Output:
    image/levfrc.png
    image/levhgt.png
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import colors
from pathlib import Path


# ============================================================
#  Utility
# ============================================================

def read_params(param_path="map/params.txt"):
    """Read nx, ny, west, east, south, north from params.txt."""
    param_path = Path(param_path)
    with param_path.open("r") as f:
        lines = [ln.strip() for ln in f if ln.strip()]

    nx = int(lines[0].split()[0])
    ny = int(lines[1].split()[0])
    west  = float(lines[4].split()[0])
    east  = float(lines[5].split()[0])
    south = float(lines[6].split()[0])
    north = float(lines[7].split()[0])

    return nx, ny, west, east, south, north


def read_2d_from_grads(bin_path, nx, ny, flip_n_s=True, dtype=np.float32):
    """Read GrADS binary file and reshape."""
    data = np.fromfile(bin_path, dtype=dtype)
    expected = nx * ny
    if data.size != expected:
        raise ValueError(f"Size mismatch: expected {expected}, got {data.size}")
    arr = data.reshape((ny, nx))
    if flip_n_s:
        arr = np.flipud(arr)
    return arr


# ============================================================
#  Levee thickening (2×2 expansion with UPAREA priority)
# ============================================================

def thicken_2x2_with_uparea(lev_masked, uparea, undef_up=-9999):
    """
    Expand levee pixels to 2×2 pixels (i,j),(i+1,j),(i,j+1),(i+1,j+1).
    When collisions occur, pick the one with larger UPAREA.
    """
    data = lev_masked.filled(np.nan)
    up = uparea.astype(float).copy()

    ny, nx = data.shape

    # Ignore undefined or non-positive uparea
    up[(up == undef_up) | (up <= 0.0)] = np.nan

    best_up = np.full_like(up, np.nan)
    best_val = np.full_like(data, np.nan)

    offsets = [(0,0), (0,1), (1,0), (1,1)]

    for dy, dx in offsets:
        val_c = np.full_like(data, np.nan)
        up_c  = np.full_like(up, np.nan)

        y_src_start = 0
        y_src_end = ny - dy if dy > 0 else ny
        x_src_start = 0
        x_src_end = nx - dx if dx > 0 else nx

        y_dst_start = dy
        y_dst_end = dy + (y_src_end - y_src_start)
        x_dst_start = dx
        x_dst_end = dx + (x_src_end - x_src_start)

        if y_src_end <= y_src_start or x_src_end <= x_src_start:
            continue

        val_c[y_dst_start:y_dst_end, x_dst_start:x_dst_end] = \
            data[y_src_start:y_src_end, x_src_start:x_src_end]
        up_c[y_dst_start:y_dst_end, x_dst_start:x_dst_end] = \
            up[y_src_start:y_src_end, x_src_start:x_src_end]

        valid = ~np.isnan(val_c) & ~np.isnan(up_c)
        better = valid & (np.isnan(best_up) | (up_c > best_up))

        best_up[better] = up_c[better]
        best_val[better] = val_c[better]

    return np.ma.masked_invalid(best_val)


# ============================================================
#  Plot helper
# ============================================================

def plot_variable(var, bg, extent, bounds, cmap, norm,
                  title, fname, xlim=(-130,180), ylim=(-60,75)):
    """Generic plotting routine."""
    fig, ax = plt.subplots(figsize=(10,5), dpi=150)

    # background land/sea
    ax.imshow(bg, origin="lower", extent=extent,
              cmap="gray", vmin=0, vmax=1,
              zorder=0, interpolation="nearest")

    # overlay variable
    im = ax.imshow(var, origin="lower", extent=extent,
                   cmap=cmap, norm=norm,
                   zorder=2, interpolation="nearest")

    # crop
    ax.set_xlim(*xlim)
    ax.set_ylim(*ylim)

    # labels
    ax.set_title(title)
    ax.set_xlabel("Longitude [deg]")
    ax.set_ylabel("Latitude [deg]")
    ax.set_xticks(np.arange(-120, 181, 60))
    ax.set_yticks(np.arange(-60, 76, 30))

    # colorbar
    cbar = fig.colorbar(im, ax=ax, boundaries=bounds)
    cbar.set_label(title)
    cbar.set_ticks(bounds)
    cbar.ax.set_yticklabels([f"{b:.2f}" for b in bounds])

    # save
    out = Path("image") / fname
    plt.savefig(out, dpi=400, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved: {out}")


# ============================================================
#  Main
# ============================================================

def main():

    # Load grid parameters
    nx, ny, west, east, south, north = read_params("map/params.txt")
    extent = [west, east, south, north]

    # Read variables
    levfrc = read_2d_from_grads("map/levfrc.bin", nx, ny, flip_n_s=True)
    levhgt = read_2d_from_grads("map/levhgt.bin", nx, ny, flip_n_s=True)
    levphyhgt = read_2d_from_grads("map/levphyhgt.bin", nx, ny, flip_n_s=True)
    uparea = read_2d_from_grads("map/uparea.bin", nx, ny, flip_n_s=True)

    undef = -9999.0

    # Masks
    levfrc_mask    = np.ma.masked_where(levfrc    == undef, levfrc)
    levhgt_mask    = np.ma.masked_where(levhgt    == undef, levhgt)
    levphyhgt_mask = np.ma.masked_where(levphyhgt == undef, levphyhgt)

    # Background land-sea mask
    land = uparea > 0
    sea  = uparea == undef

    bg = np.zeros_like(uparea, dtype=float)
    bg[land] = 0.40
    bg[sea]  = 0.00

    # ----------------------------------------------------------
    # 1) LEVFRC (with 2x2 thickening)
    # ----------------------------------------------------------
    levfrc_expanded = thicken_2x2_with_uparea(levfrc_mask, uparea)

    frcbounds = [0.00, 0.01, 0.02, 0.03, 0.04, 0.05,
                 0.10, 0.20, 0.30, 0.50, 1.00]
    ncolor = len(frcbounds) - 1
    base = plt.get_cmap("turbo")
    colors_frc = base(np.linspace(0, 1, ncolor))
    cmap_frc = colors.ListedColormap(colors_frc)
    norm_frc = colors.BoundaryNorm(frcbounds, cmap_frc.N)

    plot_variable(
        levfrc_expanded, bg, extent,
        frcbounds, cmap_frc, norm_frc,
        "LEVFRC (fractional levee position)",
        "levfrc.png"
    )

    # ----------------------------------------------------------
    # 2) LEVHGT (discrete height, turbo colormap, 2×2 thickening)
    # ----------------------------------------------------------

    # 0 以下の値は可視化対象から除外（マスク）
    levhgt_pos = np.ma.masked_less_equal(levhgt_mask, 0.0)

    # 2×2 thickening (LEVHGT も LEVFRC と同じルールで太らせる)
    levhgt_expanded = thicken_2x2_with_uparea(levhgt_pos, uparea)

    # 指定された離散クラス
    hgtbounds = [0, 1, 2, 3, 5, 7, 10, 15, 20, 25, 30, 35]
    n_hcolors = len(hgtbounds) - 1

    # turbo colormap
    base_hgt_cmap = plt.get_cmap("rainbow")
    hcolor_samples = base_hgt_cmap(np.linspace(0, 1, n_hcolors))
    cmap_hgt = colors.ListedColormap(hcolor_samples)

    # boundary norm
    norm_hgt = colors.BoundaryNorm(hgtbounds, n_hcolors)

    # draw
    plot_variable(
        levhgt_expanded, bg, extent,
        hgtbounds, cmap_hgt, norm_hgt,
        "LEVHGT (levee top height [m])",
        "levhgt.png"
    )

    # ----------------------------------------------------------
    # 3) LEVPHYHGT (levee physical height, same style as LEVHGT)
    # ----------------------------------------------------------

    # 0 以下の値は可視化対象から除外（マスク）
    levphyhgt_pos = np.ma.masked_less_equal(levphyhgt_mask, 0.0)

    # 2×2 thickening (LEVPHYHGT も LEVFRC と同じルールで太らせる)
    levphyhgt_expanded = thicken_2x2_with_uparea(levphyhgt_pos, uparea)
    levphyhgt_expanded = np.ma.masked_less_equal(levphyhgt_expanded, 0.0)

    # LEVHGT と同じ hgtbounds, cmap_hgt, norm_hgt を再利用
    plot_variable(
        levphyhgt_expanded, bg, extent,
        hgtbounds, cmap_hgt, norm_hgt,
        "LEVPHYHGT (levee physical height [m])",
        "levphyhgt.png"
    )


# Run
if __name__ == "__main__":
    Path("image").mkdir(exist_ok=True)
    main()
