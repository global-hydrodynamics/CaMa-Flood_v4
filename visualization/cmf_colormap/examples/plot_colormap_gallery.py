#!/usr/bin/env python3
"""Generate registry-driven CMF colormap previews."""

from __future__ import annotations

import colorsys
import csv
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import FuncNorm, LinearSegmentedColormap, Normalize, TwoSlopeNorm
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np

from cmf_colormap import get_cmap, list_colormaps
from cmf_colormap.core import _interpolate_rgb

HERE = Path(__file__).resolve().parent
DATA_DIR = Path(__file__).resolve().parents[1] / "src" / "cmf_colormap" / "data"


def _asymmetric_signed_flow_norm():
    """Build the documented nonlinear flow example, separate from palette data."""
    vmin, vmax = -1000.0, 10000.0
    threshold = 1.0
    linear_fraction = 0.05

    def forward(values):
        values = np.asarray(values, dtype=np.float64)
        transformed = np.empty_like(values)
        negative = values < -threshold
        central = (values >= -threshold) & (values <= threshold)
        positive = values > threshold
        transformed[negative] = -linear_fraction - (1 - linear_fraction) * (
            np.log10(np.abs(values[negative]) / threshold) /
            np.log10(abs(vmin) / threshold)
        )
        transformed[central] = linear_fraction * values[central] / threshold
        transformed[positive] = linear_fraction + (1 - linear_fraction) * (
            np.log10(values[positive] / threshold) /
            np.log10(vmax / threshold)
        )
        return transformed

    def inverse(values):
        values = np.asarray(values, dtype=np.float64)
        data = np.empty_like(values)
        negative = values < -linear_fraction
        central = (values >= -linear_fraction) & (values <= linear_fraction)
        positive = values > linear_fraction
        data[negative] = -threshold * 10 ** (
            ((-linear_fraction - values[negative]) / (1 - linear_fraction)) *
            np.log10(abs(vmin) / threshold)
        )
        data[central] = threshold * values[central] / linear_fraction
        data[positive] = threshold * 10 ** (
            ((values[positive] - linear_fraction) / (1 - linear_fraction)) *
            np.log10(vmax / threshold)
        )
        return data

    return FuncNorm((forward, inverse), vmin=vmin, vmax=vmax, clip=False)


def _gradient(cmap, count=1024):
    """Return a normalized horizontal color strip."""
    values = np.linspace(0, 1, count)[None, :]
    return cmap(Normalize(0, 1)(values))


def plot_gallery():
    """Write a compact 0–1 gallery for all registered maps."""
    with (DATA_DIR / "colormaps.csv").open(encoding="utf-8", newline="") as stream:
        registry = {row["name"]: row for row in csv.DictReader(stream)}
    names = sorted(list_colormaps(), key=lambda name: int(registry[name]["display_order"]))
    fig, axes = plt.subplots(len(names), 1, figsize=(9, max(2.0, 1.35 * len(names))), squeeze=False)
    for ax, name in zip(axes[:, 0], names):
        ax.imshow(_gradient(get_cmap(name)), aspect="auto", extent=(0, 1, 0, 1))
        ax.set_yticks([])
        ax.set_xlim(0, 1)
        if registry[name]["type"] == "diverging":
            ax.axvline(0.5, color="#222222", linewidth=1.1, linestyle="--")
            ax.set_xticks((0, 0.25, 0.5, 0.75, 1))
            ax.set_xticklabels(("0", "0.25", "0.5\nzero", "0.75", "1"))
        else:
            ax.set_xticks(np.linspace(0, 1, 6))
        ax.set_ylabel(registry[name]["display_name"], rotation=0, ha="right", va="center", labelpad=12)
    axes[-1, 0].set_xlabel("Normalized colormap position")
    fig.suptitle("CMF Colormaps — overlay-aware geospatial palettes")
    fig.tight_layout()
    path = HERE / "colormap_gallery.png"
    fig.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote {path}")
    return names


def presentation_order():
    """Return registry names in the presentation order without changing API order."""
    with (DATA_DIR / "colormaps.csv").open(encoding="utf-8", newline="") as stream:
        records = list(csv.DictReader(stream))
    return [record["name"] for record in sorted(records, key=lambda row: int(row["display_order"]))]


def _hsv_diagnostics(position, controls):
    """Interpolate exact RGB values and derive HSV for diagnostic displays."""
    rgb = np.asarray([_interpolate_rgb(controls, float(x)) for x in position])
    hsv = np.asarray([colorsys.rgb_to_hsv(*color) for color in rgb])
    return rgb, hsv


def plot_detailed_preview(name, example_kind):
    """Write control, background, HSV, curve, and example-scale diagnostics."""
    definition = json.loads((DATA_DIR / f"{name}.json").read_text(encoding="utf-8"))
    controls = definition["control_colors"]
    display_name = name.replace("_", "-").title()
    cmap = get_cmap(name)
    position = np.linspace(0, 1, 1024)
    _, hsv = _hsv_diagnostics(position, controls)
    diverging_example = example_kind in ("signed_overlay", "asymmetric_signed_flow")
    figure = plt.figure(figsize=(18, 15 if diverging_example else 12))
    if diverging_example:
        grid = figure.add_gridspec(
            5, 1, height_ratios=(1.65, 2.2, 1.25, 1.4, 2.0), hspace=0.42
        )
    else:
        grid = figure.add_gridspec(4, 1, height_ratios=(1.55, 2.2, 1.25, 1.6), hspace=0.42)
    figure.subplots_adjust(left=0.18, right=0.99, top=0.92, bottom=0.08)

    # Panel 1: exact stored control colors, with HSV calculated only for display.
    controls_grid = grid[0].subgridspec(2, len(controls), height_ratios=(1.0, 1.15), hspace=0.08)
    for index, item in enumerate(controls):
        ax = figure.add_subplot(controls_grid[0, index])
        ax.set_facecolor(item["hex"])
        ax.set_xticks([])
        ax.set_yticks([])
        control_position = item.get("position", index / (len(controls) - 1))
        ax.set_title(
            f"{index + 1}\n{item['name']}\n{item['hex']}\n{control_position:.3f}",
            fontsize=7,
        )
        if diverging_example and index == len(controls) // 2:
            for spine in ax.spines.values():
                spine.set_edgecolor("#202020")
                spine.set_linewidth(2.5)
        ax = figure.add_subplot(controls_grid[1, index])
        hue, saturation, value = colorsys.rgb_to_hsv(*_interpolate_rgb(controls, control_position))
        ax.text(0.5, 0.68, f"H {hue * 360:.1f}°", ha="center", fontsize=8)
        ax.text(0.5, 0.39, f"S {saturation:.3f}", ha="center", fontsize=8)
        ax.text(0.5, 0.10, f"V {value:.3f}", ha="center", fontsize=8)
        ax.axis("off")
    # Panel 2: thick opaque gradient bars over selectable background colors.
    background_grid = grid[1].subgridspec(3, 1, hspace=0.34)
    for row, (background, label) in enumerate((("#FFFFFF", "White #FFFFFF"),
                                                ("#C0C0C0", "Light gray #C0C0C0"),
                                                ("#555555", "Dark gray #555555"))):
        ax = figure.add_subplot(background_grid[row, 0])
        ax.set_facecolor(background)
        ax.imshow(_gradient(cmap), aspect="auto", extent=(0, 1, 0.25, 0.75), interpolation="nearest")
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.set_yticks([])
        if row == 2:
            ax.set_xticks(np.linspace(0, 1, 12))
            ax.set_xticklabels([f"{x:.2f}" for x in np.linspace(0, 1, 12)], fontsize=7)
        else:
            ax.set_xticks([])
        ax.set_ylabel(label, rotation=0, ha="right", va="center", labelpad=15, fontsize=8)

    # Panel 3: H-only, saturation grayscale (S^3 display transform), and true V.
    diagnostics_grid = grid[2].subgridspec(3, 1, hspace=0.28)
    hue_rgb = np.asarray([colorsys.hsv_to_rgb(float(h), 1.0, 1.0) for h in hsv[:, 0]])
    for row, (strip, label) in enumerate((
        (hue_rgb[None, :, :], "Hue only (S=1, V=1)"),
        (np.repeat((hsv[:, 1] ** 3)[None, :, None], 3, axis=2), "Saturation grayscale (display = S³)"),
        (np.repeat(hsv[:, 2][None, :, None], 3, axis=2), "Value grayscale (actual V)"),
    )):
        ax = figure.add_subplot(diagnostics_grid[row, 0])
        ax.imshow(strip, aspect="auto", extent=(0, 1, 0, 1), interpolation="nearest")
        ax.set_xlim(0, 1)
        ax.set_yticks([])
        ax.set_xticks([])
        ax.set_ylabel(label, rotation=0, ha="right", va="center", labelpad=15, fontsize=8)

    # Panel 4: actual HSV curves and an illustrative value scale.
    curve_grid = grid[3].subgridspec(1, 2, width_ratios=(1.0, 1.0), wspace=0.24)
    ax = figure.add_subplot(curve_grid[0, 0])
    ax.plot(position, hsv[:, 1], label="Saturation", color="#3B6FB6")
    ax.plot(position, hsv[:, 2], label="Value", color="#D97B22")
    ax.set(xlim=(0, 1), ylim=(0, 1.02), xlabel="Normalized position", ylabel="HSV component")
    ax.grid(alpha=0.25)
    ax.legend()

    ax = figure.add_subplot(curve_grid[0, 1])
    if example_kind == "log_discharge":
        values = np.geomspace(5, 1_000_000, 512)
        ax.imshow(_gradient(cmap, len(values)), aspect="auto",
                  extent=(np.log10(values[0]), np.log10(values[-1]), 0, 1),
                  interpolation="nearest")
        ticks = np.array([5, 10, 100, 1_000, 10_000, 100_000, 1_000_000])
        ax.set_xticks(np.log10(ticks), [f"{value:g}" for value in ticks])
        ax.set_xlabel("Example discharge (m³/s; logarithmic)")
        ax.set_title("Demonstration only: controls equally spaced in log value", fontsize=9)
    elif example_kind == "linear_error":
        values = np.linspace(0, 100, 512)
        ax.imshow(_gradient(cmap, len(values)), aspect="auto",
                  extent=(0, 100, 0, 1), interpolation="nearest")
        ax.set_xticks(np.arange(0, 101, 20))
        ax.set_xlabel("Example relative error (%)")
        ax.set_title("Illustrative linear normalization only (0–100%)", fontsize=9)
    elif example_kind == "synthetic_terrain":
        x = np.linspace(0, 1, 360)
        y = np.linspace(0, 1, 240)
        xx, yy = np.meshgrid(x, y)
        terrain = (
            0.50
            + 0.19 * np.sin(2 * np.pi * (1.1 * xx + 0.15 * np.sin(2 * np.pi * yy)))
            + 0.14 * np.cos(2 * np.pi * (1.35 * yy - 0.1 * np.sin(2 * np.pi * xx)))
            + 0.09 * np.sin(2 * np.pi * (xx + yy))
        )
        terrain = (terrain - terrain.min()) / (terrain.max() - terrain.min())
        ax.imshow(terrain, cmap=cmap, origin="lower", extent=(0, 1, 0, 1), aspect="auto")
        river_x = np.linspace(0.03, 0.97, 500)
        river_y = 0.53 + 0.22 * np.sin(2.5 * np.pi * river_x) + 0.08 * np.sin(8 * np.pi * river_x)
        ax.plot(river_x, river_y, color="#30343A", linewidth=2.3, label="Main channel")
        for start, end, phase in ((0.12, 0.48, 0.2), (0.47, 0.78, 1.1), (0.68, 0.94, 2.0)):
            tributary_x = np.linspace(start, end, 150)
            tributary_y = (
                0.53 + 0.22 * np.sin(2.5 * np.pi * tributary_x)
                + 0.08 * np.sin(8 * np.pi * tributary_x)
                + 0.12 * np.sin(5 * np.pi * tributary_x + phase)
            )
            ax.plot(tributary_x, tributary_y, color="#30343A", linewidth=1.35)
        ax.set(xlim=(0, 1), ylim=(0, 1), xlabel="Synthetic x", ylabel="Synthetic y")
        ax.set_title("Synthetic terrain with darker river network (illustration)", fontsize=9)
        ax.legend(loc="upper right", fontsize=8, framealpha=0.85)
    elif example_kind == "signed_overlay":
        values = np.linspace(-100, 100, 512)
        norm = TwoSlopeNorm(vmin=-100, vcenter=0, vmax=100)
        ax.imshow(values[None, :], cmap=cmap, norm=norm, aspect="auto",
                  extent=(-100, 100, 0, 1), interpolation="nearest")
        ax.axvline(0, color="#202020", linewidth=1.2, linestyle="--")
        ax.set_xticks((-100, -50, 0, 50, 100))
        ax.set_xlabel("Illustrative signed error (%)")
        ax.set_title("TwoSlopeNorm: zero maps to position 0.5", fontsize=9)
        ax.text(0, 0.5, "zero", ha="center", va="center", fontsize=8,
                bbox={"facecolor": "white", "alpha": 0.8, "edgecolor": "none"})
        ax.set_yticks([])
    elif example_kind == "asymmetric_signed_flow":
        values = np.linspace(-1000, 10000, 1024)
        norm = _asymmetric_signed_flow_norm()
        ax.imshow(values[None, :], cmap=cmap, norm=norm, aspect="auto",
                  extent=(-1000, 10000, 0, 1), interpolation="nearest")
        ax.set(xlim=(-1000, 10000), ylim=(0, 1))
        ax.axvline(0, color="#202020", linewidth=1.2, linestyle="--")
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_title("Separate log branches with a linear ±1 m³/s center", fontsize=9)
        flow_ticks = [-1000, -300, -100, -30, -10, 0, 10, 30,
                      100, 300, 1000, 3000, 10000]
        colorbar_axes = make_axes_locatable(ax).append_axes(
            "bottom", size="10%", pad=0.42
        )
        colorbar = figure.colorbar(
            plt.cm.ScalarMappable(norm=norm, cmap=cmap),
            cax=colorbar_axes, orientation="horizontal", ticks=flow_ticks,
            extend="both",
        )
        colorbar.ax.set_xticklabels([
            f"−{abs(value):,}" if value < 0 else f"{value:,}" for value in flow_ticks
        ], fontsize=7)
        colorbar.set_label("Illustrative signed outflow (m³/s)", fontsize=8)

        overlay = figure.add_subplot(grid[4])
        x = np.linspace(0, 1, 240)
        y = np.linspace(0, 1, 150)
        xx, yy = np.meshgrid(x, y)
        relief = 0.5 + 0.23 * np.sin(2 * np.pi * (xx + 0.13 * np.sin(2 * np.pi * yy)))
        relief += 0.16 * np.cos(2 * np.pi * (1.2 * yy - 0.1 * np.sin(2 * np.pi * xx)))
        relief = (relief - relief.min()) / (relief.max() - relief.min())
        overlay.imshow(relief, cmap="gray", origin="lower", extent=(0, 1, 0, 1),
                       aspect="auto", alpha=0.72)
        river_x = np.linspace(0.04, 0.96, 400)
        river_y = 0.48 + 0.22 * np.sin(2.6 * np.pi * river_x) + 0.07 * np.sin(9 * np.pi * river_x)
        overlay.plot(river_x, river_y, color="#333333", linewidth=2.0, label="Synthetic river")
        station_x = (
            np.linspace(0.12, 0.87, 13) if example_kind == "asymmetric_signed_flow"
            else np.asarray((0.12, 0.25, 0.37, 0.49, 0.61, 0.74, 0.87))
        )
        station_y = 0.48 + 0.22 * np.sin(2.6 * np.pi * station_x) + 0.07 * np.sin(9 * np.pi * station_x)
        if example_kind == "asymmetric_signed_flow":
            signed_error = np.asarray((
                -1500, -1000, -300, -100, -10, -1, 0, 1, 10, 100, 1000, 10000, 12000,
            ))
            norm = _asymmetric_signed_flow_norm()
        else:
            signed_error = np.asarray((-82, -35, -8, 0, 6, 42, 91))
            norm = TwoSlopeNorm(vmin=-100, vcenter=0, vmax=100)
        points = overlay.scatter(
            station_x, station_y, c=signed_error, cmap=cmap, norm=norm,
            s=100, edgecolors="#242424", linewidths=0.8, zorder=3,
        )
        zero_index = int(np.flatnonzero(signed_error == 0)[0])
        zero_note = ("0 is pale warm yellow" if example_kind == "asymmetric_signed_flow"
                     else "0% is pale mint, not white")
        overlay.annotate(zero_note, (station_x[zero_index], station_y[zero_index]),
                         xytext=(0.53, 0.88), arrowprops={"arrowstyle": "->"}, fontsize=8)
        overlay.set(xlim=(0, 1), ylim=(0, 1), xlabel="Synthetic longitude", ylabel="Synthetic latitude")
        overlay_title = (
            "Illustrative signed outflow over a synthetic shaded-relief background"
            if example_kind == "asymmetric_signed_flow"
            else "Illustrative gauge errors over a synthetic shaded-relief background"
        )
        overlay.set_title(overlay_title, fontsize=9)
        overlay.legend(loc="lower left", fontsize=8)
        colorbar_axes = make_axes_locatable(overlay).append_axes(
            "bottom", size="8%", pad=0.42
        )
        if example_kind == "asymmetric_signed_flow":
            flow_ticks = [-1000, -300, -100, -30, -10, 0, 10, 30,
                          100, 300, 1000, 3000, 10000]
            colorbar = figure.colorbar(
                points, cax=colorbar_axes, orientation="horizontal",
                ticks=flow_ticks, extend="both",
            )
            colorbar.ax.set_xticklabels([
                f"−{abs(value):,}" if value < 0 else f"{value:,}" for value in flow_ticks
            ], fontsize=7)
            colorbar.set_label("Synthetic signed outflow (m³/s)", fontsize=8)
        else:
            figure.colorbar(
                points, cax=colorbar_axes, orientation="horizontal",
                label="Synthetic signed error (%)",
            )
    else:
        raise ValueError(f"Unknown preview example kind: {example_kind}")
    ax.set_yticks([])

    figure.suptitle(f"{display_name} v1.0 — definition and display diagnostics", fontsize=15)
    path = HERE / f"{name}_detailed.png"
    figure.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(figure)
    print(f"Wrote {path}")


def plot_shade_rainbow_detail():
    """Write the Shade-Rainbow log-scale reference preview."""
    plot_detailed_preview("shade_rainbow", "log_discharge")


def plot_white_rainbow_detail():
    """Write the White-Rainbow linear error-scale reference preview."""
    plot_detailed_preview("white_rainbow", "linear_error")


def plot_pale_terrain_detail():
    """Write the Pale-Terrain synthetic terrain and river-context preview."""
    plot_detailed_preview("pale_terrain", "synthetic_terrain")


def plot_diverge_rainbow_detail():
    """Write signed-scale and overlay-context Diverge-Rainbow diagnostics."""
    plot_detailed_preview("diverge_rainbow", "signed_overlay")


def plot_diverge_spectral_detail():
    """Write signed-flow diagnostics with an asymmetric logarithmic example."""
    plot_detailed_preview("diverge_spectral", "asymmetric_signed_flow")


def plot_diverge_asymmetry_detail():
    """Write signed-flow diagnostics for the extended red-to-violet palette."""
    plot_detailed_preview("diverge_asymmetry", "asymmetric_signed_flow")


def plot_diverge_spectral_blue_comparison():
    """Compare the previous blue endpoint with the vivid cobalt-blue endpoint."""
    definition = json.loads(
        (DATA_DIR / "diverge_spectral.json").read_text(encoding="utf-8")
    )
    controls = definition["control_colors"]
    positions = [item["position"] for item in controls]
    colors = [item["hex"] for item in controls]
    previous_colors = [*colors[:-1], "#1769C2"]
    previous_cmap = LinearSegmentedColormap.from_list(
        "diverge_spectral_previous_blue", list(zip(positions, previous_colors))
    )
    current_cmap = get_cmap("diverge_spectral")
    values = np.linspace(0.0, 1.0, 1024)[None, :]
    figure, axes = plt.subplots(1, 2, figsize=(12, 3.8), sharey=True)
    for axis, cmap, label in zip(
        axes,
        (previous_cmap, current_cmap),
        ("Previous blue endpoint #1769C2", "Vivid cobalt-blue endpoint #005BFF"),
    ):
        axis.imshow(values, cmap=cmap, aspect="auto", extent=(0, 1, 0, 1))
        axis.axvline(0.5, color="#202020", linewidth=1.2, linestyle="--")
        axis.set_yticks([])
        axis.set_xticks((0, 0.5, 1.0))
        axis.set_xticklabels(("negative", "zero", "positive"))
        axis.set_title(label, fontsize=10)
    for axis in axes:
        axis.get_xticklabels()[0].set_ha("left")
        axis.get_xticklabels()[-1].set_ha("right")
    figure.suptitle("Diverge-Spectral blue-endpoint comparison")
    figure.text(
        0.5, 0.015,
        "Both use the same red–orange–pale-yellow–green stops and zero at 0.5; only the final blue stop differs.",
        ha="center", fontsize=9,
    )
    figure.subplots_adjust(left=0.06, right=0.99, top=0.78, bottom=0.27, wspace=0.18)
    path = HERE / "diverge_spectral_blue_comparison.png"
    figure.savefig(path, dpi=180)
    plt.close(figure)
    print(f"Wrote {path}")


if __name__ == "__main__":
    plot_gallery()
    detail_by_name = {
        "shade_rainbow": plot_shade_rainbow_detail,
        "white_rainbow": plot_white_rainbow_detail,
        "diverge_rainbow": plot_diverge_rainbow_detail,
        "diverge_spectral": plot_diverge_spectral_detail,
        "diverge_asymmetry": plot_diverge_asymmetry_detail,
        "pale_terrain": plot_pale_terrain_detail,
    }
    for colormap_name in presentation_order():
        detail_by_name[colormap_name]()
    plot_diverge_spectral_blue_comparison()
