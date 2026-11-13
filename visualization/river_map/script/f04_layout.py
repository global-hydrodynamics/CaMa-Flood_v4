"""Shared axes, colorbar, and legend layout for p01-f04 panels."""

from matplotlib.ticker import FuncFormatter, LogLocator

from river_map_common import create_map_axes


COLORBAR_LABEL = "Upstream area at unit-catchment (km²)"


def create_f04_layout(geometry, extent, title):
    """Create matching map, colorbar, and reserved legend axes for f04 panels."""
    fig, map_ax = create_map_axes(geometry, *extent, title)
    grid = fig.add_gridspec(
        2, 2, left=0.10, right=0.90, bottom=0.06, top=0.92,
        height_ratios=(1.0, 0.18), width_ratios=(1.0, 0.04),
        hspace=0.08, wspace=0.10,
    )
    map_ax.set_subplotspec(grid[0, 0])
    colorbar_ax = fig.add_subplot(grid[0, 1])
    legend_ax = fig.add_subplot(grid[1, :])
    legend_ax.set_axis_off()
    map_ax.set_xlabel("Longitude")
    map_ax.set_ylabel("Latitude")
    return fig, map_ax, colorbar_ax, legend_ax


def format_f04_colorbar(fig, colorbar_ax, mappable):
    """Draw the common f04 log colorbar and decimal tick labels."""
    colorbar = fig.colorbar(mappable, cax=colorbar_ax)
    colorbar.set_label(COLORBAR_LABEL)
    colorbar.locator = LogLocator(base=10, subs=(1.0, 2.0, 5.0), numticks=18)
    colorbar.formatter = FuncFormatter(lambda value, _: f"{value:.0f}")
    colorbar.update_ticks()
    return colorbar


def add_f04_legend(legend_ax, handles):
    """Place legend content in the shared row or keep its axes blank."""
    legend_ax.set_axis_off()
    if handles:
        legend_ax.legend(
            handles=handles, loc="center", ncols=3, fontsize=7.0,
            framealpha=0.94, columnspacing=1.15, handlelength=2.0,
        )
