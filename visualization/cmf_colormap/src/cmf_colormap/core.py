"""Load CMF colormaps from the package's data registry."""

from __future__ import annotations

import csv
import json
import math
import re
from bisect import bisect_right
from functools import lru_cache
from importlib.resources import files
from typing import Any

from matplotlib.colors import LinearSegmentedColormap

_DATA = files("cmf_colormap").joinpath("data")
_HEX_COLOR = re.compile(r"^#[0-9A-Fa-f]{6}$")
_CMAP_LUT_SIZE = 131_072
_REGISTERED: dict[str, LinearSegmentedColormap] = {}


def _read_definition(name: str) -> dict[str, Any]:
    """Read and validate one JSON definition named by the registry."""
    record = _registry_by_name().get(name)
    if record is None:
        available = ", ".join(list_colormaps()) or "(none)"
        raise ValueError(f"Unknown CMF colormap {name!r}. Available colormaps: {available}")

    path = _DATA.joinpath(record["definition"])
    try:
        definition = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise ValueError(f"Could not read definition for {name!r} from {path}: {exc}") from exc

    if definition.get("name") != name:
        raise ValueError(f"Registry name {name!r} does not match definition name in {path}")
    if definition.get("version") != record["version"]:
        raise ValueError(f"Registry version for {name!r} does not match {path}")
    if definition.get("type") != record["type"]:
        raise ValueError(f"Registry type for {name!r} does not match {path}")
    if definition.get("type") == "diverging":
        center = definition.get("center_position")
        if not isinstance(center, (int, float)) or not 0.0 < center < 1.0:
            raise ValueError(f"Diverging definition in {path} must have a center_position in (0, 1)")
    if definition.get("interpolation") != record["interpolation"]:
        raise ValueError(f"Registry interpolation for {name!r} does not match {path}")
    if definition.get("interpolation") != "linear_srgb":
        raise ValueError(f"Unsupported interpolation in {path}: {definition.get('interpolation')!r}")

    colors = definition.get("control_colors")
    if not isinstance(colors, list) or len(colors) < 2:
        raise ValueError(f"{path} must define at least two control colors")
    for index, color in enumerate(colors):
        if not isinstance(color, dict) or not _HEX_COLOR.fullmatch(color.get("hex", "")):
            raise ValueError(f"Invalid HEX color at control index {index} in {path}")
        if not isinstance(color.get("name"), str) or not color["name"].strip():
            raise ValueError(f"Missing color name at control index {index} in {path}")
    explicit_positions = ["position" in color for color in colors]
    if any(explicit_positions) and not all(explicit_positions):
        raise ValueError(f"Control positions must be provided for every color in {path}")
    if all(explicit_positions):
        positions = [color["position"] for color in colors]
        if any(not isinstance(position, (int, float)) or isinstance(position, bool) or
               not math.isfinite(position) for position in positions):
            raise ValueError(f"Control positions must be numeric in {path}")
        if (positions[0] != 0 or positions[-1] != 1 or
                any(left >= right for left, right in zip(positions, positions[1:]))):
            raise ValueError(f"Control positions must increase strictly from 0 to 1 in {path}")
    return definition


@lru_cache(maxsize=1)
def _registry_by_name() -> dict[str, dict[str, str]]:
    """Return registry entries keyed by unique colormap name."""
    path = _DATA.joinpath("colormaps.csv")
    try:
        with path.open("r", encoding="utf-8", newline="") as stream:
            rows = list(csv.DictReader(stream))
    except OSError as exc:
        raise RuntimeError(f"Could not read CMF colormap registry {path}: {exc}") from exc

    required = {
        "name", "display_name", "type", "definition", "version", "interpolation",
        "display_order", "description",
    }
    if not rows or not required.issubset(rows[0]):
        raise ValueError(f"{path} must contain the columns: {', '.join(sorted(required))}")
    result: dict[str, dict[str, str]] = {}
    for line, row in enumerate(rows, start=2):
        name = row.get("name", "").strip()
        if not name:
            raise ValueError(f"Missing colormap name in {path} on line {line}")
        if name in result:
            raise ValueError(f"Duplicate colormap name {name!r} in {path}")
        if not all(row.get(column, "").strip() for column in required):
            raise ValueError(f"Incomplete registry entry for {name!r} in {path} on line {line}")
        result[name] = {key: value.strip() for key, value in row.items() if key is not None}
    return result


def list_colormaps() -> list[str]:
    """Return CMF colormap names in stable CSV row order.

    This API order is kept separate from the presentation order used by galleries
    and documentation, which is defined by the registry's ``display_order``.
    """
    return list(_registry_by_name())


def _hex_to_rgb(value: str) -> tuple[float, float, float]:
    """Convert an sRGB HEX triplet to channel values in the range 0–1."""
    return tuple(int(value[offset:offset + 2], 16) / 255.0 for offset in (1, 3, 5))


def _interpolate_rgb(control_colors: list[dict[str, Any]], position: float) -> tuple[float, float, float]:
    """Piecewise-linear interpolation in numerical sRGB channels."""
    if not 0.0 <= position <= 1.0:
        raise ValueError("position must be in the normalized interval [0, 1]")
    colors = [_hex_to_rgb(item["hex"]) for item in control_colors]
    positions = _control_positions(control_colors)
    if position == 1.0:
        return colors[-1]
    left = max(0, bisect_right(positions, position) - 1)
    fraction = (position - positions[left]) / (positions[left + 1] - positions[left])
    return tuple(
        colors[left][channel] * (1.0 - fraction) + colors[left + 1][channel] * fraction
        for channel in range(3)
    )


def _control_positions(control_colors: list[dict[str, Any]]) -> list[float]:
    """Return explicit stop positions or the legacy evenly spaced positions."""
    if "position" in control_colors[0]:
        return [float(color["position"]) for color in control_colors]
    return [index / (len(control_colors) - 1) for index in range(len(control_colors))]


@lru_cache(maxsize=None)
def get_cmap(name: str) -> LinearSegmentedColormap:
    """Return a Matplotlib colormap for a registry name.

    The stored definition is continuously piecewise-linear in numerical sRGB.
    Matplotlib represents a colormap with a finite lookup table; a 131,072-step
    table keeps this adapter's sampling error small while the JSON remains the
    authoritative mathematical definition.
    """
    definition = _read_definition(name)
    colors = definition["control_colors"]
    control_positions = _control_positions(colors)
    cmap = LinearSegmentedColormap.from_list(
        f"cmf.{name}",
        list(zip(control_positions, [color["hex"] for color in colors])),
        N=_CMAP_LUT_SIZE,
    )
    cmap.name = name
    cmap.cmf_version = definition["version"]
    return cmap


def register_colormaps() -> None:
    """Register all CMF maps in Matplotlib as ``cmf.<name>``.

    Registration is explicit and repeatable. An unrelated existing Matplotlib
    colormap with a CMF name is left untouched and reported as a collision.
    """
    from matplotlib import colormaps

    for name in list_colormaps():
        registered_name = f"cmf.{name}"
        cmap = get_cmap(name)
        if registered_name in _REGISTERED and registered_name not in colormaps:
            # The caller may have explicitly unregistered a map during tests.
            _REGISTERED.pop(registered_name)
        if registered_name in _REGISTERED:
            continue
        if registered_name in colormaps:
            raise ValueError(
                f"Matplotlib already has a different colormap named {registered_name!r}"
            )
        colormaps.register(cmap, name=registered_name)
        _REGISTERED[registered_name] = cmap
