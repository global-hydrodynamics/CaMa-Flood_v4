"""Focused tests for package data, interpolation, and Matplotlib integration."""

import csv
import json
import unittest
from importlib.resources import files

import numpy as np
from matplotlib import colormaps

from cmf_colormap import get_cmap, list_colormaps, register_colormaps
from cmf_colormap.core import _interpolate_rgb


class ColormapTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.data = files("cmf_colormap").joinpath("data")
        with cls.data.joinpath("colormaps.csv").open(encoding="utf-8", newline="") as stream:
            cls.registry = list(csv.DictReader(stream))
        cls.definition = json.loads(
            cls.data.joinpath("shade_rainbow.json").read_text(encoding="utf-8")
        )

    def test_registry_is_unique_and_matches_each_definition(self):
        names = [row["name"] for row in self.registry]
        self.assertEqual(len(names), len(set(names)))
        self.assertEqual(names, list_colormaps())
        for row in self.registry:
            definition = json.loads(
                self.data.joinpath(row["definition"]).read_text(encoding="utf-8")
            )
            self.assertEqual(row["name"], definition["name"])
            self.assertEqual(row["type"], definition["type"])
            self.assertEqual(row["version"], definition["version"])

    def test_shade_rainbow_controls_are_exact_and_evenly_spaced(self):
        controls = self.definition["control_colors"]
        self.assertEqual(
            [item["hex"] for item in controls],
            ["#AAAAAA", "#C3ADC8", "#B49DE3", "#6096F4", "#00B7E7",
             "#00D5BD", "#1BF607", "#D2F500", "#F6D800", "#FF9914",
             "#F22F29", "#EB00ED"],
        )
        self.assertEqual(len(controls), 12)
        self.assertEqual(self.definition["interpolation"], "linear_srgb")
        self.assertFalse({"units", "vmin", "vmax", "normalization"} & self.definition.keys())
        for color in controls:
            self.assertRegex(color["hex"], r"^#[0-9A-Fa-f]{6}$")

    def test_interpolation_endpoints_and_intermediate_rgb(self):
        controls = self.definition["control_colors"]
        first = tuple(int(controls[0]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        second = tuple(int(controls[1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        final = tuple(int(controls[-1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        np.testing.assert_array_equal(_interpolate_rgb(controls, 0), first)
        np.testing.assert_array_equal(_interpolate_rgb(controls, 1), final)
        midpoint = _interpolate_rgb(controls, 0.5 / (len(controls) - 1))
        np.testing.assert_allclose(midpoint, (np.asarray(first) + np.asarray(second)) / 2, atol=1e-15)
        with self.assertRaises(ValueError):
            _interpolate_rgb(controls, -0.1)

    def test_matplotlib_colormap_tracks_the_specification(self):
        cmap = get_cmap("shade_rainbow")
        colors = self.definition["control_colors"]
        expected_first = np.asarray(_interpolate_rgb(colors, 0))
        expected_last = np.asarray(_interpolate_rgb(colors, 1))
        np.testing.assert_allclose(cmap(0.0)[:3], expected_first, atol=2e-5)
        np.testing.assert_allclose(cmap(1.0)[:3], expected_last, atol=2e-5)
        position = 0.43
        np.testing.assert_allclose(
            cmap(position)[:3], _interpolate_rgb(colors, position), atol=2e-5
        )
        for index, item in enumerate(colors):
            expected = tuple(int(item["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
            np.testing.assert_allclose(cmap(index / 11)[:3], expected, atol=6e-5)

    def test_unknown_name_lists_available_options(self):
        with self.assertRaisesRegex(ValueError, "shade_rainbow"):
            get_cmap("missing")

    def test_registration_is_explicit_and_repeatable(self):
        name = "cmf.shade_rainbow"
        if name in colormaps:
            self.fail(f"Unexpected pre-existing Matplotlib colormap {name!r}")
        register_colormaps()
        registered = colormaps[name](0.37)
        register_colormaps()
        np.testing.assert_allclose(colormaps[name](0.37), registered)


if __name__ == "__main__":
    unittest.main()
