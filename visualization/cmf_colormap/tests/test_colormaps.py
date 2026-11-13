"""Focused tests for package data, interpolation, and Matplotlib integration."""

import csv
import importlib.util
import json
import tempfile
import unittest
from importlib.resources import files
from pathlib import Path
from unittest import mock

import numpy as np
from matplotlib import colormaps
from matplotlib.colors import TwoSlopeNorm

from cmf_colormap import get_cmap, list_colormaps, register_colormaps
from cmf_colormap.core import _interpolate_rgb


class ColormapTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.data = files("cmf_colormap").joinpath("data")
        with cls.data.joinpath("colormaps.csv").open(encoding="utf-8", newline="") as stream:
            cls.registry = list(csv.DictReader(stream))
        cls.registry_by_name = {row["name"]: row for row in cls.registry}
        cls.definition = json.loads(
            cls.data.joinpath("shade_rainbow.json").read_text(encoding="utf-8")
        )
        cls.white_definition = json.loads(
            cls.data.joinpath("white_rainbow.json").read_text(encoding="utf-8")
        )
        cls.pale_definition = json.loads(
            cls.data.joinpath("pale_terrain.json").read_text(encoding="utf-8")
        )
        cls.diverge_definition = json.loads(
            cls.data.joinpath("diverge_rainbow.json").read_text(encoding="utf-8")
        )
        cls.diverge_spectral_definition = json.loads(
            cls.data.joinpath("diverge_spectral.json").read_text(encoding="utf-8")
        )
        cls.diverge_asymmetry_definition = json.loads(
            cls.data.joinpath("diverge_asymmetry.json").read_text(encoding="utf-8")
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
            self.assertEqual(row["interpolation"], definition["interpolation"])
            self.assertTrue(row["display_name"])
        self.assertEqual(
            list_colormaps(),
            ["shade_rainbow", "white_rainbow", "pale_terrain", "diverge_rainbow",
             "diverge_spectral", "diverge_asymmetry"],
        )
        display_order = [int(row["display_order"]) for row in self.registry]
        self.assertEqual(sorted(display_order), [1, 2, 3, 4, 5, 6])
        self.assertEqual(
            [row["name"] for row in sorted(self.registry, key=lambda row: int(row["display_order"]))],
            ["shade_rainbow", "white_rainbow", "diverge_rainbow",
             "diverge_spectral", "diverge_asymmetry", "pale_terrain"],
        )

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

    def test_white_rainbow_definition_matches_approved_controls(self):
        expected = [
            "#FFFFFF", "#EEFFFF", "#BEFFFF", "#69F5F8", "#7AFCCB",
            "#D9F960", "#FFE300", "#FFAA00", "#FF6A00", "#F52E16", "#E60000",
        ]
        definition = self.white_definition
        controls = definition["control_colors"]
        self.assertEqual(definition["name"], "white_rainbow")
        self.assertEqual(definition["version"], "1.0.0")
        self.assertEqual(definition["type"], "sequential")
        self.assertEqual(definition["interpolation"], "linear_srgb")
        self.assertEqual([item["hex"] for item in controls], expected)
        self.assertEqual(len(controls), 11)
        self.assertEqual(controls[0]["hex"], "#FFFFFF")
        self.assertEqual(controls[-1]["hex"], "#E60000")
        positions = [index / (len(controls) - 1) for index in range(len(controls))]
        np.testing.assert_allclose(np.diff(positions), np.full(10, 0.1), atol=1e-15)
        self.assertEqual(self.registry_by_name["white_rainbow"]["display_name"], "White-Rainbow")
        self.assertEqual(self.registry_by_name["white_rainbow"]["interpolation"], "linear_srgb")

    def test_white_rainbow_interpolation_is_piecewise_linear_srgb(self):
        controls = self.white_definition["control_colors"]
        first = tuple(int(controls[0]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        second = tuple(int(controls[1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        expected = tuple((left + right) / 2 for left, right in zip(first, second))
        np.testing.assert_allclose(_interpolate_rgb(controls, 0.05), expected, atol=1e-15)
        np.testing.assert_array_equal(_interpolate_rgb(controls, 0), (1.0, 1.0, 1.0))
        final = tuple(int(controls[-1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        np.testing.assert_array_equal(_interpolate_rgb(controls, 1), final)

    def test_white_rainbow_matplotlib_rendering(self):
        cmap = get_cmap("white_rainbow")
        self.assertEqual(cmap.name, "white_rainbow")
        np.testing.assert_allclose(cmap(0.0)[:3], (1.0, 1.0, 1.0), atol=1e-12)
        final = tuple(int("E60000"[i:i + 2], 16) / 255 for i in (0, 2, 4))
        np.testing.assert_allclose(cmap(1.0)[:3], final, atol=1e-12)

    def test_pale_terrain_definition_matches_approved_controls(self):
        expected = [
            "#E8FAFA", "#EEFBF6", "#F4F9E8", "#FFFCE9",
            "#FFF4E5", "#FCEBE5", "#F9DADB", "#F6C9CE",
        ]
        definition = self.pale_definition
        controls = definition["control_colors"]
        self.assertEqual(definition["name"], "pale_terrain")
        self.assertEqual(definition["version"], "1.0.0")
        self.assertEqual(definition["type"], "sequential")
        self.assertEqual(definition["interpolation"], "linear_srgb")
        self.assertEqual([item["hex"] for item in controls], expected)
        self.assertEqual(len(controls), 8)
        positions = [index / (len(controls) - 1) for index in range(len(controls))]
        np.testing.assert_allclose(np.diff(positions), np.full(7, 1 / 7), atol=1e-15)
        self.assertEqual(controls[0]["hex"], "#E8FAFA")
        self.assertEqual(controls[-1]["hex"], "#F6C9CE")
        self.assertEqual(self.registry_by_name["pale_terrain"]["display_name"], "Pale-Terrain")

    def test_pale_terrain_interpolation_and_matplotlib_rendering(self):
        controls = self.pale_definition["control_colors"]
        first = tuple(int(controls[0]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        second = tuple(int(controls[1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        expected = tuple((left + right) / 2 for left, right in zip(first, second))
        np.testing.assert_allclose(_interpolate_rgb(controls, 1 / 14), expected, atol=1e-15)
        cmap = get_cmap("pale_terrain")
        np.testing.assert_allclose(cmap(0.0)[:3], first, atol=1e-12)
        final = tuple(int(controls[-1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        np.testing.assert_allclose(cmap(1.0)[:3], final, atol=1e-12)

    def test_diverge_rainbow_definition_matches_approved_controls(self):
        expected = [
            "#1555F4", "#257DF6", "#15A6F4", "#04C7ED", "#25E6DF",
            "#91F5DC", "#D8FADB", "#EAFEAD", "#F7FB6E", "#FFF036",
            "#FFD227", "#FF9020", "#F32C2E",
        ]
        definition = self.diverge_definition
        controls = definition["control_colors"]
        self.assertEqual(definition["name"], "diverge_rainbow")
        self.assertEqual(definition["version"], "1.0.0")
        self.assertEqual(definition["type"], "diverging")
        self.assertEqual(definition["interpolation"], "linear_srgb")
        self.assertEqual(definition["center_position"], 0.5)
        self.assertEqual([item["hex"] for item in controls], expected)
        self.assertEqual(len(controls), 13)
        positions = [index / (len(controls) - 1) for index in range(len(controls))]
        np.testing.assert_allclose(np.diff(positions), np.full(12, 1 / 12), atol=1e-15)
        self.assertEqual(controls[0]["hex"], "#1555F4")
        self.assertEqual(controls[6]["hex"], "#D8FADB")
        self.assertEqual(controls[-1]["hex"], "#F32C2E")
        self.assertEqual(self.registry_by_name["diverge_rainbow"]["display_name"], "Diverge-Rainbow")
        self.assertEqual(self.registry_by_name["diverge_rainbow"]["type"], "diverging")

    def test_diverge_rainbow_interpolation_and_midpoint_normalization(self):
        controls = self.diverge_definition["control_colors"]
        center = tuple(int(controls[6]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        np.testing.assert_array_equal(_interpolate_rgb(controls, 0.5), center)
        first = tuple(int(controls[0]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        second = tuple(int(controls[1]["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
        np.testing.assert_allclose(
            _interpolate_rgb(controls, 1 / 24),
            (np.asarray(first) + np.asarray(second)) / 2,
            atol=1e-15,
        )
        cmap = get_cmap("diverge_rainbow")
        np.testing.assert_allclose(cmap(0.5)[:3], center, atol=2e-5)
        norm = TwoSlopeNorm(vmin=-50, vcenter=0, vmax=100)
        self.assertEqual(norm(0), 0.5)

    def test_diverge_rainbow_matplotlib_registration(self):
        cmap = get_cmap("diverge_rainbow")
        self.assertEqual(cmap.name, "diverge_rainbow")

    def test_diverge_spectral_definition_and_exact_stop_positions(self):
        definition = self.diverge_spectral_definition
        controls = definition["control_colors"]
        self.assertEqual(definition["name"], "diverge_spectral")
        self.assertEqual(definition["version"], "1.0.0")
        self.assertEqual(definition["type"], "diverging")
        self.assertEqual(definition["interpolation"], "linear_srgb")
        self.assertEqual(definition["center_position"], 0.5)
        self.assertEqual(
            [item["position"] for item in controls],
            [0.0, 0.22, 0.43, 0.50, 0.70, 1.0],
        )
        self.assertEqual(
            [item["hex"] for item in controls],
            ["#C51B1D", "#E96F1F", "#F6B13A", "#FFF9D8", "#49D17D", "#005BFF"],
        )
        cmap = get_cmap("diverge_spectral")
        for item in controls:
            position = item["position"]
            expected = tuple(int(item["hex"][i:i + 2], 16) / 255 for i in (1, 3, 5))
            np.testing.assert_allclose(cmap(position)[:3], expected, atol=2e-5)
        center = tuple(int("FFF9D8"[i:i + 2], 16) / 255 for i in (0, 2, 4))
        np.testing.assert_allclose(cmap(0.5)[:3], center, atol=2e-5)
        self.assertEqual(self.registry_by_name["diverge_spectral"]["display_name"], "Diverge-Spectral")

    def test_diverge_spectral_asymmetric_example_norm(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_spectral_example", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        norm = gallery._asymmetric_signed_flow_norm()
        values = np.asarray([-1000, -1, 0, 1, 10000], dtype=np.float64)
        positions = np.asarray(norm(values))
        np.testing.assert_allclose(positions, [0, 0.475, 0.5, 0.525, 1], atol=1e-12)
        np.testing.assert_allclose(norm.inverse(positions), values, rtol=1e-12)

    def test_diverge_asymmetry_definition_and_registered_colors(self):
        definition = self.diverge_asymmetry_definition
        controls = definition["control_colors"]
        expected_positions = [0.00, 0.14, 0.28, 0.40, 0.50, 0.67, 0.84, 1.00]
        expected_colors = [
            "#9F5A45", "#E04B1F", "#E96F1F", "#F6B13A", "#FFF9D8",
            "#49D17D", "#005BFF", "#7040FF",
        ]
        self.assertEqual(definition["name"], "diverge_asymmetry")
        self.assertEqual(definition["type"], "diverging")
        self.assertEqual(definition["center_position"], 0.5)
        self.assertEqual([item["position"] for item in controls], expected_positions)
        self.assertEqual([item["hex"] for item in controls], expected_colors)
        self.assertFalse({"units", "vmin", "vmax", "normalization"} & definition.keys())
        cmap = get_cmap("diverge_asymmetry")
        for control in controls:
            expected = tuple(
                int(control["hex"][index:index + 2], 16) / 255
                for index in (1, 3, 5)
            )
            np.testing.assert_allclose(
                cmap(control["position"])[:3], expected, atol=2e-5,
            )
        self.assertEqual(
            self.registry_by_name["diverge_asymmetry"]["display_name"],
            "Diverge-Asymmetry",
        )

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
        names = (
            "cmf.shade_rainbow", "cmf.white_rainbow", "cmf.pale_terrain",
            "cmf.diverge_rainbow", "cmf.diverge_spectral",
            "cmf.diverge_asymmetry",
        )
        if any(name in colormaps for name in names):
            self.fail("Unexpected pre-existing CMF Matplotlib colormap")
        register_colormaps()
        registered = {name: colormaps[name](0.37) for name in names}
        register_colormaps()
        for name in names:
            np.testing.assert_allclose(colormaps[name](0.37), registered[name])

    def test_gallery_generation_includes_all_colormaps(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_gallery", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                self.assertEqual(
                    gallery.plot_gallery(),
                    ["shade_rainbow", "white_rainbow", "diverge_rainbow",
                     "diverge_spectral", "diverge_asymmetry", "pale_terrain"],
                )
                self.assertEqual(
                    gallery.presentation_order(),
                    ["shade_rainbow", "white_rainbow", "diverge_rainbow",
                     "diverge_spectral", "diverge_asymmetry", "pale_terrain"],
                )
            self.assertGreater((Path(directory) / "colormap_gallery.png").stat().st_size, 0)

    def test_white_rainbow_detailed_preview_uses_linear_example(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_gallery_detail", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                gallery.plot_white_rainbow_detail()
            self.assertGreater((Path(directory) / "white_rainbow_detailed.png").stat().st_size, 0)

    def test_pale_terrain_detailed_preview_generates_synthetic_river_context(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_pale_detail", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                gallery.plot_pale_terrain_detail()
            self.assertGreater((Path(directory) / "pale_terrain_detailed.png").stat().st_size, 0)

    def test_diverge_rainbow_detailed_preview_generates_signed_overlay(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_diverge_detail", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                gallery.plot_diverge_rainbow_detail()
            self.assertGreater((Path(directory) / "diverge_rainbow_detailed.png").stat().st_size, 0)

    def test_diverge_spectral_detailed_preview_generates_signed_flow_scale(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_spectral_detail", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                gallery.plot_diverge_spectral_detail()
            self.assertGreater((Path(directory) / "diverge_spectral_detailed.png").stat().st_size, 0)

    def test_diverge_spectral_blue_comparison_preview_generates(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_spectral_blue_compare", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                gallery.plot_diverge_spectral_blue_comparison()
            self.assertGreater(
                (Path(directory) / "diverge_spectral_blue_comparison.png").stat().st_size,
                0,
            )

    def test_diverge_asymmetry_detailed_preview_generates(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_colormap_gallery.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_asymmetry_detail", script)
        gallery = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gallery)
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.object(gallery, "HERE", Path(directory)):
                gallery.plot_diverge_asymmetry_detail()
            self.assertGreater(
                (Path(directory) / "diverge_asymmetry_detailed.png").stat().st_size,
                0,
            )

    def test_river_overlay_examples_are_connected_deterministic_linework(self):
        script = Path(__file__).resolve().parents[1] / "examples" / "plot_river_overlay_examples.py"
        spec = importlib.util.spec_from_file_location("cmf_colormap_river_examples", script)
        examples = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(examples)
        first = examples.build_synthetic_network()
        second = examples.build_synthetic_network()
        self.assertGreater(len(first), 80)
        self.assertEqual(len(first), len(second))
        for edge_a, edge_b in zip(first, second):
            np.testing.assert_array_equal(edge_a["coordinates"], edge_b["coordinates"])
            self.assertEqual(edge_a["flow"], edge_b["flow"])
            self.assertEqual(edge_a["error"], edge_b["error"])
        flows = np.asarray([edge["flow"] for edge in first])
        errors = np.asarray([edge["error"] for edge in first])
        self.assertEqual(flows.max(), 64)
        starts = {tuple(edge["coordinates"][0]) for edge in first}
        for edge in first[:-1]:  # The final edge is the network outlet.
            self.assertIn(tuple(edge["coordinates"][-1]), starts)
        self.assertLess(errors.min(), 10)
        self.assertGreater(errors.max(), 50)
        self.assertGreater(np.unique(errors).size, 10)

        figure, discharge_lines, error_lines = examples.build_comparison_figure()
        try:
            discharge_segments = discharge_lines.get_segments()
            error_segments = error_lines.get_segments()
            self.assertEqual(len(discharge_segments), len(first))
            self.assertEqual(len(error_segments), len(first))
            np.testing.assert_array_equal(
                figure.axes[0].images[0].get_array(),
                figure.axes[1].images[0].get_array(),
            )
            for flow_segment, error_segment in zip(discharge_segments, error_segments):
                np.testing.assert_array_equal(flow_segment, error_segment)
            self.assertIsNone(discharge_lines.get_alpha())
            self.assertIsNone(error_lines.get_alpha())
            with tempfile.TemporaryDirectory() as directory:
                output = Path(directory) / "river_overlay_comparison.png"
                examples.plot_river_overlay_comparison(output)
                self.assertGreater(output.stat().st_size, 0)
        finally:
            examples.plt.close(figure)


if __name__ == "__main__":
    unittest.main()
