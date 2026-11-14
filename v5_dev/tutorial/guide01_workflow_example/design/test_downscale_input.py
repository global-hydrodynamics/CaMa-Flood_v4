#!/usr/bin/env python3
"""Small contract test for the NetCDF-to-legacy-downscaler input adapter."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import tempfile
import unittest

import netCDF4
import numpy as np

SCRIPT = Path(__file__).resolve().parents[1] / "a02-prepare_downscale_input.py"
SPEC = importlib.util.spec_from_file_location("downscale_adapter", SCRIPT)
ADAPTER = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(ADAPTER)


class AdapterContractTests(unittest.TestCase):
    def test_exact_record_mask_clipping_and_binary_order(self) -> None:
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            output_nc, map_nc, output_bin = root / "rivdph.nc", root / "rivhgt.nc", root / "flddph.bin"
            with netCDF4.Dataset(output_nc, "w") as ds:
                ds.createDimension("time", 2)
                ds.createDimension("lat", 2)
                ds.createDimension("lon", 3)
                t = ds.createVariable("time", "f8", ("time",))
                t.units = "hours since 2000-01-01 00:00:00"
                t[:] = [0, 24]
                lat = ds.createVariable("lat", "f4", ("lat",))
                lat[:] = [1, 0]
                lon = ds.createVariable("lon", "f4", ("lon",))
                lon[:] = [0, 1, 2]
                v = ds.createVariable("rivdph", "f4", ("time", "lat", "lon"), fill_value=np.float32(-9999))
                v.units = "m"
                v[0] = np.ones((2, 3), dtype="f4") * 20
                v[1] = np.array([[0.5, 2.0, -9999], [3.0, 4.0, 2.5]], dtype="f4")
            with netCDF4.Dataset(map_nc, "w") as ds:
                ds.createDimension("lat", 2)
                ds.createDimension("lon", 3)
                lat = ds.createVariable("lat", "f4", ("lat",))
                lat[:] = [1, 0]
                lon = ds.createVariable("lon", "f4", ("lon",))
                lon[:] = [0, 1, 2]
                v = ds.createVariable("rivhgt", "f4", ("lat", "lon"), fill_value=np.float32(-9999))
                v.units = "m"
                v[:] = np.array([[1, 1, 1], [2, 4, 1]], dtype="f4")
            result = ADAPTER.prepare_input(output_nc, map_nc, "2000-01-02T00:00:00", output_bin)
            raw = np.fromfile(output_bin, dtype="<f4").reshape(2, 3)
            np.testing.assert_array_equal(raw[0, :2], [0.0, 1.0])
            self.assertEqual(raw[0, 2], np.float32(1.0e20))
            np.testing.assert_array_equal(raw[1], [1.0, 0.0, 1.5])
            legacy_mask = np.array([[True, True, False], [True, True, True]])
            legacy_result = np.full((2, 3), np.float32(1.0e20), dtype=np.float32)
            legacy_result[legacy_mask] = np.maximum(
                np.array([[0.5, 2.0, 0.0], [3.0, 4.0, 2.5]], dtype=np.float32)[legacy_mask]
                - np.array([[1, 1, 1], [2, 4, 1]], dtype=np.float32)[legacy_mask], 0.0
            )
            np.testing.assert_array_equal(raw, legacy_result)
            self.assertEqual(result["time_index_zero_based"], 1)
            self.assertEqual(result["shape"], (2, 3))
            self.assertEqual(output_bin.stat().st_size, 24)


if __name__ == "__main__":
    unittest.main()
