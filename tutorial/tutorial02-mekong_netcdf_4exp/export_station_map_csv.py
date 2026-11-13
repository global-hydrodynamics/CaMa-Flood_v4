#!/usr/bin/env python3
"""Export Tutorial 02 station coordinates and Mekong map-cell attributes."""

import argparse
import csv
import importlib.util
from pathlib import Path

import numpy as np
from netCDF4 import Dataset


TUTORIAL_DIR = Path(__file__).resolve().parent
REPOSITORY_ROOT = TUTORIAL_DIR.parents[1]
MAP_ROOT = REPOSITORY_ROOT / "map" / "mekong_15min"
UPAREA_FILE = MAP_ROOT / "netcdf" / "uparea.nc"
NETWORK_SCRIPT = TUTORIAL_DIR / "v01-network_maps.py"
DEFAULT_OUTPUT = TUTORIAL_DIR / "station_map_locations.csv"


def load_station_definitions():
    """Load the six stations from the existing Tutorial 02 figure source."""
    spec = importlib.util.spec_from_file_location("tutorial02_network_maps", NETWORK_SCRIPT)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Cannot load station definitions from {NETWORK_SCRIPT}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def read_map_grid():
    """Read grid coordinates and upstream area from the 15-minute map."""
    with Dataset(UPAREA_FILE) as data:
        lon = np.asarray(data.variables["lon"][:], dtype=float)
        lat = np.asarray(data.variables["lat"][:], dtype=float)
        uparea_var = data.variables["uparea"]
        uparea = np.ma.asarray(uparea_var[:], dtype=float)
        units = getattr(uparea_var, "units", "")

    if uparea.shape != (lat.size, lon.size):
        raise ValueError(
            f"uparea shape {uparea.shape} does not match lat/lon "
            f"dimensions {(lat.size, lon.size)}"
        )
    if units and units.lower().replace("^", "") not in {"m2", "m**2", "m²"}:
        raise ValueError(f"Expected uparea units of m2, found {units!r}")
    if not (np.all(np.diff(lon) > 0) and np.all(np.diff(lat) < 0)):
        raise ValueError("Expected longitude ascending and latitude descending")
    return lon, lat, uparea


def main():
    parser = argparse.ArgumentParser(
        description="Write station coordinates, mapped uparea, and map/Python indices."
    )
    parser.add_argument(
        "--output", type=Path, default=DEFAULT_OUTPUT,
        help=f"CSV output path (default: {DEFAULT_OUTPUT})",
    )
    args = parser.parse_args()

    network = load_station_definitions()
    stations = [dict(station) for station in network.STATIONS]
    geometry = network.read_grid_geometry()
    nx, ny = geometry[0], geometry[1]
    catmx, catmy, catvalid = network.read_catchment_mapping(
        geometry, np.ones((ny, nx), dtype=bool)
    )
    network.map_station_pixels(stations, geometry, catmx, catmy, catvalid)
    lon, lat, uparea = read_map_grid()

    fields = [
        "station_name", "plot_label", "longitude_deg_e", "latitude_deg_n",
        "upstream_drainage_area_km2", "map_x_1based", "map_y_1based",
        "python_array_column_0based", "python_array_row_0based",
        "index_language", "map_cell_center_longitude_deg_e",
        "map_cell_center_latitude_deg_n", "cell_assignment_method",
    ]
    rows = []
    for station in stations:
        name = station["name"]
        ix, iy = station["cell"]
        station_lon, station_lat = station["station"]
        if not (1 <= ix <= nx and 1 <= iy <= ny):
            raise ValueError(f"{name}: map cell {(ix, iy)} is outside {nx} x {ny}")

        area_m2 = uparea[iy - 1, ix - 1]
        if np.ma.is_masked(area_m2) or not np.isfinite(float(area_m2)) or area_m2 <= 0:
            raise ValueError(f"{name}: no valid positive uparea at map cell {(ix, iy)}")

        method = (
            "GRDC allocated model cell" if name not in {"My Thuan", "Can Tho"}
            else "geographic point resolved through 1-minute catmxy"
        )
        plot_label = {
            "Ubon Ratchathani": "Ubon Ratchathani (Mun)",
            "Prek Kdam": "Prek Kdam (Tonle Sap)",
            "My Thuan": "My Thuan (Tien)",
            "Can Tho": "Can Tho (Hau/Bassac)",
        }.get(name, name)
        rows.append({
            "station_name": name,
            "plot_label": plot_label,
            "longitude_deg_e": f"{station_lon:.8f}",
            "latitude_deg_n": f"{station_lat:.8f}",
            "upstream_drainage_area_km2": f"{float(area_m2) / 1e6:.2f}",
            "map_x_1based": ix,
            "map_y_1based": iy,
            "python_array_column_0based": ix - 1,
            "python_array_row_0based": iy - 1,
            "index_language": "Python",
            "map_cell_center_longitude_deg_e": f"{lon[ix - 1]:.6f}",
            "map_cell_center_latitude_deg_n": f"{lat[iy - 1]:.6f}",
            "cell_assignment_method": method,
        })

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} Tutorial 02 station records to {args.output}")
    print("Map x/y indices are one-based; Python array indices are zero-based [row, column].")


if __name__ == "__main__":
    main()
