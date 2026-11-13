"""Read and validate Tutorial 02 station locations from the shared CSV."""

import argparse
import csv
from pathlib import Path


DEFAULT_STATION_CSV = Path(__file__).resolve().with_name("station_map_locations.csv")


def read_station_records(path=DEFAULT_STATION_CSV):
    """Read validated station metadata rows from the shared CSV."""
    required = {
        "station_name", "plot_label", "longitude_deg_e", "latitude_deg_n",
        "upstream_drainage_area_km2", "map_x_1based", "map_y_1based",
        "index_language", "cell_assignment_method",
    }
    path = Path(path)
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        missing = required - set(reader.fieldnames or ())
        if missing:
            raise ValueError(f"{path}: missing CSV columns: {', '.join(sorted(missing))}")
        stations = []
        for line_number, row in enumerate(reader, start=2):
            try:
                ix = int(row["map_x_1based"])
                iy = int(row["map_y_1based"])
            except (TypeError, ValueError) as exc:
                raise ValueError(f"{path}:{line_number}: map x/y must be integers") from exc
            if ix < 1 or iy < 1:
                raise ValueError(f"{path}:{line_number}: map x/y indices must be one-based")
            if row["index_language"] != "Python":
                raise ValueError(
                    f"{path}:{line_number}: map indices must declare Python array use"
                )
            try:
                row["longitude_deg_e"] = float(row["longitude_deg_e"])
                row["latitude_deg_n"] = float(row["latitude_deg_n"])
                row["upstream_drainage_area_km2"] = float(
                    row["upstream_drainage_area_km2"]
                )
            except (TypeError, ValueError) as exc:
                raise ValueError(f"{path}:{line_number}: invalid station coordinates/area") from exc
            row["map_x_1based"] = ix
            row["map_y_1based"] = iy
            stations.append(row)

    if not stations:
        raise ValueError(f"{path}: no station rows found")
    return stations


def read_stations(path=DEFAULT_STATION_CSV):
    """Return station display labels and one-based Mekong map-cell indices."""
    return [
        (row["plot_label"], row["map_x_1based"], row["map_y_1based"])
        for row in read_station_records(path)
    ]


def main():
    parser = argparse.ArgumentParser(description="Validate the Tutorial 02 station CSV.")
    parser.add_argument("csv_file", nargs="?", type=Path, default=DEFAULT_STATION_CSV)
    args = parser.parse_args()
    stations = read_station_records(args.csv_file)
    print(f"Read {len(stations)} station records from {args.csv_file}")


if __name__ == "__main__":
    main()
