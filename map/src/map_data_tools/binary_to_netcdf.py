"""Convert category 11 binary maps to the CaMa-Flood CRIVCLINC NetCDF contract."""
from __future__ import annotations

import argparse
from pathlib import Path

try:
    from netCDF4 import Dataset
except ImportError:  # provide a clear CLI error without making metadata/CTL tools depend on it
    Dataset = None

try:
    from .map_metadata import (
        MapDataError, check_overwrite, default_registry_path, grid_coordinates,
        load_core_data, read_core_specs, read_dataset_config, read_params,
        read_registry, validate_missing_value,
    )
except ImportError:  # direct script execution
    from map_metadata import (
        MapDataError, check_overwrite, default_registry_path, grid_coordinates,
        load_core_data, read_core_specs, read_dataset_config, read_params,
        read_registry, validate_missing_value,
    )


def convert(map_dir: Path, output: Path, registry: Path | None,
            config_path: Path | None, overwrite: bool) -> Path:
    if Dataset is None:
        raise MapDataError("NetCDF conversion requires Python package netCDF4; install requirements.txt")
    map_dir = map_dir.resolve()
    output = output.resolve()
    check_overwrite([output], overwrite)
    config = read_dataset_config(config_path, map_dir)
    params = read_params(map_dir)
    groups = read_core_specs(read_registry(registry))
    variables = load_core_data(map_dir, groups, params, config)
    lon, lat = grid_coordinates(params, config)
    output.parent.mkdir(parents=True, exist_ok=True)
    try:
        with Dataset(output, "w", format="NETCDF3_64BIT_OFFSET") as nc:
            nc.createDimension("lon", len(lon))
            nc.createDimension("lat", len(lat))
            nc.createDimension("lfp", int(params["NLFP"]))
            nc.Conventions = "CF-1.10"
            nc.title = f"CaMa-Flood core river-network map: {config['dataset_id']}"
            nc.dataset_id = config["dataset_id"]
            nc.map_grid = "regular_lonlat_from_params"
            nc.binary_byte_order = config["byte_order"]
            nc.binary_row_order = config["row_order"]

            lonvar = nc.createVariable("lon", "f8", ("lon",))
            lonvar[:] = lon
            lonvar.long_name = "Longitude"
            lonvar.standard_name = "longitude"
            lonvar.units = "degrees_east"
            lonvar.axis = "X"

            latvar = nc.createVariable("lat", "f8", ("lat",))
            latvar[:] = lat
            latvar.long_name = "Latitude"
            latvar.standard_name = "latitude"
            latvar.units = "degrees_north"
            latvar.axis = "Y"

            lfp = nc.createVariable("lfp", "i4", ("lfp",))
            lfp[:] = range(1, int(params["NLFP"]) + 1)
            lfp.long_name = "Floodplain profile layer index"
            lfp.units = "1"

            for filename, entries in groups.items():
                sentinel = validate_missing_value(filename, entries[0]["dtype"], config)
                for row in entries:
                    name = row["name"]
                    is_profile = name == "fldhgt"
                    dims = ("lfp", "lat", "lon") if is_profile else ("lat", "lon")
                    type_code = "i4" if row["dtype"] == "int32" else "f4"
                    fill = int(sentinel) if type_code == "i4" else float(sentinel)
                    var = nc.createVariable(name, type_code, dims, fill_value=fill)
                    var[:] = variables[name]
                    var.long_name = row["long_name"]
                    if row["units"].strip():
                        var.units = row["units"].strip()
                    var.missing_value = fill
                    description = row["physical_description"].strip()
                    if description:
                        var.description = description
                    if row["vertical_datum"].strip():
                        var.vertical_datum = row["vertical_datum"].strip()
    except Exception:
        # Avoid leaving a partial output that looks valid.
        if output.exists():
            output.unlink()
        raise
    return output


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--map-dir", type=Path, required=True, help="map directory containing params.txt and category 11 files")
    parser.add_argument("--config", type=Path, help="dataset JSON (default: <map-dir>/map_metadata.json)")
    parser.add_argument("--metadata-csv", type=Path, default=default_registry_path())
    parser.add_argument("--output", type=Path, help="NetCDF output (default: <map-dir>/river_network.nc)")
    parser.add_argument("--overwrite", action="store_true", help="replace an existing output file")
    args = parser.parse_args(argv)
    try:
        output = convert(args.map_dir, args.output or args.map_dir / "river_network.nc",
                         args.metadata_csv, args.config, args.overwrite)
    except (MapDataError, OSError) as exc:
        parser.error(str(exc))
    print(f"wrote {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
