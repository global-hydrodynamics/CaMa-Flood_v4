#!/usr/bin/env python3
"""Convert registered high-resolution CaMa-Flood Binary tiles to NetCDF4."""
from __future__ import annotations

import argparse
import array
import ctypes
import ctypes.util
import csv
import math
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


NC_CLOBBER = 0
NC_NETCDF4 = 0x1000
NC_GLOBAL = -1
NC_BYTE = 1
NC_SHORT = 3
NC_INT = 4
NC_FLOAT = 5
NC_DOUBLE = 6
NETCDF_CHUNK_EDGE = 256
TYPE_INFO = {
    "int8": (1, "b", ctypes.c_byte, NC_BYTE, "schar"),
    "int16": (2, "h", ctypes.c_short, NC_SHORT, "short"),
    "int32": (4, "i", ctypes.c_int, NC_INT, "int"),
    "float32": (4, "f", ctypes.c_float, NC_FLOAT, "float"),
}


class ConversionError(ValueError):
    """Invalid source, geometry, or registry information."""


def netcdf_library() -> ctypes.CDLL:
    candidates = [ctypes.util.find_library("netcdf")]
    nc_config = shutil.which("nc-config")
    if nc_config:
        libdir = subprocess.check_output([nc_config, "--libdir"], text=True).strip()
        candidates.extend(str(Path(libdir) / name) for name in ("libnetcdf.dylib", "libnetcdf.so"))
    candidates.extend(("libnetcdf.dylib", "libnetcdf.so"))
    for candidate in candidates:
        if candidate:
            try:
                return ctypes.CDLL(candidate)
            except OSError:
                pass
    raise RuntimeError("NetCDF C library not found; install NetCDF and make nc-config available")


def configure_api(lib: ctypes.CDLL) -> None:
    ci, sz, cp = ctypes.c_int, ctypes.c_size_t, ctypes.c_char_p
    lib.nc_strerror.argtypes = [ci]
    lib.nc_strerror.restype = cp
    lib.nc_create.argtypes = [cp, ci, ctypes.POINTER(ci)]
    lib.nc_def_dim.argtypes = [ci, cp, sz, ctypes.POINTER(ci)]
    lib.nc_def_var.argtypes = [ci, cp, ci, ci, ctypes.POINTER(ci), ctypes.POINTER(ci)]
    lib.nc_def_var_chunking.argtypes = [ci, ci, ci, ctypes.POINTER(sz)]
    lib.nc_def_var_deflate.argtypes = [ci, ci, ci, ci, ci]
    lib.nc_inq_varid.argtypes = [ci, cp, ctypes.POINTER(ci)]
    lib.nc_put_att_text.argtypes = [ci, ci, cp, sz, cp]
    lib.nc_put_att_schar.argtypes = [ci, ci, cp, ci, sz, ctypes.POINTER(ctypes.c_byte)]
    lib.nc_put_att_short.argtypes = [ci, ci, cp, ci, sz, ctypes.POINTER(ctypes.c_short)]
    lib.nc_put_att_int.argtypes = [ci, ci, cp, ci, sz, ctypes.POINTER(ctypes.c_int)]
    lib.nc_put_att_float.argtypes = [ci, ci, cp, ci, sz, ctypes.POINTER(ctypes.c_float)]
    lib.nc_put_var_double.argtypes = [ci, ci, ctypes.POINTER(ctypes.c_double)]
    lib.nc_put_var_double.restype = ci
    for suffix, ctype in (("schar", ctypes.c_byte), ("short", ctypes.c_short),
                          ("int", ctypes.c_int), ("float", ctypes.c_float)):
        ptr = ctypes.POINTER(ctype)
        lib.__getattr__(f"nc_put_vara_{suffix}").argtypes = [ci, ci, ctypes.POINTER(sz), ctypes.POINTER(sz), ptr]
        lib.__getattr__(f"nc_get_vara_{suffix}").argtypes = [ci, ci, ctypes.POINTER(sz), ctypes.POINTER(sz), ptr]
    lib.nc_enddef.argtypes = [ci]
    lib.nc_close.argtypes = [ci]


def check(lib: ctypes.CDLL, status: int, operation: str) -> None:
    if status:
        detail = lib.nc_strerror(status).decode("utf-8", "replace")
        raise RuntimeError(f"{operation}: {detail}")


def put_text(lib: ctypes.CDLL, ncid: int, varid: int, name: str, value: str) -> None:
    encoded = value.encode("utf-8")
    check(lib, lib.nc_put_att_text(ncid, varid, name.encode(), len(encoded), encoded), f"write {name}")


def array_pointer(data: array.array, ctype: type[ctypes._SimpleCData]):
    return (ctype * len(data)).from_buffer(data)


def configure_variable_storage(
    lib: ctypes.CDLL, ncid: int, varid: int, ny: int, nx: int, compression_level: int
) -> None:
    """Set bounded 2-D chunks and lossless shuffle/deflate filters."""
    chunks = (ctypes.c_size_t * 2)(min(NETCDF_CHUNK_EDGE, ny), min(NETCDF_CHUNK_EDGE, nx))
    check(lib, lib.nc_def_var_chunking(ncid, varid, 0, chunks), "set NetCDF chunking")
    deflate = int(compression_level > 0)
    check(
        lib,
        lib.nc_def_var_deflate(ncid, varid, 1, deflate, compression_level),
        "set NetCDF shuffle/deflate filters",
    )


def read_location(path: Path) -> list[dict[str, int | float | str]]:
    try:
        lines = [line.strip() for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]
    except OSError as exc:
        raise ConversionError(f"cannot read {path}: {exc}") from exc
    if len(lines) < 2:
        raise ConversionError(f"{path} must contain an area count, header, and area records")
    try:
        area_count = int(lines[0].split()[0])
    except (ValueError, IndexError) as exc:
        raise ConversionError(f"invalid area count in {path}") from exc
    expected_header = ["iarea", "carea", "west", "east", "south", "north", "nx", "ny", "csize"]
    if [item.lower() for item in lines[1].split()] != expected_header:
        raise ConversionError(f"{path} header must contain: {' '.join(expected_header)}")
    if len(lines) != area_count + 2:
        raise ConversionError(f"{path} declares {area_count} areas but contains {len(lines) - 2} records")

    areas = []
    for line_number, line in enumerate(lines[2:], 3):
        fields = line.split()
        if len(fields) != 9:
            raise ConversionError(f"{path}:{line_number}: expected 9 fields, found {len(fields)}")
        try:
            area = {
                "iarea": int(fields[0]), "carea": fields[1],
                "west": float(fields[2]), "east": float(fields[3]),
                "south": float(fields[4]), "north": float(fields[5]),
                "nx": int(fields[6]), "ny": int(fields[7]), "resolution": float(fields[8]),
            }
        except ValueError as exc:
            raise ConversionError(f"{path}:{line_number}: invalid numeric field") from exc
        nx, ny = int(area["nx"]), int(area["ny"])
        west, east = float(area["west"]), float(area["east"])
        south, north = float(area["south"]), float(area["north"])
        resolution = float(area["resolution"])
        if not area["carea"] or nx < 1 or ny < 1 or resolution <= 0:
            raise ConversionError(f"{path}:{line_number}: invalid area name, dimensions, or resolution")
        if east <= west or north <= south:
            raise ConversionError(f"{path}:{line_number}: invalid geographic extent")
        if not math.isclose((east - west) / nx, resolution, rel_tol=1e-6, abs_tol=1e-8):
            raise ConversionError(f"{path}:{line_number}: nx does not match extent and resolution")
        if not math.isclose((north - south) / ny, resolution, rel_tol=1e-6, abs_tol=1e-8):
            raise ConversionError(f"{path}:{line_number}: ny does not match extent and resolution")
        areas.append(area)
    if len({str(area["carea"]) for area in areas}) != len(areas):
        raise ConversionError(f"{path} contains duplicate carea names")
    return areas


def registry_rows(path: Path) -> list[dict[str, str]]:
    try:
        with path.open(newline="", encoding="utf-8-sig") as stream:
            return list(csv.DictReader(stream))
    except OSError as exc:
        raise ConversionError(f"cannot read registry {path}: {exc}") from exc


def rows_for_tile(rows: list[dict[str, str]], hires: str, carea: str, variable: str) -> list[dict[str, str]]:
    wanted = f"{hires}/{carea}.{variable}.bin"
    selected = []
    for row in rows:
        filenames = [item.strip() for item in (row.get("binary_filename") or "").split(";")]
        if any(item.replace("<HIRES>", hires).replace("<AREA>", carea) == wanted for item in filenames):
            selected.append(row)
    if not selected:
        raise ConversionError(f"{wanted} has no registered high-resolution variable; it will not be converted")
    return selected


def validate_tile(path: Path, area: dict[str, int | float | str], rows: list[dict[str, str]]) -> list[dict[str, str]]:
    rows = sorted(rows, key=lambda row: int(row.get("record_index") or "1"))
    dtypes = {row.get("dtype", "").strip().lower() for row in rows}
    dimensions = {row.get("dimensions", "").strip() for row in rows}
    record_counts = {int(row.get("record_count") or "1") for row in rows}
    if len(dtypes) != 1 or len(dimensions) != 1 or len(record_counts) != 1:
        raise ConversionError(f"{path.name}: registry records disagree on dtype, dimensions, or record count")
    dtype = next(iter(dtypes))
    if dtype not in TYPE_INFO:
        raise ConversionError(f"{path.name}: unsupported registered dtype {dtype!r}")
    if next(iter(dimensions)) != "NX_HIRES,NY_HIRES":
        raise ConversionError(f"{path.name}: unsupported registered dimensions {next(iter(dimensions))!r}")
    record_count = next(iter(record_counts))
    indices = [int(row.get("record_index") or "1") for row in rows]
    if record_count < 1 or sorted(indices) != list(range(1, record_count + 1)):
        raise ConversionError(f"{path.name}: registry record indices must cover 1..{record_count}")
    if any(int(row.get("record_count") or "1") != record_count for row in rows):
        raise ConversionError(f"{path.name}: registry record_count is inconsistent")
    itemsize = TYPE_INFO[dtype][0]
    expected = int(area["nx"]) * int(area["ny"]) * record_count * itemsize
    actual = path.stat().st_size
    if actual != expected:
        raise ConversionError(f"{path} has {actual} bytes; location.txt and registry require {expected}")
    return rows


def put_missing_value(lib: ctypes.CDLL, ncid: int, varid: int, dtype: str, value: str, attribute: str) -> None:
    itemsize, _, ctype, nc_type, suffix = TYPE_INFO[dtype]
    if not value.strip():
        return
    number = float(value)
    if dtype.startswith("int"):
        if not number.is_integer():
            raise ConversionError(f"{dtype} missing value must be an integer: {value}")
        bits = itemsize * 8
        if not -(2 ** (bits - 1)) <= int(number) < 2 ** (bits - 1):
            raise ConversionError(f"missing value {value} is outside the range of {dtype}")
    stored = ctype(int(number) if dtype.startswith("int") else number)
    function = getattr(lib, f"nc_put_att_{suffix}")
    check(lib, function(ncid, varid, attribute.encode(), nc_type, 1, ctypes.byref(stored)), f"write {attribute}")


def native_chunk(raw: bytes, dtype: str) -> array.array:
    itemsize, code, _, _, _ = TYPE_INFO[dtype]
    data = array.array(code)
    if data.itemsize != itemsize:
        raise RuntimeError(f"this Python platform uses {data.itemsize}-byte {dtype} buffers")
    data.frombytes(raw)
    if itemsize > 1 and sys.byteorder != "little":
        data.byteswap()
    return data


def write_coordinates(lib: ctypes.CDLL, ncid: int, area: dict[str, int | float | str], lon_var: int, lat_var: int) -> None:
    nx, ny = int(area["nx"]), int(area["ny"])
    west, north = float(area["west"]), float(area["north"])
    resolution = float(area["resolution"])
    lon = array.array("d", (west + (index + 0.5) * resolution for index in range(nx)))
    lat = array.array("d", (north - (index + 0.5) * resolution for index in range(ny)))
    put_text(lib, ncid, lon_var, "units", "degrees_east")
    put_text(lib, ncid, lon_var, "standard_name", "longitude")
    put_text(lib, ncid, lat_var, "units", "degrees_north")
    put_text(lib, ncid, lat_var, "standard_name", "latitude")
    check(lib, lib.nc_put_var_double(ncid, lon_var, array_pointer(lon, ctypes.c_double)), "write longitude")
    check(lib, lib.nc_put_var_double(ncid, lat_var, array_pointer(lat, ctypes.c_double)), "write latitude")


def write_netcdf(
    source_path: Path,
    output_path: Path,
    area: dict[str, int | float | str],
    rows: list[dict[str, str]],
    compression_level: int,
) -> None:
    nx, ny = int(area["nx"]), int(area["ny"])
    dtype = rows[0]["dtype"].strip().lower()
    itemsize, _, ctype, nc_type, suffix = TYPE_INFO[dtype]
    record_bytes = nx * ny * itemsize
    fd, temporary_name = tempfile.mkstemp(prefix=f".{output_path.name}.", suffix=".tmp", dir=output_path.parent)
    os.close(fd)
    temporary_path = Path(temporary_name)
    lib = netcdf_library()
    configure_api(lib)
    ncid = ctypes.c_int()
    created = False
    try:
        check(lib, lib.nc_create(os.fsencode(temporary_path), NC_CLOBBER | NC_NETCDF4, ctypes.byref(ncid)), "create NetCDF4 file")
        created = True
        lon_dim, lat_dim, lon_var, lat_var = (ctypes.c_int() for _ in range(4))
        check(lib, lib.nc_def_dim(ncid.value, b"lon", nx, ctypes.byref(lon_dim)), "define longitude dimension")
        check(lib, lib.nc_def_dim(ncid.value, b"lat", ny, ctypes.byref(lat_dim)), "define latitude dimension")
        check(lib, lib.nc_def_var(ncid.value, b"lon", NC_DOUBLE, 1, ctypes.byref(lon_dim), ctypes.byref(lon_var)), "define longitude")
        check(lib, lib.nc_def_var(ncid.value, b"lat", NC_DOUBLE, 1, ctypes.byref(lat_dim), ctypes.byref(lat_var)), "define latitude")
        put_text(lib, ncid.value, NC_GLOBAL, "Conventions", "CF-1.10")
        put_text(lib, ncid.value, NC_GLOBAL, "title", f"CaMa-Flood high-resolution map tile {source_path.name}")
        put_text(lib, ncid.value, NC_GLOBAL, "carea", str(area["carea"]))
        put_text(lib, ncid.value, NC_GLOBAL, "grid_resolution_degrees", str(area["resolution"]))
        variable_ids: dict[str, int] = {}
        for row in rows:
            name = row["name"].strip()
            varid = ctypes.c_int()
            dimids = (ctypes.c_int * 2)(lat_dim.value, lon_dim.value)
            check(lib, lib.nc_def_var(ncid.value, name.encode(), nc_type, 2, dimids, ctypes.byref(varid)), f"define {name}")
            variable_ids[name] = varid.value
            configure_variable_storage(lib, ncid.value, varid.value, ny, nx, compression_level)
            put_text(lib, ncid.value, varid.value, "long_name", row.get("long_name", name).strip())
            units = (row.get("units") or "").strip()
            if units:
                put_text(lib, ncid.value, varid.value, "units", units)
            special = (row.get("special_values") or "").strip()
            if special:
                put_text(lib, ncid.value, varid.value, "special_values", special)
            missing = (row.get("missing_value") or "").strip()
            if missing:
                put_missing_value(lib, ncid.value, varid.value, dtype, missing, "missing_value")
                put_missing_value(lib, ncid.value, varid.value, dtype, missing, "_FillValue")
        check(lib, lib.nc_enddef(ncid.value), "finish NetCDF definition")
        write_coordinates(lib, ncid.value, area, lon_var.value, lat_var.value)

        # Match the bounded input chunks to the NetCDF storage chunks.
        max_x = min(nx, NETCDF_CHUNK_EDGE)
        max_y = min(ny, NETCDF_CHUNK_EDGE)
        with source_path.open("rb") as source:
            for row in rows:
                name = row["name"].strip()
                record = int(row.get("record_index") or "1") - 1
                varid = variable_ids[name]
                writer = getattr(lib, f"nc_put_vara_{suffix}")
                reader = getattr(lib, f"nc_get_vara_{suffix}")
                for y0 in range(0, ny, max_y):
                    rows_in_chunk = min(max_y, ny - y0)
                    for x0 in range(0, nx, max_x):
                        cols_in_chunk = min(max_x, nx - x0)
                        raw = bytearray()
                        for y in range(y0, y0 + rows_in_chunk):
                            offset = record * record_bytes + (y * nx + x0) * itemsize
                            source.seek(offset)
                            part = source.read(cols_in_chunk * itemsize)
                            if len(part) != cols_in_chunk * itemsize:
                                raise ConversionError(f"{source_path} ended while reading {name} at row {y}")
                            raw.extend(part)
                        data = native_chunk(bytes(raw), dtype)
                        start = (ctypes.c_size_t * 2)(y0, x0)
                        count = (ctypes.c_size_t * 2)(rows_in_chunk, cols_in_chunk)
                        data_pointer = array_pointer(data, ctype)
                        check(lib, writer(ncid.value, varid, start, count, data_pointer), f"write {name}")
                        readback = array.array(data.typecode, [0]) * len(data)
                        check(lib, reader(ncid.value, varid, start, count, array_pointer(readback, ctype)), f"verify {name}")
                        if readback.tobytes() != data.tobytes():
                            raise RuntimeError(f"NetCDF readback differs from Binary values for {name}")
        check(lib, lib.nc_close(ncid.value), "close NetCDF file")
        created = False
        if output_path.exists():
            raise FileExistsError(f"{output_path} appeared during conversion")
        os.replace(temporary_path, output_path)
    except Exception:
        if created:
            lib.nc_close(ncid.value)
        temporary_path.unlink(missing_ok=True)
        raise


def convert_variable(
    map_dir: Path,
    hires: str,
    variable: str,
    registry_path: Path,
    force: bool,
    compression_level: int,
) -> int:
    hires_dir = map_dir / hires
    areas = read_location(hires_dir / "location.txt")
    rows = registry_rows(registry_path)
    output_dir = map_dir / f"{hires}_nc"
    converted = skipped = 0
    for area in areas:
        source = hires_dir / f"{area['carea']}.{variable}.bin"
        selected = rows_for_tile(rows, hires, str(area["carea"]), variable)
        if not source.is_file():
            raise ConversionError(f"registered tile input is missing: {source}")
        selected = validate_tile(source, area, selected)
        output = output_dir / f"{area['carea']}.{variable}.nc"
        if output.exists() and not force:
            print(f"SKIP {output}: already exists (pass --force to overwrite)")
            skipped += 1
            continue
        output_dir.mkdir(parents=True, exist_ok=True)
        if force and output.exists():
            # Build the replacement completely before atomically replacing the old file.
            replacement = output.with_name(f".{output.name}.replacement")
            replacement.unlink(missing_ok=True)
            try:
                write_netcdf(source, replacement, area, selected, compression_level)
                os.replace(replacement, output)
            except Exception:
                replacement.unlink(missing_ok=True)
                raise
        else:
            write_netcdf(source, output, area, selected, compression_level)
        print(f"SUCCESS {source} -> {output}")
        converted += 1
    if converted == 0 and skipped == 0:
        raise ConversionError(f"no tiles found for {hires} {variable}")
    return converted


def main() -> int:
    repo_root = Path(__file__).resolve().parents[3]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--map-dir", type=Path, required=True, help="map root containing high-resolution directories")
    parser.add_argument("--hires", required=True, help="high-resolution directory tag, such as 1min")
    parser.add_argument("--variable", required=True, help="registered high-resolution Binary file stem, such as catmxy")
    parser.add_argument("--registry", type=Path, default=repo_root / "variable_table/map_variables.csv", help="map variable registry")
    parser.add_argument("--force", action="store_true", help="replace existing NetCDF files")
    parser.add_argument(
        "--compression-level", type=int, choices=range(10), default=6,
        help="lossless NetCDF4 zlib/deflate level (0 disables deflate; default: 6)",
    )
    args = parser.parse_args()
    args.map_dir = args.map_dir.resolve()
    args.registry = args.registry.resolve()
    try:
        count = convert_variable(
            args.map_dir, args.hires, args.variable, args.registry, args.force, args.compression_level
        )
    except (OSError, ValueError, RuntimeError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1
    print(f"SUCCESS: converted {count} tile(s) for {args.hires} {args.variable}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
