#!/usr/bin/env python3
"""Convert one CaMa-Flood GrADS binary map to an individual NetCDF file.

This converter uses Python's standard library and the NetCDF C library.  The
binary grids are stored little-endian, with the x index varying fastest.
"""

from __future__ import annotations

import argparse
import array
import ctypes
import ctypes.util
import csv
import os
from pathlib import Path
import shutil
import subprocess
import sys


NC_CLOBBER = 0
NC_NETCDF4 = 0x1000
NC_GLOBAL = -1
NC_INT = 4
NC_FLOAT = 5
NC_DOUBLE = 6


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
    ci = ctypes.c_int
    sz = ctypes.c_size_t
    charp = ctypes.c_char_p
    lib.nc_strerror.argtypes = [ci]
    lib.nc_strerror.restype = charp
    lib.nc_create.argtypes = [charp, ci, ctypes.POINTER(ci)]
    lib.nc_def_dim.argtypes = [ci, charp, sz, ctypes.POINTER(ci)]
    lib.nc_def_var.argtypes = [ci, charp, ci, ci, ctypes.POINTER(ci), ctypes.POINTER(ci)]
    lib.nc_inq_varid.argtypes = [ci, charp, ctypes.POINTER(ci)]
    lib.nc_put_att_text.argtypes = [ci, ci, charp, sz, charp]
    lib.nc_put_att_float.argtypes = [ci, ci, charp, ci, sz, ctypes.POINTER(ctypes.c_float)]
    lib.nc_put_att_int.argtypes = [ci, ci, charp, ci, sz, ctypes.POINTER(ci)]
    lib.nc_enddef.argtypes = [ci]
    lib.nc_put_vara_float.argtypes = [ci, ci, ctypes.POINTER(sz), ctypes.POINTER(sz), ctypes.POINTER(ctypes.c_float)]
    lib.nc_put_vara_int.argtypes = [ci, ci, ctypes.POINTER(sz), ctypes.POINTER(sz), ctypes.POINTER(ci)]
    lib.nc_put_var_double.argtypes = [ci, ci, ctypes.POINTER(ctypes.c_double)]
    lib.nc_get_vara_float.argtypes = [ci, ci, ctypes.POINTER(sz), ctypes.POINTER(sz), ctypes.POINTER(ctypes.c_float)]
    lib.nc_get_vara_int.argtypes = [ci, ci, ctypes.POINTER(sz), ctypes.POINTER(sz), ctypes.POINTER(ci)]
    lib.nc_close.argtypes = [ci]


def check(lib: ctypes.CDLL, status: int, operation: str) -> None:
    if status:
        message = lib.nc_strerror(status).decode("utf-8", "replace")
        raise RuntimeError(f"{operation}: {message}")


def read_params(path: Path) -> tuple[int, int, int, float, float, float, float, float]:
    lines = path.read_text(encoding="utf-8").splitlines()
    if len(lines) < 8:
        raise ValueError(f"{path} must contain NX, NY, NLFP, resolution, west, east, south, north")
    values = [line.split()[0] for line in lines[:8]]
    return (int(values[0]), int(values[1]), int(values[2]), *(float(v) for v in values[3:8]))


def registry_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as stream:
        return list(csv.DictReader(stream))


def registered_base_filenames(row: dict[str, str]) -> list[str]:
    """Return literal binary filenames that can occur in a map root."""
    value = (row.get("binary_filename") or "").strip()
    filenames = []
    for item in value.split(";"):
        item = item.strip()
        if not item or Path(item).name != item or any(char in item for char in "*?<>[]"):
            continue
        if item.endswith(".bin"):
            filenames.append(item)
    return filenames


def conversion_support(rows: list[dict[str, str]]) -> tuple[bool, str]:
    """Check whether all registry rows for one file fit this raster converter."""
    dtypes = {row.get("dtype", "").strip() for row in rows}
    dimensions = {row.get("dimensions", "").strip() for row in rows}
    if not dtypes <= {"float32", "int32"}:
        return False, f"unsupported storage type(s): {', '.join(sorted(dtypes))}"
    if len(dimensions) != 1:
        return False, "registry rows declare different dimensions"
    dimension = next(iter(dimensions))
    if dimension not in {"NX,NY", "NX,NY,NLFP"}:
        return False, f"unsupported dimensions: {dimension or 'blank'}"
    if dimension == "NX,NY,NLFP" and len(rows) != 1:
        return False, "layered files require one variable row"
    return True, ""


def generate_conversion_list(map_dir: Path, registry_path: Path, list_path: Path) -> int:
    """Write supported registered base-map binaries in registry workflow order."""
    rows = registry_rows(registry_path)
    rows_by_filename: dict[str, list[tuple[int, dict[str, str]]]] = {}
    for row_index, row in enumerate(rows):
        for filename in registered_base_filenames(row):
            rows_by_filename.setdefault(filename, []).append((row_index, row))

    binary_files = {path.name for path in map_dir.glob("*.bin") if path.is_file()}
    candidates = []
    for filename in sorted(binary_files):
        file_rows = rows_by_filename.get(filename, [])
        if not file_rows:
            print(f"WARNING: {filename} is not registered; it will not be listed.")
            continue

        supported, reason = conversion_support([row for _, row in file_rows])
        if not supported:
            print(f"WARNING: {filename} is registered but not supported: {reason}.")
            continue

        chosen_index, chosen_row = min(
            file_rows,
            key=lambda item: (int(item[1]["category_id"]), item[0]),
        )
        category_id = int(chosen_row["category_id"])
        category = (chosen_row.get("category") or "Registered map products").strip()
        candidates.append((category_id, chosen_index, filename, category))

    candidates.sort(key=lambda item: (item[0], item[1]))
    list_path.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        "# Binary-to-NetCDF conversion list",
        "# Generated from map_variables.csv",
        "# Comment out files that should not be converted.",
    ]
    previous_category = None
    for category_id, _, filename, category in candidates:
        if category_id != previous_category:
            lines.extend(("", f"# [{category_id}] {category}"))
            previous_category = category_id
        lines.append(filename)
    list_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Generated {len(candidates)} conversion candidates: {list_path}")
    return len(candidates)


def select_variables(filename: str, rows: list[dict[str, str]], requested: list[str] | None) -> list[dict[str, str]]:
    matches = [row for row in rows if Path(row.get("binary_filename", "")).name == filename]
    if not matches and requested:
        matches = [row for row in rows if row.get("name") in requested]
    if not matches:
        raise ValueError(f"{filename} is not registered in variable_table/map_variables.csv; use --variables with a registered variable")
    if filename.startswith("nextxy"):
        for row in matches:
            row["_individual_name"] = "nextx" if row.get("record_index") == "1" else "nexty"
    else:
        for row in matches:
            row["_individual_name"] = row.get("name", "")
    if requested:
        wanted = set(requested)
        selected = [row for row in matches if row["_individual_name"] in wanted or row.get("name") in wanted]
        found = {row["_individual_name"] for row in selected} | {row.get("name", "") for row in selected}
        missing = wanted - found
        if missing:
            raise ValueError(f"variables not registered for {filename}: {', '.join(sorted(missing))}")
        return selected
    return matches


def native_array(raw: bytes, dtype: str, byte_order: str) -> array.array:
    code = {"float32": "f", "int32": "i"}.get(dtype)
    if code is None:
        raise ValueError(f"unsupported registry dtype {dtype!r}; expected float32 or int32")
    data = array.array(code)
    if data.itemsize != 4:
        raise RuntimeError(f"this Python platform uses {data.itemsize}-byte {dtype} buffers")
    data.frombytes(raw)
    source_little = byte_order == "little" or (byte_order == "native" and sys.byteorder == "little")
    if byte_order == "big" or (byte_order == "native" and sys.byteorder == "big"):
        source_little = False
    if source_little != (sys.byteorder == "little"):
        data.byteswap()
    return data


def pointer(data: array.array, ctype: type[ctypes._SimpleCData]):
    return (ctype * len(data)).from_buffer(data)


def put_text(lib: ctypes.CDLL, ncid: int, varid: int, name: str, value: str) -> None:
    encoded = value.encode("utf-8")
    check(lib, lib.nc_put_att_text(ncid, varid, name.encode(), len(encoded), encoded), f"write attribute {name}")


def convert(args: argparse.Namespace) -> None:
    nx, ny, nlfp, resolution, west, east, south, north = read_params(args.map_dir / "params.txt")
    rows = select_variables(args.input.name, registry_rows(args.registry), args.variables)
    rows.sort(key=lambda row: int(row.get("record_index") or "1"))
    records = nlfp if rows[0].get("dimensions") == "NX,NY,NLFP" else int(rows[0].get("record_count") or "1")
    expected_bytes = nx * ny * records * 4
    actual_bytes = args.input.stat().st_size
    if actual_bytes != expected_bytes:
        raise ValueError(f"{args.input} has {actual_bytes} bytes; registry and params.txt require {expected_bytes}")
    if args.output.resolve() == args.input.resolve():
        raise ValueError("output path must not be the input Binary file")
    if args.output.exists() and not args.force:
        raise FileExistsError(f"{args.output} exists; pass --force to replace it")
    args.output.parent.mkdir(parents=True, exist_ok=True)

    lib = netcdf_library()
    configure_api(lib)
    ci = ctypes.c_int
    ncid = ci()
    check(lib, lib.nc_create(os.fsencode(args.output), NC_CLOBBER | NC_NETCDF4, ctypes.byref(ncid)), "create NetCDF file")
    try:
        lon_dim, lat_dim, layer_dim = ci(), ci(), ci()
        check(lib, lib.nc_def_dim(ncid.value, b"lon", nx, ctypes.byref(lon_dim)), "define lon dimension")
        check(lib, lib.nc_def_dim(ncid.value, b"lat", ny, ctypes.byref(lat_dim)), "define lat dimension")
        check(lib, lib.nc_def_dim(ncid.value, b"layer", nlfp, ctypes.byref(layer_dim)), "define layer dimension")
        lon_var, lat_var = ci(), ci()
        check(
            lib,
            lib.nc_def_var(
                ncid.value, b"lon", NC_DOUBLE, 1, ctypes.byref(lon_dim), ctypes.byref(lon_var)
            ),
            "define lon coordinate",
        )
        check(
            lib,
            lib.nc_def_var(
                ncid.value, b"lat", NC_DOUBLE, 1, ctypes.byref(lat_dim), ctypes.byref(lat_var)
            ),
            "define lat coordinate",
        )
        put_text(lib, ncid.value, NC_GLOBAL, "Conventions", "CF-1.10")
        put_text(lib, ncid.value, NC_GLOBAL, "title", f"CaMa-Flood map variable(s) converted from {args.input.name}")
        for row in rows:
            name = row["_individual_name"]
            dtype = row.get("dtype", "")
            nc_type = NC_INT if dtype == "int32" else NC_FLOAT
            dimensions = row.get("dimensions", "")
            if dimensions == "NX,NY,NLFP":
                dimids = (ci * 3)(layer_dim.value, lat_dim.value, lon_dim.value)
                rank = 3
            elif dimensions == "NX,NY":
                dimids = (ci * 2)(lat_dim.value, lon_dim.value)
                rank = 2
            else:
                raise ValueError(f"{name}: unsupported registered dimensions {dimensions!r}")
            varid = ci()
            check(lib, lib.nc_def_var(ncid.value, name.encode(), nc_type, rank, dimids, ctypes.byref(varid)), f"define {name}")
            put_text(lib, ncid.value, varid.value, "long_name", row.get("long_name", name))
            units = row.get("units", "")
            if units:
                put_text(lib, ncid.value, varid.value, "units", units)
            sentinel = row.get("missing_value", "")
            if sentinel:
                if dtype == "int32":
                    value = ci(int(sentinel))
                    check(
                        lib,
                        lib.nc_put_att_int(
                            ncid.value,
                            varid.value,
                            b"missing_value",
                            NC_INT,
                            1,
                            ctypes.byref(value),
                        ),
                        f"write {name} missing_value",
                    )
                    check(
                        lib,
                        lib.nc_put_att_int(
                            ncid.value,
                            varid.value,
                            b"_FillValue",
                            NC_INT,
                            1,
                            ctypes.byref(value),
                        ),
                        f"write {name} _FillValue",
                    )
                else:
                    value = ctypes.c_float(float(sentinel))
                    check(
                        lib,
                        lib.nc_put_att_float(
                            ncid.value,
                            varid.value,
                            b"missing_value",
                            NC_FLOAT,
                            1,
                            ctypes.byref(value),
                        ),
                        f"write {name} missing_value",
                    )
                    check(
                        lib,
                        lib.nc_put_att_float(
                            ncid.value,
                            varid.value,
                            b"_FillValue",
                            NC_FLOAT,
                            1,
                            ctypes.byref(value),
                        ),
                        f"write {name} _FillValue",
                    )
        check(lib, lib.nc_enddef(ncid.value), "finish NetCDF definition")

        lon_values = array.array("d", (west + (i + 0.5) * resolution for i in range(nx)))
        lat_values = array.array("d", (north - (i + 0.5) * resolution for i in range(ny)))
        put_text(lib, ncid.value, lon_var.value, "units", "degrees_east")
        put_text(lib, ncid.value, lon_var.value, "standard_name", "longitude")
        put_text(lib, ncid.value, lat_var.value, "units", "degrees_north")
        put_text(lib, ncid.value, lat_var.value, "standard_name", "latitude")
        check(
            lib,
            lib.nc_put_var_double(
                ncid.value, lon_var.value, pointer(lon_values, ctypes.c_double)
            ),
            "write lon coordinate",
        )
        check(
            lib,
            lib.nc_put_var_double(
                ncid.value, lat_var.value, pointer(lat_values, ctypes.c_double)
            ),
            "write lat coordinate",
        )

        plane_bytes = nx * ny * 4
        with args.input.open("rb") as source:
            for row in rows:
                name = row["_individual_name"]
                layers = nlfp if row.get("dimensions") == "NX,NY,NLFP" else 1
                record = int(row.get("record_index") or "1")
                varid = ci()
                check(lib, lib.nc_inq_varid(ncid.value, name.encode(), ctypes.byref(varid)), f"find {name} for writing")
                if layers == 1:
                    start = (ctypes.c_size_t * 2)(0, 0)
                    count = (ctypes.c_size_t * 2)(ny, nx)
                for layer in range(1, layers + 1):
                    file_record = layer if layers > 1 else record
                    source.seek((file_record - 1) * plane_bytes)
                    raw = source.read(plane_bytes)
                    if len(raw) != plane_bytes:
                        raise ValueError(f"{args.input} ended before record {file_record}")
                    data = native_array(raw, row["dtype"], args.byte_order)
                    ctype = ctypes.c_int if row["dtype"] == "int32" else ctypes.c_float
                    if layers > 1:
                        start = (ctypes.c_size_t * 3)(layer - 1, 0, 0)
                        count = (ctypes.c_size_t * 3)(1, ny, nx)
                    writer = lib.nc_put_vara_int if row["dtype"] == "int32" else lib.nc_put_vara_float
                    reader = lib.nc_get_vara_int if row["dtype"] == "int32" else lib.nc_get_vara_float
                    check(
                        lib,
                        writer(ncid.value, varid.value, start, count, pointer(data, ctype)),
                        f"write {name} record {layer}",
                    )
                    decoded = array.array(data.typecode, [0]) * len(data)
                    check(
                        lib,
                        reader(ncid.value, varid.value, start, count, pointer(decoded, ctype)),
                        f"read back {name} record {layer}",
                    )
                    if decoded.tobytes() != data.tobytes():
                        raise RuntimeError(f"round-trip values differ for {name} record {layer}")
    finally:
        check(lib, lib.nc_close(ncid.value), "close NetCDF file")
    print(f"Converted and verified {', '.join(row['_individual_name'] for row in rows)}: {args.output}")


def main() -> int:
    root = Path(__file__).resolve().parents[3]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--map-dir", type=Path, required=True, help="map directory containing params.txt")
    parser.add_argument("--input", type=Path, help="registered Binary map file")
    parser.add_argument("--output", type=Path, help="destination NetCDF file")
    parser.add_argument("--registry", type=Path, default=root / "variable_table/map_variables.csv", help="map variable registry")
    parser.add_argument("--generate-list", action="store_true", help="write a human-editable base-map conversion list")
    parser.add_argument("--list-file", type=Path, help="conversion-list path; defaults to <map-dir>/netcdf/bin2nc_list.txt")
    parser.add_argument("--variables", help="comma-separated registry variable names; defaults to all variables in the file")
    parser.add_argument(
        "--byte-order",
        choices=("little", "big", "native"),
        default="little",
        help="Binary byte order (standard distributed maps are little-endian)",
    )
    parser.add_argument("--force", action="store_true", help="replace an existing NetCDF output file")
    args = parser.parse_args()
    args.map_dir = args.map_dir.resolve()

    if args.generate_list:
        if args.input or args.output or args.variables or args.force:
            parser.error("--generate-list cannot be combined with conversion options")
        args.registry = args.registry.resolve()
        args.list_file = (args.list_file or args.map_dir / "netcdf/bin2nc_list.txt").resolve()
        try:
            generate_conversion_list(args.map_dir, args.registry, args.list_file)
        except (OSError, ValueError) as error:
            print(f"ERROR: {error}", file=sys.stderr)
            return 1
        return 0

    if args.input is None or args.output is None or args.list_file is not None:
        parser.error("conversion requires --input and --output; --list-file is only for --generate-list")
    args.input = args.input.resolve()
    args.output = args.output.resolve()
    args.registry = args.registry.resolve()
    args.variables = [item.strip() for item in args.variables.split(",") if item.strip()] if args.variables else None
    try:
        convert(args)
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
