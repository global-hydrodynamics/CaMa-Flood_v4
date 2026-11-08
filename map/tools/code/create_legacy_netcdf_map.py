#!/usr/bin/env python3
"""Assemble individual map NetCDF files into the legacy CaMa-Flood containers."""

from __future__ import annotations

import argparse
import array
import ctypes
import ctypes.util
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


NC_NOWRITE = 0
NC_CLOBBER = 0
NC_NETCDF4 = 0x1000
NC_GLOBAL = -1
NC_CHAR, NC_INT, NC_FLOAT, NC_DOUBLE = 2, 4, 5, 6
TYPE_ARRAY = {NC_INT: ("i", ctypes.c_int), NC_FLOAT: ("f", ctypes.c_float), NC_DOUBLE: ("d", ctypes.c_double)}


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


def configure(lib: ctypes.CDLL) -> None:
    ci, sz, cp, vp = ctypes.c_int, ctypes.c_size_t, ctypes.c_char_p, ctypes.c_void_p
    lib.nc_strerror.argtypes = [ci]
    lib.nc_strerror.restype = cp
    lib.nc_open.argtypes = [cp, ci, ctypes.POINTER(ci)]
    lib.nc_create.argtypes = [cp, ci, ctypes.POINTER(ci)]
    lib.nc_close.argtypes = [ci]
    lib.nc_inq_dimid.argtypes = [ci, cp, ctypes.POINTER(ci)]
    lib.nc_inq_dimlen.argtypes = [ci, ci, ctypes.POINTER(sz)]
    lib.nc_inq_dimname.argtypes = [ci, ci, cp]
    lib.nc_def_dim.argtypes = [ci, cp, sz, ctypes.POINTER(ci)]
    lib.nc_inq_varid.argtypes = [ci, cp, ctypes.POINTER(ci)]
    lib.nc_inq_var.argtypes = [ci, ci, cp, ctypes.POINTER(ci), ctypes.POINTER(ci), ctypes.POINTER(ci), ctypes.POINTER(ci)]
    lib.nc_def_var.argtypes = [ci, cp, ci, ci, ctypes.POINTER(ci), ctypes.POINTER(ci)]
    lib.nc_def_var_deflate.argtypes = [ci, ci, ci, ci, ci]
    lib.nc_inq_natts.argtypes = [ci, ctypes.POINTER(ci)]
    lib.nc_inq_attname.argtypes = [ci, ci, ci, cp]
    lib.nc_inq_att.argtypes = [ci, ci, cp, ctypes.POINTER(ci), ctypes.POINTER(sz)]
    lib.nc_get_att_text.argtypes = [ci, ci, cp, cp]
    lib.nc_put_att_text.argtypes = [ci, ci, cp, sz, cp]
    for suffix, argtype in (("int", vp), ("float", vp), ("double", vp)):
        getattr(lib, f"nc_get_att_{suffix}").argtypes = [ci, ci, cp, argtype]
        getattr(lib, f"nc_put_att_{suffix}").argtypes = [ci, ci, cp, ci, sz, argtype]
    for suffix in ("int", "float", "double"):
        getattr(lib, f"nc_get_var_{suffix}").argtypes = [ci, ci, vp]
        getattr(lib, f"nc_put_var_{suffix}").argtypes = [ci, ci, vp]
    lib.nc_enddef.argtypes = [ci]


def check(lib: ctypes.CDLL, status: int, action: str) -> None:
    if status:
        raise RuntimeError(f"{action}: {lib.nc_strerror(status).decode('utf-8', 'replace')}")


def read_map_shape(path: Path) -> tuple[int, int, int]:
    lines = path.read_text(encoding="utf-8").splitlines()
    if len(lines) < 3:
        raise ValueError(f"{path} must contain NX, NY, and NLFP on its first three lines")
    try:
        return tuple(int(lines[index].split()[0]) for index in range(3))
    except (IndexError, ValueError) as exc:
        raise ValueError(f"could not read NX, NY, NLFP from {path}") from exc


def cstring(value: str) -> ctypes.Array:
    return ctypes.create_string_buffer(value.encode("utf-8"), 256)


def read_var(lib: ctypes.CDLL, path: Path, name: str) -> dict:
    ncid = ctypes.c_int()
    check(lib, lib.nc_open(os.fsencode(path), NC_NOWRITE, ctypes.byref(ncid)), f"open {path}")
    try:
        varid = ctypes.c_int()
        check(lib, lib.nc_inq_varid(ncid.value, name.encode(), ctypes.byref(varid)), f"find {name} in {path.name}")
        varname, xtype, ndims, natts = cstring(""), ctypes.c_int(), ctypes.c_int(), ctypes.c_int()
        dimids = (ctypes.c_int * 16)()
        check(lib, lib.nc_inq_var(ncid.value, varid.value, varname, ctypes.byref(xtype), ctypes.byref(ndims), dimids, ctypes.byref(natts)), f"inspect {name}")
        if xtype.value not in TYPE_ARRAY and xtype.value != NC_CHAR:
            raise ValueError(f"{path.name}:{name} uses unsupported NetCDF type {xtype.value}")
        dimensions = []
        for dimid in dimids[:ndims.value]:
            dimname = cstring("")
            length = ctypes.c_size_t()
            check(lib, lib.nc_inq_dimname(ncid.value, dimid, dimname), f"read {name} dimension name")
            check(lib, lib.nc_inq_dimlen(ncid.value, dimid, ctypes.byref(length)), f"read {name} dimension length")
            dimensions.append((dimname.value.decode(), length.value))
        count = 1
        for _, length in dimensions:
            count *= length
        if xtype.value == NC_CHAR:
            raise ValueError(f"{path.name}:{name} is text, expected a numeric map variable")
        code, ctype = TYPE_ARRAY[xtype.value]
        data = array.array(code, [0]) * count
        getter = {NC_INT: "int", NC_FLOAT: "float", NC_DOUBLE: "double"}[xtype.value]
        check(lib, getattr(lib, f"nc_get_var_{getter}")(ncid.value, varid.value, ctypes.c_void_p(data.buffer_info()[0])), f"read {name} from {path.name}")
        attrs = []
        for index in range(natts.value):
            attname = cstring("")
            attype, length = ctypes.c_int(), ctypes.c_size_t()
            check(lib, lib.nc_inq_attname(ncid.value, varid.value, index, attname), f"inspect {name} attribute")
            decoded_name = attname.value.decode()
            check(lib, lib.nc_inq_att(ncid.value, varid.value, decoded_name.encode(), ctypes.byref(attype), ctypes.byref(length)), f"inspect {decoded_name}")
            if attype.value == NC_CHAR:
                value = ctypes.create_string_buffer(length.value + 1)
                check(lib, lib.nc_get_att_text(ncid.value, varid.value, decoded_name.encode(), value), f"read {decoded_name}")
                attrs.append((decoded_name, attype.value, value.value.decode("utf-8", "replace")))
            elif attype.value in TYPE_ARRAY:
                attr_code, attr_ctype = TYPE_ARRAY[attype.value]
                values = array.array(attr_code, [0]) * length.value
                getter = {NC_INT: "int", NC_FLOAT: "float", NC_DOUBLE: "double"}[attype.value]
                check(
                    lib,
                    getattr(lib, f"nc_get_att_{getter}")(
                        ncid.value,
                        varid.value,
                        decoded_name.encode(),
                        ctypes.c_void_p(values.buffer_info()[0]),
                    ),
                    f"read {decoded_name}",
                )
                attrs.append((decoded_name, attype.value, values))
            else:
                raise ValueError(f"{path.name}:{name} has unsupported attribute type {attype.value}")
        return {"name": name, "type": xtype.value, "dimensions": dimensions, "data": data, "attrs": attrs, "ctype": ctype}
    finally:
        check(lib, lib.nc_close(ncid.value), f"close {path}")


def put_attribute(lib: ctypes.CDLL, ncid: int, varid: int, attr: tuple) -> None:
    name, xtype, value = attr
    if xtype == NC_CHAR:
        encoded = value.encode("utf-8")
        check(lib, lib.nc_put_att_text(ncid, varid, name.encode(), len(encoded), encoded), f"write attribute {name}")
        return
    suffix = {NC_INT: "int", NC_FLOAT: "float", NC_DOUBLE: "double"}[xtype]
    ctype = TYPE_ARRAY[xtype][1]
    check(
        lib,
        getattr(lib, f"nc_put_att_{suffix}")(
            ncid, varid, name.encode(), xtype, len(value), ctypes.c_void_p(value.buffer_info()[0])
        ),
        f"write attribute {name}",
    )


def write_file(lib: ctypes.CDLL, path: Path, variables: list[dict], title: str) -> None:
    ncid = ctypes.c_int()
    check(lib, lib.nc_create(os.fsencode(path), NC_CLOBBER | NC_NETCDF4, ctypes.byref(ncid)), f"create {path}")
    try:
        dimensions = {}
        for variable in variables:
            for name, length in variable["dimensions"]:
                if name in dimensions and dimensions[name][0] != length:
                    raise ValueError(f"inconsistent dimension length for {name}")
                if name not in dimensions:
                    dimid = ctypes.c_int()
                    check(lib, lib.nc_def_dim(ncid.value, name.encode(), length, ctypes.byref(dimid)), f"define dimension {name}")
                    dimensions[name] = (length, dimid.value)
        varids = {}
        for variable in variables:
            dimids = (ctypes.c_int * len(variable["dimensions"]))( *(dimensions[name][1] for name, _ in variable["dimensions"]) )
            varid = ctypes.c_int()
            check(
                lib,
                lib.nc_def_var(
                    ncid.value,
                    variable["name"].encode(),
                    variable["type"],
                    len(variable["dimensions"]),
                    dimids,
                    ctypes.byref(varid),
                ),
                f"define {variable['name']}",
            )
            check(lib, lib.nc_def_var_deflate(ncid.value, varid.value, 1, 1, 3),
                  f"set lossless compression for {variable['name']}")
            varids[variable["name"]] = varid.value
            for attr in variable["attrs"]:
                put_attribute(lib, ncid.value, varid.value, attr)
        check(lib, lib.nc_put_att_text(ncid.value, NC_GLOBAL, b"Conventions", 7, b"CF-1.10"), "write conventions")
        encoded_title = title.encode("utf-8")
        check(lib, lib.nc_put_att_text(ncid.value, NC_GLOBAL, b"title", len(encoded_title), encoded_title), "write title")
        check(lib, lib.nc_enddef(ncid.value), f"finish {path}")
        for variable in variables:
            suffix = {NC_INT: "int", NC_FLOAT: "float", NC_DOUBLE: "double"}[variable["type"]]
            check(
                lib,
                getattr(lib, f"nc_put_var_{suffix}")(
                    ncid.value,
                    varids[variable["name"]],
                    ctypes.c_void_p(variable["data"].buffer_info()[0]),
                ),
                f"write {variable['name']}",
            )
    finally:
        check(lib, lib.nc_close(ncid.value), f"close {path}")


def confirm_overwrite(outputs: list[Path], force: bool) -> bool:
    existing = [path for path in outputs if path.exists()]
    if not existing or force:
        return True
    print("Existing consolidated map file(s): " + ", ".join(str(path) for path in existing))
    if not sys.stdin.isatty():
        print("Non-interactive mode: preserving existing files; skipped.")
        return False
    try:
        answer = input("Replace existing output file(s)? [y/N]: ").strip()
    except EOFError:
        answer = ""
    if answer.lower() == "y":
        return True
    print("Existing files preserved; skipped.")
    return False


def assemble(args: argparse.Namespace) -> bool:
    if args.nextxy not in {"nextxy.nc", "nextxy_noedge.nc"}:
        raise ValueError("--nextxy must be nextxy.nc or nextxy_noedge.nc")
    if args.rivwth not in {"rivwth.nc", "rivwth_merit.nc"}:
        raise ValueError("--rivwth must be rivwth.nc or rivwth_merit.nc")
    map_dir = args.map_dir.resolve()
    netcdf_dir = map_dir / "netcdf"
    nx, ny, nlfp = read_map_shape(map_dir / "params.txt")
    inputs = {
        "network": netcdf_dir / args.nextxy,
        "coordinates": netcdf_dir / "lonlat.nc",
        "rivwth": netcdf_dir / args.rivwth,
        "ctmare": netcdf_dir / "ctmare.nc",
        "elevtn": netcdf_dir / "elevtn.nc",
        "nxtdst": netcdf_dir / "nxtdst.nc",
        "rivlen": netcdf_dir / "rivlen.nc",
        "fldhgt": netcdf_dir / "fldhgt.nc",
        "rivhgt": netcdf_dir / "rivhgt.nc",
        "rivman": netcdf_dir / "rivman.nc",
    }
    missing = [str(path) for path in inputs.values() if not path.is_file()]
    if missing:
        raise FileNotFoundError("required individual NetCDF input(s) missing: " + ", ".join(missing))
    lib = netcdf_library()
    configure(lib)
    coordinates = [read_var(lib, inputs["coordinates"], name) for name in ("lon", "lat")]
    coord_map = {item["name"]: item for item in coordinates}
    if coordinates[0]["dimensions"] != [("lon", nx)] or coordinates[1]["dimensions"] != [("lat", ny)]:
        raise ValueError(f"lonlat.nc coordinates do not match params.txt grid size ({nx}, {ny})")
    network = [read_var(lib, inputs["network"], name) for name in ("nextx", "nexty")]
    topo = [read_var(lib, inputs[key], key) for key in ("ctmare", "elevtn", "nxtdst", "rivlen", "fldhgt")]
    params = [read_var(lib, inputs[key], key) for key in ("rivwth", "rivhgt", "rivman")]
    all_vars = coordinates + network + topo + params
    for variable in all_vars:
        dims = variable["dimensions"]
        if variable["name"] in {"lon", "lat"}:
            continue
        if variable["name"] == "fldhgt":
            valid = [dim[0] for dim in dims] == ["layer", "lat", "lon"]
            valid = valid and [dim[1] for dim in dims] == [nlfp, ny, nx]
        else:
            valid = [dim[0] for dim in dims] == ["lat", "lon"]
            valid = valid and [dim[1] for dim in dims] == [ny, nx]
        if not valid:
            raise ValueError(f"{variable['name']} has unsupported dimensions {dims}")
    for variable in all_vars:
        if variable["name"] in {"lon", "lat"}:
            continue
        with_path = inputs["network"] if variable["name"] in {"nextx", "nexty"} else inputs[variable["name"]]
        source_coords = [read_var(lib, with_path, name) for name in ("lon", "lat")]
        for source, expected_coord in zip(source_coords, coordinates):
            if source["dimensions"] != expected_coord["dimensions"] or source["data"].tobytes() != expected_coord["data"].tobytes():
                raise ValueError(f"{with_path.name} coordinate {source['name']} does not match lonlat.nc")
    if not confirm_overwrite([netcdf_dir / "rivclim.nc", netcdf_dir / "rivpar.nc"], args.force):
        return False
    outputs = [netcdf_dir / "rivclim.nc", netcdf_dir / "rivpar.nc"]
    temp_paths = []
    try:
        for output, variables in zip(outputs, (coordinates + network + topo, params)):
            with tempfile.NamedTemporaryFile(prefix=f".{output.stem}.", suffix=".tmp.nc", dir=netcdf_dir, delete=False) as temp:
                temp_path = Path(temp.name)
            temp_paths.append(temp_path)
            write_file(lib, temp_path, variables, f"CaMa-Flood legacy map container: {output.name}")
            os.chmod(temp_path, 0o644)
            for variable in variables:
                observed = read_var(lib, temp_path, variable["name"])
                if observed["type"] != variable["type"] or observed["dimensions"] != variable["dimensions"]:
                    raise RuntimeError(f"verification failed for {output.name}:{variable['name']} type or dimensions")
                if observed["data"].tobytes() != variable["data"].tobytes():
                    raise RuntimeError(f"verification failed for {output.name}:{variable['name']}")
                if observed["attrs"] != variable["attrs"]:
                    raise RuntimeError(f"verification failed for {output.name}:{variable['name']} attributes")
        for temp_path, output in zip(temp_paths, outputs):
            os.replace(temp_path, output)
        for output, variables in zip(outputs, (coordinates + network + topo, params)):
            print(f"Created {output} ({output.stat().st_size} bytes): " + ", ".join(var["name"] for var in variables))
        return True
    finally:
        for temp_path in temp_paths:
            temp_path.unlink(missing_ok=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--map-dir", type=Path, required=True, help="map root containing params.txt and netcdf/ individual maps")
    parser.add_argument("--nextxy", default="nextxy_noedge.nc", help="network file: nextxy.nc or nextxy_noedge.nc")
    parser.add_argument("--rivwth", default="rivwth_merit.nc", help="width file: rivwth.nc or rivwth_merit.nc")
    parser.add_argument("--force", action="store_true", help="overwrite existing rivclim.nc and rivpar.nc without prompting")
    args = parser.parse_args()
    try:
        created = assemble(args)
    except (OSError, ValueError, RuntimeError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1
    return 0 if created else 3


if __name__ == "__main__":
    raise SystemExit(main())
