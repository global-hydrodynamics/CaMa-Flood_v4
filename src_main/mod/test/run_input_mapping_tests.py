#!/usr/bin/env python3
"""Compile and run synthetic input-mapping tests in a disposable directory.

Requires Python 3 and gfortran. No model data or repository build products used.
"""
import argparse
import os
from pathlib import Path
import shlex
import sys
import re
from collections import Counter
import struct
import subprocess
import tempfile

SRC = Path(__file__).resolve().parents[2]
SOURCES = [
    "parkind1.F90", "yos_cmf_input.F90", "yos_cmf_time.F90", "yos_cmf_map.F90",
    "cmf_utils_mod.F90", "common/const_mod.f90", "common/text_mod.f90",
    "common/funit_mod.f90", "common/numeric_utils_mod.f90", "common/array_mod.f90",
    "common/time_recorder_class.f90", "common/mapframe_mod.f90", "common/datetime_mod.f90",
    "common/bin_mod.f90", "common/time_mod.f90", "mod/glob_mod.f90",
    "mod/camaframe_mod.f90", "mod/inpmat_mod.f90", "mod/dim_converter.f90",
    "io/io_namelist_mod.f90", "io/input_conf_class.f90",
]

def run(cmd, cwd):
    result = subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode:
        raise RuntimeError(shlex.join(cmd) + "\n" + result.stdout)
    return result.stdout


def fixtures(path, mode):
    info = "2\n1\n1\n2\n2\n2\nignored_embedded_filename.bin\n0\n2\n2\n0\n"
    (path / "a.info").write_text(info)
    (path / "b.info").write_text(info)
    (path / "other").mkdir()
    (path / "other/c.info").write_text(info)
    x = [1, 2, 2, 0]
    y = [1, 1, 1, 0]
    weights = [1., 2., 3., 0.]
    if mode == "indices": x[0] = 3
    if mode == "weights": weights[0] = -1.
    if mode == "nonfinite": weights[0] = float("nan")
    if mode == "padding": y[3] = 1
    if mode == "holes":
        x[0] = y[0] = 0
        weights[0] = 0.
    binary = struct.pack("=8i4f", *x, *y, *weights)
    (path / "a.bin").write_bytes(binary)
    (path / "b.bin").write_bytes(struct.pack("=8i4f", 1,1,0,2, 1,1,0,1, 1.,1.,0.,1.))
    (path / "alias.bin").symlink_to("a.bin")
    (path / "info-alias").symlink_to("a.info")
    (path / "forcing.bin").write_bytes(struct.pack("=8f", 0.,0.,0.,0.,10.,20.,30.,40.))
    (path / "catm.bin").write_bytes(struct.pack("=2f", 10.,20.))
    mapping = ", diminfo_file='a.info', inpmat_file='a.bin'"
    if mode in ("no-mapping", "catm"): mapping = ""
    if mode == "partial": mapping = ", diminfo_file='a.info'"
    if mode == "partial-binary": mapping = ", inpmat_file='a.bin'"
    if mode == "catm-files": mapping += ", is_catm=.true."
    item = f"&input_item item='TEST', fmt='bin', path='forcing.bin', slice_index=2{mapping} /\n"
    if mode == "catm":
        item = "&input_item item='TEST', fmt='bin', path='catm.bin', is_catm=.true. /\n"
    if mode == "netcdf":
        item = f"&input_item item='TEST', fmt='nc', path='forcing.nc'{mapping} /\n&input_nc item='TEST', var_name='forcing' /\n"
    nml = item + "&input_domain item='TEST', left=0, right=2, top=2, bottom=0 /\n"
    if mode != "netcdf": nml += "&input_shape item='TEST', nx=2, ny=2, slice_count=2 /\n"
    if mode == "catm": nml = nml.replace("ny=2, slice_count=2", "ny=1, slice_count=1")
    nml += "&input_tres item='TEST', dt=1, dt_unit='hour' /\n"
    (path / "input.nml").write_text(nml)
    if mode == "missing": (path / "a.bin").unlink()
    if mode == "missing-info": (path / "a.info").unlink()
    if mode == "size": (path / "a.bin").write_bytes(binary[:-4])
    if mode == "destination": (path / "a.info").write_text(info.replace("2\n1\n1", "3\n1\n1", 1))
    if mode == "nonpositive": (path / "a.info").write_text(info.replace("2\n1\n1", "0\n1\n1", 1))
    if mode == "bounds": (path / "a.info").write_text(info.rsplit("0\n", 1)[0] + "NaN\n")
    if mode == "metadata": (path / "a.info").write_text("2\n")
    if mode == "trailing":
        (path / "a.info").write_text(info.replace("2\nignored", "3\nignored"))
        (path / "a.bin").write_bytes(struct.pack("=12i6f", *x,0,0,*y,0,0,*weights,0.,0.))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fc", default=os.environ.get("FC", "gfortran"))
    parser.add_argument("--netcdf", action="store_true", help="also test NetCDF input using nf-config")
    args = parser.parse_args()
    compile_flags = []
    link_flags = []
    if args.netcdf:
        compile_flags = shlex.split(subprocess.check_output(["nf-config", "--fflags"], text=True))
        link_flags = shlex.split(subprocess.check_output(["nf-config", "--flibs"], text=True))
    positive = ["cache", "input", "catm", "trailing"]
    if args.netcdf: positive.append("netcdf")
    negative = {
        "partial": "specify both", "partial-binary": "specify both",
        "catm-files": "not used for is_catm", "missing": "cannot resolve", "missing-info": "cannot resolve",
        "size": "incorrect file size", "destination": "destination shape mismatch",
        "source-shape": "source shape mismatch", "cache-shape": "source shape mismatch",
        "indices": "out of range", "weights": "finite and non-negative", "nonfinite": "finite and non-negative",
        "padding": "inconsistent zero padding", "holes": "non-contiguous", "metadata": "invalid file",
        "no-mapping": "gridded input requires diminfo_file and inpmat_file", "nonpositive": "non-positive dimension", "bounds": "non-finite domain bounds",
    }
    for single in (False, True):
        with tempfile.TemporaryDirectory(prefix="cama-input-mapping-") as temp:
            build = Path(temp)
            flags = ["-cpp", "-ffree-line-length-none", "-O0", "-g", "-fcheck=all", "-fbacktrace"]
            if single: flags += ["-DSinglePrec_CMF"]
            sources = list(SOURCES)
            if args.netcdf:
                flags += ["-DUseCDF_CMF"]
                sources[1:1] = ["common/cmf_cf_time_mod.F90"]
                sources.insert(sources.index("io/input_conf_class.f90"), "common/nc_mod.f90")
            objects = []
            for source in sources:
                obj = build / (Path(source).stem + ".o")
                run(shlex.split(args.fc) + flags + compile_flags + ["-c", str(SRC/source), "-o", str(obj)], build)
                objects.append(str(obj))
            exe = build / "test_input_mapping"
            run(shlex.split(args.fc) + flags + compile_flags + [str(SRC/"mod/test/test_input_mapping.f90"), *objects, *link_flags, "-o", str(exe)], build)
            # Some macOS compiler wrappers emit duplicate LC_RPATH commands.
            if sys.platform == "darwin":
                commands = run(["otool", "-l", str(exe)], build)
                paths = re.findall(r"cmd LC_RPATH\s+cmdsize \d+\s+path (.*?) \(offset", commands)
                for path, count in Counter(paths).items():
                    if count > 1:
                        run(["install_name_tool", "-delete_rpath", path, str(exe)], build)
            for mode in positive + list(negative):
                case = build / mode
                case.mkdir()
                fixtures(case, mode)
                arg = "input" if mode in ("partial", "partial-binary", "catm-files", "no-mapping") else mode
                result = subprocess.run([str(exe), arg], cwd=case, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                if mode in negative:
                    assert result.returncode != 0 and negative[mode] in result.stdout, (mode, result.stdout)
                else:
                    assert result.returncode == 0 and "INPUT_MAPPING_PASS" in result.stdout, (mode, result.stdout)
            print(f"PASS: {'single' if single else 'double'}, NetCDF={args.netcdf}, {len(positive)+len(negative)} cases", flush=True)

if __name__ == "__main__":
    main()
