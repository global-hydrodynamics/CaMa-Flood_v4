#!/usr/bin/env python3
'''人工データで本番の InputConf → read_nc → map2vec 経路を検証する。'''
import argparse
from collections import Counter
import importlib.util
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile

import netCDF4
import numpy as np

SRC = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('mapping_tests', SRC / 'mod/test/run_input_mapping_tests.py')
mapping_tests = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mapping_tests)


def run(cmd, cwd):
    result = subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode:
        raise RuntimeError(shlex.join(cmd) + '\n' + result.stdout)
    return result.stdout


def fixture(path, count=4, dim='member', order=None, pad=0, begin=0, special=False,
            packed=False, auxiliary=False, unknown=False, coord=True):
    if order is None:
        order = ('time', dim, 'lat', 'lon') if count else ('time', 'lat', 'lon')
    lengths = {'time': 2, 'lat': 2, 'lon': 3, dim: count, 'unused': 7, 'other': 1}
    with netCDF4.Dataset(path, 'w') as ds:
        for i in range(pad):
            ds.createDimension(f'padding{i}', 2)
        for name in dict.fromkeys((*order, 'unused')):
            ds.createDimension(name, lengths[name])
        for name in ('time', 'lon', 'lat'):
            c = ds.createVariable(name, 'f8', (name,))
            if name == 'time':
                c.units = 'hours since 2000-01-01 00:00:00'
                c.calendar = 'standard'
                c[:] = [begin, begin + 1]
            elif name == 'lon':
                c[:] = [0.5, 1.5, 2.5]
                if not unknown:
                    c.axis = 'X'
            else:
                c[:] = [1.5, 0.5]
                if not unknown:
                    c.axis = 'Y'
        if count and coord:
            c = ds.createVariable('selection_coordinate' if auxiliary else dim, 'f8', (dim,))
            c.units = 'm'
            c.positive = 'down'
            c[:] = np.arange(count, 0, -1)
        v = ds.createVariable('forcing', 'i2' if packed else 'f4', order,
                              fill_value=-32767 if packed else np.float32(1.e20))
        v.units = 'K'
        if packed:
            v.scale_factor = np.float32(2.)
            v.add_offset = np.float32(5.)
            v.set_auto_maskandscale(False)
        if auxiliary:
            v.coordinates = 'selection_coordinate'
        # Distinct time, slice and horizontal contributions expose wrong axes.
        shape = tuple(lengths[name] for name in order)
        indices = np.indices(shape)
        values = np.zeros(shape, dtype='f4')
        for axis, name in enumerate(order):
            if name == 'time':
                values += (indices[axis] + begin + 1) * 1000
            elif name == dim:
                values += (indices[axis] + 1) * 100
            elif name == 'lat':
                values += (indices[axis] + 1) * 10
            elif name == 'lon':
                values += indices[axis] + 1
        if special:
            # Every selected slice carries distinct invalid values; never fall back.
            for t in range(2):
                for k in range(count):
                    values[t, k, 0, :] = [np.nan, 1.e20, -9999.]
        v[:] = values
        if unknown:
            ds.renameVariable('lon', 'xcoord')
            ds.renameDimension('lon', 'xcoord')
            if unknown is True:
                ds.renameVariable('lat', 'ycoord')
                ds.renameDimension('lat', 'ycoord')


def reference(path, index, dim):
    with netCDF4.Dataset(path) as ds:
        v = ds['forcing']
        v.set_auto_maskandscale(False)
        key = [slice(None)] * v.ndim
        count = 1
        if dim in v.dimensions:
            count = len(ds.dimensions[dim])
            key[v.dimensions.index(dim)] = count - 1 if index == -1 else index - 1
        values = np.asarray(v[tuple(key)], dtype='f4')
        # Slicing removes the extra axis, retaining (time, Y, X).
        return count, values.reshape(2, 6)


def prepare(case, *, index=1, count=4, dim='member', explicit=None, order=None,
            next_count=None, next_order=None, next_dim=None, next_pad=3, binary=False,
            special=False, packed=False, auxiliary=False, unknown=False,
            old_index=False, old_count=False, mapped=False, next_change=None, extra_nc='', omit_index=False, coord=True):
    fixture(case / 'forcing.nc', count, dim, order, special=special, packed=packed,
            auxiliary=auxiliary, unknown=unknown, coord=coord)
    request = '' if explicit is None else explicit
    nml = f"&input_item item='TEST', fmt='{'bin' if binary else 'nc'}', "
    nml += f"path='forcing.{'bin' if binary else 'nc'}', is_catm=.true., scale=2., offset=3."
    if not omit_index:
        nml += f", {'z_in' if old_index else 'slice_index'}={index}"
    nml += ' /\n'
    if binary:
        nml += "&input_domain item='TEST', left=0, right=3, top=2, bottom=0 /\n"
        nml += f"&input_shape item='TEST', nx=3, ny=2, {'nz' if old_count else 'slice_count'}={count} /\n"
        nml += "&input_tres item='TEST', dt=1, dt_unit='hour' /\n"
        with netCDF4.Dataset(case / 'forcing.nc') as ds:
            ds['forcing'].set_auto_maskandscale(False)
            np.asarray(ds['forcing'][:], dtype='f4').tofile(case / 'forcing.bin')
    else:
        nml += f"&input_nc item='TEST', var_name='forcing', slice_dimname='{request}'{extra_nc} /\n"
    if old_count and not binary:
        nml += "&input_shape item='TEST', nx=3, ny=2, nz=4 /\n"
    if mapped:
        nml = nml.replace('is_catm=.true.', "diminfo_file='mapping.info', inpmat_file='mapping.bin'")
        (case / 'mapping.info').write_text('3\n2\n1\n3\n2\n2\nmapping.bin\n0\n3\n2\n0\n')
        x = np.array([[1, 2, 3, 1, 2, 3], [2, 0, 0, 0, 0, 0]], dtype='i4')
        y = np.array([[1, 1, 1, 2, 2, 2], [1, 0, 0, 0, 0, 0]], dtype='i4')
        weights = np.array([[1, 1, 1, 1, 1, 1], [3, 0, 0, 0, 0, 0]], dtype='f4')
        (case / 'mapping.bin').write_bytes(x.tobytes() + y.tobytes() + weights.tobytes())
    (case / 'input.nml').write_text(nml)
    sources = [(case / 'forcing.nc', dim)]
    if next_count is not None:
        next_dim = next_dim or dim
        fixture(case / 'next.nc', next_count, next_dim, next_order, next_pad, begin=2)
        if next_change:
            with netCDF4.Dataset(case / 'next.nc', 'a') as ds:
                if next_change == 'coordinates':
                    ds['lon'][1] = 1.6
                elif next_change == 'time':
                    ds['time'][:] = [2, 4]
        sources.append((case / 'next.nc', next_dim))
    arrays = []
    settings = [str(2 * len(sources))]
    for source, selected_dim in sources:
        try:
            actual_count, values = reference(source, index, selected_dim)
            if values.size != 12:
                raise ValueError('unsupported fixture')
        except (IndexError, ValueError):
            actual_count = count
            values = np.zeros((2, 6), dtype='f4')
        resolved = actual_count if index == -1 else index
        for _ in range(2):
            settings.append(f'{actual_count} {index} {resolved}')
        valid = np.isfinite(values) & (values >= 0) & (values < 1.e16)
        values[valid] *= np.float32(2.)
        # Match the existing separate scale/offset validity checks.
        valid = np.isfinite(values) & (values >= 0) & (values < 1.e16)
        values[valid] += np.float32(3.)
        if mapped:
            values[:, 0] = (values[:, 0] + 3 * values[:, 1]) / 4
        arrays.append(values)
    np.concatenate(arrays).astype('f4').tofile(case / 'expected.bin')
    (case / 'expected.txt').write_text('\n'.join(settings) + '\n')


def build_driver(build, fc, single):
    flags = ['-cpp', '-ffree-line-length-none', '-O0', '-g', '-fcheck=all', '-fbacktrace', '-DUseCDF_CMF']
    if single:
        flags.append('-DSinglePrec_CMF')
    compile_flags = shlex.split(subprocess.check_output(['nf-config', '--fflags'], text=True))
    link_flags = shlex.split(subprocess.check_output(['nf-config', '--flibs'], text=True))
    sources = list(mapping_tests.SOURCES)
    sources[1:1] = ['common/cmf_cf_time_mod.F90']
    sources.insert(sources.index('io/input_conf_class.f90'), 'common/nc_mod.f90')
    sources += ['common/key_table_class.f90', 'common/ranked_array_class.f90', 'common/util_mod.f90', 'io/input_mod.f90']
    objects = []
    for source in sources:
        obj = build / (Path(source).stem + '.o')
        run(shlex.split(fc) + flags + compile_flags + ['-c', str(SRC / source), '-o', str(obj)], build)
        objects.append(str(obj))
    exe = build / 'test_input_slice'
    run(shlex.split(fc) + flags + compile_flags + [str(SRC / 'io/test/test_input_slice.f90'), *objects,
                                                 *link_flags, '-o', str(exe)], build)
    if sys.platform == 'darwin':
        commands = run(['otool', '-l', str(exe)], build)
        paths = re.findall(r'cmd LC_RPATH\s+cmdsize \d+\s+path (.*?) \(offset', commands)
        for path, count in Counter(paths).items():
            if count > 1:
                run(['install_name_tool', '-delete_rpath', path, str(exe)], build)
    return exe


def real_files(exe, build, root):
    # One representative file per model; two records, two slices per file.
    for model in sorted(root.iterdir()):
        files = sorted(model.rglob('*.nc'))
        if not files:
            continue
        path = files[0]
        with netCDF4.Dataset(path) as ds:
            dim = ds['tsl'].dimensions[1]
            count = len(ds.dimensions[dim])
            leap = int(ds['time'].calendar not in ('365_day', 'noleap'))
        for record, index in ((1, 1), (2, -1)):
            command = [str(exe), 'read', str(path), 'tsl', dim, str(index), str(record), str(leap)]
            result = subprocess.run(command, cwd=build, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            log = result.stdout
            if result.returncode:
                # This pre-existing time-parser limitation is outside slice selection.
                if 'CF reference date must use YYYY-MM-DD: 1850-1-1 ' in log:
                    print(f'UNTESTED real: {model.name}: existing CF parser rejects 1850-1-1', flush=True)
                    break
                raise RuntimeError(shlex.join(command) + '\n' + log)
            actual = np.fromfile(build / 'actual.bin', dtype='f4')
            # Close each reader before opening the other (external-volume HDF locks).
            with netCDF4.Dataset(path) as ds:
                v = ds['tsl']
                v.set_auto_maskandscale(False)
                expected = np.asarray(v[record-1, count-1 if index == -1 else index-1, :, :], dtype='f4').ravel()
            np.testing.assert_array_equal(actual, expected)
            assert 'INPUT_SLICE_PASS' in log
        else:
            print(f'PASS real: {model.name}, count={count}, 2 records × 1 slice', flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fc', default=os.environ.get('FC', 'gfortran'))
    parser.add_argument('--real-dir', type=Path, help='代表ファイルを読み取り専用で比較する任意のディレクトリ')
    args = parser.parse_args()
    cases = []
    def add(name, error=None, log=None, **options):
        cases.append((name, options, error, log))
    add('legacy', count=0, omit_index=True)
    add('legacy-unlabelled', count=0, unknown=True)
    add('legacy-partially-labelled', count=0, unknown='partial')
    add('explicit-partially-labelled', unknown='partial', explicit='member')
    add('ambiguous-partially-labelled', unknown='partial', error='cannot identify horizontal axes')
    for index in (1, 2, 4, -1):
        add(f'index{index}', index=index, explicit='member', log='slice coordinate member:')
    add('auto', index=-1)
    add('singleton', count=1, index=-1)
    add('singleton-positive', count=1, index=1)
    add('depth', dim='depth', index=-1, explicit='depth')
    add('auxiliary', index=-1, auxiliary=True, log='slice coordinate selection_coordinate:')
    add('no-coordinate', index=-1, coord=False)
    add('slice-first-fortran', index=-1, order=('time', 'lat', 'lon', 'member'))
    add('slice-middle-fortran', index=2, order=('time', 'lat', 'member', 'lon'))
    add('explicit-unlabelled-horizontal', unknown=True, explicit='member', index=2)
    add('ambiguous-unlabelled', error='cannot identify horizontal axes', unknown=True)
    add('missing', index=-1, special=True)
    add('packing-existing-raw-values', packed=True, index=2)
    add('mapped', index=-1, mapped=True)
    add('mapped-reordered', index=2, mapped=True, order=('time', 'lat', 'member', 'lon'))
    for index in (0, -2, 5):
        add(f'invalid{index}', error='slice_index is out of range', index=index)
    for dim in ('absent', 'unused'):
        add(f'wrong-{dim}', error='not a dimension of this variable', explicit=dim)
    for dim in ('time', 'lat', 'lon'):
        add(f'horizontal-{dim}', error='selects time or a horizontal', explicit=dim)
    add('multiple', error='at most one extra dimension', order=('time', 'other', 'member', 'lat', 'lon'))
    add('multiple-explicit', error='at most one extra dimension', explicit='member', order=('time', 'other', 'member', 'lat', 'lon'))
    add('legacy-name', error='not a dimension of this variable', count=0, explicit='member')
    add('legacy-last', error='requires an extra dimension', count=0, index=-1)
    add('legacy-second', error='requires an extra dimension', count=0, index=2)
    add('swapped-horizontal', error='unsupported order', order=('time', 'member', 'lon', 'lat'))
    add('nonlast-time', error='unsupported order', order=('member', 'lat', 'lon', 'time'))
    add('switch', index=-1, next_count=2, next_order=('time', 'lat', 'member', 'lon'), log='slice_count changed: 4 -> 2')
    add('switch-named', index=-1, explicit='member', next_count=2, next_order=('time', 'lat', 'lon', 'member'))
    add('switch-auto-renamed', index=-1, next_count=3, next_dim='ensemble')
    add('switch-out-of-range', index=4, next_count=2, error='slice_index is out of range')
    add('switch-missing-name', index=2, explicit='member', next_count=3, next_dim='ensemble', error='not a dimension of this variable')
    add('switch-coordinates', next_count=2, index=-1, next_change='coordinates', error='horizontal coordinates changed')
    add('switch-time', next_count=2, index=-1, next_change='time', error='time interval changed')
    add('switch-no-extra', index=-1, next_count=0, error='requires an extra dimension')
    add('switch-multiple', next_count=2, next_order=('time', 'other', 'member', 'lat', 'lon'), error='at most one extra dimension')
    add('old-z-in', old_index=True, error='migrate z_in to slice_index')
    add('old-nz-on-netcdf', old_count=True, error='input_shape nz to slice_count')
    add('old-nz', binary=True, old_count=True, error='input_shape nz to slice_count')
    add('nc-user-count', extra_nc=', slice_count=4', error='invalid namelist')
    for index in (1, 2, -1):
        add(f'binary{index}', binary=True, index=index)
    for index in (0, -2, 5):
        add(f'binary-invalid{index}', binary=True, index=index, error='invalid binary shape or slice_index')
    for single in (False, True):
        with tempfile.TemporaryDirectory(prefix='cama-input-slice-') as temp:
            build = Path(temp)
            exe = build_driver(build, args.fc, single)
            for name, options, error, log in cases:
                case = build / name
                case.mkdir()
                prepare(case, **options)
                result = subprocess.run([str(exe), 'input'], cwd=case, text=True,
                                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                if error:
                    assert result.returncode != 0 and error in result.stdout, (name, result.stdout)
                else:
                    assert result.returncode == 0 and 'INPUT_SLICE_PASS' in result.stdout, (name, result.stdout)
                    if log:
                        assert log in result.stdout, (name, result.stdout)
                    if name in ('index1', 'index2', 'index4', 'index-1', 'auxiliary', 'depth'):
                        coordinate = re.search(r'slice coordinate \w+:\s+(\S+)', result.stdout)
                        index = options['index']
                        assert float(coordinate[1]) == (1 if index == -1 else 5-index), result.stdout
                    if name == 'no-coordinate':
                        assert 'slice coordinate ' not in result.stdout, result.stdout
            print(f'PASS: {"single" if single else "double"}, {len(cases)} synthetic cases', flush=True)
            if args.real_dir:
                real_files(exe, build, args.real_dir)


if __name__ == '__main__':
    main()
