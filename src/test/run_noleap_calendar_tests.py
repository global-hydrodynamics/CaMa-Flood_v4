#!/usr/bin/env python3
"""Exercise production clocks, runoff, atmospheric input, output and restart I/O.

Requires gfortran, nf-config, numpy and netCDF4. Fixtures and build products
are disposable; no experiment settings or existing model data are used.
"""
import argparse
from collections import Counter
import calendar as civil_calendar
from datetime import datetime, timedelta
import importlib.util
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile

import netCDF4
import numpy as np

sys.dont_write_bytecode = True
SRC = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('mapping_tests', SRC / 'mod/test/run_input_mapping_tests.py')
mapping = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mapping)


def run(cmd, cwd, success=True):
    result = subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if success and result.returncode:
        raise RuntimeError(shlex.join(cmd) + '\n' + result.stdout)
    if not success and result.returncode == 0:
        raise RuntimeError('Expected a nonzero error exit: ' + shlex.join(cmd) + '\n' + result.stdout)
    return result.stdout


def build_driver(build, fc, single):
    flags = ['-cpp', '-ffree-line-length-none', '-O0', '-g', '-fcheck=all', '-fbacktrace', '-DUseCDF_CMF']
    if single:
        flags += ['-DSinglePrec_CMF']
    inc = shlex.split(subprocess.check_output(['nf-config', '--fflags'], text=True))
    libs = shlex.split(subprocess.check_output(['nf-config', '--flibs'], text=True))
    sources = list(mapping.SOURCES)
    sources[1:1] = ['common/cmf_cf_time_mod.F90']
    sources.insert(sources.index('cmf_utils_mod.F90'), 'yos_cmf_prog.F90')
    sources.insert(sources.index('cmf_utils_mod.F90'), 'yos_cmf_diag.F90')
    sources.insert(sources.index('io/input_conf_class.f90'), 'common/nc_mod.f90')
    sources += ['cmf_ctrl_time_mod.F90', 'cmf_ctrl_forcing_mod.F90', 'cmf_ctrl_restart_mod.F90',
                'cmf_ctrl_output_mod.F90', 'io/restart_mod.f90']
    objects = []
    for source in sources:
        obj = build / (Path(source).stem + '.o')
        run(shlex.split(fc) + flags + inc + ['-c', str(SRC / source), '-o', str(obj)], build)
        objects.append(str(obj))
    executables = []
    for name, source, objs in [('test_calendar', 'test/test_noleap_calendar.F90', objects),
                               ('test_cf_time', 'common/test/test_cf_time.F90', objects[:2]),
                               ('test_datetime', 'common/test/test_datetime.f90', objects)]:
        exe = build / name
        run(shlex.split(fc) + flags + inc + [str(SRC / source), *objs, *libs, '-o', str(exe)], build)
        if sys.platform == 'darwin':
            commands = run(['otool', '-l', str(exe)], build)
            paths = re.findall(r'cmd LC_RPATH\s+cmdsize \d+\s+path (.*?) \(offset', commands)
            for path, count in Counter(paths).items():
                if count > 1:
                    run(['install_name_tool', '-delete_rpath', path, str(exe)], build)
        executables.append(exe)
    run([str(executables[1])], build)
    run([str(executables[2])], build)
    return executables[0]


def dates(start, end, hours, noleap):
    result = []
    now = start
    while now < end:
        if not (noleap and now.month == 2 and now.day == 29):
            result.append(now)
        now += timedelta(hours=hours)
    return result


def values(dts):
    # Civil-date labels are identical in both calendars. Small exactly
    # representable values allow comparison in both model precisions.
    origin = datetime(1990, 1, 1)
    return np.array([(dt-origin).days * 8 + dt.hour // 3 for dt in dts], dtype='f4')


def fixture(path, dts, calendar, hours, stamp='start'):
    with netCDF4.Dataset(path, 'w') as ds:
        for name, length in [('lon', 2), ('lat', 2), ('time', len(dts))]:
            ds.createDimension(name, length)
        for name in ['lon', 'lat']:
            v = ds.createVariable(name, 'f8', (name,))
            v[:] = [0.5,1.5] if name == 'lon' else [1.5,0.5]
            v.units = 'degrees_east' if name == 'lon' else 'degrees_north'
        t = ds.createVariable('time', 'f8', ('time',))
        t.units = 'hours since 1999-01-01 00:00:00'
        t.calendar = calendar
        lower = netCDF4.date2num(dts, t.units, calendar=calendar)
        t[:] = lower + (hours if stamp == 'end' else hours/2 if stamp == 'center' else 0)
        if stamp != 'start':
            ds.createDimension('bnds', 2)
            b = ds.createVariable('time_bnds', 'f8', ('time', 'bnds'))
            b[:] = np.column_stack((lower, lower + hours))
            t.bounds = 'time_bnds'
        v = ds.createVariable('forcing', 'f4', ('time', 'lat', 'lon'))
        v[:] = values(dts)[:, None, None]


def case(root, name, exe, start, end, *, calendar='365_day', hours=24, atm_calendar='standard',
         rof_calendar=None, stamp='start', monthly=False, legacy_leap=True, resume=None, nc_restart=False,
         reject=None, force_end=None, model_hours=None):
    folder = root / name
    folder.mkdir()
    noleap = calendar in ('365_day', 'noleap', 'NoLeAp') or (calendar is None and not legacy_leap)
    model_hours = model_hours or hours
    model_dates = dates(start, end, model_hours, noleap)
    rof_calendar = rof_calendar or ('365_day' if noleap else 'standard')
    forcing_end = force_end or datetime(2005, 1, 1)
    rof_dates = dates(datetime(1999, 1, 1), forcing_end, hours, rof_calendar in ('365_day', 'noleap'))
    atm_dates = dates(datetime(1999, 1, 1), datetime(2005, 1, 1), hours, atm_calendar in ('365_day', 'noleap'))
    fixture(folder/'runoff.nc', rof_dates, rof_calendar, hours, stamp)
    atm_name = 'atm.nc'
    if monthly:
        groups = {}
        for dt in atm_dates:
            groups.setdefault((dt.year, dt.month), []).append(dt)
        for (year, month), dts in groups.items():
            fixture(folder/f'atm-{year:04}{month:02}.nc', dts, atm_calendar, hours, stamp)
        atm_name = f'atm-{start.year:04}{start.month:02}.nc'
    else:
        fixture(folder/atm_name, atm_dates, atm_calendar, hours, stamp)
    binary_records = np.arange(1, 366*24//hours+1, dtype='f4')
    np.repeat(binary_records, 4).tofile(folder/'annual.bin')
    cal_setting = '' if calendar is None else f", CALENDAR='{calendar}'"
    nml = (f'&NSIMTIME SYEAR={start.year},SMON={start.month},SDAY={start.day},SHOUR={start.hour},'
           f'EYEAR={end.year},EMON={end.month},EDAY={end.day},EHOUR={end.hour}{cal_setting} /\n'
           "&NFORCE LINPCDF=.true., LINPDAY=.false., LINTERP=.false., CROFCDF='runoff.nc', CVNROF='forcing', SYEARIN=-1 /\n"
           f"&input_item item='TAIR', fmt='nc', path='{atm_name}', is_catm=.true. /\n"
           "&input_nc item='TAIR', var_name='forcing' /\n"
           "&input_item item='ANNUAL', fmt='bin', path='annual.bin', is_catm=.true. /\n"
           "&input_domain item='ANNUAL', left=0,right=2,top=2,bottom=0 /\n"
           "&input_shape item='ANNUAL', nx=2,ny=2 /\n"
           f"&input_tres item='ANNUAL', dt={hours},dt_unit='hour' /\n"
           f"&NOUTPUT CVARSOUT='rivsto',COUTDIR='./',COUTTAG='',LOUTCDF=.true.,IFRQ_OUT={model_hours},NDLEVEL=0 /\n"
           "&restart_config item='STATE', file='.state.bin',recnum=1,mapfmt=.false. /\n")
    (folder/'input_cmf.nam').write_text(nml)
    (folder/'settings.txt').write_text(f'{"T" if legacy_leap else "F"} {model_hours*3600} {hours*3600} {"T" if resume else "F"} {"T" if nc_restart else "F"}\n')
    state = 0.
    if resume:
        previous, dt = resume
        label = dt.strftime('%Y%m%d%H')
        shutil.copy(previous/f'restart{label}{".nc" if nc_restart else ".bin"}', folder/'resume.core')
        shutil.copy(previous/f'restart{label}.state.bin', folder/f'restart{label}.state.bin')
        state = float(np.fromfile(previous/f'restart{label}.state.bin', dtype='f8')[0])
    rof_indices = {dt: i+1 for i, dt in enumerate(rof_dates)}
    atm_indices = {dt: i+1 for i, dt in enumerate(atm_dates)}
    lines = [str(len(model_dates))]
    expected_rof = []
    for dt, value in zip(model_dates, values([dt.replace(hour=dt.hour//hours*hours) for dt in model_dates])):
        # In rejection cases, a missing noleap February 29 has no index.
        anchor = dt.replace(hour=dt.hour//hours*hours)
        rr = rof_indices.get(anchor, 0)
        ar = atm_indices.get(anchor, 0)
        if monthly:
            ar = next(i+1 for i,d in enumerate(groups[(dt.year,dt.month)]) if d == anchor)
        elapsed = (dt-datetime(dt.year,1,1)).total_seconds()
        if noleap and civil_calendar.isleap(dt.year) and dt.month > 2:
            elapsed -= 86400
        br = int(elapsed // (hours*3600)) + 1
        lines.append(f'{dt:%Y%m%d} {dt.hour} {rr} {ar} {br} {value:.0f}')
        if len(lines) == 2 or (len(lines)-2)*model_hours % hours == 0:
            expected_rof.append(rr)
        state += 2*float(value)
    lines.append(f'{end:%Y%m%d} {end.hour} {state:.0f}')
    (folder/'expected.txt').write_text('\n'.join(lines)+'\n')
    output = run([str(exe)], folder, success=reject is None)
    if reject:
        if reject not in output or 'NOLEAP_CALENDAR_PASS' in output:
            raise AssertionError(name+' expected rejection '+reject+'\n'+output)
        return folder
    if 'NOLEAP_CALENDAR_PASS' not in output:
        raise AssertionError(name+'\n'+output)
    matches = re.findall(r'CMF::FORCING_GET_CDF: read runoff:\s+(\d+)\s+(\d+)\s+(\d+)', output)
    assert [int(x[2]) for x in matches] == expected_rof, (name, matches[:5])
    with netCDF4.Dataset(folder/'o_rivsto.nc') as ds:
        t = ds['time']
        cal = getattr(t, 'calendar', 'standard')
        assert cal == ('365_day' if noleap else 'standard'), (name, cal)
        decoded = netCDF4.num2date(t[:], t.units, calendar=cal)
        # Output timestamps label the end of each model step.
        expected_end = model_dates[1:] + [end]
        assert [(d.year,d.month,d.day,d.hour) for d in decoded] == [(d.year,d.month,d.day,d.hour) for d in expected_end]
    with netCDF4.Dataset(folder/f'restart{end:%Y%m%d%H}.nc') if nc_restart else open(folder/f'restart{end:%Y%m%d%H}.bin', 'rb') as f:
        if nc_restart:
            t = f['time']; cal = getattr(t, 'calendar', 'standard')
            dt = netCDF4.num2date(t[0], t.units, calendar=cal)
            assert (dt.year,dt.month,dt.day,dt.hour) == (end.year,end.month,end.day,end.hour)
    if noleap:
        assert not any('0229' in p.name for p in folder.glob('restart*'))
    return folder



def check_baseline(root, current_exe, fc, single, ref):
    global SRC
    current_src = SRC
    baseline_src = root/'baseline-src'
    baseline_src.mkdir()
    sources = list(mapping.SOURCES) + [
        'common/cmf_cf_time_mod.F90', 'common/nc_mod.f90', 'yos_cmf_prog.F90', 'yos_cmf_diag.F90',
        'cmf_ctrl_time_mod.F90', 'cmf_ctrl_forcing_mod.F90', 'cmf_ctrl_restart_mod.F90',
        'cmf_ctrl_output_mod.F90', 'io/restart_mod.f90', 'common/test/test_cf_time.F90',
        'common/test/test_datetime.f90']
    for source in sources:
        target = baseline_src/source
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(subprocess.check_output(['git','show',f'{ref}:src/{source}'], cwd=current_src.parent))
    (baseline_src/'test').mkdir()
    shutil.copy(current_src/'test/test_noleap_calendar.F90', baseline_src/'test/test_noleap_calendar.F90')
    build = root/'baseline-build'; build.mkdir()
    try:
        SRC = baseline_src
        old_exe = build_driver(build, fc, single)
    finally:
        SRC = current_src
    interval = (datetime(2000,2,27), datetime(2001,3,2))
    old = case(root,'baseline-standard',old_exe,*interval,calendar=None,hours=3)
    new = case(root,'current-standard',current_exe,*interval,calendar=None,hours=3)
    for filename in ['o_rivsto.nc']:
        with netCDF4.Dataset(old/filename) as a, netCDF4.Dataset(new/filename) as b:
            assert set(a.variables) == set(b.variables)
            for key in a.variables:
                assert np.array_equal(a[key][:], b[key][:]), key
                assert a[key].__dict__ == b[key].__dict__, key
    for filename in ['restart2001030200.bin', 'restart2001030200.state.bin']:
        assert (old/filename).read_bytes() == (new/filename).read_bytes(), filename
    print(f'PASS: default Gregorian data, timestamps and binary restarts identical to {ref}')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fc', default=os.environ.get('FC', 'gfortran'))
    parser.add_argument('--baseline-ref', help='optional public Git ref for exact Gregorian regression')
    args = parser.parse_args()
    if sys.platform == 'darwin' and 'SDKROOT' not in os.environ:
        os.environ['SDKROOT'] = subprocess.check_output(['xcrun','--sdk','macosx','--show-sdk-path'], text=True).strip()
    for single in (False, True):
        with tempfile.TemporaryDirectory(prefix='cama-calendar-') as tmp:
            root = Path(tmp); build = root/'build'; build.mkdir()
            exe = build_driver(build, args.fc, single)
            common = (datetime(2001,2,28), datetime(2001,3,2))
            leap = (datetime(2000,2,28), datetime(2000,3,2))
            for index, cal in enumerate(('standard','365_day','noleap','NoLeAp',None)):
                case(root, f'common-{index}', exe, *common, calendar=cal)
                case(root, f'leap-{index}', exe, *leap, calendar=cal)
            case(root,'legacy-noleap',exe,*leap,calendar=None,legacy_leap=False)
            case(root,'explicit-standard',exe,*leap,calendar='standard',legacy_leap=False)
            case(root,'gregorian-alias',exe,*leap,calendar='gregorian')
            for stamp in ('start','center','end'):
                case(root,'subdaily-'+stamp,exe,*leap,hours=3,stamp=stamp)
            case(root,'hourly-clock-three-hour-forcing',exe,*leap,hours=3,model_hours=1)
            case(root,'hourly-clock-daily-forcing',exe,*leap,hours=24,model_hours=1,atm_calendar='standard')
            case(root,'noleap-atmosphere',exe,*leap,atm_calendar='noleap')
            case(root,'gregorian-runoff',exe,*leap,rof_calendar='standard',hours=3)
            case(root,'monthly-files',exe,*leap,hours=3,monthly=True)
            years = (datetime(1999,12,31),datetime(2005,1,1))
            case(root,'multi-year',exe,*years,hours=3)
            for nc in (False, True):
                for boundary in (datetime(2000,2,28),datetime(2000,3,1),datetime(2001,1,1)):
                    name=f'restart-{nc}-{boundary:%Y%m%d}'
                    first=case(root,name,exe,datetime(1999,12,31),boundary,hours=3,nc_restart=nc)
                    case(root,name+'-resume',exe,boundary,datetime(2001,3,2),hours=3,
                         nc_restart=nc,resume=(first,boundary))
                    if nc and boundary == datetime(2000,3,1):
                        case(root,name+'-wrong-calendar',exe,boundary,datetime(2001,3,2),
                             calendar='standard',rof_calendar='standard',hours=3,nc_restart=True,
                             resume=(first,boundary),reject='Restart calendar differs')
            case(root,'reject-360-day',exe,*leap,calendar='360_day',reject='Unsupported simulation CALENDAR')
            case(root,'reject-Feb29',exe,datetime(2000,2,29),datetime(2000,3,2),reject='Date Problem')
            case(root,'reject-missing-day',exe,*leap,calendar='standard',atm_calendar='noleap',reject='LLEAPYR=')
            case(root,'reject-short-gregorian',exe,*leap,rof_calendar='standard',
                 force_end=datetime(2000,3,1),reject='Run end later than forcing data')
            if args.baseline_ref:
                check_baseline(root, exe, args.fc, single, args.baseline_ref)
            print(f'PASS: {"single" if single else "double"}, 39 calendar/integration cases + CF/datetime regression')


if __name__ == '__main__':
    main()
