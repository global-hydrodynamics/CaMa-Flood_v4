# CaMa-Flood v5 build configuration

The legacy v4 source tree continues to use [`adm/Mkinclude`](../adm/Mkinclude). The v5 model and map tools use the separate machine-specific `config/Mkinclude`, so a v5 build cannot silently inherit v4 compiler settings.

`config/Mkinclude` is intentionally ignored by Git because compiler paths, libraries, and flags depend on the local machine. The tracked templates are the reusable starting points; the active file is created separately on each workstation or disposable CI checkout. Never add the active file to Git.

## Recommended GNU Fortran setup

The recommended workstation setup uses GNU Fortran with OpenMP, NetCDF-Fortran, and single precision. MPI and Heatlink are disabled by default. On macOS or Linux, install a compatible GNU Fortran toolchain and NetCDF-Fortran so `gfortran` and `nf-config` are available, then run the root setup script:

```sh
./s01-setup_fortran.sh
```

After reviewing the generated staging file, activate it explicitly:

```sh
if [ ! -e config/Mkinclude ]; then
    cp Mkinclude config/Mkinclude
else
    printf '%s\n' 'config/Mkinclude already exists; review it before replacing it.'
fi
```

The setup script never copies the file automatically and never changes `config/Mkinclude` or `adm/Mkinclude`. It requires a successful NetCDF-Fortran compile, link, and read/write check before offering to generate the v5 configuration. OpenMP is enabled when its compiler check succeeds. Without an interactive terminal, `s01-setup_fortran.sh` runs diagnostics only; it is not the initializer used by CI.

## Choosing a template manually

On a new machine with no active configuration, copy a suitable tracked template to the ignored path `config/Mkinclude`, then adjust it for the local installation. Check first so this does not overwrite an existing configuration:

```sh
if [ ! -e config/Mkinclude ]; then
    cp config/Mkinclude_gfortran config/Mkinclude
else
    printf '%s\n' 'config/Mkinclude already exists; leaving it unchanged.'
fi
```

- [`Mkinclude_gfortran`](Mkinclude_gfortran): tracked GNU Fortran baseline used by Linux CI and a manual starting point for macOS and Linux.
- [`Mkinclude_gfortran_debug`](Mkinclude_gfortran_debug): GNU Fortran with runtime checks and debug information.
- [`Mkinclude_linux_ifx`](Mkinclude_linux_ifx): Linux Intel oneAPI `ifx`; load the compiler environment and provide a compatible NetCDF-Fortran `nf-config`.

GNU templates obtain Fortran include and link flags from `nf-config --fflags` and `nf-config --flibs`. Where available, `nc-config --libs` supplements the NetCDF-C search path needed by installations such as Homebrew; its duplicate `-lnetcdf` flag is filtered because `nf-config` already supplies it. Set `NF_CONFIG` or `NC_CONFIG` when either tool has a nonstandard name, or override `INC` and `LIB` for a custom installation. A missing `config/Mkinclude` produces a clear Make error; v5 builds do not fall back to `adm/Mkinclude`.

## Build entry points

```sh
make -C src_main all
make -C map/tools/code all
```

The active `config/Mkinclude` is local and machine-specific. It is ignored by Git; the three templates and this guide are tracked. Keep compiler outputs and local configuration out of commits. Optional MPI, Heatlink, ICI, and sediment configurations require additional setup and are not implied to be validated by the standard templates.

## Automated builds

The GitHub Actions source and map validation job starts from a clean checkout, installs the GNU Fortran and NetCDF-Fortran dependencies, and copies the tracked `Mkinclude_gfortran` template to `config/Mkinclude` in that disposable runner. It then builds the v5 model and regional map tools and runs the relevant checks. The generated active file remains ignored and is discarded with the runner. Local setup does not replace an existing active file. V5 builds must never use `adm/Mkinclude` as a fallback; that file remains the legacy v4 configuration.
