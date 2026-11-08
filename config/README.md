# CaMa-Flood v5 build configuration

The legacy v4 source tree continues to use [`adm/Mkinclude`](../adm/Mkinclude). The v5 model and map tools use the separate machine-specific `config/Mkinclude`, so a v5 build cannot silently inherit v4 compiler settings.

## Recommended GNU Fortran setup

The standard v5 configuration uses GNU Fortran with OpenMP, NetCDF-Fortran, and single precision. MPI and Heatlink are disabled by default. On macOS or Linux, install a compatible GNU Fortran toolchain and NetCDF-Fortran so `gfortran` and `nf-config` are available, then run the root setup script:

```sh
./s01-setup_fortran.sh
```

After reviewing the generated staging file, activate it explicitly:

```sh
cp Mkinclude config/Mkinclude
```

The setup script never copies the file automatically and never changes `adm/Mkinclude`. It requires a successful NetCDF-Fortran compile, link, and read/write check before offering to generate the v5 configuration. OpenMP is enabled when its compiler check succeeds.

## Choosing a template manually

Copy one template to the ignored active path `config/Mkinclude` and adjust it for the local installation:

```sh
cp config/Mkinclude_gfortran config/Mkinclude
```

- [`Mkinclude_gfortran`](Mkinclude_gfortran): optimized GNU Fortran configuration for macOS and Linux.
- [`Mkinclude_gfortran_debug`](Mkinclude_gfortran_debug): GNU Fortran with runtime checks and debug information.
- [`Mkinclude_linux_ifx`](Mkinclude_linux_ifx): Linux Intel oneAPI `ifx`; load the compiler environment and provide a compatible NetCDF-Fortran `nf-config`.

GNU templates obtain Fortran include and link flags from `nf-config --fflags` and `nf-config --flibs`. Where available, `nc-config --libs` supplements the NetCDF-C search path needed by installations such as Homebrew; its duplicate `-lnetcdf` flag is filtered because `nf-config` already supplies it. Set `NF_CONFIG` or `NC_CONFIG` when either tool has a nonstandard name, or override `INC` and `LIB` for a custom installation. A missing `config/Mkinclude` produces a clear Make error; v5 builds do not fall back to `adm/Mkinclude`.

## Build entry points

```sh
make -C src_main all
make -C map/tools/code all
```

The active `config/Mkinclude` is local and machine-specific. It is ignored by Git; the three templates and this guide are tracked. Keep compiler outputs and local configuration out of commits. Optional MPI, Heatlink, ICI, and sediment configurations require additional setup and are not implied to be validated by the standard templates.
