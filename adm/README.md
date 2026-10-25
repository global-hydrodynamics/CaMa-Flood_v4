# Build configuration (`adm/`)

`adm/` contains the Make include files that select a compiler and configure build-time features for CaMa-Flood. The source Makefiles include `adm/Mkinclude`; the selected file supplies compiler commands, flags, library paths, and preprocessor definitions. You normally configure these settings here, then build with the existing Makefiles and scripts.

## Quick start: five settings to check

1. **Compiler:** choose an installed compiler configuration and make it the active `adm/Mkinclude`.
2. **`FC` and `FCMP`:** set both to the compiler appropriate to the sources they build; for MPI, `FCMP` must also be the MPI wrapper used for model compilation and linking.
3. **MPI:** enable `DMPI=-DUseMPI_CMF` and use an MPI-enabled `FCMP` only when building the MPI model.
4. **NetCDF:** enable `DCDF=-DUseCDF_CMF` and configure the Fortran NetCDF include and link paths when NetCDF input/output is needed.
5. **`SinglePrecisionMode`:** the build macro is spelled `SinglePrec_CMF`; uncomment `DSINGLE=-DSinglePrec_CMF` to enable it, or comment it out for standard JPRB precision. It is a compile-time choice, not a namelist setting. The checked-in active `adm/Mkinclude` currently enables this macro.

OpenMP is configured separately from MPI. The checked-in configurations enable it by adding the compiler's OpenMP option to `FCMP`; at run time, scripts such as those under `gosh/` set `OMP_NUM_THREADS`.

## How the Mkinclude system works

Makefiles include the fixed path `adm/Mkinclude`, including `src/Makefile`, the utility and map Makefiles, and several optional component Makefiles. The compiler-specific files in this directory are templates; `make` does not automatically select one based on the host.

From the model root (a GitHub checkout or extracted package), first keep a copy of your active file, then copy the chosen template to `adm/Mkinclude`:

```sh
cp adm/Mkinclude adm/Mkinclude.backup
cp adm/Mkinclude_linux_ifx adm/Mkinclude
```

Restore the backup or copy another template to switch back. This makes the selected template the active configuration for this installation. After changing compiler, preprocessor definitions, or precision mode, do a clean build so modules and objects are rebuilt consistently. From `gosh/`, `./compile.sh` cleans and rebuilds by default; `./compile.sh no` requests a forced build and `./compile.sh clean` only cleans. See [`gosh/README.md`](../gosh/README.md) for the sample execution workflow.

The Makefiles use `FFLAGS` for compilation, `LFLAGS` for executable linking, `INC` for include paths, and `LIB` for libraries. `CFLAGS` is the existing variable name for Fortran preprocessor definitions; it is not a C-only flag variable in this build. Do not confuse a definition such as `-DUseCDF_CMF` with compiler options such as `-O3` or link options such as `-lnetcdff`.

## Compiler selection and `FC` / `FCMP`

The checked-in configurations provide GNU and Intel ifort variants for macOS/Linux, plus debug variants and a Heatlink-specific configuration. [`Mkinclude_linux_ifx`](Mkinclude_linux_ifx) provides an Intel ifx template. Inspect the active `adm/Mkinclude` before building; package releases may supply a different active configuration. Compiler paths and flags may need adjustment for the local installation.

`FC` and `FCMP` are used in different parts of the existing build:

- `FC` compiles utility and map-preprocessing programs (for example, `util/Makefile` uses it for `igetday`).
- `FCMP` compiles the CaMa-Flood Fortran modules and main program under `src/`, and links the model executable. It is also used by some component Makefiles and source-level test targets.

The standard GNU include sets `FC=gfortran` and `FCMP=gfortran -fopenmp`; the Linux Intel include sets `FC=ifort` and `FCMP=ifort -qopenmp`. This separation lets the model compiler command carry OpenMP or MPI wrapper options while simple preprocessing tools use a plain compiler command. For an MPI build, the `FCMP` compiler/wrapper must provide the MPI module and link libraries; setting only `DMPI` is not enough. Keeping `FCMP` and `FC` inconsistent with the compiler used to build dependent `.mod` files can cause missing module files, incompatible module formats, or link errors.

## Major build options

### NetCDF

NetCDF support is optional compile-time code guarded by `UseCDF_CMF`. Enable `DCDF=-DUseCDF_CMF` in the active include and provide the NetCDF-Fortran module include directory and libraries. The ifx template supports a configurable `NETCDF_ROOT`; when set, it enables the macro and supplies `INC` and `LIB` for the usual `include/` and `lib/` layout:

```sh
cd gosh
NETCDF_ROOT=/path/to/netcdf ./compile.sh
```

For separate installation prefixes, edit `INC` and `LIB` in the active include. NetCDF Fortran needs a compiler-compatible `netcdf.mod` and linkable Fortran library (commonly `-lnetcdff`, often together with `-lnetcdf`). A missing module usually points to an incorrect include path or compiler mismatch; unresolved `nf90_*` symbols usually point to missing or incompatible link libraries. Setting a NetCDF namelist switch cannot add NetCDF support to a binary built without `UseCDF_CMF`.

### MPI

MPI support is compiled through `UseMPI_CMF`, enabled by `DMPI=-DUseMPI_CMF`. The `FCMP` command must be an MPI wrapper compatible with the selected compiler; for Intel oneAPI MPI this is commonly `mpiifx`, while GNU installations commonly provide `mpifort` or `mpif90`. The ifx template shows `mpiifx -qopenmp` as an alternative to the standard `FCMP` line. Configure `FCMP` and `DMPI` together, then clean and rebuild.

An MPI-enabled executable is not launched automatically in parallel. The execution command must also use the site's MPI launcher and process-count settings, as in the example under [`gosh/`](../gosh/README.md). MPI and OpenMP are distinct: MPI uses processes and communication; OpenMP uses threads within each process. A hybrid run needs both build support and suitable launch/thread settings.

### OpenMP

OpenMP compiler flags are part of the compiler command in the checked-in includes: GNU uses `-fopenmp`; Intel ifort/ifx use `-qopenmp`. These options are present in `FCMP`, so they apply while compiling and linking model code. The runtime thread count is normally set with `OMP_NUM_THREADS` in an execution script. Removing the compiler flag disables OpenMP code generation even if that environment variable is set.

### `SinglePrecisionMode`

`SinglePrecisionMode` is the descriptive name used in the repository's configuration comments. Its actual preprocessor macro is `SinglePrec_CMF`, passed through the Make variable `DSINGLE` and included in `CFLAGS`:

```make
DSINGLE=-DSinglePrec_CMF   # enable SinglePrecisionMode
# DSINGLE=                 # omit the macro for standard JPRB precision
# The checked-in adm/Mkinclude currently enables DSINGLE; restore that file
# to return to the repository-provided default configuration.
CFLAGS=$(DMPI) $(DCDF) $(DATM) $(DSINGLE) $(DHEATLINK)
```

In [`src/parkind1.F90`](../src/parkind1.F90), that macro changes `JPRB` from `SELECTED_REAL_KIND(13,300)` to `SELECTED_REAL_KIND(6,37)`. Many model state, forcing, and diagnostic arrays use `JPRB`, so those values use a smaller floating-point kind in this mode. It does **not** change every real value: `JPRD` remains `SELECTED_REAL_KIND(13,300)` and is used for important river and floodplain storage and water-budget accumulators; `JPRM` remains the four-byte map/output kind.

The source establishes these storage kinds, but does not establish a measured whole-model memory reduction or accuracy/performance result. Arrays declared as `JPRB` use fewer bytes per element in SinglePrecisionMode; retained `JPRD` arrays do not. Results can differ because calculations use a different precision, so numerical reproducibility between modes is not implied.

The standard gridded binary and NetCDF model output fields are written as four-byte/`NF90_FLOAT` values in either mode. Binary restart precision is a separate runtime setting, `LRESTDBL` (`.TRUE.` for double, `.FALSE.` for single); NetCDF restart state variables are defined as `NF90_DOUBLE`. The compile-time mode can affect the values before output conversion even where the file representation stays the same. Rebuild all dependent objects and modules after switching modes.

### Other compile-time and runtime settings

The active include combines optional preprocessor definitions in `CFLAGS`, including `UseMPI_CMF`, `UseCDF_CMF`, `SinglePrec_CMF`, `NoAtom_CMF`, and, where configured, `heatlink`. `FFLAGS` contains compiler-specific optimization, diagnostics, preprocessing, and language-mode settings; debug includes replace optimization flags with checks and backtraces. Optional components can also require extra libraries or Makefile variables. Not every model option is controlled here: compile-time macros come from the active Make include (the standard execution scripts invoke Make but do not automatically select a compiler configuration), while Fortran namelists control runtime settings. For example, `OMP_NUM_THREADS` is a Shell environment setting, while `LDAMOUT` is a runtime namelist value.

## Using Intel ifx

`Mkinclude_linux_ifx` uses `ifx` for `FC` and `FCMP` and enables OpenMP with `-qopenmp`. The flag is documented by Intel for ifx; `-fpp` enables Fortran preprocessing, which this source requires for `#ifdef` options. To activate the configuration, copy it to the fixed active path as shown above. Source the oneAPI environment setup before building so the compiler and libraries are available.

For MPI, install/configure Intel MPI for the same oneAPI compiler, enable `DMPI`, and change `FCMP` to `mpiifx -qopenmp`. For NetCDF, set `NETCDF_ROOT` to an installation built for a compatible compiler/runtime. Keep `FC=ifx` unless utility/map programs also need a wrapper. The repository's standard build does not automatically select MPI, NetCDF, or ifx based on the runtime script.

The ifx file is a compiler-configuration template. Verify compilation and the required model workflows with your Intel oneAPI, MPI, and NetCDF versions before using that configuration for production simulations.

## Troubleshooting

- **`ifx: command not found`:** load the oneAPI compiler environment or install the Intel Fortran compiler through the site's supported setup.
- **Missing `netcdf.mod` or `nf90_*` link errors:** check `NETCDF_ROOT`, `INC`, and `LIB`; ensure NetCDF-Fortran was built for a compatible compiler.
- **Missing MPI module or unresolved MPI symbols:** use the compiler-matched MPI wrapper for `FCMP` and enable `UseMPI_CMF` together.
- **Unexpected mixture of compilers or precision modes:** clean the build before rebuilding. Fortran `.mod` files and object files depend on compiler and preprocessing settings.
- **Parallel execution still uses one process:** build support and launching are separate; use the local MPI launcher and the appropriate process count.

## Related documentation

- [Source architecture and Fortran modules](../src/README.md)
- [Sample build and run scripts](../gosh/README.md)
- [User guideline](../doc/CaMa-Flood_Guideline.md)
- [Main manual](../doc/Manual_CaMa-Flood_v420.docx)
- [HPC note](../doc/Note_HighPerformanceComputing.docx)
