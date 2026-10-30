# Future parameter workflow

The `src_param` workflow migration is planned for Phase B2. Shared Fortran/Python implementations will live under `map/tools/code/`; Fortran sources will use the `param_*.F90` prefix and the unified Makefile's `params` target. Build shared executables explicitly from `map/tools/code/`. Map-local scripts will call those executables through a relative path while preserving the map-local working directory required by programs' relative input and output paths. No parameter-generation programs or scripts are implemented in Phase B1.
