# Map parameterization and preparation workflows

`script_params/` groups workflows for preparing map-specific parameters and related input products. Its planned scope includes:

- Channel width, depth, roughness, and related parameters.
- User-configured runoff input matrices and climatological discharge preparation.
- Bifurcation and MPI map setup.
- Flow-gauge, level-gauge, and dam location allocation.

Shared Fortran sources and executables belong in [`../code/`](../code/), with `param_*` and `allocate_*` executable prefixes where useful. The approved shell families are `script_region/`, `script_params/`, and `script_analysis/`; do not create a separate `script_allocate/`. The legacy tools under `map/src/` remain unchanged.

## General runoff input matrix

[`s02-generate_inpmat.sh`](s02-generate_inpmat.sh) invokes the shared `param_generate_inpmat` program to create a map-to-runoff mapping matrix for a user-selected regular runoff grid. Build the program from the repository root:

```sh
make -C map/tools/code param
```

Copy the wrapper into a target map's `script_params/` directory, edit its runoff-grid configuration and output names, then run `./s02-generate_inpmat.sh` from that directory (the wrapper also resolves its map paths when invoked elsewhere). Its retained example is a 0.25-degree regular grid with longitude bounds -180 to 180 degrees, latitude bounds -60 to 90 degrees, and `NtoS` row order. The configured grid settings describe the runoff data and are independent of the CaMa-Flood map resolution and bounds. `TAG` selects a high-resolution map directory; by default the wrapper prefers `1min/`, then `15sec/`. Set `TAG` explicitly when another supported map tag is needed.

The target map must provide `params.txt`, `<TAG>/location.txt`, and each area's `<AREA>.catmxy.bin`. The catchment file has two direct-access int16 records (x indices, then y indices); dimensions come from `location.txt`. `<AREA>.grdare.bin` is an optional direct-access float32 area grid in km2; if absent, the legacy area calculation is used. The runoff data file itself is not read when building the matrix; its grid metadata must be used to configure the wrapper consistently.

The program runs with the map root as its working directory and writes the configured dimension metadata and matrix there. The metadata records the base-map and input-grid dimensions, maximum contribution count, matrix filename, and base-map bounds. The binary matrix contains three grouped direct-access record blocks: `mmax` int32 x-index records, `mmax` int32 y-index records, then `mmax` float32 overlap-area records in m2. Those output paths may replace existing files; back them up or choose new names before running. The wrapper checks the executable and required inputs and returns failure if the command fails or either output is missing.

`param_generate_inpmat` is the configurable matrix generator. It is distinct from `region_default_inpmat`, which produces the standard annual-runoff-climatology matrix distributed with prepared maps. The matrices have different roles and settings and must not be treated as interchangeable.

The `inpmat` produced by this workflow will be required for a later sample simulation. Which runoff/map sample to use, and which supporting datasets should be included in GitHub versus the downloadable package, are deferred decisions. Select that sample after reviewing the data each distribution route can include; this does not change the current example settings.

The current migration was compared against the legacy v4 executable on a temporary copy of the Git-tracked Mekong map. See the [Phase B2 migration plan](../../../docs/map_params_migration_plan.md) for the exact comparison and its limits. This execution-level regression does not constitute model or scientific validation.

## Other workflows

The Python dam-parameter estimation workflow remains under legacy `map/src/src_dam/` and is distinct from Fortran dam location allocation. No other parameter or allocation utilities have been migrated by this step.
