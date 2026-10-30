# Regional map preparation template

This template prepares a regional CaMa-Flood map from a compatible source map. It is the v5 workflow under `map/tools/`; the legacy v4 workflow remains under `map/src/src_region/`. Fortran programs are shared under `map/tools/code/` and built once for use by all map datasets.

## Inputs and configuration

The source directory must contain `params.txt`, `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, and `bifori.txt`. These requirements are derived from the current Fortran readers. Dimensions and binary layouts must match the source map parameters.

High-resolution processing is optional. For a supported tag (`1min`, `30sec`, `15sec`, or `3sec`), the source needs a `location.txt` and the layers it lists. The parameterization input matrix uses the first available regional high-resolution tag in the order `1min`, `30sec`, `15sec`, `3sec`, and also requires the sample annual-mean runoff input under `map/data`.

Configure the settings at the top of `s01-regional_map.sh`:

```sh
SOURCE="../../glb_15min/"
WEST="90.0"
EAST="110.0"
SOUTH="5.0"
NORTH="35.0"
```

`SOURCE` is relative to the current `script_region/` directory or an absolute path; retain its trailing slash. Configure bounds for the target region and source-map coverage.

## Build and run

Build shared regional executables once from the repository root, or rebuild after changing compiler settings:

```sh
make -C map/tools/code region
```

Then copy the template to a new map directory, configure the copied script, and run it from that map's `script_region/` directory:

```sh
mkdir -p map/myregion_15min
cp -a map/tools/script_region map/myregion_15min/
cd map/myregion_15min/script_region
./s01-regional_map.sh
```

The script checks that shared executables exist under `../../tools/code/`, then calls them through that path. It does not compile. Keep `script_region/` as the current working directory because the Fortran programs use relative input and output paths. The Makefile uses the active repository `adm/Mkinclude` compiler configuration and writes executables and compiler intermediates directly to `map/tools/code/`. Clean shared build products from the repository root with:

```sh
make -C map/tools/code clean
```

The naming convention is `region_<program>.F90` → `region_<program>`. Parameter tools will use `param_<program>.F90` → `param_<program>` and the same Makefile's `params` target.

## Outputs and checks

The workflow writes `region_info.txt`, `dim_change.txt`, and `nextxy_noedge.bin` in `script_region/`. Programs write map outputs mostly in the parent directory: `params.txt`; core binaries `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, and `lsmask.bin`; `bifori.txt`; and derived network files such as `rivseq.bin`, `basin.bin`, `bsncol.bin`, `upgrid.bin`, `uparea_grid.bin`, `nxtdst_grid.bin`, and `rivlen_grid.bin`. When source high-resolution tags are available, regional tag directories and their `location.txt` files are created. When an eligible tag and sample runoff data are available, `inpmat_test-1deg.bin` and `diminfo_test-1deg.txt` are produced.

The input matrix supports channel width/depth parameter calculation from sample annual-mean runoff. It is not the simulation runoff matrix; that is prepared in the later `src_param` workflow. CTL generation is a separate manual step after reviewing the regionalization outputs:

```sh
./s02-write_ctl.sh --dry-run
./s02-write_ctl.sh
```

The wrapper scans the base map and each immediate child directory with `location.txt`, using `params.txt` for the base grid and each high-resolution `location.txt` for its own grid. It replaces existing CTLs by default; use `--no-overwrite` to preserve them. For the regionalization input matrix, the shared filename suffix (such as `_test-1deg`) identifies the runoff-data/configuration variant: `inpmat_test-1deg.bin` is paired with `diminfo_test-1deg.txt`. This description file is written by the current Fortran program and supplies the matrix dimensions and record layout. The regional workflow does not invoke CTL generation automatically. Inspect terminal output, `../params.txt`, and expected files for configured source layers and optional branches. Check dimensions and file sizes against source parameters and output metadata. These checks establish basic file consistency only, not river-network correctness or scientific validity. The shell script checks build products but does not stop on every failed processing program, so review each program's output.

## Current limitations

This workflow does not invoke the separate CTL generator, migrate channel-parameter generation, or validate scientific properties. It assumes required source files and compatible binary formats. The actual v5 workflow remains unverified until the maintainer reports results.
