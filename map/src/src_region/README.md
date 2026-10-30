# Regional map preparation

This guide explains how to create a regional CaMa-Flood river map from an existing global map using the reusable tools in `map/src/src_region/`. The normal user workflow is to configure and run `s01-regional_map.sh` once. The script calls the Fortran map-preparation programs and generates GrADS CTL metadata for the regional map and available high-resolution maps at the end.

The source code repository provides these processing tools; it does not contain every global map dataset. Obtain a compatible map dataset separately and follow its access and data-use terms. The [Mekong development/test example](../../README.md#development-and-test-sample) is described in the map overview.

## Prerequisites and source data

You need:

- A CaMa-Flood distribution or source tree with `map/src/src_region/`, `adm/Mkinclude`, and `map/`.
- A compatible global map directory with its `params.txt` and core map binaries.
- A Fortran compiler and `make`, configured through the active `adm/Mkinclude`.
- The source map's `bifori.txt`; the current orchestration script always runs the regional bifurcation step and expects this file.
- High-resolution source layers only when you want the regional workflow to prepare high-resolution products or the parameterization input matrix. The script checks supported high-resolution directories and skips absent ones.

The base regional extraction program reads the following files from the source map (**code-derived requirements**):

```text
params.txt
nextxy.bin  downxy.bin  elevtn.bin  fldhgt.bin
ctmare.bin  grdare.bin  uparea.bin  lonlat.bin
nxtdst.bin  rivlen.bin  width.bin
```

The source `params.txt` supplies grid dimensions, floodplain layer count, grid spacing, and geographic bounds. The binaries must match those dimensions, data types, record counts, and the formats expected by the Fortran readers. Matching filenames alone does not establish compatibility.

For each high-resolution tag that is to be processed, the source directory needs `location.txt` and the catchment-index layer files named in it, including `<area>.catmxy.bin`. Other source layers are read when available. The current `s01-regional_map.sh` checks `1min`, `30sec`, `15sec`, and `3sec`; it does not automatically process every tag supported internally by `combine_hires`.

The GitHub source checkout and map data are separate concerns. Download or otherwise obtain the global map dataset from the CaMa-Flood distribution source, then place it where your regional configuration can reference it. High-resolution files are not needed to extract the base map, but later high-resolution products and the parameterization input matrix depend on their availability. `generate_inpmat` creates a matrix for calculating channel width and depth parameters with the sample annual-mean runoff data in `map/data`; it does not create the runoff input matrix used by a simulation. The simulation runoff input matrix is expected to be prepared later in `src_param`.

## Quick Start

The example below assumes a package or checkout root with `map/` and `adm/`, and a global source at `map/glb_15min/`. Replace the source path and bounds for your map.

```sh
cd map
mkdir myregion_15min
cp -a src/src_region myregion_15min/
cd myregion_15min/src_region
```

Edit `s01-regional_map.sh` and set `SOURCE`, `WEST`, `EAST`, `SOUTH`, and `NORTH`. Then build and run the complete workflow from the copied `src_region/` directory:

```sh
make
./s01-regional_map.sh
```

Inspect the generated `../params.txt`, map binaries, optional high-resolution directories, `../bifori.txt`, parameterization input-matrix files, and CTLs. The final workflow steps generate map CTLs and CTLs for each available high-resolution directory. The script does not validate scientific suitability or guarantee that every internal command succeeded; use the checks below.

## Prepare the regional working directory

Keep three locations distinct:

- The original global map is read-only input. Do not place generated regional files inside it.
- `map/src/src_region/` contains reusable source programs and scripts.
- `map/myregion_15min/` is the regional output root. The copied `src_region/` directory is the working directory; most generated map files are written to its parent.

The copy command in the Quick Start uses the package's ordinary source files. It does not depend on the relative symbolic links used by some Git-tracked map examples. Keep the directory relationship intact: the Makefile includes `../../../adm/Mkinclude`, and the programs use paths relative to the working directory and its parent.

## Configure the target region

Edit the settings near the beginning of `s01-regional_map.sh`:

```sh
SOURCE="../../glb_15min/"
WEST="90.0"
EAST="110.0"
SOUTH="5.0"
NORTH="35.0"
```

- `SOURCE` is the source map directory, relative to the current working directory or an absolute path. Keep the trailing slash: the Fortran program appends filenames directly to this value.
- `WEST` and `EAST` are longitude boundaries in degrees.
- `SOUTH` and `NORTH` are latitude boundaries in degrees.

Run the script while your current directory is the map-local `src_region/`. The script writes these five values, in that order, to `region_info.txt`; the Fortran programs read that file.

Choose bounds inside the source map's coverage and on its grid boundaries, using the source resolution and longitude convention. The extraction code calculates grid indices from the source spacing and may adjust bounds close to a grid edge. It does not provide comprehensive validation for out-of-range, reversed, wrapped, or otherwise incompatible bounds. Do not assume that a region crossing the longitude wrap is supported. Confirm the resulting dimensions and bounds in `params.txt`.

For example, the Mekong development domain uses a 15-minute parent map and bounds 90–110°E, 5–35°N. It is an example only; the same values are not appropriate for every source map.

## Compile the tools

The copied `Makefile` builds all nine utilities used by the workflow. It reads the active `adm/Mkinclude` through a relative path. From the regional working directory, run:

```sh
make
```

The required build dependencies are a compatible Fortran compiler and `make`. Review the active compiler settings in `adm/Mkinclude`; the available configurations and compiler selection are described in [`adm/README.md`](../../../adm/README.md). NetCDF and GrADS are not required to build or run this regional map preparation workflow: the CTL programs write text metadata and do not invoke GrADS.

To remove local executables and build byproducts before rebuilding with changed compiler settings, use `make clean`, then `make`. Do not run `make clean` while a workflow is using those executables.

## Run regional map generation

From the copied map-local `src_region/`, run the orchestration script once:

```sh
./s01-regional_map.sh
```

The script writes `region_info.txt`, runs base-domain extraction, bifurcation adjustment, and derived river-network setup, processes each available high-resolution tag in its configured list, and selects the first available tag from its parameterization-matrix priority list. It then generates base-map CTLs and CTLs for each available high-resolution map.

The parameterization input matrix is conditional. In the current script, the generation priority is `1min`, `30sec`, `15sec`, then `3sec`. `generate_inpmat` writes `inpmat_test-1deg.bin` and `diminfo_test-1deg.txt`; source inspection shows that it maps the regional high-resolution pixels to a regular 1-degree grid. This matrix is used alongside the sample annual-mean runoff data in `map/data` during the later channel width/depth parameter calculation. It is not the runoff input matrix used by a simulation; that matrix is expected to be prepared later in `src_param`.

The shell script does not enable `set -e` or check each program's return code before continuing. A later command may run after an earlier command reports an error. Review the complete terminal output and verify the expected files; a final shell exit status by itself is not proof that all stages succeeded. The script also prints a diagnostic path with a trailing `o` (`location.txto`); the actual file test uses `location.txt`.

## Generated files and directory structure

The current source code writes products mainly under the regional map root:

- `params.txt`: regional dimensions, layer count, spacing, and bounds.
- Core regional binaries: `nextxy.bin`, `downxy.bin`, `elevtn.bin`, `fldhgt.bin`, `ctmare.bin`, `grdare.bin`, `uparea.bin`, `lonlat.bin`, `nxtdst.bin`, `rivlen.bin`, `width.bin`, and `lsmask.bin`.
- Derived network products from `set_map`: `rivseq.bin`, `uparea_grid.bin`, `upgrid.bin`, `nxtdst_grid.bin`, `rivlen_grid.bin`, `basin.bin`, and `bsncol.bin`.
- `bifori.txt`: region-adjusted bifurcation pathways. The current script runs this step unconditionally.
- `<tag>/` directories: high-resolution products and `location.txt` for tags whose source `location.txt` is found and whose inputs can be processed.
- `inpmat_test-1deg.bin` and `diminfo_test-1deg.txt`: parameterization input matrix and metadata, generated when the script finds a supported high-resolution tag in its priority list. These are not the simulation runoff input matrix.
- `.ctl` files: GrADS descriptions for the base map and high-resolution maps, generated at the end of `s01-regional_map.sh`. They are metadata, not map data.
- In the working `src_region/`: `region_info.txt`, `dim_change.txt`, `nextxy_noedge.bin`, and local executables/build files.

The CTL writer creates descriptions for a fixed list of filenames and does not first confirm that every corresponding binary exists. A CTL file can therefore refer to an optional or absent binary. Check its `dset` entry and confirm that the target file exists.

This workflow prepares regional network and map products. It does not run the CaMa-Flood model and does not by itself produce all channel parameters or other optional inputs required by a particular simulation. Prepare those separately using the relevant map-parameter workflow and feature documentation.

## Check the results

At minimum, inspect the reported command output and confirm that the regional dimensions and bounds are plausible:

```sh
cat ../params.txt
```

Check that the core inputs and expected derived files are non-empty:

```sh
for f in nextxy.bin downxy.bin elevtn.bin fldhgt.bin ctmare.bin grdare.bin uparea.bin lonlat.bin nxtdst.bin rivlen.bin width.bin lsmask.bin rivseq.bin uparea_grid.bin upgrid.bin nxtdst_grid.bin rivlen_grid.bin basin.bin bsncol.bin; do
  test -s "../$f" || echo "Missing or empty: ../$f"
done
test -s ../bifori.txt || echo "Missing or empty: ../bifori.txt"
```

If a high-resolution source was available, check the corresponding regional `location.txt` and listed binary layers. If the parameterization input matrix was generated, check both the matrix and its `diminfo` file. Confirm that each generated CTL points to an existing binary and describes the dimensions and resolution in `params.txt` or the high-resolution `location.txt`.

These checks confirm file presence and basic metadata consistency only. They do not establish river-network correctness, conservation, or scientific suitability. The tools do not provide a complete automated validation of those properties.

## Troubleshooting and limitations

- **A source file cannot be opened:** confirm `SOURCE`, its trailing slash, the working directory, and the required files listed above.
- **The output bounds or grid dimensions are unexpected:** compare the requested bounds with the source `params.txt` and grid spacing. Use bounds on source-grid edges and inspect the resulting `params.txt`.
- **High-resolution products are absent:** the current script only enters a tag when its source `location.txt` exists. Check the source tag and all layers named by that file.
- **No parameterization input matrix is generated:** the script requires one of its recognized high-resolution tags under the regional map root. The current Fortran generator also assumes its configured 1-degree regular input grid. The simulation runoff input matrix is prepared later under `src_param`.
- **The script continues after an error:** inspect the output from each stage and the products listed above. The current shell script does not stop on every failed child command.
- **A CTL exists but visualization fails:** confirm that its `dset` target exists and that the file dimensions, type, and map bounds match the control file.

The descriptions in this manual reflect source inspection. They do not claim that the complete workflow has been executed successfully for every map, compiler, region, or high-resolution dataset. The Mekong example records the current development input and verification separately.
