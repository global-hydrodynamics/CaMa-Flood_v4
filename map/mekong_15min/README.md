# `mekong_15min` official sample map

`mekong_15min` is the official, self-contained sample map distributed with the CaMa-Flood v4 source repository and user package. Its data can be used directly from this directory; no path to the maintainer's computer or external map download is needed.

## Dataset contents

This regional map was prepared from the global 15-minute (`glb_15min`) map for 90–110°E, 5–35°N. It has `NX=80`, `NY=120`, `NLFP=10`, and 0.25° map cells. The complete `1min/` high-resolution dataset is included (1200 × 1800 cells; approximately 72 MB of binary layers).

The directory contains the core river-network/topography maps, channel parameters (`rivwth`, `rivhgt`, `rivman` and the GWD-LR width alternative), the `inpmat_test-1deg.bin` runoff input matrix and paired `diminfo_test-1deg.txt`, bifurcation tables and derived maps, auxiliary/network products, high-resolution layers, and GrADS CTL metadata. MPI-specific preparation products are included as reference files; their availability does not make MPI configuration necessary for ordinary runs.

[`map_metadata.json`](map_metadata.json) supplies explicit little-endian, north-to-south, regular lon/lat settings and relative paths for the six category-11 core files used by the CTL and NetCDF conversion utilities. It describes this dataset only; do not reuse it for another map.

## Using and validating the sample

Use this directory as the map input when adapting map tools or testing the core binary-to-NetCDF workflow. The included map files and conversion settings are relative to this directory. Some legacy CTLs describe optional products that are not included (`catmpx.bin`, `outfix.bin`, `outupa.bin`, and `rivhgt_pth.bin`); those files are not required by the standard sample map.

Run the lightweight repository check from the root with `python3 map/tests/validate_mekong_sample.py`. GitHub Actions runs the same check without downloads or model simulations.

## Regeneration and provenance

The sample's map-specific regionalization settings are preserved in [`src_region/region_info.txt`](src_region/region_info.txt) and [`src_region/s01-regional_map.sh`](src_region/s01-regional_map.sh). In the Git repository, its Fortran source entries are relative links to the maintained implementation in [`../src/src_region/`](../src/src_region/), keeping one maintained source copy. The source archive command may materialize those links as files inside the archive so the downloaded package retains a self-contained map-local workflow. Build the common tools using the shared Makefile, then run the sample-local workflow from `src_region/` as documented in [`map/src/src_region/README.md`](../src/src_region/README.md).

Regenerating this regional sample requires a compatible parent map and its high-resolution source layers at the relative path recorded in `region_info.txt`. Reproducing the included channel parameters also requires the runoff climatology and width data used by `map/src/src_param/s01-channel_params.sh`. Those source datasets are separate from this self-contained sample and are not needed to use the included outputs. The detailed original global-map generation procedure is not part of this repository.
