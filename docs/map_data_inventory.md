# CaMa-Flood Map Data Inventory (Review Draft)

**Status:** Phase 1 reference inventory. The packaged v4.30 `glb_15min` map and standard sample run are the verified example, not a universal map specification. Common variable definitions now live in [the shared CSV](../map/map_variables.csv), with schema and design rationale in [map metadata specification](../map/map_metadata_spec.md). This document retains reference-dataset evidence and reader/workflow findings; it does not decide a NetCDF layout or conversion behavior.

## 1. Scope and reference configuration

The verified sample is [`gosh/test1-glb_15min.sh`](../gosh/test1-glb_15min.sh#L80). It selects binary maps (`LMAPCDF=.FALSE.`), provides file names through the map namelist, selects `rivwth_gwdlr.bin` as `CRIVWTH`, and enables bifurcation input with `LPTHOUT=.TRUE.`. Those choices describe the sample configuration, not mandatory settings for every run.

This document preserves the source and dataset evidence for the core reference configuration. The broader product register, including package-observed auxiliary/MPI products and traceable source-only outputs, is maintained in [`map/map_variables.csv`](../map/map_variables.csv). Specialized products remain marked provisional where the source evidence does not establish their semantics.

The packaged map data were inspected read-only. The binary data are distributed separately from this source repository.

## 2. General binary map format

The following are general properties verified in the v4.30 model readers, rather than assumptions based on the global sample's dimensions:

- The binary readers use Fortran direct-access, unformatted records without a file header. Record length is calculated as `4 × NX × NY` bytes.
- Network indices use `JPIM` (4-byte integer); real map data use `JPRM` (4-byte real). The arrays are read with Fortran shape `(NX, NY)`; x is the first/fastest array index.
- A scalar map has one record. `nextxy.bin` contains two records, `nextx` followed by `nexty`. `fldhgt.bin` contains `NLFP` records, one profile layer per record.
- Dataset-specific byte order and row orientation must be obtained from that dataset's metadata or provenance. The reference CTLs specify little-endian storage and `yrev`; the model's `LMAPEND` setting controls byte conversion and defaults to false. Do not assume every map package has the same endian convention, geographic extent, or `NLFP`.
- For the inspected package, real map grids store `-9999` outside network cells. The model configuration defines `IMIS=-9999` and `RMIS=1.E20`; these distinct constants must not be silently conflated. The reader does not rewrite all real-grid `-9999` values to `RMIS`.
- `nextxy` uses special negative codes. The package contains `-9`, `-10`, and `-9999`; the v4.20 manual identifies these as river mouth, inland termination, and ocean/undefined, respectively. The runtime sequence logic treats negative non-missing x indices as endpoints and does not distinguish the first two codes in that path.

The general dimensions used in this document are:

- `NX`: number of map columns.
- `NY`: number of map rows.
- `NLFP`: number of floodplain-profile layers for a dataset.

For a file described as `(NX, NY)`, the in-memory Fortran array has those bounds. For a multi-record profile, record `k` is layer `k`, for `1 ≤ k ≤ NLFP`. A future NetCDF dimension/order mapping remains undecided.

## 3. Common metadata and verified distribution

The shared CSV is the authoritative variable- and product-level registry for variable names, proposed long names and units, types, symbolic dimensions, records, missing-value conventions, workflow/runtime roles, distribution roles, observed package availability, source processing tools, model readers, and current NetCDF lookup names. It applies across map datasets; it does not encode the concrete dimensions of this reference dataset.

The reference run selects core binary inputs `nextxy.bin`, `ctmare.bin`, `elevtn.bin`, `nxtdst.bin`, `rivlen.bin`, `fldhgt.bin`, `rivwth_gwdlr.bin`, `rivhgt.bin`, and `rivman.bin`, plus `bifprm.txt` because its bifurcation switch is enabled. The core network/topography maps are distributed. Channel geometry fields are intended to be generated or checked for a target map; the inspected `glb_15min` package happens to include precomputed copies.

The maintainer confirmed that original global maps were produced in Yamazaki Laboratory; [Yamazaki et al. (2011)](https://doi.org/10.1029/2010WR009726) describes the FLOW methodology at overview level, while detailed production procedures are not public. Repository regional extraction tools crop or derive regional products and are not original global-map generators.

## 4. Reference dataset: packaged `glb_15min`

These values are specific to the inspected v4.30 package and must not be generalized to other map datasets:

- `NX=1440`, `NY=720`, `NLFP=10`.
- Grid spacing: 0.25° (15 arc minutes), longitude edges −180° to 180°, latitude edges −90° to 90°.
- Observed binary sizes: `nextxy.bin` 8,294,400 bytes (two int32 records); scalar float32 maps 4,147,200 bytes each; `fldhgt.bin` 41,472,000 bytes (10 records).
- Package CTLs specify `options yrev little_endian` and `undef -9999`. These are evidence for this dataset's GrADS interpretation, not universal defaults for every map.
- The standard sample pairs this map with `diminfo_test-1deg.txt`, which describes a distinct runoff/input grid of 360 × 180 and `INPN=7`. A separate sample diminfo uses `INPN=1` for the 1-degree grid workflow; see Section 6.

## 5. Metadata and configuration reference

The field-by-field formats and producer/reader roles for `params.txt`, `mapdim.txt`, `diminfo*.txt`, and GrADS CTLs are documented in [`map_metadata_spec.md`](../map/map_metadata_spec.md#metadata-and-configuration-files).

Reference-dataset observations: `params.txt` describes the 1440 × 720 × 10 global 0.25° map; the sample `diminfo_test-1deg.txt` pairs those map dimensions with a separate 360 × 180 input grid and `INPN=7`. The package's `mapdim.txt` repeats only the map dimensions and is an auxiliary input to external map-generation algorithms, not the simulation. The top-level CTLs use generic `Flow Direction` titles for scalar maps, and `rivman.bin` has no corresponding CTL in the inspected directory.

## 6. Existing NetCDF reader correspondence

The reader mapping is listed in [`map_variables.csv`](../map/map_variables.csv). In source, `READ_MAP_CDF` and `READ_TOPO_CDF` request the module-level variables through `CRIVCLINC` and `CRIVPARNC`. No NetCDF map dataset was present in the inspected v4.30 package. The maintainer confirmed that these module-level files were generated at ECMWF and that code to reproduce this workflow is planned; their complete output contract remains open.

Some partial NetCDF output code already exists in `calc_rivwth.F90`. It writes `rivpar.nc` with `lon`, `lat`, `rivwth`, and `rivhgt`, but omits `rivman`. It sets `missing_value=1.e20` while writing `-9999` outside the network. This partial parameter output is not the complete module-level container expected by the model reader. Do not infer the future conversion or fill-value policy from it.

## 7. Processing and model-reading path

The reference script supplies map paths to the model. `CMF_RIVMAP_INIT` reads network topology and configured bifurcation input; `CMF_TOPO_INIT` reads topography and channel parameters. Binary and NetCDF paths are implemented separately in `src/cmf_ctrl_maps_mod.F90`.

For the verified channel-parameter sequence and program inputs/outputs, see [the channel workflow section in `map_metadata_spec.md`](../map/map_metadata_spec.md#channel-parameter-generation-workflow). Regional tools such as [`cut_domain.F90`](../map/src/src_region/cut_domain.F90) crop existing maps; [`set_map.F90`](../map/src/src_region/set_map.F90) can derive separate grid-oriented products. Neither is the original global-map generator.

## 7. Open Questions

Remaining metadata and conversion questions are grouped in [the `Open Questions` section of `map_metadata_spec.md`](../map/map_metadata_spec.md#open-questions). This inventory does not repeat them; the current reference binary audit and package evidence can stand independently.

## 8. Evidence checked

- Reference map configuration: [`test1-glb_15min.sh`](../gosh/test1-glb_15min.sh#L80).
- General binary/NetCDF map readers and `NMAP`: [`cmf_ctrl_maps_mod.F90`](../src/cmf_ctrl_maps_mod.F90#L232).
- Dimension configuration and sentinel constants: [`cmf_ctrl_nmlist_mod.F90`](../src/cmf_ctrl_nmlist_mod.F90#L180).
- Direct-access runoff input matrix layout: [`cmf_ctrl_forcing_mod.F90`](../src/cmf_ctrl_forcing_mod.F90#L450), [`inpmat_mod.f90`](../src/mod/inpmat_mod.f90#L62).
- Map dimension and input-matrix metadata generation: [`generate_inpmat.F90`](../map/src/src_param/generate_inpmat.F90#L45) and [`s02-generate_inpmat.sh`](../map/src/src_param/s02-generate_inpmat.sh#L20).
- Parameter generation and regional processing: [`calc_rivwth.F90`](../map/src/src_param/calc_rivwth.F90#L155), [`set_gwdlr.F90`](../map/src/src_param/set_gwdlr.F90#L1), [`cut_domain.F90`](../map/src/src_region/cut_domain.F90#L1), [`set_map.F90`](../map/src/src_region/set_map.F90#L1).
- Historical scientific meanings and map conventions: [CaMa-Flood v4.20 manual](../doc/Manual_CaMa-Flood_v420.docx). Public overview of the FLOW-based map-generation approach: [Yamazaki et al. (2011), *A physically based description of floodplain inundation dynamics in a global river routing model*](https://doi.org/10.1029/2010WR009726).

- Reference-package evidence: `glb_15min/params.txt`, `mapdim.txt`, the three `diminfo_*.txt` examples, CTLs, file sizes, and binary samples were inspected read-only from the v4.30 package. No package data were changed or copied into the repository.
