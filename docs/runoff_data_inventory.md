# CaMa-Flood v5 Runoff Input Data Tools: Phase A Inventory

**Status:** Phase A inventory, 2026-11-01, supplemented by the maintainer-assigned Phase B registry task on 2026-11-02; this addendum records the current Phase B work.
**Project:** Issue [#126](https://github.com/global-hydrodynamics/CaMa-Flood_v4/issues/126), branch `feature/runoff-data-tools`, based on `master` at `bf8201771b964e1ae8234823b19164200ea97a63` (PR #125 Map Tools merge).  
**Evidence labels:** “Repository” means files tracked or present in this Git checkout. “Package observation” means read-only files under the separately distributed local v4.30 package; it is not evidence that those files are in a GitHub clone.

## 1. Executive summary

The repository has an ERA5-Land-specific download and conversion workflow, not a general ERA5 downloader. It can request runoff from the Copernicus Climate Data Store, convert accumulated source values to millimetres per time step, and write either NetCDF or GrADS-style float32 binary plus a CTL. The tracked Japan example contains a raw January 2000 NetCDF and a converted NetCDF. Git tracks the binary CTL and configuration summary but not binary records; this workstation currently has 31 ignored binary files, whose sizes do not match the tracked 24-hour report/CTL description. The package-only v4.30 samples are much larger and include daily 1-degree binary, daily 1-hour Japan binary, and 0.25-degree NetCDF forcing.

The model converts runoff values to a depth rate using `DROFUNIT`, then applies catchment area or the input matrix to form discharge. The v5 channel-parameter workflow separately consumes a one-record annual-mean runoff climatology in mm/s and an input matrix to derive annual mean discharge. These two uses of runoff have different time semantics and must not be conflated.

The highest-priority issue for maintainer review is temporal interpretation. The downloader requests timestamps beginning at 00:00; ECMWF documents ERA5-Land accumulations valid at 00:00 as the preceding day's accumulation. The converter groups by the timestamp's calendar day, treats the first accumulated value of each day as an increment, and labels output with that same time coordinate. The existing sample and source code establish that behavior, but not whether the intended CaMa-Flood date labels align with the physical accumulation periods. This is documented as open question KG-0019; no correction is proposed or made in Phase A.

## 2. Scope and sources inspected

This inventory covers `inp/`, the standard model forcing reader, the v5 channel runoff-to-discharge program, related `script_params/` documentation and scripts, existing tests, and the local read-only v4.30 package samples. It does not assess all external runoff products or establish scientific validation of any forcing dataset.

Key evidence:

- Repository files: [`inp/README.md`](../inp/README.md), [`inp/download_ERA5/ReadMe.md`](../inp/download_ERA5/ReadMe.md), scripts and sample files under [`inp/download_ERA5/`](../inp/download_ERA5/).
- Download request and dataset name: [`download_era5land.py`](../inp/download_ERA5/download_era5land.py#L63) and its request builder at lines 259–340.
- Conversion logic: [`convert_CMF_nc.py`](../inp/download_ERA5/convert_CMF_nc.py#L236) and [`convert_CMF_bin.py`](../inp/download_ERA5/convert_CMF_bin.py#L236).
- Model forcing contract: [`cmf_ctrl_forcing_mod.F90`](../src/cmf_ctrl_forcing_mod.F90#L35) (namelist and defaults), lines 336–490 (input-matrix readers), and lines 790–855 (runoff conversion/interpolation).
- Channel parameterization: [`s01-channel_params.sh`](../map/tools/script_params/s01-channel_params.sh#L17) and [`param_channel_calc_outclm.F90`](../map/tools/code/param_channel_calc_outclm.F90#L29).
- Existing lightweight tests: `map/tools/tests/test_channel_params_wrapper.py`, `test_inpmat_naming.py`, `test_bifurcation_migration.py`, and `test_mekong_ctl_integration.py`. No runoff converter-specific tests or `inp/` test directory were found.
- External temporal definition: [ECMWF ERA5-Land data documentation](https://confluence.ecmwf.int/pages/viewpage.action?pageId=143034781), which states that forecast accumulations build from 00 UTC and a CDS-valid 00 UTC accumulation covers the previous 24 hours.

## 3. Repository inventory

### 3.1 Tracked files and current roles

| Path | Role and observed contract | GitHub checkout status |
| --- | --- | --- |
| `inp/README.md` | Explains runoff units, `DROFUNIT`, runoff-grid versus river-map distinction, package examples, ERA5-Land workflow, and known unit-selection cautions. | Tracked; already substantial. |
| `inp/download_ERA5/ReadMe.md` | CDS credentials/licence, workflow commands, settings, dependencies, converter outputs, and configuration summary. | Tracked. |
| `s01-download.sh` + `download_era5land.py` | Request ERA5-Land runoff as NetCDF; daily or monthly file grouping; configurable time step, area, retries, overwrite/skip behavior. | Tracked. Shell line 1 is `#/bin/bash` (not a valid shebang); script contains Bash-specific arithmetic. |
| `s02-convert_CMF_nc.sh` + `convert_CMF_nc.py` | Convert source accumulated runoff into float32 NetCDF `runoff` with time-step metadata and `_FillValue`. | Tracked. |
| `s03-convert_CMF_bin.sh` + `convert_CMF_bin.py` | Convert to little-endian float32 binary grids, one file per day with subdaily records inside, plus CTL. | Tracked. The shell currently passes one `TIMESTEP_HOURS` value as both input and output time step; Python supports a distinct output interval. |
| `make_CMF_runoff_info.py` | Inspect a converted NetCDF or binary/CTL product and emit a text summary including grid, time step, file pattern, and suggested forcing settings. | Tracked. Product-specific inspection, not a proven universal metadata detector. |
| `ori_japan/ERA5-Land_runoff_200001.nc` | Raw ERA5-Land Japan sample, runoff variable `ro`, dimensions `valid_time=31`, `latitude=221`, `longitude=321`; 0.1-degree grid, latitude descending. Source units are metres, with accumulated-variable metadata. | Tracked, approximately 1.0 MiB. |
| `runoff_japan_nc/runoff_200001.nc` | Converted Japan sample, `runoff(time,lat,lon)`, 31 daily-labelled records, 221×321, float32; output units `mm` per configured 24-hour step, missing data encoded as `1e20`. | Tracked, approximately 1.3 MiB. |
| `runoff_japan_nc/runoff_info_CaMaFlood.txt` | Configuration report for the converted NetCDF sample. | Tracked. |
| `runoff_japan_bin/runoff.ctl` | CTL describing the binary grid/time layout. | Tracked, but the daily binary records are not tracked. This workstation has 31 ignored files; their two-record-per-file layout conflicts with the CTL/report metadata. |
| `runoff_japan_bin/runoff_info_CaMaFlood.txt` | Binary configuration report; records that 31 files were found when it was generated. | Tracked. Its 24-hour settings and 62-record summary do not reconcile with two grid records in each of 31 ignored local files. |

The small tracked Japan samples total about 2.3 MiB for the raw and converted NetCDF data. The binary directory contains metadata only. The generated report is evidence of a prior conversion state, not proof that binary samples are currently distributable from Git.

### 3.2 v4.30 package-only examples

The following were inspected in the maintainer-provided v4.30 distribution package as **read-only local reference data**. They are not present in this Git checkout.

| Package directory | Observed format and coverage | Unit/configuration evidence |
| --- | --- | --- |
| `test_1deg/` | Global 360×180 daily plain-binary files `Roff____YYYYMMDD.one`, 2000-01-01 through 2001-12-31, plus CTL; approximately 183 MiB total. | CTL declares mm/day; `gosh/test1-glb_15min.sh` uses `IFRQ_INP=24`, `DROFUNIT=86400000`. |
| `test_15min_nc/` | `Runoff(time,lat,lon)` in yearly NetCDF files, 1440×600 at 0.25 degree, 60°S–90°N, daily time; approximately 522 MiB total. | NetCDF says `mm day-1`; `gosh/test2-conus_06min_netcdf.sh` uses `IFRQ_INP=24`, `DROFUNIT=86400000`. This sample is not the ERA5-Land Japan sample. |
| `test_jpn_1hr/` | Japan 1500×1320 at 1/60 degree, 1-hour records in daily binary files over 2007-09-03–2007-09-09; approximately 1.2 GiB total. | CTL says mm/hr; `gosh/test3-jpn_fcast.sh` uses `IFRQ_INP=1`, `DROFUNIT=3600000`. |

These data demonstrate package workflows and units. They are too large for automatic inclusion in a small GitHub fixture and have a separate distribution role.

## 4. Existing processing workflow

```text
CDS ERA5-Land runoff (source accumulated metres)
  └─ s01-download.sh / download_era5land.py
       ├─ s02-convert_CMF_nc.sh / convert_CMF_nc.py → NetCDF runoff
       └─ s03-convert_CMF_bin.sh / convert_CMF_bin.py → float32 daily binaries + CTL
                                                   └→ runoff_info_CaMaFlood.txt

For model execution: runoff field / DROFUNIT → depth rate → catchment or INPMAT area → m3/s

For channel parameterization: annual-mean runoff climatology [mm/s]
  + inpdim_<ROFFTAG>.txt + inpmat_<ROFFTAG>.bin
  → param_channel_calc_outclm → outclm.bin [annual mean discharge, m3/s]
  → channel width/depth/roughness parameter tools
```

The downloader hard-codes CDS dataset id `reanalysis-era5-land`; no separate ERA5 atmospheric forcing downloader was found. It requests the `runoff` variable and times `00:00, 00:00+TSTEP, ...` for each selected day. `DOWNLOAD_UNIT` controls file grouping, not whether the data are daily means. Credentials, account access, and licence acceptance are external prerequisites.

The converters expect accumulated runoff in metres. They difference values within groups formed by UTC calendar date, convert metres to millimetres, and can aggregate increments to a coarser output step in Python. The NetCDF path writes float32 with CF-1.8 declaration, `units="mm"`, a duration attribute, and fill value `1e20`; the binary path writes float32 (little-endian by default), replaces non-finite values with the configured undef (default `1e20`), and lays out each 2-D record with X varying fastest. Subdaily records are concatenated in each day's file. The binary CTL uses `template`, `little_endian`, and `yrev` options. The shell wrapper does not expose a separate output time-step setting.

The converter's current temporal behavior is explicit but not scientifically confirmed: it keeps the first accumulated value at each day boundary as that day's first increment. For subsequent values, it uses time differences. A negative difference falls back to the current accumulated value; the default clips remaining negative values to zero. For NetCDF conversion, `--first-step` offers `keep`, `zero`, or `missing`; default is `keep`. The binary conversion always keeps the first value. These policies and the ERA5-Land valid-time convention require a maintainer decision before changing them.

For model execution, `DROFUNIT` is a divisor converting the input value to m/s. For example, mm accumulated in 24 hours uses 86,400,000; mm accumulated in one hour uses 3,600,000. With `LINTERP` and `CINPMAT`, the model multiplies each runoff value by the corresponding input-cell overlap area and divides by `DROFUNIT`, yielding discharge contributions in m³/s. The input grid is separate from the CaMa-Flood map grid. In v5 map preparation, `ROFFTAG` names the paired `inpdim_<ROFFTAG>.txt` and `inpmat_<ROFFTAG>.bin`; it is distinct from a high-resolution map tag.

## 5. Mean runoff and channel-parameter workflow

The current v5 standard recipe is [`script_params/s01-channel_params.sh`](../map/tools/script_params/s01-channel_params.sh). It calls `param_channel_calc_outclm`, then derives width/depth/roughness and merges MERIT Hydro width. The default runoff climatology is `map/data/ELSE_GPCC_clm-1981-2010.one`, a one-record 360×180 float32 binary described as mm/s in its CTL and README. Its provenance is attributed to ELSE_GPCC and Kim et al. (2009), DOI [10.1029/2009GL039006](https://doi.org/10.1029/2009GL039006). The CTL comment calls it a “daily runoff climatology,” which conflicts with the single-record annual-mean use. The source does not reconstruct how the 1981–2010 climatology was generated.

`param_channel_calc_outclm.F90` reads the map/runoff dimensions and input-matrix filename from `inpdim`, then reads `nextxy.bin`, `ctmare.bin`, the `inpmat` records, and one runoff field. In matrix mode it sums `max(runoff_mm_s,0) × overlap_area_m2 × 1e-3` over contributing runoff cells, then accumulates discharge downstream in network order and writes one float32 `outclm.bin` record. The model catchment area file is not used for this matrix path. The alternative `near` mode is a separate legacy resolution-conversion path. `map/tools/script_params/README.md` states that this workflow uses annual-mean runoff; old `map/src/` programs are preserved and include a distinct historical max-30-day path.

The present code establishes the downstream calculation contract, but not the underlying climatology's exact temporal statistic, missing-data policy, source version, or the scientific rationale for selecting annual mean rather than the historical alternative. The maintainer has designated the current climatology file for repository tracking; that instruction does not settle provenance of unrelated upstream products.

## 6. Confirmed facts, issues, and design proposal

### Confirmed from repository/source/package evidence

- Only ERA5-Land download is implemented. The scripts use `ro`/runoff and assume accumulated metres at input.
- The current repository tracks a raw and converted Japan NetCDF sample, but not the binary runoff files referenced by the tracked binary CTL and summary.
- The package's 1-degree binary, 0.25-degree NetCDF, and Japan hourly binary samples are separate package-only examples with different units and intervals.
- The standard model reader uses `DROFUNIT`; it does not establish units from a NetCDF `units` attribute. The input matrix maps the runoff grid to model catchments.
- Root `requirements.txt` already includes NumPy, pandas, xarray, netCDF4, and cdsapi (CDS authentication remains external). This is broader than the packages installed by the ERA5 README's one-off pip example; no environment was installed during this audit.
- Existing tests cover v5 channel-wrapper arguments, map input-matrix tag naming, map/CTL integration, and a climatology-dependent bifurcation test path. They do not validate converter math, ERA5 time alignment, missing-data behavior, or runoff binary round trips.
- The shell downloader has a malformed interpreter line (`#/bin/bash`) and Bash-specific syntax; its currently documented invocation through Bash may work, but direct execution and portability should be addressed as a discrete technical task.

### Important implementation risks to resolve before Phase C

1. **ERA5-Land time alignment (KG-0019):** official valid-time semantics and the converter's date grouping/first-value rule can shift the meaning of daily labels or misinterpret a boundary value. The source sample and one successful conversion do not prove temporal correctness.
2. **Missing data / noleap calendars:** source NaNs become `1e20` in outputs; aggregation uses `skipna=False`. The root PR #120 is open and concerns 365_day/noleap forcing calendars. Integration requirements should be coordinated without changing PR #120 here.
3. **Runoff metadata are split across file formats:** units may be in a CTL or NetCDF attributes, while temporal accumulation/reset semantics are partly in documentation and converter assumptions. The model itself relies on user-configured `IFRQ_INP`, `DROFUNIT`, and variable name.
4. **Binary sample inconsistency:** binary records are not Git-tracked although the CTL and report are. This workstation has 31 ignored records, but their observed two-record-per-file layout conflicts with the documented 24-hour/62-record metadata; a GitHub clone still cannot run this binary example.
5. **Downloader portability:** fix the malformed shebang and choose/document supported shell invocation. This is a shell portability issue, not an algorithm issue.

### Phase B registry design proposed in Phase A

Create a narrowly scoped registry of datasets/products actually included or used by the repository rather than a catalog of all external runoff products. Proposed field groups:

| Group | Candidate fields | Purpose |
| --- | --- | --- |
| Identity and role | `dataset_id`, `ROFFTAG`, `product_role` | Separate a source dataset from a converted product and distinguish samples from distribution-package examples. `ROFFTAG` is optional when no runoff-grid matrix is registered. |
| Provenance/access | `source`, `product`, `version`, `source_url`, `license_or_terms`, `redistribution_status` | Preserve dataset provenance and distinguish known licence facts from unreviewed redistribution. |
| Variable semantics | `variable`, `long_name`, `units`, `temporal_semantics`, `missing_value`, `dtype` | Identify the measured quantity, amount/rate semantics, and sentinels. |
| Spatial/temporal grid | `grid_resolution`, `dimensions`, `domain`, `coordinate_names`, `longitude_convention`, `grid_order`, `calendar`, `time_step`, `period` | Describe coordinates and coverage needed for conversion and model setup. |
| File and workflow | `format`, `filename_pattern`, `download_script`, `processing_script`, `configuration_report`, `sample_available`, `distribution_scope` | Link the product to reproducible commands and identify whether it lives in Git, the package, or both. |

Avoid storing generated file facts twice if they can be validated from the NetCDF/CTL; the registry should be a concise index, not a replacement for self-describing file metadata. Decide whether each row represents a source dataset, a specific product, or both before fixing the CSV schema. Do not set any `map_variables.csv` `workflow_stage` values as part of this project.

## 7. Small-sample strategy

1. **Keep the existing real Japan NetCDF samples as a candidate:** the raw and converted files are small (about 2.3 MiB together), realistic, and already tracked. Preserve source attribution and verify redistribution terms before adding any new extracted subset or changing the current sample.
2. **Add synthetic fixtures for expected values:** tiny 2–3 day, 2×2 grids can test accumulated-to-increment conversion, UTC day boundaries, reset/negative values, missing cells, latitude ordering, float32 binary ordering, CTL dimensions, and unit metadata with hand-computable expected results. These fixtures contain no external data licensing burden.
3. **Test model-facing contracts separately:** use a synthetic runoff array and tiny `inpmat` to verify `DROFUNIT` and area-weighted discharge; use temporary files and never overwrite Mekong or standard map products.
4. **Retain package samples as manual integration references:** do not copy the 183 MiB–1.2 GiB package samples into ordinary unit tests. A package validation procedure can be optional and local.
5. **Test binary production without storing redundant real binaries:** generate a tiny binary fixture from the tracked NetCDF sample or synthetic data. If a binary real-data example is valuable for users, decide its size and tracked/distribution role explicitly.

No Phase A test was run against external data or the model. Existing tracked sample structure was inspected only.

## 8. Phased implementation proposal

| Phase | Proposed work | Review/validation checkpoint |
| --- | --- | --- |
| **B — Dataset metadata** | The initial registry and `inp/README.md` have been prepared on the assigned branch. Unresolved configuration values remain blank and are listed below. | Review the registry fields, confirm the two conflicting unit/layout cases, and preserve KG-0019 as open before finalizing those values. |
| **C — ERA5-Land download/conversion** | After time-alignment decision, improve downloader/converter interfaces, shell portability, and error/overwrite behavior only where approved. Keep ERA5 distinct from ERA5-Land; do not change scientific interpretation silently. | Synthetic tests for daily boundaries, first/last samples, reset handling, negative values, missing values, output periods, NetCDF and binary layouts; compare documented command options to shell wrappers. |
| **D — Small samples and tests** | Formalize existing Japan sample role; add tiny synthetic fixtures and tests; decide whether binary sample records belong in GitHub or only package. | Tests run without network/CDS credentials, large data, or model simulation; verify no committed examples are overwritten. |
| **E — Mean runoff/channel inputs** | Trace climatology production/provenance, time averaging, missing-value handling, units, and input-matrix weighting. Document existing annual-mean workflow and distinguish it from legacy max-30-day code. | Reproduce a tiny known runoff-to-discharge calculation and verify output layout/units; compare legacy and v5 only where an approved numerical-parity claim is intended. |

The Phase A proposal described future work at the time it was written. The maintainer subsequently assigned the bounded initial registry and README work; this does not authorize changes to scientific behavior or runoff data.

## 9. Questions for maintainer review

These questions are recorded before resolution. They do not prevent completing the inventory or designing synthetic tests; temporal answers are needed before changing ERA5-Land conversion behavior.

### Important — scientific/time interpretation

1. **Date labels and valid-time 00:00:** should output record dates refer to the ERA5-Land valid timestamp, the UTC day whose accumulation ends at 00:00, or the CaMa-Flood simulation day? ECMWF says a 00:00 valid-time accumulation represents the preceding day, while both wrappers request times starting at 00:00 and converters retain the timestamp date. This affects which runoff amount is applied on each model day and the inclusive start/end coverage. See KG-0019.
2. **First interval and resets:** for hourly records, should the first value within each day be retained, zeroed, marked missing, or derived from a prior timestamp? What is the accepted treatment of negative differences at forecast-cycle resets? The NC converter exposes `keep|zero|missing` for first step but defaults to `keep`; the binary path hardcodes `keep`.
3. **Mean climatology definition:** what exact averaging statistic/temporal coverage does `ELSE_GPCC_clm-1981-2010.one` represent, and what is its missing-data treatment? Its filename and CTL comment do not fully establish the computation; the model program only reads one field.

### Important — interface and data policy

4. **Metadata registry row granularity — resolved for the initial registry:** the maintainer task specifies one row per product, with source and converted products separate and products on the same grid sharing a `ROFFTAG`.
5. **Binary example:** the binary records are not tracked in Git; 31 ignored files are present in this workstation and conflict with the CTL/report time layout. Whether a real binary sample should be tracked or omitted in favor of synthetic fixtures remains undecided.
6. **Real-sample redistribution:** the current raw and converted ERA5-Land Japan files are tracked, but this Phase A review did not independently audit every applicable licence term. Should new extracted real samples be added only after a separate permission review?
7. **ERA5 scope:** is the current scope specifically ERA5-Land runoff only, or should future phases include the distinct atmospheric ERA5 product? Current code implements only ERA5-Land.

### Documentation / coordination

8. **Open PR #120:** its 365_day/noleap model calendar support is related to future NetCDF forcing tests. Which acceptance cases should this project own versus leave with PR #120? This Phase A branch does not modify that work.

## 10. Phase A outcome

The Phase A inventory was completed on 2026-11-01. The maintainer later assigned Phase B initial registry work; see the addendum below. This original Phase A work did not modify a converter, model, scientific algorithm, runoff dataset, or run a model. It distinguishes repository-tracked samples from package-only data and makes no claim of scientific validation or numerical equivalence.


## Phase B initial registry review — 2026-11-02

The maintainer assigned a bounded initial registry and `inp/README.md` update on `feature/runoff-data-tools`. `inp/runoff_datasets.csv` uses one row per product, with separate rows for the raw ERA5-Land source and the converted CaMa-Flood products. Products on the same grid share a `ROFFTAG`. Package-only v4.30 examples are distinguished from tracked repository samples and ignored workstation-local binary records. No large data were copied or added.

The registry covers `test_1deg`, `test_15min_nc`, `test_jpn_1hr`, the raw ERA5-Land Japan sample, and its converted NetCDF and binary samples. Grid dimensions and cell-edge bounds were checked against CTLs or NetCDF coordinates and the `param_inpmat_generate` edge/resolution arithmetic. Input units and recommended `DROFUNIT` values were checked against data metadata and the existing `gosh/` settings. `inp/validate_runoff_datasets.py` checks CSV structure, unique identifiers, grid dimensions, tag consistency, and configuration values. These checks establish metadata consistency only, not scientific validation.

### Questions for maintainer review

1. For the package `test_jpn_1hr` sample, should stored values be interpreted as `mm/hr` as configured by `gosh/test3-jpn_fcast.sh` (`DROFUNIT=3600000`), or as `mm/day` as written in `runoff.ctl`? The registry leaves `units` and `DROFUNIT` blank until confirmed.
2. For the ERA5-Land binary example, what are the intended record interval and output layout? The tracked CTL declares 62 records at `1dy`, the report says 24-hour records, and the 31 ignored local files each contain two float32 grid records. The registry leaves its interval, units, and `DROFUNIT` unresolved.
3. KG-0019 remains open: should converted ERA5-Land record dates identify valid timestamps or the physical accumulation periods? The registry does not change converter or model time interpretation.

The package-only `test_1deg` CTL also declares 1000 time records while the inspected package contains 731 daily files spanning 2000-01-01 to 2001-12-31. The registry documents actual file coverage and the CTL discrepancy; the model run configuration uses 24-hour records and `DROFUNIT=86400000`.
