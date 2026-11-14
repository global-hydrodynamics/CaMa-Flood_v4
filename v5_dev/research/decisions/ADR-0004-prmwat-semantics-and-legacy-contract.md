# ADR-0004: Preserve permanent-water aggregation semantics in the v5 migration

- Status: accepted for this migration
- Date: 2026-11-01
- Event IDs: `EV-0171`–`EV-0172`, `EV-0175`–`EV-0176`; validation: `EV-0173`, `EV-0177`
- Decision owner: Dai Yamazaki (maintainer)

## Context

The legacy `calc_prmwat.F90` contains an area aggregation rule whose water-class meaning and output role were not sufficiently clear from its name and comments. The v5 migration must expose the capability through shared tools without changing established numerical behavior.

## Decision

Preserve the legacy selection rule for valid unit-catchment assignments:

```text
(rivwth > 0 OR rivwth == -1) AND flddif <= 5 m
```

Treat `rivwth > 0` as a river-centerline width class and `rivwth == -1` as water surface outside the centerline, per maintainer confirmation. Apply the `flddif` elevation filter so `prmwat` includes permanent water only where CaMa-Flood can reproduce inundation. Isolated hilltop lakes outside the modeled flood extent must not be counted as existing simulated floodwater, or their area would be incorrectly excluded from newly inundated area during post-processing. Sum selected high-resolution pixel areas in m2 by unit catchment, then cap each result by `ctmare`. Preserve `-9999` outside network cells and 0 for network cells with no selected pixels. When `grdare` is absent, preserve the legacy scaled `rgetarea` method.

The maintainer described the intended elevation criterion as `flddif < 5 m`. Existing v4 and migrated v5 source use `flddif <= 5 m`, which includes values exactly equal to 5 m. This migration documents the mismatch and preserves the existing comparison; a change to strict `<` requires explicit confirmation because it changes outputs for exactly-5 m pixels. The functional reason for filtering high lakes is confirmed, while the numerical derivation of the 5 m cutoff remains unknown.

`prmwat.bin` is a post-processing reference area, not a high-resolution water mask or an input read by the current simulation core. Keep it optional/auxiliary in the registry. Do not alter the 5 m threshold, its exact-equality boundary, or the handling of `flddif=-9999` without a separate maintainer-approved scientific change.

## Consequences and limits

The v5 program adds safety checks while retaining the valid-input algorithm, file name, float32 type, dimensions, and single-record format. The `flddif=-9999` comparison remains accepted by the legacy `<=5` condition and is reported; it is not silently filtered. The numerical derivation of the threshold and independent accuracy of the fallback are not established here. Byte-for-byte comparison on the Mekong sample and a synthetic fallback fixture demonstrates migration compatibility only, not scientific suitability.

## Evidence

- Legacy implementation: `map/src/src_param/calc_prmwat.F90`
- v5 implementation and tests: `map/tools/code/param_water_calc_prmwat.F90`; `map/tools/tests/test_prmwat.py`
- Product and workflow documentation: `map/map_variables.csv`; `map/map_metadata_spec.md`; `map/tools/script_params/README.md`
- Maintainer instruction: DR-0032 and clarification DR-0033; no stable conversation archive ID supplied
- Validation: VR-0057; documentation follow-up VR-0058
