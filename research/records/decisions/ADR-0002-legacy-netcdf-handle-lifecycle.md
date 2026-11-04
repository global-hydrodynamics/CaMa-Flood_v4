# ADR-0002: Repair legacy consolidated NetCDF slope-reader handles

- Status: accepted and implemented
- Date: 2026-11-03
- Event IDs: `EV-0291`–`EV-0293`
- Decision owner: maintainer

## Context

Source review found two file-ID lifecycle errors in the legacy consolidated NetCDF slope-option path in `src_main/cmf_ctrl_maps_mod.F90`: `READ_TOPO_CDF` queried and read `elevslope` using the `NCID` of `CRIVCLINC` after closing it, and `SET_SLOPEMIX` queried/read `mask_slope` using a local `NCID` that had never been opened.

## Decision

Repair the handle usage without changing the scientific equations, variable meanings, namelist settings, or legacy container contract. Read `elevslope` while `CRIVPARNC` is open in `READ_TOPO_CDF`. In `SET_SLOPEMIX`, explicitly open `CRIVPARNC`, read `mask_slope`, then close it. Keep both options legacy consolidated-NetCDF-only; do not add individual-file support under `LMAPCDF=.FALSE.`.

## Consequences and validation limits

- This is a resource/identifier lifecycle fix, not a numerical-method change.
- The non-NetCDF Binary-only and NetCDF-enabled builds both compile after the change.
- The 40-day Mekong Binary and individual-map regression cases still complete and their five selected outputs remain byte-identical. The individual-mode option guards still return a nonzero status with an `LMAPCDF` diagnostic.
- No suitable consolidated NetCDF map fixture is available, so direct runtime validation of `elevslope` and `mask_slope` in legacy mode remains open.

## Evidence

- Maintainer instruction and scope: `EV-0291`.
- Implementation: `src_main/cmf_ctrl_maps_mod.F90`, `EV-0292`.
- Build and Mekong regression validation: `EV-0293`, `VR-0093`.
- Related input-path boundary: `ADR-0001-map-input-path-scope.md`; open legacy coverage question: `KG-0027`.
