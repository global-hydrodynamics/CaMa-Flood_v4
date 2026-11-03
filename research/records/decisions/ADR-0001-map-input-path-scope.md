# ADR-0001: Scope individual Map input to the legacy Binary variable set

- Status: accepted
- Date: 2026-11-03
- Event IDs: `EV-0288`
- Decision owner: maintainer

## Context

The individual Binary/NetCDF Map reader added `.bin`/`.nc` dispatch beneath `LMAPCDF=.FALSE.`. The first implementation also added individual-file handling for `elevslope` and `mask_slope`, which belong to ECMWF-specific branches that were only present in the old consolidated NetCDF path.

## Alternatives considered

1. Extend the new individual-file interface to every input available in any NetCDF path. This would introduce new data and interface contracts for special options.
2. Limit individual-file support to the variables read through the legacy Binary path, while preserving the old consolidated NetCDF path and special options behind `LMAPCDF=.TRUE.`.

## Decision and rationale

Choose alternative 2. `LMAPCDF=.TRUE.` retains the legacy consolidated containers and behavior. `LMAPCDF=.FALSE.` may mix Binary and individual NetCDF files for the existing Binary-path map variables. `elevslope` / `LSLOPEMOUTH` and `mask_slope` / `LSLPMIX` remain available only through the legacy consolidated NetCDF route. If either option is enabled with individual-file mode, initialization must report a clear error before attempting to use absent data.

This preserves the shared ECMWF code path and avoids inventing a new namelist parameter or individual-file format for specialized variables.

## Consequences and compatibility constraints

- Keep the old consolidated read routines and special-variable interface separate from individual-file input. The two identified NetCDF handle lifecycle defects in the slope reads were later repaired under a separate maintainer instruction (ADR-0002); no equation, option meaning, or individual-file contract changed.
- Do not add individual slope-variable readers or new file contracts in this implementation.
- Report consolidated-mode runtime behavior as unverified until a suitable fixture is available.
- Preserve the existing slope calculation equations.

## Evidence and implementation links

- Maintainer clarification: EV-0288.
- Implementation and tests: EV-0289–EV-0290; `src_main/cmf_ctrl_maps_mod.F90`.
- User-facing contract: `src_main/README.md`; `docs/model_source_workflow_plan.md`.
- Validation detail: `research/records/validation_runs/VR-0092.md`.

## Supersedes / superseded by

Supersedes the broader slope-input scope recorded in the initial individual-Map implementation summary (EV-0285 / VR-0091). No earlier accepted ADR is superseded.
