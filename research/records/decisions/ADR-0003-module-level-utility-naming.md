# ADR-0003: Name module-level shared utilities with a module-specific prefix

- Status: accepted and implemented
- Date: 2026-11-03
- Event IDs: `EV-0298`–`EV-0300`
- Decision owner: maintainer

## Context

CaMa-Flood distinguishes principal model procedures, which use the `CMF_` prefix, from procedure-specific internal helpers. Four Map input helpers were module-level procedures with generic names, making their role less obvious from call sites. Source inspection found no call sites outside `CMF_CTRL_MAPS_MOD`; two readers are intended for reuse across map-reading workflows.

## Alternatives considered

- Keep generic names and distinguish procedures only through comments.
- Move each reader beneath its current principal caller, which would constrain the intended shared Map I/O utility scope.
- Add a module-specific utility prefix while keeping the helpers at module scope.

## Decision and rationale

Use `<MODULE_FUNCTION>_UTIL_<PURPOSE>` for generic module-level utilities. For `CMF_CTRL_MAPS_MOD`, rename `MAP_INPUT_ERROR`, `MAP_INPUT_KIND`, `READ_MAP_REAL2D`, and `READ_MAP_INT2D` to `MAP_UTIL_INPUT_ERROR`, `MAP_UTIL_INPUT_KIND`, `MAP_UTIL_READ_REAL2D`, and `MAP_UTIL_READ_INT2D`. Keep principal procedures prefixed by `CMF_`; procedure-specific internal routines need neither prefix. Document this as the convention for new code and individually refactored modules, not as authorization for a repository-wide rename.

The four utilities remain at module scope for common Map input/error handling and anticipated reuse by both river-network and topography workflows. No procedure was moved, split, or merged.

## Consequences and compatibility constraints

- No scientific calculations, arguments, input formats, or `PUBLIC`/`PRIVATE` attributes changed.
- These module procedures are public by default. No repository-internal callers outside the module were found, but downstream code that directly calls one of the old procedure names would need to update that call. This explicit interface rename was approved by the maintainer for this task.
- Binary-only and NetCDF-enabled builds both compile with the new procedure names. This does not substitute for a model execution test.

## Evidence

- Maintainer instruction and approved naming scope: `EV-0298`.
- Implementation in `src_main/cmf_ctrl_maps_mod.F90` and common guidance in `src_main/FORTRAN_STYLE.md`: `EV-0299`.
- Isolated Binary-only and NetCDF-enabled build validation: `EV-0300`; `VR-0095`.

## Supersedes / superseded by

None.
