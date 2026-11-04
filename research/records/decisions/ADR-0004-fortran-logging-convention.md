# ADR-0004: Standardize Fortran log hierarchy and diagnostic context

- Status: accepted and applied to `CMF_CTRL_MAPS_MOD`
- Date: 2026-11-03
- Event IDs: `EV-0304`–`EV-0306`
- Decision owner: maintainer

## Context

The map control module mixed principal-procedure labels, internal routine labels, and unqualified diagnostics. This made initialization logs harder to follow and errors harder to locate when read as isolated lines. Logging needed clearer source context without adding routine-level verbosity or changing model behavior.

## Decision

Use unindented `CMF::<PRINCIPAL>: START/END` messages at normal entry and exit boundaries. Label important principal progress with `CMF::<PRINCIPAL>:`. Indent ordinary internal and shared-utility messages by two spaces and identify their source routine. Begin warnings/errors with `WARNING` or `ERROR` and include the principal and the relevant internal/helper source.

The shared map readers accept an optional caller-context string so existing external calls remain source-compatible. Contextual NetCDF reporting is handled within `CMF_CTRL_MAPS_MOD`, avoiding changes to the common `NCERROR` interface. NetCDF failures retain STOP 10; existing STOP 9 conditions and error predicates remain unchanged.

## Consequences and scope

- The common logging-design recommendation is documented in `src_main/FORTRAN_DESIGN.md`.
- It is applied only to `src_main/cmf_ctrl_maps_mod.F90` in this task.
- Existing useful paths, variables, counts, and processing stages remain logged; no loop-level messages or routine OPEN/CLOSE messages were added.
- Exact diagnostic strings changed. Repository scripts/tests were searched; no exact-string consumer was found. Legacy `src/` and `gosh/` were not changed.
- Both build modes and an isolated 40-day Mekong run passed. The run's log contained 18,843 lines, compared with 18,819 in the existing prior log; this is an overall comparison and not an attribution of every line difference to the logging edits.
- The sample did not trigger warnings or errors. Legacy consolidated NetCDF failure messages and GPA/MPI special paths remain unverified at runtime.

## Evidence

- Maintainer instruction: `EV-0304`.
- Implementation: `EV-0305`.
- Builds and isolated Mekong runtime/log inspection: `EV-0306`; `VR-0097`.

## Knowledge-gap review

Completed. No new material question was found for the standard Mekong path. Existing uncertainty about non-geographic/GPA coordinates remains tracked in `KG-0028`; specialized legacy NetCDF diagnostic paths were not exercised in this task.
