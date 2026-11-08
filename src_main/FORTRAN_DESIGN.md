# CaMa-Flood Fortran Design Conventions

## Purpose and scope

This document describes implementation and refactoring conventions for CaMa-Flood Fortran code under `src_main/`. These rules concern architecture, scientific meaning, data contracts, numerical behavior, and runtime responsibilities; applying them can require understanding code beyond the edited file.

The existing source and maintainer-confirmed decisions are the authority for behavior. Distinguish verified facts, recommendations, assumptions, and unresolved questions. Do not infer scientific meaning from an identifier or formula alone. File-local layout and the final formatting pass are described in [`FORTRAN_STYLE.md`](FORTRAN_STYLE.md).

## 1. Module responsibilities and procedure organization

**Observed architecture:**

- `CMF_DRV_*` coordinates model execution, initialization, and finalization.
- `CMF_CTRL_*` handles configuration, initialization, I/O, and other control operations.
- `CMF_CALC_*` performs numerical and scientific calculations.
- `YOS_CMF_*` holds shared configuration, maps, model state, and related data.
- `CMF_UTILS_MOD` provides common utilities.
- Related subsidiary operations may be internal procedures under the principal procedure that owns them.

**Procedure conventions:**

- Principal procedures called by a driver or another model module use the `CMF_` prefix. This distinguishes model entry points and reduces name collisions when CaMa-Flood is coupled to another model such as IFS.
- Module-level implementation utilities use a module-specific functional prefix followed by `_UTIL_`, such as `MAP_UTIL_*`, `FORCING_UTIL_*`, or `OUTPUT_UTIL_*`. Utilities may provide I/O, input checks, error reporting, and data conversion; a utility need not have multiple callers.
- In new or substantially refactored modules, place the principal `CMF_*` procedures first so their workflow is visible before implementation details. Group module-level implementation utilities below the principal procedures.
- Mark implementation utilities `PRIVATE` where this preserves existing interfaces. Identify supported public interfaces explicitly where practical; preserve established public procedures, generics, and data contracts unless a separate change is approved.
- These are recommendations for new or substantially refactored v5 modules, not requirements imposed retroactively on legacy code or external contributions.
- Internal procedures may still be used when their scope and host association are beneficial. Moving a helper to module scope requires its former host-associated inputs to be passed explicitly; do not promote temporaries to module state.
- Put new procedures in the module that owns their responsibility unless a substantive reason supports a different location. Avoid creating modules solely to wrap a small utility.
- Do not rename established procedures just to enforce a naming convention. Apply naming conventions to new code and focused module refactors while preserving public interfaces.
- `USE MODULE, ONLY: ...` is common and is preferred for new dependencies when practical. Preserve existing imports and their build guards during unrelated edits.
- Preserve existing copyright, license, and attribution text.

The **how to document procedure hierarchy in source** (lists, purpose-comment placement, and their visual form) is specified in `FORTRAN_STYLE.md`.

## 2. Names and historical array conventions

**Observed naming patterns:**

- Uppercase names are common for modules, principal routines, model variables, and namelist parameters, though capitalization is not uniform throughout the codebase.
- Prefixes often suggest role or type: `I` for integer, `D` and `R` for real quantities or arrays, `L` for logical options, and `C` for character values (often filenames). `P` appears in prognostic and parameter-related names; its meaning must be determined from context.
- Examples include `I2NEXTX`, `R2TEMP`, `D2RIVWTH`, `LRESTART`, and `CRIVHGT`.

**Historical `2` convention:** In a name such as `D2RIVWTH`, `2` traces to a quantity originally defined on a two-dimensional geographic map. Map-grid arrays such as `(NX,NY)` are converted to river-cell vectors by `mapR2vecD` and related routines. The historical `D2` name remains when the storage shape is `(NSEQMAX,1)`, so the number does not reliably indicate current array rank. Exceptions exist.

`I2NEXTX` and `I2NEXTY` are geographic index maps used to construct the network; `R2TEMP` commonly holds temporary map-grid data; `D2RIVWTH` is a vectorized river-width quantity. Check declarations, dimensions, and usage rather than inferring shape from a prefix.

**Recommendations:**

- Retain identifiers and established naming patterns when extending code.
- Do not rename a legacy variable merely to reflect its current array rank.
- Choose clear, locally consistent names for new variables; do not impose a rigid global prefix taxonomy where the source has exceptions.

## 3. Types, declarations, and Namelist meaning

**Observed precision conventions:**

- Numerical kinds are commonly imported from `PARKIND1`, including `JPIM`, `JPRB`, `JPRM`, and `JPRD`.
- In the current `src_main/parkind1.F90`, `JPIM` is a 4-byte integer kind, `JPRM` a 4-byte real kind, `JPRD` a double-precision real kind, and `JPRB` is single precision with `SinglePrec_CMF` and otherwise double precision. These meanings come from kind definitions and build flags, not identifier letters.
- Kind-qualified constants such as `0._JPRB` are used in numerical code.

**Recommendations:**

- Use the precision kinds and declaration styles already used by the surrounding code.
- Do not casually change real or integer kinds, memory layout, allocation lifetime, or conversions between map-grid and vectorized arrays.
- Prefer explicitly kind-qualified real literals in new precision-sensitive calculations. Check actual array rank and extent rather than inferring them from a name.
- Follow `FORTRAN_STYLE.md` for declaration layout; layout cleanup must preserve declared names, types, kinds, attributes, initializers, dimensions, imports, and order.

### Namelist semantics

- Each Namelist variable's comment should explain its actual functional or scientific role. Include verified units, Boolean behavior, file role, and sentinel meanings where applicable.
- For a new or modified Namelist variable, verify its declaration, applicable default, Namelist registration, and required reporting/logging against its use. Defaults and logging are not universally required.
- Preserve established Namelist names, defaults, aliases, and compatibility behavior unless the maintainer approves an interface change.
- The source formatting for one-variable-per-line declarations, `!!` comments, alignment, and registration labels is in `FORTRAN_STYLE.md`.

## 4. Scientific meaning and semantic comments

Comments added during implementation or substantial refactoring should accurately describe information that cannot be understood from syntax alone, including:

- Physical or functional meanings of variables, units, and conversion factors.
- Non-obvious numerical algorithms, their rationale, and important ordering rules.
- Input/output contracts, array dimensions and orientation, indexing conventions, sentinels, and limitations.
- Scientific assumptions and dependencies between processing stages.

Prioritize **Why** a step is needed, then **What** it establishes, then **How** a non-obvious algorithm works. Add only the information a reader cannot readily obtain from the code; do not narrate simple syntax or repeat the same point in nearby comments.

Inspect related code, documentation, data contracts, or maintainer decisions as needed to establish meaning. Do not infer scientific intent from variable names or an equation alone. Keep verified behavior separate from historical explanation, inference, and unresolved questions. Report a material uncertainty instead of silently resolving it by speculation.

`FORTRAN_STYLE.md` describes how to present established meaning concisely, including inline comment syntax, placement, and process-heading layout.

## 5. Numerical behavior and compatibility

**Required preservation rule:** Unless a task explicitly approves a scientific change, implementation and refactoring must preserve established numerical behavior and data contracts.

In particular, preserve:

- Floating-point expression and summation order, precision kinds, thresholds, limiters, special values, and adaptive time stepping.
- River-network ordering, 1-based map indices, map-to-vector transformations, array orientation, and storage/update order.
- OpenMP semantics, MPI behavior, and coupling/restart timing.
- Public driver/coupler routines, Namelist names and defaults, script entry points, and paths expected by established workflows.
- Binary layouts, record order, endianness, types, dimensions, missing values, filenames, and NetCDF variable names, dimensions, order, and metadata conventions.

Do not substitute mathematically equivalent expressions without evaluating their numerical consequences. Do not mix an intentional numerical change with a readability or I/O refactor. Changes that alter scientific behavior, backward compatibility, a public interface, or a distribution format require maintainer approval under the root `AGENTS.md`.

## 6. I/O and data contracts

- Keep file I/O in the appropriate control or utility procedures rather than inserting it into physical calculation kernels.
- Verify Binary record layout, endianness, type, dimensions, sentinel values, and names against the actual reader/writer.
- For NetCDF, verify variable names, types, dimensions, order/orientation, and missing-value behavior against the actual Fortran interface.
- Check open/read/close failures and report the relevant file and operation.
- Keep optional NetCDF behavior under its supported compile-time guards and preserve Binary-only builds.
- Respect legacy aliases and compatibility requirements. These conventions do not authorize changing an existing file format or Namelist contract.

For map-specific shared I/O within `CMF_CTRL_MAPS_MOD`, prefer an appropriate module-level utility there before creating another module solely to wrap I/O.

## 7. Parallelism and conditional compilation

**Observed conventions:** the source uses `!$OMP` directives and scope clauses including `PRIVATE`, `THREADPRIVATE`, and `REDUCTION`; preprocessor guards include `UseCDF_CMF` and `UseMPI_CMF`.

When implementing or refactoring:

- Preserve OpenMP variable scope, loop scheduling, iteration order, and reductions.
- Preserve MPI data ownership, collective behavior, and rank-specific I/O/log handling.
- Keep optional code behind the correct compile-time guards and verify the supported build configurations affected by a change.
- Do not treat compile-time flags and runtime options as interchangeable.
- Do not change parallel execution merely to make formatting more uniform.

## 8. Runtime logging and error behavior

Logging is part of runtime behavior. Apply these conventions to modules using `LOGNAM`:

- A principal `CMF_*` procedure should log an unindented `CMF::<NAME>: START ...` at entry and `CMF::<NAME>: END` after normal completion. A failed execution need not log `END`.
- Prefix meaningful progress messages from a principal procedure with its `CMF::<NAME>:` label.
- Indent ordinary messages from internal procedures by two spaces and identify the procedure. Internal procedures do not need their own START/END messages.
- Indent shared module-level utility messages by two spaces and identify the utility when useful.
- Begin standalone diagnostics with `WARNING` or `ERROR` and identify their source. Retain principal caller context for shared-utility failures where available; avoid duplicate reports at multiple call levels.
- Keep useful settings, filenames, and summary values. Avoid per-iteration output, routine OPEN/CLOSE messages, unnecessary repetition, and decorative separators.
- Preserve established `WRITE(LOGNAM,*)` output. Do not change error conditions, STOP behavior, call order, or diagnostic frequency as a side effect of unrelated work.
- Before changing existing messages, check whether repository scripts or tests depend on their exact text.

The source formatting of log prefixes and indentation should follow local code; do not convert list-directed output to explicit formats merely to control leading spaces.

## 9. Implementation and refactoring workflow

Before editing:

1. Read the root `AGENTS.md`, the applicable directory instructions, this document, and `FORTRAN_STYLE.md` for the target file.
2. Inspect the owning module, directly related procedures, declarations, arrays, and relevant conditional-compilation paths. Inspect other modules only when needed to establish a caller/callee contract, shared meaning, or dependency.
3. Identify numerical behavior, data contracts, interfaces, and parallel semantics that must remain unchanged.

During editing:

1. Make the smallest coherent change that satisfies the approved scope.
2. Preserve neighboring architecture, attribution, identifiers, and established contracts.
3. Avoid unrelated renaming, formatting, optimization, or scientific changes.
4. Add semantic comments only when their meaning is supported by source, documentation, or a maintainer decision.

After editing, validate proportionally to risk: inspect the diff, run `git diff --check`, compile affected supported configurations when feasible, and run focused tests or numerical comparisons when the change can affect calculations or I/O. Distinguish compilation from test execution. Report unverified assumptions, unsupported cases, and out-of-scope findings rather than silently fixing them.

For a dedicated final formatting pass, follow the narrower procedure in `FORTRAN_STYLE.md`; do not repeat a project-wide design review unless a concrete issue requires it.

## 10. Observed exceptions

These conventions were inferred from representative `src_main/` modules including `cmf_ctrl_maps_mod.F90`, `cmf_ctrl_forcing_mod.F90`, `cmf_ctrl_output_mod.F90`, `cmf_calc_outflw_mod.F90`, `cmf_drv_control_mod.F90`, and `yos_cmf_input.F90`. They do not assert uniform conformance across all existing files. When source and this guide disagree, preserve verified behavior and report the discrepancy for maintainer review. Do not use this guide to justify an unapproved interface, compatibility, or numerical change.
