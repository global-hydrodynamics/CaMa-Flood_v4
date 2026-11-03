# CaMa-Flood Fortran Coding Style

## Purpose and scope

This document describes the established conventions of CaMa-Flood's Fortran source in `src_main/` and provides conservative guidance for human and AI contributors. It is intended to preserve the project's recognizable coding style and numerical behavior, **not** to mandate a wholesale formatting refactor.

The existing source is the final reference for local conventions. If this document conflicts with verified behavior or an established interface, investigate and update the document rather than silently modifying scientific code. Repository-wide development and approval rules in the root `AGENTS.md` take precedence.

This guide distinguishes **observed conventions** from **recommended practices**. Examples are illustrative and may not reproduce a complete, compilable module.

## 1. Module responsibilities and organization

**Observed conventions:**

- `CMF_DRV_*`: driver-level execution, initialization, and finalization.
- `CMF_CTRL_*`: model configuration, initialization, I/O, and control functions.
- `CMF_CALC_*`: numerical and scientific calculations.
- `YOS_CMF_*`: shared configuration, map, model state, and other data.
- `CMF_UTILS_MOD`: common utility routines.
- Source modules generally use uppercase `MODULE ...` / `END MODULE ...` and a `CONTAINS` section.
- File headers often include `PURPOSE`, a list of principal routines under `CONTAINS`, copyright, and license information.
- Related subsidiary operations may be placed in an enclosing procedure's internal `CONTAINS` section.

**Subroutine hierarchy documentation (required for new or substantially updated modules):**

- Immediately after module-level `CONTAINS`, list the module's principal `CMF_` procedures with one-line purposes. Keep module-level shared utilities in a separate, clearly labeled list so they are not confused with model entry points.
- A principal routine's declaration should be followed by a one-line purpose. If it contains internal procedures, list them there in source order with one-line purposes.
- Immediately after that routine's internal `CONTAINS`, repeat the internal-procedure list so the hierarchy is visible both before the main body and before the implementations. Keep the two lists identical.
- Put a concise purpose comment immediately after each internal procedure declaration. Include only procedures actually defined in that scope; note compile-time-conditional procedures as such when useful.
- Keep descriptions focused on responsibility and data flow. Do not infer scientific rationale, units, or behavior that is not established by the code or authoritative documentation.
- When changing an existing module, update affected lists and summaries without mechanically adding this structure to unrelated files or renaming established procedures.

**Procedure naming:**

- Principal model procedures called by a driver or another model module use the `CMF_` prefix. This distinguishes model entry points and helps avoid name collisions when CaMa-Flood is coupled to another model, such as IFS.
- Generic utilities placed at module scope for use across principal procedures use a module-specific functional prefix followed by `_UTIL_` (for example, `MAP_UTIL_*`, `FORCING_UTIL_*`, or `OUTPUT_UTIL_*`). Utilities include shared I/O, input checks, error reporting, and data conversion.
- Procedures specific to one principal procedure may remain internal under that procedure's `CONTAINS`; they do not require `CMF_` or `_UTIL_` prefixes.
- Consider both current callers and the procedure's responsibility and plausible reuse when choosing module-level or internal scope. Do not rename existing procedures solely to enforce this convention; apply it to new code and module-level refactors.

**Recommended practices:**

- Put a new routine in the existing module that owns its responsibility unless a clear, substantive reason justifies another module.
- Use a **module-level procedure** when multiple procedures in that module need a shared utility. Use a **procedure-local internal subroutine** when only its parent needs it.
- Update a header's routine list when adding or removing relevant routines; do not invent a routine list for every module if the existing file does not use one.
- Preserve existing license and attribution text.

## 2. Names and historical array conventions

**Observed conventions:**

- Uppercase names are common for modules, principal routines, model variables, and namelist parameters, although legacy capitalization is not perfectly uniform.
- Prefixes often suggest a variable's purpose or type: `I` for integer, `D` and `R` for real-valued quantities or arrays, `L` for logical options, and `C` for character values (often filenames). `P` appears in prognostic and parameter-related names; its meaning must be determined from context.
- Examples include `I2NEXTX`, `R2TEMP`, `D2RIVWTH`, `LRESTART`, and `CRIVHGT`.

**Important historical rule:** The `2` in a name such as `D2RIVWTH` traces back to its origin as a quantity defined on a **two-dimensional geographic map**. CaMa-Flood converts map-grid arrays such as `(NX,NY)` into river-cell vector representations using `mapR2vecD` and related map-to-vector routines. The historical `D2` name remains even when the storage shape becomes `(NSEQMAX,1)`. Thus the numeric part of a name does **not** reliably specify the present Fortran array rank. There are exceptions to naming patterns.

For example:

```fortran
REAL(KIND=JPRM),ALLOCATABLE :: R2TEMP(:,:)
! D2RIVWTH is a model river-vector array, allocated elsewhere.
CALL mapR2vecD(R2TEMP,D2RIVWTH)
```

`I2NEXTX` and `I2NEXTY` are geographic index maps used to construct the network; `R2TEMP` commonly holds temporary map-grid data; `D2RIVWTH` is the vectorized river-width quantity. Check **declarations, dimensions, and usage** rather than deducing shape from a prefix.

**Recommended practices:**

- Retain existing identifiers and established naming patterns when extending a routine.
- Never rename a legacy variable just to reflect its current array rank.
- Choose clear, locally consistent names for new variables; avoid assuming a global, rigid prefix taxonomy where the source has exceptions.

## 3. Types, declarations, and precision

**Observed conventions:**

- The code commonly imports numerical kinds from `PARKIND1`, including `JPIM`, `JPRB`, `JPRM`, and `JPRD` (and others where needed).
- In the current `src_main/parkind1.F90`, `JPIM` is selected as a 4-byte integer kind; `JPRM` is a 4-byte real kind; `JPRD` is a double-precision real kind; and `JPRB` is selected as single precision when `SinglePrec_CMF` is defined, otherwise double precision. These meanings come from the current kind definitions and build flags, not from the letters in a variable name.
- Typical declarations use explicit `KIND`, `ALLOCATABLE`, and named dimensions, for example:

```fortran
INTEGER(KIND=JPIM)              :: IX,IY
REAL(KIND=JPRB)                 :: DDEPTH
REAL(KIND=JPRM),ALLOCATABLE     :: R2TEMP(:,:)
CHARACTER(LEN=256)              :: CFILE
LOGICAL                         :: LVALID
```

- Many modules or procedures use `IMPLICIT NONE`; check the actual scope before adding declarations.
- Kind-qualified constants such as `0._JPRB` occur in numerical code.

**Recommended practices:**

- Use the precision kinds and declaration styles already in the surrounding source.
- Do not casually change real kinds, integer kinds, memory layout, allocation lifetime, or conversions between mapped and vectorized arrays.
- Prefer explicitly kind-qualified real literals in newly written precision-sensitive calculations.
- Check an array's actual rank and extent rather than inferring them from its name.

## 4. Layout and formatting

**Observed conventions:**

- Most primary model modules use free-form preprocessed `.F90` source. Lowercase `.f90` files also exist under `mod/` and in tests, so the extension and preprocessing convention is not universal.
- Module and principal routine names are commonly uppercase in the main model tree; lowercase names exist in `mod/` and other specialized code. Keyword and intrinsic capitalization varies.
- Related declarations and `USE ... ONLY:` lists are often aligned for readability.
- The code uses `&` for line continuations, and both long inline comments and standalone section comments.
- Indentation, whitespace, and `ENDDO` versus `END DO` are not fully uniform across legacy files.

**Recommended practices:**

- Match the nearest comparable code when editing an existing routine.
- In wholly new routines, use a consistent, modest indentation (typically two spaces per level), without imposing it on existing routines.
- Use readable continuation lines and explicit block endings; keep preprocessor directives easy to spot.
- Keep lines reasonably short, except where preserving a mathematical expression or local established style is clearer.
- **Do not run an automatic formatter or reformat unrelated source** as part of a feature change.

## 5. Runtime logging

**Recommended practices for modules that use `LOGNAM`:**

- A principal `CMF_*` procedure should log an unindented `CMF::<NAME>: START ...` message at entry and `CMF::<NAME>: END` after normal completion. Use the established blank-line and separator style around these boundaries; an abnormal exit does not need an `END` message.
- Prefix important progress messages written directly by a principal procedure with its `CMF::<NAME>:` label.
- Indent ordinary messages from an internal procedure by two spaces and identify the procedure, for example `  CALC_REGION: ...`. Internal procedures do not need their own START/END messages.
- Indent shared module-level utility messages by two spaces and include the utility name, for example `  MAP_UTIL_READ_REAL2D: ...`. Do not repeat a filename or variable already logged by the caller unless it helps identify the utility's work.
- Begin standalone diagnostics with `WARNING` or `ERROR` and identify their source, for example `WARNING [CMF::RIVMAP_INIT/READ_BIFPARAM]: ...`. For shared utility failures, retain the principal caller context and identify the utility where useful. Do not emit the same failure again at another call level.
- Keep existing useful settings, filenames, and summary values. Add logs only for meaningful stages; avoid per-iteration output, routine OPEN/CLOSE messages, and decorative separators beyond those used for principal boundaries.
- Continue using established `WRITE(LOGNAM,*)` output. Do not convert whole modules to explicit formats merely to control leading spaces; ensure intended indentation remains visually clear in the resulting log.

Logging edits must not change error conditions, STOP behavior, call order, model calculations, MPI/OpenMP behavior, or file I/O. Before changing a message, check whether repository scripts or tests rely on its exact text.

## 6. Comments and visual hierarchy

**Observed conventions:**

- Major module-level procedure boundaries often use `!####################################################################`.
- Sections within routines often use `!================================================` or longer variations.
- Numbered processing steps commonly use `!***`, for example:

```fortran
!*** 1. Read input map
!*** 2. Set model arrays
!*** 3. Calculate derived parameters
```

- A module header may list major routines with `! -- ROUTINE : purpose`.
- `!` and `!!` are both used for comments; their precise usage varies.

**Recommended practices:**

- Preserve the existing comment hierarchy of the edited file. Do not mechanically add separators around every small block.
- Explain **scientific meaning, units, indexing, physical assumptions, non-obvious numerical choices, I/O contracts, and compatibility constraints**, rather than narrating obvious syntax.
- Keep section numbering meaningful after inserting or moving steps.
- Correct stale comments in the touched code when needed for accuracy, but do not broaden the task to unrelated cleanup.

## 7. Dependencies and build-time options

**Observed conventions:**

- `USE MODULE, ONLY: ...` is common; long import lists may span multiple `USE` statements.
- Preprocessor guards include `#ifdef UseCDF_CMF` and `#ifdef UseMPI_CMF`.
- OpenMP directives use `!$OMP` and scope clauses, including `PRIVATE`, `THREADPRIVATE`, and `REDUCTION`.

**Recommended practices:**

- Prefer explicit `USE ... ONLY:` imports for new dependencies, except where the local source or an external module makes that impractical.
- Keep optional NetCDF code inside the appropriate compile-time guards so a Binary-only build does not acquire an unconditional NetCDF dependency.
- Maintain supported build paths and check conditional branches when changing interfaces.
- Do not modify OpenMP scoping, loop scheduling, iteration order, or reductions as a formatting cleanup.

## 8. Numerical and scientific behavior

**Required preservation rule:** Unless explicitly authorized, a style or I/O refactor must preserve the intended scientific algorithms and their established numerical behavior.

In particular:

- Preserve floating-point expression and summation order, numerical kinds, thresholds, and special values.
- Preserve river-network ordering, 1-based map indices, map-to-vector transformations, and array orientation.
- Preserve existing OpenMP semantics and MPI behavior.
- Do not substitute mathematically equivalent expressions without evaluating numerical consequences.
- Separate an intentional scientific change from a formatting or infrastructure change and seek the approval required by the root `AGENTS.md`.

## 9. I/O conventions and compatibility

**Recommended practices:**

- Keep file I/O within the appropriate control or utility routines rather than inserting file operations into physical kernels.
- Preserve legacy Binary record layout, endianness handling, types, dimensions, sentinel values, and naming conventions.
- For NetCDF, verify variable names, integer/real types, dimensions, order/orientation, and missing-value handling against the actual Fortran reader.
- Check open/read/close errors and produce messages that identify the failing file and operation.
- Respect compile-time NetCDF availability and maintain backward compatibility where required.
- For map I/O shared by multiple procedures inside `CMF_CTRL_MAPS_MOD`, prefer a module-level helper in `cmf_ctrl_maps_mod.F90` before creating another module solely to wrap I/O.

These are engineering guidelines; they do not themselves approve changes to existing map input formats or namelist semantics.

## 10. Working rules for AI contributors

Before editing:

1. Read the root `AGENTS.md`, directory-specific `AGENTS.md`, and this guide.
2. Inspect the owning module, directly related routines, declarations, arrays, and conditional compilation paths.
3. Identify what numerical behavior and interfaces must remain unchanged.

During editing:

1. Make the smallest coherent change that satisfies the approved task.
2. Preserve neighboring style, attribution, identifiers, and file/module organization.
3. Avoid unrelated renaming, formatting, optimization, or scientific changes.
4. Document non-obvious behavior and new public I/O contracts.

After editing:

1. Inspect the diff for unrelated churn and check formatting (`git diff --check`).
2. Compile relevant supported build configurations when feasible.
3. Run focused tests and compare scientific outputs with the baseline when the change can affect numerics or I/O.
4. Report unverified assumptions, unsupported cases, and findings outside the approved scope rather than silently fixing them.

## 11. Scope and exceptions

These conventions were inferred from representative `src_main/` modules such as `cmf_ctrl_maps_mod.F90`, `cmf_ctrl_forcing_mod.F90`, `cmf_ctrl_output_mod.F90`, `cmf_calc_outflw_mod.F90`, `cmf_drv_control_mod.F90`, and `yos_cmf_input.F90`. They should be checked against any additional source being edited. They do **not** assert that all existing files conform uniformly.

When the source and this guide disagree, favor preserving verified existing code behavior and report the discrepancy for maintainer review. Do not use the guide to justify an unapproved interface, formatting, or numerical change.
