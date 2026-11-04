# CaMa-Flood Fortran Source Style

## Purpose and scope

This document describes file-local formatting and readability conventions for Fortran source under `src_main/`. They should generally be checkable by reading the target file. The goal is to preserve recognizable CaMa-Flood style and make code easier to scan, not to require wholesale reformatting.

Use [`FORTRAN_DESIGN.md`](FORTRAN_DESIGN.md) during implementation and refactoring for architecture, scientific meaning, numerical behavior, interfaces, I/O contracts, parallel semantics, logging design, and semantic comments. During a dedicated final formatting pass, read this guide and the target source file; do not re-derive project-wide scientific meanings or inspect unrelated modules. If a semantic issue is evident, report it separately for design review.

The existing target file is the final reference for local style. Existing files contain variation and exceptions; these recommendations do not claim uniform conformance.

## 1. Layout and formatting

**Observed conventions:**

- Most primary model modules use free-form preprocessed `.F90` source. Lowercase `.f90` files also occur under `mod/` and in tests.
- Module and principal procedure names are commonly uppercase in the main model tree; lowercase names and variations in keyword/intrinsic capitalization also occur.
- Related declarations and `USE ... ONLY:` lists are often aligned.
- The source uses `&` for continuation lines, standalone section comments, and inline comments.
- Indentation, spacing, and `ENDDO` versus `END DO` are not uniform across legacy files.

**Recommendations:**

- Match the nearest relevant style in the target file. In wholly new code, use consistent, modest indentation (typically two spaces per nesting level) without reformatting surrounding legacy code.
- Keep preprocessor directives easy to identify. Use readable continuation lines and explicit block endings in new code.
- Limit Fortran source lines to **128 characters**, including indentation. Shorten comments or diagnostics when their essential meaning remains clear; otherwise use free-form `&` continuation.
- Do not run an automatic formatter or reformat unrelated source as part of a feature change.

## 2. Declarations and local organization

### Imports and declaration section

- Separate the `USE` import section from `IMPLICIT NONE` with one blank line, including when the last import is conditional. Keep `IMPLICIT NONE` at the start of the declaration section.
- Do not add a blank line between `IMPLICIT NONE` and the first declaration unless a meaningful group heading or established local style calls for it.
- End the declaration section immediately before executable statements with the established `!================================================` separator. Reuse an existing separator and avoid duplicates.
- Keep preprocessor directives and declarations inside the appropriate declaration section. Do not change their conditions during style work.

### Declaration groups

- Group declarations by local role: arguments, indices/scalars, working arrays, NetCDF-specific variables, or parallel-related variables, as appropriate to the target procedure.
- Keep closely related declarations together; use one blank line when the role changes. Add a short group heading only when the role is not self-evident.
- Align nearby `::` separators and inline comments where practical. Follow the target's local column style; do not add excessive padding to accommodate an unusually long declaration.
- Preserve declaration order and the order of type and attribute specifications. A formatting pass must not change names, types, kinds, attributes, initializers, dimensions, imports, or array order.
- Treat declarations as a readable inventory, not an invitation to re-investigate or rewrite unrelated variables.

### Namelist declaration layout

- Put each Namelist variable on its own declaration line and give it a concise English `!!` inline description of its role.
- Keep related settings together, separating changes in purpose with a blank line. Align `::` and descriptions where practical without excessive padding.
- Label the Namelist registration statement with its group name. Preserve the registration list during formatting-only work.
- For the **meaning, units, defaults, and compatibility role** of Namelist settings, follow `FORTRAN_DESIGN.md`; do not infer meaning while doing a local formatting pass.

## 3. Blank lines and logical grouping

Use blank lines to separate logically distinct statement groups when a numbered process heading is unnecessary.

- Keep statements serving one immediate purpose together.
- Insert one blank line when the processing purpose, data being handled, or operation type changes. Typical boundaries include initialization to calculation, validation to access, access to conversion, and one variable group to another.
- Keep an operation with its associated error check.
- Do not add blank lines mechanically around every `IF`, `DO`, `CALL`, or `END`.
- Use `!*** N.` headings for major stages and blank lines for smaller transitions. Prefer one blank line and avoid excessive vertical whitespace.
- Preserve meaningful existing spacing instead of applying automatic formatting.

**A blank line should indicate a small change in meaning, not merely a change in syntax.**

## 4. Comments and visual hierarchy

### Procedure hierarchy and purpose comments

For new or substantially updated modules, document the source hierarchy as follows:

- Immediately after module-level `CONTAINS`, list principal `CMF_` procedures with one-line purposes. If module-level shared utilities are present, list them separately under a clear label.
- Immediately after a principal procedure declaration, give its one-line purpose. If it has internal procedures, list them in source order with one-line purposes.
- Immediately after that procedure's internal `CONTAINS`, repeat the same internal-procedure list; the two lists must match.
- Put a concise purpose comment immediately after each internal procedure declaration. List only procedures actually defined in that scope; identify compile-time-conditional procedures where useful.
- Keep summaries focused on responsibility and data flow. The scientific meaning of the procedure belongs to the design guidance in `FORTRAN_DESIGN.md`.
- When editing an existing module, update affected lists and summaries without imposing this structure on unrelated files or renaming procedures.

### Comment syntax and section separators

**Observed visual conventions:**

- Major module-level procedure boundaries commonly use `!####################################################################`.
- Section separators commonly use `!================================================` or local variations.
- Numbered process headings commonly use `!***`:

```fortran
!*** 1. Read input map
!*** 2. Set model arrays
!*** 3. Calculate derived parameters
```

- Module headers may list procedures using `! -- ROUTINE : purpose`.
- Both `!` and `!!` appear in comments; local usage varies.

**Recommendations:**

- Preserve the target file's established hierarchy. Do not add separators around every small block.
- Number major stages sequentially within each procedure, based on changes in purpose, not on each statement or control-flow construct.
- Preserve suitable existing headings; remove gaps, duplicate numbers, or labels that no longer describe the following block.
- Small procedures do not need numbered headings. Avoid comments that merely translate obvious syntax.
- Consolidate duplicate prose and remove redundant decorative comments when the local structure is clearer without them.
- Correct obvious spelling or grammatical errors in comments being revised, without broadening the pass into a content rewrite.
- Keep comment lines concise and within the line-length limit. Use continuation or a shorter phrase rather than letting explanatory prose become hard to scan.

The scientific reason, physical definition, units, algorithm rationale, and accuracy of a comment are design questions covered by `FORTRAN_DESIGN.md`. A final style pass should preserve established semantic content and improve only its presentation.

## 5. Final formatting-pass scope

A dedicated formatting and readability pass should:

1. Read this guide and the target source file.
2. Apply local whitespace, declaration-layout, hierarchy-comment, line-length, and readability rules.
3. Preserve scientific calculations, variable meanings, interfaces, procedure scopes, file I/O, runtime behavior, and parallel behavior.
4. Avoid investigating unrelated modules merely to imitate their formatting.
5. Avoid changing logging content, severity, or frequency.
6. Avoid adding validation, defensive logic, or new abstractions.
7. Report semantic ambiguity or an apparent design defect separately instead of resolving it during formatting.

Do not modify scientific behavior during a style cleanup. If a requested change requires altering a design contract, stop that part and use the project's approval and decision process.
