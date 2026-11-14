# Scientific Why Discovery Workflow

This reusable workflow is intended primarily for ChatGPT. Invoke it explicitly when asked to identify scientific or algorithmic design rationale that is missing from source documentation. It is independent of any particular module or programming language.

## Review scope

Examine the supplied source and relevant available documentation for decisions whose **Why** is unclear, including:

- Scientific assumptions and physical approximations.
- Numerical algorithms and empirical parameters.
- Spatial and temporal discretization.
- Conservation properties and numerical stability.
- Boundary conditions and special handling.
- Non-obvious processing steps or legacy behavior.

Focus on why a scientific or algorithmic choice was made, not merely what the code does. Exclude routine formatting, naming conventions, and ordinary implementation details.

## Review procedure

1. Read the target source and relevant available documentation.
2. Identify processing decisions whose rationale is not sufficiently explained.
3. Separate verified implementation behavior from unknown design rationale.
4. Form concise Why questions without inventing explanations.
5. Record the procedure or component and approximate source location for each question.

Do not interpret a code comment or variable name as proof of scientific intent. If no material Why gaps are found, say so instead of inventing questions.

## Output

Produce a self-contained Markdown **Scientific Why Question List** suitable for saving and passing directly to Codex. Include the target source file and source version or commit when available. Use sequential question IDs (`WHY-001`, `WHY-002`, ...) and a compact table:

| ID | Procedure/component and location | Verified behavior | Undocumented Why question |
| --- | --- | --- | --- |

Do not include speculative answers, code changes, or documentation patches.

## Suggested invocation

> Review `[source file or component]` using `scientific_why_discovery.md`. Return only a self-contained Scientific Why Question List. Do not modify files.
