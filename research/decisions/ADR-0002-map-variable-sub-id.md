# ADR-0002: Separate map data role from processing category

- **Status:** Accepted by maintainer
- **Date:** 2026-10-31 (date precision; exact decision time not separately recorded)
- **Event IDs:** `EV-0097`
- **Decision owner:** Dai Yamazaki (maintainer)

## Context

`category_id` is organized around processing order and product grouping. The registry also needs a functional role to filter model-runtime inputs separately from preparation and analysis products. Combining these ideas in one category code would make workflow ordering and functional filtering harder to use independently.

## Alternatives considered

1. Encode both role and category in a compound category value.
2. Keep role in a separate `sub_id` field.
3. Add more than two role values immediately.

## Decision and rationale

Keep `category_id` and `sub_id` as independent fields. `sub_id=A` means a model-execution input, including a supported configurable alternative. `sub_id=B` means supporting data used for map preparation, diagnostics, or analysis rather than directly during model execution. These are the only approved values for now. Leave unverified entries blank. Preserve existing manual A/B assignments and report source-code conflicts for maintainer review.

This keeps processing-order sorting independent from functional-role filtering and avoids unsupported classification guesses.

## Consequences and compatibility constraints

The registry uses `sub_id` as its second column, followed by `workflow_stage`. Consumers must use named CSV fields rather than column offsets. Existing `category_id` values/order and existing assignments are retained. Classification does not establish scientific correctness or dataset validity. A conflicting manual value remains in the registry until the maintainer decides whether to revise it.

## Evidence and implementation links

- Specification: [`map/map_metadata_spec.md`](../../variable_table/map_metadata_spec.md).
- Registry: [`map/map_variables.csv`](../../variable_table/map_variables.csv).
- Implementation and evidence summary: [`DR-0013`](../records/discussions/DR-0013.md); validation `VR-0031`.

## Supersedes / superseded by

None.
