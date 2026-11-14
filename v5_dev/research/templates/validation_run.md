# Validation detail template

The structured entry belongs in `validation_runs.csv`; use this file for detail that does not fit the table.

- Validation ID: `VR-####`
- Event ID: `EV-####`
- Task ID: `DT-####` (if applicable)
- Source commit: full SHA; state worktree clean/dirty
- Environment ID: link to `baseline/environment_inventory.md`
- Dataset ID and checksum: identify exact inputs without a private path
- Command: exact command and working-directory basis
- Expected result:
- Observed result: include output/evidence reference
- Result: pass / fail / inconclusive / not run
- Comparison method and tolerance (if numerical):
- Scientific interpretation and limits: distinguish execution from scientific correctness
