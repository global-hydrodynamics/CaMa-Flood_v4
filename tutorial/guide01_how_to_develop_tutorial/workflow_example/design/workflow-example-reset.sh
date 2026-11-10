#!/bin/sh
# Restore the workflow example's learner starting state after developer runs.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
WORKFLOW_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

printf 'Remove this workflow example’s generated map, runs, analyses, and figures? [y/N]: '
if [ ! -t 0 ] || [ ! -t 1 ]; then
    printf '\n[ERROR] Confirmation requires an interactive terminal.\n' >&2
    exit 1
fi
IFS= read -r ANSWER || ANSWER=""
case "$ANSWER" in
    y|Y|yes|YES|Yes) ;;
    *) printf 'Reset cancelled; files were not changed.\n'; exit 1 ;;
esac

# Remove only the tutorial-owned modified map and named model-run directories.
rm -f "$WORKFLOW_DIR/map_modify/rivhgt_deep.nc"
rm -rf "$WORKFLOW_DIR/out/control" "$WORKFLOW_DIR/out/deep_channel"

# Remove only the analysis products created by the current workflow.
rm -f "$WORKFLOW_DIR/analysis/difference_at_event.nc" \
      "$WORKFLOW_DIR/analysis/event_selection.csv" \
      "$WORKFLOW_DIR/analysis/output_summary.csv" \
      "$WORKFLOW_DIR/analysis/provisional_flooded_area.csv" \
      "$WORKFLOW_DIR/analysis/provisional_sites.csv" \
      "$WORKFLOW_DIR/analysis/provisional_timeseries.csv" \
      "$WORKFLOW_DIR/analysis/downscale_input.nc"

# Remove only the reviewed figure names produced by this example.
rm -f "$WORKFLOW_DIR/figure/f01-rivhgt_map_change.png" \
      "$WORKFLOW_DIR/figure/f02-model_fields.png" \
      "$WORKFLOW_DIR/figure/f03-river_timeseries.png" \
      "$WORKFLOW_DIR/figure/f04-flood_fraction_and_cell_area.png" \
      "$WORKFLOW_DIR/figure/f05-event_sensitivity.png" \
      "$WORKFLOW_DIR/figure/f06-one_minute_inundation.png"

printf '\nWorkflow example learner state restored.\n'
printf '  design/ and sample_figure/: preserved\n'
printf '  shared map and runoff inputs: preserved\n'
