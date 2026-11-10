#!/bin/sh
# Restore Tutorial02 to its learner starting state after developer verification.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
TUTORIAL_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)

printf 'Remove Tutorial02 run outputs, learner-created variant scripts, and generated figures? [y/N]: '
if [ ! -t 0 ] || [ ! -t 1 ]; then
    printf '\n[ERROR] Confirmation requires an interactive terminal.\n' >&2
    exit 1
fi
IFS= read -r ANSWER || ANSWER=""
case "$ANSWER" in
    y|Y|yes|YES|Yes) ;;
    *) printf 'Reset cancelled; files were not changed.\n'; exit 1 ;;
esac

# Remove only this tutorial's known generated experiment directories.
for PROJECT in mek_default mek_nobif mek_kine mek_nofld; do
    if [ -d "$TUTORIAL_DIR/out/$PROJECT" ]; then
        rm -rf "$TUTORIAL_DIR/out/$PROJECT"
    fi
done

# Remove only the three learner-created alternative configurations.
rm -f "$TUTORIAL_DIR/mek_nobif.sh" \
      "$TUTORIAL_DIR/mek_kine.sh" \
      "$TUTORIAL_DIR/mek_nofld.sh"

# Remove only the figures produced by the supplied visualization workflow.
rm -f "$TUTORIAL_DIR/figure/f01a-upstream_area.png" \
      "$TUTORIAL_DIR/figure/f01b-grid_network.png" \
      "$TUTORIAL_DIR/figure/f01c-outlet_bifurcation.png" \
      "$TUTORIAL_DIR/figure/f01d-delta_catchments.png" \
      "$TUTORIAL_DIR/figure/f02-spinup_comparison.png" \
      "$TUTORIAL_DIR/figure/f03-discharge_comparison.png" \
      "$TUTORIAL_DIR/figure/f04-annual_max_discharge.png"

printf '\nTutorial02 learner state restored.\n'
printf '  Default script and shared sample inputs: preserved\n'
printf '  design/ and sample_figure/: preserved\n'
