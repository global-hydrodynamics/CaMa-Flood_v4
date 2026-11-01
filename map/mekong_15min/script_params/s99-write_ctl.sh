#!/bin/sh

# Generate GrADS CTL files for existing registered binaries in the base map
# and immediate high-resolution directories. Copy into a map-local
# script_params/ directory before running; data paths are relative to its
# parent map directory.

usage() {
  cat <<'EOF'
Usage: ./s99-write_ctl.sh [--dry-run] [--no-overwrite | --overwrite]

  --dry-run       Validate inputs and list planned CTLs without writing files.
  --overwrite     Replace existing CTLs (the default).
  --no-overwrite  Preserve existing CTLs instead of replacing them.
EOF
}

# These internal flags are set by the command-line options in usage().
DRY_RUN=0    # Set to 1 by --dry-run.
OVERWRITE=1  # Set to 0 by --no-overwrite; existing CTLs are replaced by default.
for OPTION in "$@"
do
  case "$OPTION" in
    --dry-run) DRY_RUN=1 ;;
    --overwrite) OVERWRITE=1 ;;
    --no-overwrite) OVERWRITE=0 ;;
    -h|--help) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
done

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# Keep this map-local script directory as the working directory, as in the
# reusable script_region CTL wrapper.
# Command substitution stores the resolved path; CDPATH= keeps cd output predictable.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd) || exit 1
cd "$SCRIPT_DIR" || exit 1

GENERATOR="../../tools/code/generate_ctl.py"
if [ ! -f "$GENERATOR" ]; then
  printf '\n[ERROR] Missing shared CTL generator: %s\n' "$GENERATOR" >&2
  exit 1
fi

STATUS=0
# set -- rebuilds the argument list passed to each generator invocation.
set --
[ "$DRY_RUN" -eq 0 ] || set -- "$@" --dry-run
[ "$OVERWRITE" -eq 0 ] || set -- "$@" --overwrite

# The generator examines only .bin files that already exist in each directory.
printf '\n==================================================\n'
printf '[STEP 1/2] Generate CTLs for the base map\n'
printf '==================================================\n\n'
python3 "$GENERATOR" --map-dir .. "$@" || STATUS=1
printf '\n'

# High-resolution grids provide their geometry in location.txt.
for LOCATION in ../*/location.txt
do
  [ -f "$LOCATION" ] || continue
  # Remove the /location.txt suffix to get the high-resolution map directory.
  MAP_DIR=${LOCATION%/location.txt}
  printf '\n==================================================\n'
  printf '[STEP 2/2] Generate CTLs for high-resolution map %s\n' "$MAP_DIR"
  printf '==================================================\n\n'
  python3 "$GENERATOR" --map-dir "$MAP_DIR" "$@" || STATUS=1
  printf '\n'
done

if [ "$STATUS" -eq 0 ]; then
  printf '\n==================================================\n'
  # ${0##*/} prints only this script's filename in the success message.
  printf '[SUCCESS] %s\n' "${0##*/}"
  printf '==================================================\n'
fi

exit "$STATUS"
