#!/bin/sh

# Generate GrADS CTL files for the base map and high-resolution directories.
# Run from this script's map-local script_region directory. All data paths are
# resolved by the shared generator relative to the parent map directory.

usage() {
  cat <<'EOF'
Usage: ./s02-write_ctl.sh [--dry-run] [--no-overwrite | --overwrite]

  --dry-run    Validate inputs and list planned CTLs without writing files.
  --overwrite  Replace existing CTLs (the default).
  --no-overwrite  Preserve existing CTLs instead of replacing them (default replaces).
EOF
}

DRY_RUN=0
OVERWRITE=1
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

# Like s01, keep the map-local script directory as the working directory.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd) || exit 1
cd "$SCRIPT_DIR" || exit 1

GENERATOR="../../tools/code/generate_ctl.py"
if [ ! -f "$GENERATOR" ]; then
  printf '\n[ERROR] Missing shared CTL generator: %s\n' "$GENERATOR" >&2
  exit 1
fi

STATUS=0
set --
[ "$DRY_RUN" -eq 0 ] || set -- "$@" --dry-run
[ "$OVERWRITE" -eq 0 ] || set -- "$@" --overwrite

# params.txt in the parent directory defines the base-resolution grid.
printf '\n==================================================\n'
printf '[STEP 1/2] Generate CTLs for the base map\n'
printf '==================================================\n\n'
python3 "$GENERATOR" --map-dir .. "$@" || STATUS=1
printf '\n'

# Each high-resolution directory provides its own grid through location.txt.
for LOCATION in ../*/location.txt
do
  [ -f "$LOCATION" ] || continue
  MAP_DIR=${LOCATION%/location.txt}
  printf '\n==================================================\n'
  printf '[STEP 2/2] Generate CTLs for high-resolution map %s\n' "$MAP_DIR"
  printf '==================================================\n\n'
  python3 "$GENERATOR" --map-dir "$MAP_DIR" "$@" || STATUS=1
  printf '\n'
done

if [ "$STATUS" -eq 0 ]; then
  printf '\n==================================================\n'
  printf '[SUCCESS] %s\n' "${0##*/}"
  printf '==================================================\n'
fi

exit "$STATUS"
