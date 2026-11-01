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

# Keep this map-local script directory as the working directory, as in the
# reusable script_region CTL wrapper.
SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd) || exit 1
cd "$SCRIPT_DIR" || exit 1

GENERATOR="../../tools/code/generate_ctl.py"
if [ ! -f "$GENERATOR" ]; then
  echo "Missing shared CTL generator: $GENERATOR" >&2
  exit 1
fi

STATUS=0
set --
[ "$DRY_RUN" -eq 0 ] || set -- "$@" --dry-run
[ "$OVERWRITE" -eq 0 ] || set -- "$@" --overwrite

# The generator examines only .bin files that already exist in each directory.
python3 "$GENERATOR" --map-dir .. "$@" || STATUS=1

# High-resolution grids provide their geometry in location.txt.
for LOCATION in ../*/location.txt
do
  [ -f "$LOCATION" ] || continue
  MAP_DIR=${LOCATION%/location.txt}
  python3 "$GENERATOR" --map-dir "$MAP_DIR" "$@" || STATUS=1
done

exit "$STATUS"
