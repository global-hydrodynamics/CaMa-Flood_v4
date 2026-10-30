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

# Like s01, keep the map-local script directory as the working directory.
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

# params.txt in the parent directory defines the base-resolution grid.
python3 "$GENERATOR" --map-dir .. "$@" || STATUS=1

# Each high-resolution directory provides its own grid through location.txt.
for LOCATION in ../*/location.txt
do
  [ -f "$LOCATION" ] || continue
  MAP_DIR=${LOCATION%/location.txt}
  python3 "$GENERATOR" --map-dir "$MAP_DIR" "$@" || STATUS=1
done

exit "$STATUS"
