#!/bin/sh
# Build a separate NetCDF-enabled v5 executable without replacing the default build.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
REPO_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
OUTPUT_EXE=${1:-"$REPO_DIR/out/bin/MAIN_cmf_netcdf"}
BUILD_PARENT=${TMPDIR:-/tmp}

if ! command -v nf-config >/dev/null 2>&1 || ! command -v nc-config >/dev/null 2>&1; then
    printf 'ERROR: NetCDF-Fortran and NetCDF-C development tools are required (nf-config, nc-config).\n' >&2
    exit 1
fi

BUILD_DIR=$(mktemp -d "$BUILD_PARENT/cmf-v5-netcdf-build.XXXXXX")
trap 'rm -rf "$BUILD_DIR"' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

printf '[STEP 1/3] Copy v5 sources into an isolated temporary build directory.\n'
cp -R "$REPO_DIR/src_main" "$BUILD_DIR/src_main"
cp -R "$REPO_DIR/adm" "$BUILD_DIR/adm"

printf '\n[STEP 2/3] Compile with NetCDF support.\n'
make -C "$BUILD_DIR/src_main" clean
make -C "$BUILD_DIR/src_main" all \
    CFLAGS="-DUseCDF_CMF -DSinglePrec_CMF" \
    INCDIRS="$(nf-config --fflags) $(nc-config --cflags)" \
    LIBS="$(nf-config --flibs) $(nc-config --libs)"

printf '\n[STEP 3/3] Install the separate executable at %s.\n' "$OUTPUT_EXE"
mkdir -p "$(dirname "$OUTPUT_EXE")"
cp "$BUILD_DIR/src_main/MAIN_cmf" "$OUTPUT_EXE"
printf '[SUCCESS] NetCDF-enabled executable built: %s\n' "$OUTPUT_EXE"
