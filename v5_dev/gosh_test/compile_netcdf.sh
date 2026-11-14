#!/bin/sh
# Build a separate NetCDF-enabled v5 executable without replacing the default build.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
REPO_DIR=$(CDPATH= cd "$SCRIPT_DIR/.." && pwd -P)
OUTPUT_EXE=${1:-"$REPO_DIR/out/bin/MAIN_cmf_netcdf"}
BUILD_PARENT=${TMPDIR:-/tmp}
NF_CONFIG=${NF_CONFIG:-nf-config}
NC_CONFIG=${NC_CONFIG:-nc-config}

if [ ! -f "$REPO_DIR/config/Mkinclude" ]; then
    printf 'ERROR: v5 build configuration is missing: %s\n' "$REPO_DIR/config/Mkinclude" >&2
    printf 'Run ./s01-setup_fortran.sh, then copy ./Mkinclude to config/Mkinclude.\n' >&2
    exit 1
fi

if ! command -v "$NF_CONFIG" >/dev/null 2>&1; then
    printf 'ERROR: NetCDF-Fortran is required; nf-config was not found on PATH.\n' >&2
    exit 1
fi
NC_LIBS=""
if command -v "$NC_CONFIG" >/dev/null 2>&1; then
    NC_LIBS=$("$NC_CONFIG" --libs | sed 's/-lnetcdf//g')
fi

BUILD_DIR=$(mktemp -d "$BUILD_PARENT/cmf-v5-netcdf-build.XXXXXX")
trap 'rm -rf "$BUILD_DIR"' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

printf '[STEP 1/3] Copy v5 sources into an isolated temporary build directory.\n'
cp -R "$REPO_DIR/src_main" "$BUILD_DIR/src_main"
cp -R "$REPO_DIR/config" "$BUILD_DIR/config"

printf '\n[STEP 2/3] Compile with NetCDF support.\n'
make -C "$BUILD_DIR/src_main" clean
make -C "$BUILD_DIR/src_main" all \
    CFLAGS="-DUseCDF_CMF -DSinglePrec_CMF" \
    INC="$("$NF_CONFIG" --fflags)" \
    LIB="$("$NF_CONFIG" --flibs) $NC_LIBS"

printf '\n[STEP 3/3] Install the separate executable at %s.\n' "$OUTPUT_EXE"
mkdir -p "$(dirname "$OUTPUT_EXE")"
cp "$BUILD_DIR/src_main/MAIN_cmf" "$OUTPUT_EXE"
printf '[SUCCESS] NetCDF-enabled executable built: %s\n' "$OUTPUT_EXE"
