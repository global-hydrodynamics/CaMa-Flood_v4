#!/bin/sh

set -eu

# Build the heatlink-enabled executable without replacing adm/Mkinclude.
# Usage: compile_heatlink.sh [debug|release]

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_MODE=${1:-debug}
TOOLCHAIN_ROOT=${TOOLCHAIN_ROOT:-/Users/dtokuda/miniconda3/envs/ils}
SDKROOT=${SDKROOT:-$(xcrun --show-sdk-path)}

case "$BUILD_MODE" in
    debug|release)
        ;;
    *)
        echo "Usage: $0 [debug|release]" >&2
        exit 2
        ;;
esac

export PATH="${TOOLCHAIN_ROOT}/bin:${PATH}"
export DYLD_LIBRARY_PATH="${TOOLCHAIN_ROOT}/lib:${DYLD_LIBRARY_PATH:-}"
export SDKROOT

if ! command -v gfortran >/dev/null 2>&1; then
    echo "gfortran was not found under TOOLCHAIN_ROOT=${TOOLCHAIN_ROOT}." >&2
    exit 1
fi

echo "Building heatlink executable (${BUILD_MODE})"
echo "  compiler: $(command -v gfortran)"
echo "  config:   ${ROOT}/adm/Mkinclude_heatlink"
echo "  SDK:      ${SDKROOT}"

make -C "${ROOT}/src" \
    MKINCLUDE="${ROOT}/adm/Mkinclude_heatlink" \
    BUILD_MODE="${BUILD_MODE}" clean
make -C "${ROOT}/src" \
    MKINCLUDE="${ROOT}/adm/Mkinclude_heatlink" \
    BUILD_MODE="${BUILD_MODE}" MAIN_cmf

if [ ! -x "${ROOT}/src/MAIN_cmf" ]; then
    echo "Build failed: ${ROOT}/src/MAIN_cmf was not created." >&2
    exit 1
fi

echo "Build completed: ${ROOT}/src/MAIN_cmf"

echo "Building and running common unit tests"
make -C "${ROOT}/src/common" \
    MKINCLUDE="${ROOT}/adm/Mkinclude_heatlink" \
    BUILD_MODE="${BUILD_MODE}" test

for test_exe in test_numeric_utils test_key_table test_ranked_array
do
    echo "  ${test_exe}"
    "${ROOT}/src/common/${test_exe}"
done

echo "All common unit tests passed."
