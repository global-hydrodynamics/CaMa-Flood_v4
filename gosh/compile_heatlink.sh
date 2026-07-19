#!/bin/sh

set -eu

# Build the heatlink-enabled executable without replacing adm/Mkinclude.
# Usage: compile_heatlink.sh [debug|release]

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_MODE=${1:-debug}
TOOLCHAIN_ROOT=${TOOLCHAIN_ROOT:-/Users/dtokuda/miniconda3/envs/ils}
SDKROOT=${SDKROOT:-$(xcrun --show-sdk-path)}
GFORTRAN=${TOOLCHAIN_ROOT}/bin/gfortran
NF_CONFIG=${TOOLCHAIN_ROOT}/bin/nf-config

case "$BUILD_MODE" in
    debug|release)
        ;;
    *)
        echo "Usage: $0 [debug|release]" >&2
        exit 2
        ;;
esac

if [ ! -x "${GFORTRAN}" ]; then
    echo "gfortran was not found at ${GFORTRAN}." >&2
    exit 1
fi
if [ ! -x "${NF_CONFIG}" ]; then
    echo "nf-config was not found at ${NF_CONFIG}." >&2
    exit 1
fi

NETCDF_FC=$("${NF_CONFIG}" --fc)
case "${NETCDF_FC}" in
    "${TOOLCHAIN_ROOT}"/*) ;;
    *)
        echo "nf-config selects a compiler outside TOOLCHAIN_ROOT: ${NETCDF_FC}" >&2
        exit 1
        ;;
esac
if [ ! -x "${NETCDF_FC}" ]; then
    echo "The compiler selected by nf-config is not executable: ${NETCDF_FC}" >&2
    exit 1
fi

export PATH="${TOOLCHAIN_ROOT}/bin:${PATH}"
export DYLD_LIBRARY_PATH="${TOOLCHAIN_ROOT}/lib:${DYLD_LIBRARY_PATH:-}"
export SDKROOT TOOLCHAIN_ROOT NF_CONFIG

echo "Building heatlink executable (${BUILD_MODE})"
echo "  compiler: ${NETCDF_FC}"
echo "  nf-config: ${NF_CONFIG}"
echo "  netCDF:   $("${NF_CONFIG}" --version)"
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

for test_exe in test_numeric_utils test_key_table test_ranked_array test_datetime
do
    echo "  ${test_exe}"
    "${ROOT}/src/common/${test_exe}"
done

echo "All common unit tests passed."

echo "Building and running physics unit tests"
make -C "${ROOT}/src/phys" \
    MKINCLUDE="${ROOT}/adm/Mkinclude_heatlink" \
    BUILD_MODE="${BUILD_MODE}" test

for test_exe in test_heat_budget test_ice_cover
do
    echo "  ${test_exe}"
    "${ROOT}/src/phys/${test_exe}"
done

echo "All physics unit tests passed."

echo "Building and running heatlink unit tests"
make -C "${ROOT}/src/heatlink" \
    MKINCLUDE="${ROOT}/adm/Mkinclude_heatlink" \
    BUILD_MODE="${BUILD_MODE}" test

for test_exe in test_water_storage_adapter test_heatlink_input_adapter \
    test_river_water_advection
do
    echo "  ${test_exe}"
    "${ROOT}/src/heatlink/${test_exe}"
done

echo "All heatlink unit tests passed."
