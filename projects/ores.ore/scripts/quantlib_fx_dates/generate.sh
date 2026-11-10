#!/usr/bin/env bash
# Prints the cross ladder dates QuantLib gives the cases the forward ladder tests pin.
# Needs ORE_ENGINE, a built checkout of the ORE engine whose QuantLib the build vendors.
# Usage: ORE_ENGINE=/path/to/ORE/Engine projects/ores.ore/scripts/quantlib_fx_dates/generate.sh
# CXX picks the compiler; clang++ by default.
set -euo pipefail

root="$(git rev-parse --show-toplevel)"
ore_version="${ORE_VERSION:-1.8.17.0}"
engine="${ORE_ENGINE:?Set ORE_ENGINE to a built checkout of ORE $ore_version}"
preset="${ORES_PRESET:-linux-clang-release-ninja}"
output="$root/build/output/$preset"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

tar xzf "$output/publish/vendor-packages/ore-$ore_version-x64-linux.tar.gz" -C "$work" ./lib/

"${CXX:-clang++}" -std=c++20 -O1 -Wno-deprecated-declarations \
    -I"$engine/build/linux-clang-ninja-release/QuantLib" \
    -I"$engine/QuantLib" \
    -I"$output/vcpkg_installed/x64-linux/include" \
    "$root/projects/ores.ore/scripts/quantlib_fx_dates/cross_ladder_dates.cpp" \
    -o "$work/dates" "$work/lib/libQuantLib.so.1" \
    -Wl,-rpath,"$work/lib" -Wl,--allow-shlib-undefined

"$work/dates"
