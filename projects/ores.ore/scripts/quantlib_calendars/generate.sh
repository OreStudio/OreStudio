#!/bin/bash
#
# Regenerates refdata_calendar_exceptions_populate.sql from ORE's own QuantLib.
# Run it when the ORE vendor package moves. Nothing else links QuantLib, so the
# services keep no runtime dependency on it.
#
# ORE_ENGINE is a checkout of the ORE tag the package was built from, and it
# must hold a configured build, whose ql/config.hpp states the library's ABI.
set -euo pipefail

root="$(git rev-parse --show-toplevel)"
ore_version="${ORE_VERSION:-1.8.17.0}"
engine="${ORE_ENGINE:?Set ORE_ENGINE to a built checkout of ORE $ore_version}"
preset="${ORES_PRESET:-linux-clang-release-ninja}"
output="$root/build/output/$preset"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

tar xzf "$output/publish/vendor-packages/ore-$ore_version-x64-linux.tar.gz" -C "$work" ./lib/

clang++ -std=c++20 -O1 -Wno-deprecated-declarations \
    -I"$engine/build/linux-clang-ninja-release/QuantLib" \
    -I"$engine/QuantLib" \
    -I"$output/vcpkg_installed/x64-linux/include" \
    "$root/projects/ores.ore/scripts/quantlib_calendars/generate_calendar_exceptions.cpp" \
    -o "$work/generate" "$work/lib/libQuantLib.so.1" \
    -Wl,-rpath,"$work/lib" -Wl,--allow-shlib-undefined

"$work/generate" "$root/projects/ores.sql/populate/refdata/refdata_calendar_exceptions_populate.sql" \
    | tee "$work/summary"

# The build_calendars step builds to this year. Past the data it would write
# weekdays with no holidays.
last_year="$(sed -n '1s/.* to //p' "$work/summary")"
handler="$root/projects/ores.iam/core/include/ores.iam.core/messaging/tenant_provisioning_handler.hpp"
if ! grep -q "last_generated_holiday_year = $last_year;" "$handler"; then
    echo "The generator ends in $last_year but last_generated_holiday_year in $handler does not." >&2
    exit 1
fi
