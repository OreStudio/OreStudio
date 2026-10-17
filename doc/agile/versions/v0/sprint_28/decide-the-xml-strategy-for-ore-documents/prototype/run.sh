#!/usr/bin/env bash
# Regenerates the descriptor tables, builds the prototype and prints every
# measurement the investigation reports. Rerun it to check the numbers.
#
# It needs a configured build tree, because the baseline links the real
# ores.ore.core library and borrows that tree's compile flags.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

root="$here"
while [[ ! -e "$root/.git" ]]; do
    root="$(dirname "$root")"
    [[ "$root" == "/" ]] && { echo "not inside the repository" >&2; exit 1; }
done

build="$root/build/output/linux-clang-release-ninja"
[[ -d "$build" ]] || { echo "no build tree at $build" >&2; exit 1; }

flags="$(python3 - "$build" <<'PY'
import json, re, sys
commands = json.load(open(sys.argv[1] + "/compile_commands.json"))
for entry in commands:
    if entry["file"].endswith("xml/importer.cpp"):
        text = re.sub(r"-o\s+\S+\.o", "", entry["command"])
        text = re.sub(r"-c\s+\S+$", "", text)
        text = re.sub(r"^\S*/clang\+\+", "", text)
        print(" ".join(text.split()))
        break
PY
)"

python3 "$here/gen_tables.py" \
    --schema "$root/external/ore/xsd/currencyconfig.xsd" \
    --schema "$root/external/ore/xsd/ore_types.xsd" \
    --type currencyDefinition \
    --root CurrencyConfig \
    --item Currency \
    --record currency \
    --header codec.hpp > "$here/currency_tables.cpp"

clang++ $flags -std=c++23 -O2 -Wall -Wextra -Wno-invalid-offsetof -Wno-unused-parameter \
    -I"$here" \
    "$here/bench.cpp" "$here/scanner.cpp" "$here/currency_tables.cpp" \
    -o "$here/bench" -Wl,--allow-shlib-undefined \
    -L"$build/publish/lib" \
    -lores.ore.core -lores.platform -lores.utility -lores.refdata.api -lores.trading.api \
    -L"$build/vcpkg_installed/x64-linux/lib" -lpugixml

clang++ $flags -std=c++23 -O2 -Wall -Wextra -Wno-unused-parameter -I"$here" \
    "$here/bench_baseline.cpp" \
    -o "$here/bench_baseline" -Wl,--allow-shlib-undefined \
    -L"$build/publish/lib" \
    -lores.ore.core -lores.platform -lores.utility -lores.refdata.api -lores.trading.api

export LD_LIBRARY_PATH="$build/publish/lib"
cd "$root"

echo "== one document end to end (currencies.xml, 179 records)"
for variant in 1 2 3 4 5; do
    echo "-- variant $variant"
    "$here/bench" "$variant" external/ore/examples/Input/currencies.xml 2>/dev/null |
        grep -v "info\]" || true
done

echo "== round trip"
"$here/bench" 6 external/ore/examples/Input/currencies.xml 2>/dev/null

echo "== numbers a binary float cannot hold"
for document in \
    external/ore/examples/Products/Input/portfolio.xml \
    external/ore/examples/Products/Input/curveconfig.xml \
    external/ore/examples/CurveBuilding/BootstrapConsistency/Input/ois_portfolio.xml
do
    echo "-- $(basename "$document")"
    "$here/bench" 7 "$document" 2>/dev/null | tail -4
done

echo "== parser throughput"
for document in \
    external/ore/examples/Input/currencies.xml \
    external/ore/examples/Products/Input/portfolio.xml \
    external/ore/examples/Products/Input/curveconfig.xml \
    external/ore/examples/ORE-Python/Notebooks/Dependencies/conventions.xml \
    external/ore/examples/ORE-Python/Notebooks/Dependencies/calendar_adjustments.xml
do
    echo "-- $(basename "$document")"
    "$here/bench" 5 "$document" 2>/dev/null | tail -1
    "$here/bench" 8 "$document" 2>/dev/null | tail -1
    case "$document" in
        *portfolio.xml)
            # The built library predates this checkout's domain.hpp, so the
            # generated trade layout the executable sees is not the one the
            # library was compiled with. Loading a portfolio crashes on that
            # mismatch. Rebuild the tree to take the number.
            echo "xsdcpp     : not measured (stale build tree)" ;;
        *)
            "$here/bench_baseline" "$document" 2>/dev/null | tail -3 ;;
    esac
done
