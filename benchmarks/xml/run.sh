#!/usr/bin/env bash
#
# Runs the XML benchmark and records the result.
#
# Two axes, because they answer different questions. Wall time says what a user
# waits for; cachegrind says why. Cachegrind simulates the caches, so its counts
# are identical on every run of the same binary, which is what makes a committed
# history worth diffing. Wall time is not stable on a shared machine, so it is
# recorded and read with that in mind.
#
#   ./run.sh                measure and append to history.tsv
#   ./run.sh --no-record    measure and print, leave the history alone
#
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root"

build=build/output/linux-clang-release-ninja
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

binary="$here/bench"
clang++ $flags -std=c++23 -O2 -g \
    "$here/bench.cpp" \
    projects/ores.ore/core/src/xml/document.cpp \
    projects/ores.ore/core/src/xml/scanner.cpp \
    -o "$binary" -L"$build/vcpkg_installed/x64-linux/lib" -lpugixml

commit="$(git rev-parse --short HEAD)"
date="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
cc="$(clang++ --version | head -1 | sed 's/ .*//; s/clang version //')"
record=1
[[ "${1:-}" == "--no-record" ]] && record=0

declare -a rows=()

measure() {
    local mode="$1" input="$2" path="$3" repeats="$4"

    local best=""
    for _ in 1 2 3 4 5; do
        local line ns
        line="$("$binary" "$mode" "$path" "$repeats")"
        ns="$(printf '%s' "$line" | awk '{print $2}')"
        if [[ -z "$best" ]] || awk "BEGIN{exit !($ns < $best)}"; then
            best="$ns"
        fi
    done

    local trace
    trace="$(valgrind --tool=cachegrind --cache-sim=yes --branch-sim=no \
        --cachegrind-out-file=/dev/null "$binary" "$mode" "$path" "$repeats" 2>&1)"
    local ir d1 ll
    extract() {
        printf '%s' "$trace" |
            grep -m1 -E "^==[0-9]+== $1" |
            sed 's/.*://; s/(.*//; s/[ ,]//g'
    }
    ir="$(extract 'I +refs:')"
    d1="$(extract 'D1 +misses:')"
    ll="$(extract 'LL +misses:')"

    if [[ -z "$ir" || -z "$d1" || -z "$ll" ]]; then
        echo "could not read the cachegrind summary for $mode $input" >&2
        return 1
    fi

    local shared="	$commit	$date	$cc"
    rows+=("$mode	$input	ns_per_parse	$best$shared")
    rows+=("$mode	$input	ir	$ir$shared")
    rows+=("$mode	$input	d1_misses	$d1$shared")
    rows+=("$mode	$input	ll_misses	$ll$shared")
    printf '%-6s %-22s ns/parse %10s  Ir %12s  D1 misses %10s  LL misses %8s\n' \
        "$mode" "$input" "$best" "$ir" "$d1" "$ll"
}

while IFS=$'\t' read -r name path repeats; do
    [[ -z "$name" || "$name" == \#* ]] && continue
    for mode in scan tape pugi; do
        measure "$mode" "$name" "$path" "$repeats"
    done
done < "$here/inputs.tsv"

if [[ $record -eq 1 ]]; then
    history="$here/history.tsv"
    if [[ ! -f "$history" ]]; then
        printf 'mode\tinput\tmetric\tvalue\tcommit\tdate\tcompiler\n' > "$history"
    fi
    for row in "${rows[@]}"; do
        printf '%s\n' "$row" >> "$history"
    done
    echo "recorded ${#rows[@]} rows in $history"
fi
