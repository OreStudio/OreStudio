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

build="${XML_BENCH_BUILD:-build/output/linux-clang-release-ninja}"
[[ -d "$build" ]] || { echo "no build tree at $build" >&2; exit 1; }

# The benchmark is a CMake target, so the same configure step builds it with
# the same warnings, the same -Werror and the same build type as the code it
# measures. This script builds nothing: it runs what CMake produced, which is
# what makes the numbers describe the program the project ships.
target=ores.ore.xml.benchmarks
binary="$build/publish/bin/$target"
if [[ ! -x "$binary" ]]; then
    echo "no $target in $build/publish/bin; build it first:" >&2
    echo "    ./compass.sh build $target" >&2
    exit 1
fi

opt="$(python3 - "$build" <<'PYINNER'
import json, re, sys
for entry in json.load(open(sys.argv[1] + "/compile_commands.json")):
    if entry["file"].endswith("benchmarks/xml/bench.cpp"):
        found = re.findall(r"-O[0-9s]", entry["command"])
        print(found[-1] if found else "-O0")
        break
else:
    print("-O0")
PYINNER
)"

commit="$(git rev-parse --short HEAD)"
date="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
# The version line reads "Debian clang version 21.1.8 (13)" on this platform,
# so take the token after "version" rather than the first token, which is the
# distribution.
cc="$(clang++ --version | head -1 | awk '{for (i = 1; i < NF; ++i) if ($i == "version") { print "clang " $(i + 1); exit } }')"
[[ -n "$cc" ]] || cc="$(clang++ --version | head -1)"
cc="$cc $opt"
record=1
[[ "${1:-}" == "--no-record" ]] && record=0

declare -a rows=()

counts() {
    # One cachegrind run, one repeat count, five counts. Collected into the
    # caller's variables through the echo, in a fixed order.
    local mode="$1" path="$2" runs="$3" trace
    trace="$(valgrind --tool=cachegrind --cache-sim=yes --branch-sim=no \
        --cachegrind-out-file=/dev/null "$binary" "$mode" "$path" "$runs" 2>&1)"
    local pattern
    for pattern in 'I +refs:' 'D +refs:' 'I1 +misses:' 'D1 +misses:' 'LL +misses:'; do
        printf '%s ' "$(printf '%s' "$trace" |
            { grep -m1 -E "^==[0-9]+== $pattern" || true; } |
            sed 's/.*://; s/(.*//; s/[ ,]//g')"
    done
}

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

    # The process is loaded once and its start-up dominates a short run: the
    # library this links initialises boost, postgres and the rest before main.
    # Running the same work twice, at n and at 1, and dividing the difference
    # by n - 1 measures the parse and drops the start-up. Without this the
    # first-level miss count was seventy times the parse's own.
    local big small
    read -r -a big <<< "$(counts "$mode" "$path" "$repeats")"
    read -r -a small <<< "$(counts "$mode" "$path" 1)"
    local ir dr i1 d1 ll i
    for i in 0 1 2 3 4; do
        if [[ -z "${big[$i]}" || -z "${small[$i]}" ]]; then
            echo "could not read the cachegrind summary for $mode $input" >&2
            return 1
        fi
    done
    ir="$(awk -v a="${big[0]}" -v b="${small[0]}" -v n="$repeats" 'BEGIN{printf "%.1f", (a-b)/(n-1)}')"
    dr="$(awk -v a="${big[1]}" -v b="${small[1]}" -v n="$repeats" 'BEGIN{printf "%.1f", (a-b)/(n-1)}')"
    i1="$(awk -v a="${big[2]}" -v b="${small[2]}" -v n="$repeats" 'BEGIN{printf "%.1f", (a-b)/(n-1)}')"
    d1="$(awk -v a="${big[3]}" -v b="${small[3]}" -v n="$repeats" 'BEGIN{printf "%.1f", (a-b)/(n-1)}')"
    ll="$(awk -v a="${big[4]}" -v b="${small[4]}" -v n="$repeats" 'BEGIN{printf "%.1f", (a-b)/(n-1)}')"
    local d1_rate ll_rate
    d1_rate="$(awk -v m="$d1" -v r="$dr" 'BEGIN{printf "%.1f%%", (r > 0 ? 100*m/r : 0)}')"
    ll_rate="$(awk -v m="$ll" -v a="$i1" -v b="$d1" 'BEGIN{printf "%.1f%%", ((a+b) > 0 ? 100*m/(a+b) : 0)}')"

    local shared="	$repeats	$commit	$date	$cc"
    rows+=("$mode	$input	ns_per_parse	$best$shared")
    rows+=("$mode	$input	ir	$ir$shared")
    rows+=("$mode	$input	dr	$dr$shared")
    rows+=("$mode	$input	i1_misses	$i1$shared")
    rows+=("$mode	$input	d1_misses	$d1$shared")
    rows+=("$mode	$input	ll_misses	$ll$shared")
    rows+=("$mode	$input	d1_miss_rate	$d1_rate$shared")
    rows+=("$mode	$input	ll_miss_rate	$ll_rate$shared")
    printf '%-6s %-10s ns/parse %10s  ir %12s  d1 %10s  ll %8s  d1 rate %6s\n' \
        "$mode" "$input" "$best" "$ir" "$d1" "$ll" "$d1_rate"
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
        printf 'mode\tinput\tmetric\tvalue\trepeats\tcommit\tdate\tcompiler\n' > "$history"
    fi
    for row in "${rows[@]}"; do
        printf '%s\n' "$row" >> "$history"
    done
    echo "recorded ${#rows[@]} rows in $history"
fi
