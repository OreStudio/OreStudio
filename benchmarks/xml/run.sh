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

# The tree's own flags decide the optimisation level; appending another -O
# here would silently override it and make the history incomparable with the
# product build. -g only adds symbols, which cachegrind needs for the profile.
opt="$(printf '%s' "$flags" | grep -oE '\-O[0-9s]' | tail -1)"
opt="${opt:--O0}"

binary="$here/bench"
clang++ $flags -std=c++23 -g \
    "$here/bench.cpp" \
    projects/ores.ore/core/src/xml/document.cpp \
    projects/ores.ore/core/src/xml/scanner.cpp \
    -o "$binary" -L"$build/vcpkg_installed/x64-linux/lib" -lpugixml

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
    # A grep that finds nothing returns 1, and with pipefail that would end the
    # run before the guard below could say why.
    extract() {
        printf '%s' "$trace" |
            { grep -m1 -E "^==[0-9]+== $1" || true; } |
            sed 's/.*://; s/(.*//; s/[ ,]//g'
    }

    # Cachegrind simulates the first level and one last level. It has no
    # second-level model, so a miss that leaves L1 is either served by the
    # last level or counted as an LL miss. The rates are recorded as well as
    # the counts because utilisation is a rate and a count alone does not say
    # whether the working set fits.
    local metrics=()
    for spec in \
        'ir|I +refs:' \
        'i1_misses|I1 +misses:' \
        'd1_misses|D1 +misses:' \
        'll_misses|LL +misses:' \
        'd1_miss_rate|D1 +miss rate:' \
        'll_miss_rate|LL +miss rate:'
    do
        local name="${spec%%|*}" pattern="${spec#*|}" value
        value="$(extract "$pattern")"
        if [[ -z "$value" ]]; then
            echo "could not read $name from the cachegrind summary for $mode $input" >&2
            return 1
        fi
        # Counts cover the whole run; the time is per parse. Dividing here makes
        # a row comparable with a row for a different input, which is the only
        # reason to keep a history at all. Rates are already ratios and stay.
        case "$name" in
            *_rate) ;;
            ir|i1_misses|d1_misses|ll_misses)
                value="$(awk -v v="$value" -v n="$repeats" 'BEGIN { printf "%.1f", v / n }')" ;;
        esac
        metrics+=("$name" "$value")
    done

    local shared="	$repeats	$commit	$date	$cc"
    rows+=("$mode	$input	ns_per_parse	$best$shared")
    printf '%-6s %-10s ns/parse %10s' "$mode" "$input" "$best"
    local i
    for ((i = 0; i < ${#metrics[@]}; i += 2)); do
        rows+=("$mode	$input	${metrics[$i]}	${metrics[${i}+1]}$shared")
        printf '  %s %s' "${metrics[$i]}" "${metrics[${i}+1]}"
    done
    printf '\n' 
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
