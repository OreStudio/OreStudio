#!/usr/bin/env bash
#
# Rebuilds the market datum catalogue under external/ore/catalogue from a local
# ORE engine checkout and its build.
#
#   regenerate.sh <ORE source dir> <ORE build dir>
#
set -euo pipefail

if [[ $# -ne 2 ]]; then
    echo "usage: $0 <ORE source dir> <ORE build dir>" >&2
    exit 1
fi

ore_source="$(realpath "$1")"
ore_build="$(realpath "$2")"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ore_root="$(cd "${here}/../.." && pwd)"
out="${ore_root}/catalogue"
work="$(mktemp -d)"
trap 'rm -rf "${work}"' EXIT

python3 "${here}/generate_dump.py" "${ore_source}" "${here}/datum_dump.inc"
cmake -S "${here}" -B "${work}/build" -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DORE_SOURCE_DIR="${ore_source}" -DORE_BUILD_DIR="${ore_build}" > /dev/null
cmake --build "${work}/build" > /dev/null

mkdir -p "${out}"
"${work}/build/datum_catalogue" < "${here}/forms.txt" > "${out}/forms.jsonl"
python3 "${here}/extract_enum_tokens.py" "${ore_source}" InstrumentType > "${out}/instrument_types.txt"
python3 "${here}/extract_enum_tokens.py" "${ore_source}" QuoteType > "${out}/quote_types.txt"
python3 "${here}/generate_quote_matrix.py" "${out}/forms.jsonl" "${out}/quote_types.txt" \
    | "${work}/build/datum_catalogue" > "${out}/quote_matrix.jsonl"
python3 "${here}/extract_corpus_keys.py" "${ore_root}/examples" > "${work}/corpus_keys.txt"
"${work}/build/datum_catalogue" < "${work}/corpus_keys.txt" | gzip -n -9 > "${out}/corpus.jsonl.gz"

version="$(git -C "${ore_source}" describe --tags --always 2>/dev/null || echo unknown)"
commit="$(git -C "${ore_source}" rev-parse --short HEAD 2>/dev/null || echo unknown)"
# The .jsonl content is reproducible from the same ORE commit; the compressed
# bytes are reproducible only with the same gzip, so its version is kept too.
{
    echo "ore ${version} ${commit}"
    echo "gzip $(gzip --version | head -1 | awk '{print $NF}')"
} > "${out}/ore_version.txt"
echo "Catalogue written to ${out} from ORE ${version} (${commit})."
