#!/usr/bin/env bash
# Replay every generated ores.trading shell recipe against the live fleet.
#
# Item V04 of the component clean standard. The group list is derived from
# the recipes rather than typed, so a group added later is replayed without
# anyone remembering to add it here: a group counts when its recipe source
# carries the codegen's AUTO-GENERATED marker and names a trading.v1 subject.
# The destructive generated verbs are recorded rather than run.
#
# The checkout .env must be sourced first: the harness invokes ores.shell
# directly and the shell takes its subject prefix and TLS paths from the
# environment, not from the shell's own configuration. Without it every
# recipe aborts at its sign-in line and the summary blames the credential.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO"

set -a
# shellcheck disable=SC1091
. ./.env
set +a

: "${V04_PRINCIPAL:?set V04_PRINCIPAL to a bootstrapped principal}"
: "${V04_PASSWORD:?set V04_PASSWORD to that principal's password}"

mapfile -t GROUPS < <(python3 - <<'PY'
from pathlib import Path
import re
for org in sorted(Path('doc/recipes/shell').rglob('*.org')):
    if org.parent == Path('doc/recipes/shell'):
        continue
    txt = org.read_text(errors='replace')
    if 'AUTO-GENERATED FILE' in txt and re.search(r'trading\.v1\.', txt):
        print(org.parent.name)
PY
)
GROUPS=($(printf '%s\n' "${GROUPS[@]}" | sort -u))

mapfile -t SKIPS < <(python3 - <<'PY'
from pathlib import Path
lib = Path('projects/ores.shell/scripts/library')
groups = set()
for org in Path('doc/recipes/shell').rglob('*.org'):
    if org.parent == Path('doc/recipes/shell'):
        continue
    txt = org.read_text(errors='replace')
    if 'AUTO-GENERATED FILE' in txt and 'trading.v1.' in txt:
        groups.add(org.parent.name)
for g in sorted(groups):
    for s in sorted((lib / g).glob('*.ores')):
        if s.stem.endswith('-delete') or s.stem.endswith('-delete-many'):
            print(s.stem)
PY
)
SKIP="$(IFS=,; echo "${SKIPS[*]}")"

echo "groups=${#GROUPS[@]} destructive_not_replayed=${#SKIPS[@]}"
projects/ores.codegen/scripts/check_shell_recipes.py \
    "${GROUPS[@]}" \
    --skip "$SKIP" \
    --out-dir .audit/clean-trading-v04
