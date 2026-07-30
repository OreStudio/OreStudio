#!/usr/bin/env bash
#
# Sync external/ore/examples/ AND external/ore/xsd/ from a checkout of the
# ORE Engine fork's own Examples/ and xsd/ directories, and regenerate a
# manifest.json in each recording exactly which source commit/timestamp
# the vendored copy came from. Neither directory had a manifest at all
# before this script existed -- there was no way to tell how stale the
# vendored samples/schema were relative to the engine actually being
# packaged (external/ore/packages/), and staleness between the two is
# exactly the kind of drift that produces spurious schema-validation
# failures against perfectly valid, newer example XML (see
# scripts/validate_ore_examples.sh) -- xsd/ and examples/ must be synced
# together, from the same commit, every time.
#
# What gets vendored, examples/: every example directory (Academy,
# AmericanMonteCarlo, CreditRisk, ..., XvaRisk) is synced verbatim. The
# top-level Python test harness files (conftest.py, ore_examples_helper.py,
# ore_wrapper.py, run_examples_testsuite.py, copy_out_expected.sh,
# Readme.md) are deliberately excluded -- this repo vendors the example
# DATA (Input/, ExpectedOutput/, run scripts specific to one example), not
# ORE's own pytest-based test infrastructure for running all of them at
# once.
#
# What gets vendored, xsd/: every *.xsd verbatim, plus check.sh/check.cmd
# (the upstream schema-validation helper scripts). check.py is excluded
# (not present in this repo's existing xsd/ before this script existed;
# this repo has its own equivalent, scripts/validate_ore_examples.sh).
#
# Known hand-patches reapplied after every sync (see reapply_known_patches()
# below and doc/agile/product_backlog/deferred/document_ore_vendor_hand_patches.org):
# upstream ships FX/RATE/USD/GBP and FX/RATE/USD/CHF spot quotes in 3 files
# whose value is genuinely the GBP/USD and CHF/USD rate (confirmed against
# QuantLib's FXSpotQuote convention -- "1 unit of unitCcy = quote * 1 unit
# of ccy" -- 1.3946 is ~1 GBP in USD, not ~1 USD in GBP), i.e. upstream's
# own key order is wrong, not an ORE-Studio-specific convention mismatch.
# Fixed once in PR #1423 (kept, review-confirmed safe: these 3 files each
# have their own local curveconfig.xml with zero reference to the old
# key); PR #1423 review found 4 more candidate files where the fix was
# NOT safe (they resolve the *shared* Input/curveconfig.xml, whose own
# <SpotRate> is load-bearing on the old key) and reverted those --
# deliberately NOT reapplied here, left exactly as upstream ships them.
#
# Usage:
#   ./external/ore/tools/update_ore_samples.sh <ore-engine-source-dir> [options]
#
# Arguments:
#   <ore-engine-source-dir>   Path to the ORE Engine git checkout (e.g.
#                            /home/user/Development/ORE/Engine) --
#                            NOT the build dir. Its Examples/ and xsd/
#                            subdirectories are the sync sources.
#
# Options:
#   --dry-run       Show what rsync would change without changing anything.
#   --no-delete     Don't remove files/dirs that no longer exist upstream
#                  (default: removes them, e.g. an example upstream
#                  renamed or deleted, such as TradeGenerator moving to
#                  ORE-Python/TradeGenerator as of this script's own
#                  first run).
#   --skip-validate  Don't run scripts/validate_ore_examples.sh after
#                  syncing (default: runs it, since a stale xsd/ or a
#                  genuinely malformed example is exactly what this
#                  script exists to catch before it's committed).
#
# Requires: rsync, git, python3 (for the manifests' tree hash), and
# (unless --skip-validate) xmllint.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

# Top-level files in Engine/Examples/ that are ORE's own pytest harness,
# not example data -- never vendored (see the header comment above).
EXCLUDE_EXAMPLES_FILES=(
    "conftest.py"
    "ore_examples_helper.py"
    "ore_wrapper.py"
    "run_examples_testsuite.py"
    "copy_out_expected.sh"
    "Readme.md"
)
EXCLUDE_XSD_FILES=(
    "check.py"
)

DRY_RUN=0
DO_DELETE=1
SKIP_VALIDATE=0
SRC_DIR=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run) DRY_RUN=1; shift ;;
        --no-delete) DO_DELETE=0; shift ;;
        --skip-validate) SKIP_VALIDATE=1; shift ;;
        -h|--help) grep '^#' "${BASH_SOURCE[0]}" | sed 's/^#//'; exit 0 ;;
        *)
            if [[ -z "${SRC_DIR}" ]]; then SRC_DIR="$1"; shift
            else echo "ERROR: unexpected argument: $1" >&2; exit 1
            fi
            ;;
    esac
done

if [[ -z "${SRC_DIR}" ]]; then
    echo "ERROR: <ore-engine-source-dir> is required. See --help." >&2
    exit 1
fi
SRC_DIR="$(cd "${SRC_DIR}" && pwd)"

for tool in rsync git python3; do
    if ! command -v "${tool}" >/dev/null 2>&1; then
        echo "ERROR: '${tool}' is not installed." >&2
        exit 1
    fi
done
if [[ ! -d "${SRC_DIR}/.git" ]]; then
    echo "ERROR: ${SRC_DIR} is not a git checkout -- can't record source commit." >&2
    exit 1
fi

ENGINE_COMMIT="$(git -C "${SRC_DIR}" rev-parse HEAD)"
ENGINE_COMMIT_DATE="$(git -C "${SRC_DIR}" log -1 --format=%cI)"
ENGINE_VERSION="$(git -C "${SRC_DIR}" describe --tags --always)"
ENGINE_VERSION="${ENGINE_VERSION#v}"

echo "Engine source : ${SRC_DIR}"
echo "Engine        : ${ENGINE_VERSION} (${ENGINE_COMMIT}, ${ENGINE_COMMIT_DATE})"

# Files where upstream ships a genuinely wrong FX/RATE key order (not an
# ORE-Studio-specific convention issue -- see the header comment above),
# and the exact sed fix that PR #1423 confirmed safe for each. Applied
# after every examples/ sync so a fresh rsync from upstream never
# silently re-introduces the bug.
KNOWN_PATCH_FILES=(
    "external/ore/examples/XvaRisk/Input/market_20160205_eonia_200bp_up.txt"
    "external/ore/examples/XvaRisk/Input/market_20160205_eur6m_200bp_up.txt"
    "external/ore/examples/ORE-Python/Notebooks/Example_7/Input/market.txt"
)

reapply_known_patches() {
    echo ""
    echo "--- Reapplying known hand-patches (see this script's header comment) ---"
    local f path
    for f in "${KNOWN_PATCH_FILES[@]}"; do
        path="${REPO_ROOT}/${f}"
        if [[ ! -f "${path}" ]]; then
            echo "  WARNING: ${f} not found -- skipped (did it move upstream?)" >&2
            continue
        fi
        # Value is never touched, only the two currency codes in the key
        # swap places -- matches PR #1423 exactly.
        sed -i \
            -e 's|FX/RATE/USD/GBP \([0-9.]*\)|FX/RATE/GBP/USD \1|' \
            -e 's|FX/RATE/USD/CHF \([0-9.]*\)|FX/RATE/CHF/USD \1|' \
            "${path}"
        echo "  patched: ${f}"
    done
}

# sync_one <label> <src-subdir> <dest-subdir> <description> <exclude-array-name...>
sync_one() {
    local label="$1" src_sub="$2" dest_sub="$3" description="$4"; shift 4
    local exclude_names=("$@")
    local src="${SRC_DIR}/${src_sub}"
    local dest="${REPO_ROOT}/${dest_sub}"

    if [[ ! -d "${src}" ]]; then
        echo "ERROR: ${src} not found -- is <ore-engine-source-dir> correct?" >&2
        exit 1
    fi

    echo ""
    echo "=== ${label}: ${src} -> ${dest} ==="
    local rsync_args=(-a --stats)
    [[ "${DRY_RUN}" -eq 1 ]] && rsync_args+=(--dry-run)
    [[ "${DO_DELETE}" -eq 1 ]] && rsync_args+=(--delete)
    for f in "${exclude_names[@]}"; do
        rsync_args+=(--exclude "/${f}")
    done
    # This directory's own docs (written once by hand, not sourced from
    # upstream) -- MUST also be excluded from --delete, not just from the
    # manifest's own file-count/hash below, or the very first re-sync
    # after adding one of these silently deletes it (found the hard way:
    # README.md/methodology.txt vanished on this script's own second run).
    rsync_args+=(--exclude "/manifest.json" --exclude "/README.md" --exclude "/methodology.txt")

    mkdir -p "${dest}"
    rsync "${rsync_args[@]}" "${src}/" "${dest}/"

    if [[ "${DRY_RUN}" -eq 1 ]]; then
        echo "--dry-run given; ${dest}/manifest.json not regenerated."
        return
    fi

    if [[ "${label}" == "Examples" ]]; then
        reapply_known_patches
    fi

    local file_count top_level_json tree_hash
    file_count="$(find "${dest}" -type f -not -name "manifest.json" -not -name "README.md" -not -name "methodology.txt" | wc -l)"
    top_level_json="$(find "${dest}" -mindepth 1 -maxdepth 1 \
        -not -name "manifest.json" -not -name "README.md" -not -name "methodology.txt" \
        -printf '%f\n' \
        | sort | python3 -c '
import json, sys
print(json.dumps(sorted(l.strip() for l in sys.stdin)))
')"
    # Tree hash: sha256 of every file's own sha256 + relative path, sorted
    # by path -- reproducible from a re-sync of the exact same source
    # commit, independent of mtimes/permissions, cheap enough to be a
    # single line rather than one entry per file.
    tree_hash="$(find "${dest}" -type f -not -name "manifest.json" -not -name "README.md" -not -name "methodology.txt" -print0 \
        | sort -z \
        | xargs -0 sha256sum \
        | sed "s|${dest}/||" \
        | sha256sum | cut -d' ' -f1)"

    cat > "${dest}/manifest.json" <<EOF
{
  "description": "${description}",
  "source_repo": "git@github.com:OreStudio/Engine.git (fork of git@github.com:OpenSourceRisk/Engine.git)",
  "source_path": "${src_sub}/",
  "engine_version": "${ENGINE_VERSION}",
  "engine_commit": "${ENGINE_COMMIT}",
  "engine_commit_date": "${ENGINE_COMMIT_DATE}",
  "synced_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "synced_by": "external/ore/tools/update_ore_samples.sh",
  "file_count": ${file_count},
  "top_level_entries": ${top_level_json},
  "tree_sha256": "${tree_hash}",
  "tree_sha256_method": "sha256 of the sorted 'sha256sum  relative/path' lines for every vendored file (manifest.json/README.md/methodology.txt themselves excluded) -- reproducible by re-running this script against the same engine_commit and diffing the regenerated manifest.json's own tree_sha256."
}
EOF
    echo "Wrote ${dest}/manifest.json (${file_count} files, tree_sha256=${tree_hash:0:12}...)"
}

sync_one "Examples" "Examples" "external/ore/examples" \
    "Provenance for external/ore/examples/ -- a sync of the ORE Engine fork's own Examples/ directory (minus its pytest test-harness files, see external/ore/tools/update_ore_samples.sh's header comment for the exclude list), with exactly 3 known upstream-bug hand-patches reapplied automatically after every sync (KNOWN_PATCH_FILES in that same script -- upstream ships a genuinely wrong FX/RATE key order in these files; see the script's header comment and doc/agile/product_backlog/deferred/document_ore_vendor_hand_patches.org for why). No other manual edits anywhere in this tree." \
    "${EXCLUDE_EXAMPLES_FILES[@]}"

sync_one "XSD schemas" "xsd" "external/ore/xsd" \
    "Provenance for external/ore/xsd/ -- a verbatim sync of the ORE Engine fork's own xsd/ directory, kept in lockstep with external/ore/examples/ (same engine_commit) since a schema/example version mismatch produces spurious validation failures -- see scripts/validate_ore_examples.sh." \
    "${EXCLUDE_XSD_FILES[@]}"

if [[ "${DRY_RUN}" -eq 1 ]]; then
    exit 0
fi

if [[ "${SKIP_VALIDATE}" -eq 0 ]]; then
    echo ""
    echo "--- Validating synced examples against synced xsd ---"
    "${REPO_ROOT}/scripts/validate_ore_examples.sh"
else
    echo ""
    echo "--skip-validate given; not running scripts/validate_ore_examples.sh."
fi

echo ""
echo "Remember to also rebuild+reinstall external/ore/packages/'s engine"
echo "package (external/ore/tools/package_ore.sh) if this sync also moved"
echo "engine_version forward -- packages/, examples/, and xsd/ should all"
echo "stay in step at the same engine_commit."
