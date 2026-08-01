#!/usr/bin/env bash
#
# Package a built `ore` binary (from a checkout of the ORE Engine fork,
# git@github.com:OreStudio/Engine.git) into a self-contained tarball
# matching the format ores.compute.wrapper expects: manifest.json plus
# the executable and every shared library it needs at runtime, all under
# one flat directory, no external LD_LIBRARY_PATH setup required.
#
# The previous hand-made packages under external/ore/packages/ (e.g.
# ore-1.8.15.0-x64-linux.tar.gz) shipped the executable alone, still
# dynamically linked against libOREAnalytics.so/libQuantExt.so/
# libQuantLib.so.1 with no RPATH pointing anywhere inside the package --
# `ldd` on that binary reports them "not found". ores.compute.wrapper
# (see projects/ores.compute/wrapper/src/app/application.cpp) extracts
# the package tarball into an isolated per-node cache directory and
# spawns manifest.executable directly, with no environment
# customisation -- so a package that isn't RPATH-self-contained can
# only ever work by accident, if the exact build-tree paths still
# happen to exist on the machine running the wrapper. This script fixes
# that: it bundles every non-system shared library (glibc/libm/ld-linux
# excluded -- those must always come from the host, bundling them risks
# an ABI mismatch) into a lib/ subdirectory and rewrites both the
# executable's and every bundled library's RPATH to $ORIGIN(/lib), so
# the package is genuinely portable: extract it anywhere and run it,
# nothing else required.
#
# Usage:
#   ./external/ore/tools/package_ore.sh <ore-build-dir> [options]
#
# Arguments:
#   <ore-build-dir>   Path to the ORE Engine CMake build directory
#                      containing App/ore (e.g.
#                      /home/user/Development/ORE/Engine/build/linux-clang-ninja-release).
#
# Options:
#   --version VERSION    Package version (e.g. 1.8.16.0). Defaults to
#                        `git describe --tags --always` run against the
#                        build dir's own source tree (read from
#                        CMakeCache.txt's CMAKE_HOME_DIRECTORY), with any
#                        leading 'v' stripped.
#   --out-dir DIR        Where to write the final tarball. Defaults to
#                        external/ore/packages/ under this repo's root.
#   --sample PATH        A sample to smoke-test the freshly built package
#                        against: either a directory containing Input/ore.xml,
#                        or a .tar.gz of one (auto-extracted). Defaults to
#                        external/ore/packages/TA002_IR_Swap.tar.gz.
#   --expected-output DIR  A reference ExpectedOutput/ directory (CSV files
#                        by the same name as the sample run's own Output/
#                        produces) to tolerantly diff the run against --
#                        see compare_csv.py's own doc for why this isn't a
#                        byte-exact diff. Defaults to
#                        external/ore/examples/Academy/TA002_IR_Swap/ExpectedOutput
#                        when --sample was left at its own default;
#                        otherwise the self-test still runs the sample and
#                        checks Output/ was produced, but skips diffing
#                        unless this is given explicitly.
#   --rtol FLOAT         Relative tolerance for compare_csv.py's numeric
#                        column comparisons. Default 1e-6.
#   --skip-sample-test    Don't smoke-test the package at all (not
#                        recommended -- see scripts/package_ore.sh's own
#                        commit message for why the previous hand-made
#                        packages were broken and went undetected).
#   --no-install          Build and self-test the tarball but don't copy
#                        it into --out-dir (useful to just inspect it in
#                        --keep-staging).
#   --keep-staging         Don't delete the staging/self-test scratch
#                        directory on exit; print its path instead.
#
# Requires: patchelf, ldd, tar, git (for default version detection),
# python3 (for compare_csv.py).
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

# glibc/loader/libgcc pieces that must always come from the host -- never
# bundled, since ABI/symbol-versioning compatibility across distros is
# glibc's own contract, not something a bundled copy can safely override.
EXCLUDE_LIB_RE='^(linux-vdso\.so|ld-linux|libc\.so|libm\.so|libpthread\.so|libdl\.so|librt\.so)'

VERSION=""
OUT_DIR="${REPO_ROOT}/external/ore/packages"
SAMPLE_ARG=""
SAMPLE_ARG_GIVEN=0
EXPECTED_DIR=""
EXPECTED_DIR_GIVEN=0
RTOL="1e-6"
SKIP_SAMPLE_TEST=0
DO_INSTALL=1
KEEP_STAGING=0
BUILD_DIR=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --version) VERSION="$2"; shift 2 ;;
        --out-dir) OUT_DIR="$2"; shift 2 ;;
        --sample) SAMPLE_ARG="$2"; SAMPLE_ARG_GIVEN=1; shift 2 ;;
        --expected-output) EXPECTED_DIR="$2"; EXPECTED_DIR_GIVEN=1; shift 2 ;;
        --rtol) RTOL="$2"; shift 2 ;;
        --skip-sample-test) SKIP_SAMPLE_TEST=1; shift ;;
        --no-install) DO_INSTALL=0; shift ;;
        --keep-staging) KEEP_STAGING=1; shift ;;
        -h|--help) grep '^#' "${BASH_SOURCE[0]}" | sed 's/^#//'; exit 0 ;;
        *)
            if [[ -z "${BUILD_DIR}" ]]; then BUILD_DIR="$1"; shift
            else echo "ERROR: unexpected argument: $1" >&2; exit 1
            fi
            ;;
    esac
done

DEFAULT_SAMPLE_TARBALL="${REPO_ROOT}/external/ore/packages/TA002_IR_Swap.tar.gz"
DEFAULT_EXPECTED_DIR="${REPO_ROOT}/external/ore/examples/Academy/TA002_IR_Swap/ExpectedOutput"
if [[ "${SAMPLE_ARG_GIVEN}" -eq 0 ]]; then
    SAMPLE_ARG="${DEFAULT_SAMPLE_TARBALL}"
    if [[ "${EXPECTED_DIR_GIVEN}" -eq 0 ]]; then
        EXPECTED_DIR="${DEFAULT_EXPECTED_DIR}"
    fi
fi

if [[ -z "${BUILD_DIR}" ]]; then
    echo "ERROR: <ore-build-dir> is required. See --help." >&2
    exit 1
fi
BUILD_DIR="$(cd "${BUILD_DIR}" && pwd)"

for tool in patchelf ldd tar; do
    if ! command -v "${tool}" >/dev/null 2>&1; then
        echo "ERROR: '${tool}' is not installed." >&2
        exit 1
    fi
done

ORE_EXE="${BUILD_DIR}/App/ore"
if [[ ! -x "${ORE_EXE}" ]]; then
    echo "ERROR: ore executable not found or not executable: ${ORE_EXE}" >&2
    exit 1
fi

if [[ -z "${VERSION}" ]]; then
    cache="${BUILD_DIR}/CMakeCache.txt"
    if [[ ! -f "${cache}" ]]; then
        echo "ERROR: --version not given and ${cache} not found to derive it from." >&2
        exit 1
    fi
    SRC_DIR="$(sed -n 's/^CMAKE_HOME_DIRECTORY:INTERNAL=//p' "${cache}")"
    if [[ -z "${SRC_DIR}" || ! -d "${SRC_DIR}/.git" ]]; then
        echo "ERROR: could not determine ORE source tree from ${cache}; pass --version explicitly." >&2
        exit 1
    fi
    VERSION="$(git -C "${SRC_DIR}" describe --tags --always)"
    VERSION="${VERSION#v}"
    ORE_COMMIT="$(git -C "${SRC_DIR}" rev-parse HEAD)"
    ORE_COMMIT_DATE="$(git -C "${SRC_DIR}" log -1 --format=%cI)"
else
    ORE_COMMIT=""
    ORE_COMMIT_DATE=""
fi

PKG_NAME="ore-${VERSION}-x64-linux"
echo "Package     : ${PKG_NAME}"
echo "ore binary  : ${ORE_EXE}"
if [[ -n "${ORE_COMMIT}" ]]; then
    echo "ORE commit  : ${ORE_COMMIT} (${ORE_COMMIT_DATE})"
fi

STAGE="$(mktemp -d)"
cleanup() {
    if [[ "${KEEP_STAGING}" -eq 1 ]]; then
        echo "Staging dir kept: ${STAGE}"
    else
        rm -rf "${STAGE}"
    fi
}
trap cleanup EXIT

PKG_DIR="${STAGE}/${PKG_NAME}"
mkdir -p "${PKG_DIR}/lib"

echo ""
echo "--- Resolving shared library closure ---"
cp "${ORE_EXE}" "${PKG_DIR}/${PKG_NAME}"
chmod +x "${PKG_DIR}/${PKG_NAME}"

# ldd's own output is the dependency closure already (transitively
# resolved) -- no need to walk it ourselves. Second column onward after
# "=>" is the resolved path; skip lines with no resolved path (vdso) or
# whose library name matches the glibc/loader exclude list.
declare -A seen
while IFS= read -r line; do
    libname="$(awk '{print $1}' <<<"${line}")"
    libpath="$(awk '{print $3}' <<<"${line}")"
    [[ "${libname}" =~ ${EXCLUDE_LIB_RE} ]] && continue
    [[ -z "${libpath}" || "${libpath}" == "not" ]] && continue
    [[ -n "${seen[${libname}]:-}" ]] && continue
    seen[${libname}]=1
    echo "  bundling ${libname} <- ${libpath}"
    cp -L "${libpath}" "${PKG_DIR}/lib/${libname}"
done < <(ldd "${ORE_EXE}")

missing=$(ldd "${ORE_EXE}" | grep "not found" || true)
if [[ -n "${missing}" ]]; then
    echo "ERROR: ore binary has unresolved dependencies in the build environment itself:" >&2
    echo "${missing}" >&2
    exit 1
fi

echo ""
echo "--- Rewriting RPATH for self-containment ---"
patchelf --set-rpath '$ORIGIN/lib' "${PKG_DIR}/${PKG_NAME}"
for lib in "${PKG_DIR}"/lib/*; do
    patchelf --set-rpath '$ORIGIN' "${lib}"
done

cat > "${PKG_DIR}/manifest.json" <<EOF
{
  "executable": "${PKG_NAME}",
  "args": [
    "Input/ore.xml"
  ]
}
EOF

TARBALL="${STAGE}/${PKG_NAME}.tar.gz"
# Flat layout, no wrapping directory: ores.compute.wrapper's
# read_manifest() looks for manifest.json directly at the extraction
# root (see projects/ores.compute/wrapper/src/app/application.cpp),
# and manifest.json's own "executable" field is a bare filename with no
# directory prefix -- packing "${PKG_NAME}" itself (the previous
# behaviour) archives everything one level too deep and the wrapper
# can never find manifest.json.
( cd "${PKG_DIR}" && tar czf "${TARBALL}" . )
echo ""
echo "Built: ${TARBALL} ($(du -h "${TARBALL}" | cut -f1))"

echo ""
echo "--- Verifying package is self-contained (no build-tree paths) ---"
ldd_out="$(ldd "${PKG_DIR}/${PKG_NAME}")"
if grep -q "not found" <<<"${ldd_out}"; then
    echo "ERROR: packaged binary still has unresolved dependencies:" >&2
    echo "${ldd_out}" >&2
    exit 1
fi
if grep -qF "${BUILD_DIR}" <<<"${ldd_out}"; then
    echo "ERROR: packaged binary still resolves a dependency back into the build tree:" >&2
    echo "${ldd_out}" | grep -F "${BUILD_DIR}" >&2
    exit 1
fi
echo "OK: every dependency resolves inside the package, none point at the build tree."

if [[ "${SKIP_SAMPLE_TEST}" -eq 0 ]]; then
    if [[ ! -e "${SAMPLE_ARG}" ]]; then
        echo "ERROR: --sample ${SAMPLE_ARG} does not exist." >&2
        exit 1
    fi
    JOB_DIR="${STAGE}/job"
    mkdir -p "${JOB_DIR}"
    if [[ -d "${SAMPLE_ARG}" ]]; then
        SAMPLE_DESC="${SAMPLE_ARG}"
        cp -r "${SAMPLE_ARG}/." "${JOB_DIR}/"
    else
        SAMPLE_DESC="${SAMPLE_ARG} (extracted)"
        tar xzf "${SAMPLE_ARG}" -C "${JOB_DIR}"
    fi

    echo ""
    echo "--- Self-test: running package against sample ${SAMPLE_DESC} ---"
    if [[ ! -f "${JOB_DIR}/Input/ore.xml" ]]; then
        echo "ERROR: sample has no Input/ore.xml (checked ${JOB_DIR})." >&2
        exit 1
    fi

    # Extract the actual TARBALL just built, rather than running out of
    # PKG_DIR directly -- otherwise this "self-test" never notices a
    # packing bug in the tarball itself (exactly how the flat-vs-wrapped
    # layout bug above went undetected).
    EXTRACT_DIR="${STAGE}/extracted"
    mkdir -p "${EXTRACT_DIR}"
    tar xzf "${TARBALL}" -C "${EXTRACT_DIR}"
    if [[ ! -f "${EXTRACT_DIR}/manifest.json" ]]; then
        echo "ERROR: manifest.json not found at the root of the extracted tarball -- packing is wrapping everything in an extra directory again." >&2
        exit 1
    fi

    EXE_ABS="${EXTRACT_DIR}/${PKG_NAME}"
    if ( cd "${JOB_DIR}" && "${EXE_ABS}" Input/ore.xml >"${STAGE}/run.log" 2>&1 ); then
        echo "OK: sample run exited 0."
    else
        rc=$?
        echo "ERROR: sample run failed (exit ${rc}); last 40 lines of output:" >&2
        tail -40 "${STAGE}/run.log" >&2
        exit 1
    fi
    if [[ ! -d "${JOB_DIR}/Output" ]] || [[ -z "$(ls -A "${JOB_DIR}/Output" 2>/dev/null)" ]]; then
        echo "ERROR: sample run produced no Output/ directory." >&2
        exit 1
    fi
    echo "OK: Output/ produced ($(ls "${JOB_DIR}/Output" | wc -l) file(s))."

    if [[ -n "${EXPECTED_DIR}" ]]; then
        if [[ ! -d "${EXPECTED_DIR}" ]]; then
            echo "ERROR: --expected-output ${EXPECTED_DIR} does not exist." >&2
            exit 1
        fi
        REPORT="${STAGE}/${PKG_NAME}.diff-report.txt"
        {
            echo "ORE package diff report -- for sign-off before trusting this build"
            echo "Package         : ${PKG_NAME}"
            [[ -n "${ORE_COMMIT}" ]] && echo "ORE commit      : ${ORE_COMMIT} (${ORE_COMMIT_DATE})"
            echo "Sample          : ${SAMPLE_DESC}"
            echo "Expected output : ${EXPECTED_DIR}"
            echo "Compared        : $(date -u +%Y-%m-%dT%H:%M:%SZ)"
            echo "Tolerance       : rtol=${RTOL}"
            echo ""
        } > "${REPORT}"

        echo ""
        echo "--- Diffing Output/ against ${EXPECTED_DIR} (rtol=${RTOL}) ---"
        diff_failed=0
        for expected_file in "${EXPECTED_DIR}"/*.csv; do
            [[ -e "${expected_file}" ]] || continue
            base="$(basename "${expected_file}")"
            actual_file="${JOB_DIR}/Output/${base}"
            echo "${base}:" | tee -a "${REPORT}"
            if [[ ! -f "${actual_file}" ]]; then
                echo "  FAIL: no Output/${base} was produced." | tee -a "${REPORT}"
                diff_failed=1
            else
                set +e
                python3 "${SCRIPT_DIR}/compare_csv.py" "${actual_file}" "${expected_file}" --rtol "${RTOL}" \
                    | tee -a "${REPORT}"
                cmp_rc="${PIPESTATUS[0]}"
                set -e
                [[ "${cmp_rc}" -ne 0 ]] && diff_failed=1
            fi
            echo "" | tee -a "${REPORT}"
        done

        echo "Full report: ${REPORT}"
        if [[ "${diff_failed}" -eq 1 ]]; then
            echo "ERROR: one or more Output/ CSVs didn't match ${EXPECTED_DIR} within tolerance." >&2
            echo "Review ${REPORT} before deciding whether to proceed." >&2
            exit 1
        fi
        if grep -qE "NEW COLUMNS|REMOVED COLUMNS" "${REPORT}"; then
            echo "NOTE: schema differences found (new/removed columns) -- review" >&2
            echo "${REPORT} and sign off on them before trusting this package for" >&2
            echo "production use, even though all shared-column values matched." >&2
        fi
        echo "OK: every Output/*.csv with a same-named file in ${EXPECTED_DIR} matches within tolerance."
        if [[ "${DO_INSTALL}" -eq 1 ]]; then
            mkdir -p "${OUT_DIR}"
            cp "${REPORT}" "${OUT_DIR}/${PKG_NAME}.diff-report.txt"
        fi
    else
        echo ""
        echo "INFO: no --expected-output given (and no default applies to a" >&2
        echo "custom --sample) -- ran the sample and confirmed it produced" >&2
        echo "Output/, but didn't verify the numbers against a reference." >&2
    fi
else
    echo ""
    echo "WARNING: --skip-sample-test given, the package was NOT run at all." >&2
    echo "This is how the previous external/ore/packages/ore-1.8.15.0-x64-linux.tar.gz" >&2
    echo "ended up broken (missing RPATH) without anyone noticing -- avoid this flag" >&2
    echo "unless you have another reason to trust the package already." >&2
fi

if [[ "${DO_INSTALL}" -eq 1 ]]; then
    mkdir -p "${OUT_DIR}"
    cp "${TARBALL}" "${OUT_DIR}/${PKG_NAME}.tar.gz"
    echo ""
    echo "Installed: ${OUT_DIR}/${PKG_NAME}.tar.gz"
    echo ""
    echo "Remember to update the seed engine_version/package_uri in"
    echo "projects/ores.sql/populate/compute/compute_ore_app_seed.sql if"
    echo "this replaces the version currently seeded there."
else
    echo ""
    echo "--no-install given; not copying into ${OUT_DIR}."
    if [[ "${KEEP_STAGING}" -eq 1 ]]; then
        echo "Tarball left at: ${TARBALL}"
    fi
fi

exit 0
