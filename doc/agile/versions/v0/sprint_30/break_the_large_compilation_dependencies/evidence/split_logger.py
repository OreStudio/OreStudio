"""Move every call site from Boost.Log's macro to the ores logger handle.

Run it on any tree: it is idempotent, and it rewrites only what belongs to the
handle.

  * ``BOOST_LOG_SEV(`` becomes ``ORES_LOG_SEV(``.
  * ``ores.logging/make_logger.hpp`` becomes ``ores.logging/logger.hpp``.
  * ``<boost/log/sources/severity_feature.hpp>`` and
    ``ores.logging/boost_severity.hpp`` leave the files that no longer need
    them; a file that still names the backend severity keeps its include.

The same rewrite goes to the codegen templates and the model archetypes, so a
regeneration does not bring the old spelling back, and to the files that name
Boost.Log's severity directly, which take it from ``ores::logging::backend``.

What it must not touch, and why:

  * ``ores.logging/src/logger.cpp`` is the backend: the one file in the tree
    that calls Boost's macro on the Boost logger, on purpose.
  * ``ores.logging/include/ores.logging/boost_severity.hpp`` and
    ``ores.logging/src/boost_severity.cpp`` are where the backend severity type
    and its conversions live, so rewriting their includes would delete the type.
  * Anything under ``build/``, ``vcpkg/`` or ``.git/`` is generated or vendored.

Usage::

    python3 split_logger.py            # rewrite the tree
    python3 split_logger.py --check    # report stragglers, change nothing
    python3 split_logger.py --dry-run  # list the files it would rewrite

Exit codes: 0 after a rewrite or when nothing is left to do, 1 in ``--check``
mode when a file still carries the old spelling.
"""
import argparse
import pathlib
import re
import sys

SUFFIXES = (".hpp", ".cpp", ".h", ".ipp", ".org", ".mustache")

# The logging library's own implementation: the backend type, and the one call
# of Boost's macro that the handle exists to hide.
KEEP = {
    "projects/ores.logging/src/logger.cpp",
    "projects/ores.logging/src/boost_severity.cpp",
    "projects/ores.logging/include/ores.logging/boost_severity.hpp",
}

# These name the backend severity on purpose, and take it from `backend`.
BACKEND = (
    "projects/ores.logging/include/ores.logging/lifecycle_manager.hpp",
    "projects/ores.logging/src/lifecycle_manager.cpp",
    "projects/ores.logging/src/logging_configuration.cpp",
    "projects/ores.logging/src/logging_options_validator.cpp",
    "projects/ores.logging/tests/boost_severity_tests.cpp",
    "projects/ores.telemetry/core/src/log/telemetry_sink_backend.cpp",
    "projects/ores.telemetry/core/src/log/database_sink_backend.cpp",
    "projects/ores.telemetry/database/tests/database_sink_backend_tests.cpp",
    "projects/ores.telemetry/core/tests/boost_severity_tests.cpp",
)

SKIP = ("build/", "vcpkg/", ".git/")


def skipped(rel: str) -> bool:
    return rel in KEEP or any(part in rel for part in SKIP)


def sweep(text: str) -> str:
    text = text.replace("BOOST_LOG_SEV(", "ORES_LOG_SEV(")
    text = text.replace("ores.logging/make_logger.hpp", "ores.logging/logger.hpp")

    body = text
    kept = []
    for line in text.splitlines(keepends=True):
        if line.lstrip().startswith("#include"):
            if "boost/log/sources/severity_feature.hpp" in line:
                continue
            if '"ores.logging/boost_severity.hpp"' in line:
                rest = body.replace(line, "")
                if not re.search(r"\bboost_severity\b", rest):
                    continue
        kept.append(line)
    return "".join(kept)


def qualify(text: str) -> str:
    """Name the backend severity where the file has no declaration of its own.

    A file that writes `using ... boost_severity;` has already decided how the
    name is spelled in that scope, and qualifies nothing.
    """
    if re.search(r"using\s+(?:[\w:]*::)?boost_severity\s*;", text):
        return text
    text = re.sub(r"(?<![\w:])boost_severity\b(?!\.hpp)", "backend::boost_severity", text)
    return text.replace("backend::backend::", "backend::")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="report only")
    parser.add_argument("--dry-run", action="store_true", help="list only")
    args = parser.parse_args()

    counts = {"macro": 0, "include": 0, "severity_feature": 0, "boost_severity": 0}
    pending = []

    for path in sorted(pathlib.Path("projects").rglob("*")):
        if path.suffix not in SUFFIXES or not path.is_file():
            continue
        rel = path.as_posix()
        if skipped(rel):
            continue
        original = path.read_text(errors="ignore")
        counts["macro"] += original.count("BOOST_LOG_SEV(")
        counts["include"] += original.count("ores.logging/make_logger.hpp")
        counts["severity_feature"] += original.count(
            "boost/log/sources/severity_feature.hpp")
        counts["boost_severity"] += original.count('"ores.logging/boost_severity.hpp"')
        text = sweep(original)
        if text != original:
            pending.append(rel)
            if not (args.check or args.dry_run):
                path.write_text(text)

    for rel in BACKEND:
        path = pathlib.Path(rel)
        if not path.is_file():
            continue
        original = path.read_text()
        text = qualify(original)
        if text != original:
            pending.append(rel)
            if not (args.check or args.dry_run):
                path.write_text(text)

    if args.dry_run or args.check:
        for rel in pending:
            print(rel)
    counts["files"] = len(pending)
    print(counts)

    if args.check and pending:
        print(f"{len(pending)} file(s) still carry the old spelling", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
