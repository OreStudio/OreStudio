"""Move every call site from Boost.Log's macro to the ores logger handle.

The sweep is idempotent: it rewrites the macro, the header a call site includes,
and the two backend includes that no longer need to be there, and it applies the
same rewrite to the codegen templates and the model archetypes so a regeneration
does not bring the old spelling back.
"""
import pathlib
import re

SUFFIXES = (".hpp", ".cpp", ".h", ".ipp", ".org", ".mustache")
# These name the backend on purpose: the logging library's own implementation.
KEEP = {
    "projects/ores.logging/src/logger.cpp",
    "projects/ores.logging/src/boost_severity.cpp",
    "projects/ores.logging/include/ores.logging/boost_severity.hpp",
}
DETAIL = (
    "projects/ores.logging/include/ores.logging/lifecycle_manager.hpp",
    "projects/ores.logging/src/lifecycle_manager.cpp",
    "projects/ores.telemetry/core/src/log/telemetry_sink_backend.cpp",
    "projects/ores.telemetry/core/src/log/database_sink_backend.cpp",
    "projects/ores.telemetry/database/tests/database_sink_backend_tests.cpp",
    "projects/ores.telemetry/core/tests/boost_severity_tests.cpp",
)

stats = {"macro": 0, "include": 0, "severity_feature": 0, "boost_severity": 0, "files": 0}

for p in sorted(pathlib.Path("projects").rglob("*")):
    if p.suffix not in SUFFIXES or not p.is_file():
        continue
    rel = p.as_posix()
    if "build/output" in rel or rel in KEEP:
        continue
    text = p.read_text(errors="ignore")
    original = text

    count = text.count("BOOST_LOG_SEV(")
    text = text.replace("BOOST_LOG_SEV(", "ORES_LOG_SEV(")
    stats["macro"] += count

    rewritten = text.replace("ores.logging/make_logger.hpp", "ores.logging/logger.hpp")
    if rewritten != text:
        text = rewritten
        stats["include"] += 1

    lines = []
    for line in text.splitlines(keepends=True):
        if line.lstrip().startswith("#include"):
            if "boost/log/sources/severity_feature.hpp" in line:
                stats["severity_feature"] += 1
                continue
            if ('"ores.logging/boost_severity.hpp"' in line
                    and not re.search(r"\bboost_severity\b",
                                      "".join(l for l in text.splitlines(keepends=True) if l is not line)
                                      .replace('#include "ores.logging/boost_severity.hpp"', ""))):
                stats["boost_severity"] += 1
                continue
        lines.append(line)
    text = "".join(lines)

    if text != original:
        p.write_text(text)
        stats["files"] += 1

for rel in DETAIL:
    p = pathlib.Path(rel)
    if not p.exists():
        continue
    text = p.read_text()
    text = text.replace("ores::logging::boost_severity", "ores::logging::detail::boost_severity")
    text = text.replace("logging::boost_severity", "logging::detail::boost_severity")
    text = text.replace(" boost_severity ", " detail::boost_severity ")
    text = text.replace("(boost_severity ", "(detail::boost_severity ")
    text = text.replace("boost_severity::", "detail::boost_severity::")
    text = text.replace("detail::detail::", "detail::")
    p.write_text(text)

print(stats)
