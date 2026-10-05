#!/usr/bin/env python3
"""Check that every open read is on the allow-list of Authorised reads.

A read checks its resource's read code unless the security documentation
allow-lists its subject, with the reason it is open. This check holds the
generated surface to that rule, so a read cannot become open by accident:

1. A generated read handler that checks no permission must serve a subject
   the allow-list names.
2. A generated history registrar must register its entity's read code, since
   the history subject is never on the allow-list.
3. Every subject the allow-list names must exist in a protocol header, so a
   stale row fails rather than allowing a subject nobody serves.

The hand-written handlers are not read here: they decide their own checks,
and the allow-list is where an open one is recorded and reviewed.

Run::

    python3 projects/ores.codegen/scripts/check_open_reads.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
ALLOW_LIST_DOC = REPO_ROOT / "doc" / "knowledge" / "security" / "authorised-reads.org"

GENERATED_HANDLER = "Template: cpp_nats_handler.hpp.mustache"
GENERATED_HISTORY = "Template: cpp_history_provider_registrar.cpp.mustache"
READ_PREFIXES = ("list_", "get_")
METHOD_RE = re.compile(r"^    void (\w+)\(ores::nats::message msg\) \{$", re.M)
SERVES_RE = re.compile(r"@brief Serves ([a-z0-9_.-]+)\.")
HISTORY_CODE_RE = re.compile(r'register_history_provider\(\s*"[^"]+",\s*"([^"]*)",')
SUBJECT_RE = re.compile(r'nats_subject\s*=\s*"([a-z0-9_.-]+)"')
ROW_RE = re.compile(r"^\| =([a-z0-9_.-]+)= \|", re.M)


def allow_list(doc: str) -> set[str]:
    """The subjects the allow-list section of Authorised reads names."""
    start = doc.index("** The allow-list")
    end = doc.index("\n** ", start + 1)
    return set(ROW_RE.findall(doc[start:end]))


def open_reads(handler: str) -> list[tuple[str, str]]:
    """``(method, subject)`` for each read method that checks no permission."""
    found = []
    starts = [m for m in METHOD_RE.finditer(handler)]
    for i, m in enumerate(starts):
        name = m.group(1)
        if not name.startswith(READ_PREFIXES):
            continue
        end = starts[i + 1].start() if i + 1 < len(starts) else len(handler)
        if "has_permission(" in handler[m.end():end]:
            continue
        # The subject is read from the comment directly above the method; a
        # method with no comment of its own is reported by its name alone.
        before = handler[: m.start()].rstrip()
        comment = before[before.rfind("/**"):] if before.endswith("*/") else ""
        serves = SERVES_RE.findall(comment)
        found.append((name, serves[0] if serves else "an unnamed subject"))
    return found


def main() -> int:
    allowed = allow_list(ALLOW_LIST_DOC.read_text(encoding="utf-8"))
    failures = []

    for path in sorted(REPO_ROOT.glob("projects/ores.*/core/include/*/messaging/*_handler.hpp")):
        text = path.read_text(encoding="utf-8")
        if GENERATED_HANDLER not in text:
            continue
        for method, subject in open_reads(text):
            if subject not in allowed:
                failures.append(f"{path.relative_to(REPO_ROOT)}: {method} serves {subject} "
                                "with no permission check, and Authorised reads does not "
                                "allow-list it")

    histories = 0
    for path in sorted(REPO_ROOT.glob("projects/ores.*/core/src/messaging/*_history_provider_registrar.cpp")):
        text = path.read_text(encoding="utf-8")
        if GENERATED_HISTORY not in text:
            continue
        for code in HISTORY_CODE_RE.findall(text):
            histories += 1
            if not code:
                failures.append(f"{path.relative_to(REPO_ROOT)}: registers a history with no "
                                "read code")

    subjects = set()
    for path in REPO_ROOT.glob("projects/ores.*/api/include/*/messaging/*_protocol.hpp"):
        subjects |= set(SUBJECT_RE.findall(path.read_text(encoding="utf-8")))
    for subject in sorted(allowed - subjects):
        failures.append(f"{ALLOW_LIST_DOC.relative_to(REPO_ROOT)}: allow-lists {subject}, "
                        "which no protocol header defines")

    if failures:
        print("Open reads that are not allowed:")
        for failure in failures:
            print(f"  {failure}")
        return 1
    print(f"open reads are allow-listed ({len(allowed)} subject(s) allowed, "
          f"{histories} history provider(s) guarded).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
