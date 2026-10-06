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

A method is a read when it decodes a request a protocol defines, whatever its
name; the name is only a fallback for a method whose body does not decode. A
model may inject a hand-written method under any name, and a name is not a
permission.

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
ALLOW_LIST_HEADING = "** The allow-list"
METHOD_RE = re.compile(r"^    void (\w+)\(ores::nats::message msg\) \{$", re.M)
STRUCT_RE = re.compile(r"struct (\w+_request) \{")
DECODE_RE = re.compile(r"decode<(\w+_request)>")
NATS_SUBJECT_RE = re.compile(r'nats_subject\s*=\s*"([a-z0-9_.-]+)"')
SERVES_RE = re.compile(r"@brief Serves ([a-z0-9_.-]+)\.")
HISTORY_CALL_RE = re.compile(r"register_history_provider\s*\(")
HISTORY_CODE_RE = re.compile(r'register_history_provider\(\s*"[^"]+",\s*"([^"]*)",')
ROW_RE = re.compile(r"^\| =([a-z0-9_.-]+)= \|", re.M)


class MissingAllowList(Exception):
    """Authorised reads has no allow-list section to read."""


def allow_list(doc: str) -> set[str]:
    """The subjects the allow-list section of Authorised reads names."""
    heading = re.search(rf"^{re.escape(ALLOW_LIST_HEADING)}\s*$", doc, re.M)
    if not heading:
        raise MissingAllowList(
            f"{ALLOW_LIST_DOC.relative_to(REPO_ROOT)} has no "
            f"'{ALLOW_LIST_HEADING}' section to read")
    start = heading.start()
    following = re.search(r"^\*\* ", doc[heading.end():], re.M)
    end = heading.end() + following.start() if following else len(doc)
    return set(ROW_RE.findall(doc[start:end]))


def protocol_subjects(text: str) -> dict[str, str]:
    """The subject each request struct in a protocol header addresses."""
    found = {}
    current = None
    for line in text.splitlines():
        struct = STRUCT_RE.match(line)
        if struct:
            current = struct.group(1)
            continue
        subject = NATS_SUBJECT_RE.search(line)
        if subject and current:
            found[current] = subject.group(1)
            current = None
    return found


def read_subjects_by_request() -> dict[str, str]:
    """Every protocol's request struct mapped to the subject it addresses."""
    mapping = {}
    for path in REPO_ROOT.glob("projects/ores.*/api/include/*/messaging/*_protocol.hpp"):
        mapping.update(protocol_subjects(path.read_text(encoding="utf-8")))
    return mapping


def open_reads(handler: str,
               subjects_by_request: dict[str, str] | None = None) -> list[tuple[str, str]]:
    """``(method, subject)`` for each read method that checks no permission."""
    if subjects_by_request is None:
        subjects_by_request = read_subjects_by_request()
    found = []
    starts = [m for m in METHOD_RE.finditer(handler)]
    for i, m in enumerate(starts):
        name = m.group(1)
        end = starts[i + 1].start() if i + 1 < len(starts) else len(handler)
        body = handler[m.end():end]
        decoded = sorted({subjects_by_request[d] for d in DECODE_RE.findall(body)
                          if d in subjects_by_request})
        if not name.startswith(READ_PREFIXES) and not decoded:
            continue
        if "has_permission(" in body:
            continue
        # The subject is read from the comment directly above the method; an
        # injected method has no such comment, so its decoded request names it.
        before = handler[: m.start()].rstrip()
        comment = before[before.rfind("/**"):] if before.endswith("*/") else ""
        serves = SERVES_RE.findall(comment)
        if serves:
            found.append((name, serves[0]))
        elif decoded:
            found.extend((name, subject) for subject in decoded)
        else:
            found.append((name, "an unnamed subject"))
    return found


def main() -> int:
    failures = []
    try:
        allowed = allow_list(ALLOW_LIST_DOC.read_text(encoding="utf-8"))
    except MissingAllowList as exc:
        print("Open reads that are not allowed:")
        print(f"  {exc}")
        return 1
    subjects_by_request = read_subjects_by_request()

    for path in sorted(REPO_ROOT.glob("projects/ores.*/core/include/*/messaging/*_handler.hpp")):
        text = path.read_text(encoding="utf-8")
        if GENERATED_HANDLER not in text:
            continue
        for method, subject in open_reads(text, subjects_by_request):
            if subject not in allowed:
                failures.append(f"{path.relative_to(REPO_ROOT)}: {method} serves {subject} "
                                "with no permission check, and Authorised reads does not "
                                "allow-list it")

    histories = 0
    for path in sorted(REPO_ROOT.glob("projects/ores.*/core/src/messaging/*_history_provider_registrar.cpp")):
        text = path.read_text(encoding="utf-8")
        if GENERATED_HISTORY not in text:
            continue
        calls = len(HISTORY_CALL_RE.findall(text))
        codes = HISTORY_CODE_RE.findall(text)
        histories += len(codes)
        if len(codes) != calls:
            failures.append(f"{path.relative_to(REPO_ROOT)}: registers {calls} history "
                            f"provider(s) but {len(codes)} name a literal read code")
        for code in codes:
            if not code:
                failures.append(f"{path.relative_to(REPO_ROOT)}: registers a history with no "
                                "read code")

    subjects = set()
    for path in REPO_ROOT.glob("projects/ores.*/api/include/*/messaging/*_protocol.hpp"):
        subjects |= set(NATS_SUBJECT_RE.findall(path.read_text(encoding="utf-8")))
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
