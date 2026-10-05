#!/usr/bin/env python3
"""Check that a service architecture pattern and its uses link each other.

A pattern page owns the mechanism and names each use in its =* In ORE Studio=
section. The page that uses the pattern owns the use and names the pattern in
its =* Patterns used= section, or links it in its body. The link must go both
ways, or a reader who lands on either page misses the other.

Three rules:

1. Every page a pattern lists under =* In ORE Studio= links the pattern back.
2. Every pattern a page lists under =* Patterns used= lists that page back
   under =* In ORE Studio=.
3. Every link under =* Patterns used= names a service architecture pattern.

Point-in-time plans, external reference pages and agile records are exempt:
they record what was true, or describe something outside the platform.

Usage: python3 build/scripts/check_pattern_uses.py
Exits non-zero and names each broken link.
"""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SEARCH = [ROOT / "doc", ROOT / "projects"]
PATTERN_TYPE = "service_architecture_pattern"
EXEMPT = ("doc/plans/", "doc/knowledge/external/", "doc/agile/")

LINK = re.compile(r"\[\[id:([0-9A-Fa-f-]{36})\]\[[^\]]*\]\]")
HEADING = re.compile(r"^\* (.+?)\s*$", re.M)


def doc_id(text):
    m = re.search(r"^:ID:\s*(\S+)\s*$", text, re.M)
    return m.group(1).upper() if m else None


def doc_type(text):
    m = re.search(r"^#\+type:\s*(\S+)\s*$", text, re.M)
    return m.group(1) if m else ""


def section(text, title):
    """The body of the top-level section with this title, or empty."""
    heads = [(m.start(), m.group(1)) for m in HEADING.finditer(text)]
    for i, (start, name) in enumerate(heads):
        if name == title:
            end = heads[i + 1][0] if i + 1 < len(heads) else len(text)
            return text[start:end]
    return ""


def links(body):
    return [m.group(1).upper() for m in LINK.finditer(body)]


def load(roots=SEARCH, base=ROOT):
    docs = {}
    for root in roots:
        for p in root.rglob("*.org"):
            rel = p.relative_to(base).as_posix()
            if "/build/" in f"/{rel}" or "/vcpkg/" in f"/{rel}":
                continue
            try:
                text = p.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            did = doc_id(text)
            if did:
                docs[did] = (rel, text)
    return docs


def exempt(rel):
    return rel.startswith(EXEMPT)


def check(docs):
    """Every broken link, as one message each."""
    problems = []
    patterns = {d for d, (_, t) in docs.items() if doc_type(t) == PATTERN_TYPE}
    for pid in sorted(patterns):
        prel, ptext = docs[pid]
        for uid in links(section(ptext, "In ORE Studio")):
            if uid in patterns or uid not in docs:
                continue
            urel, utext = docs[uid]
            if exempt(urel):
                continue
            if pid not in links(utext):
                problems.append(f"{urel} is a use listed by {prel} "
                                f"but does not link the pattern back")
    for did, (rel, text) in sorted(docs.items()):
        if exempt(rel) or did in patterns:
            continue
        for pid in links(section(text, "Patterns used")):
            if pid not in patterns:
                target = docs[pid][0] if pid in docs else pid
                problems.append(f"{rel} lists {target} under Patterns used, "
                                f"which is not a service architecture pattern")
                continue
            prel, ptext = docs[pid]
            if did not in links(section(ptext, "In ORE Studio")):
                problems.append(f"{rel} uses {prel}, which does not list it "
                                f"under In ORE Studio")
    return problems


def main():
    problems = check(load())
    for p in problems:
        print(f"❌ {p}")
    if problems:
        return 1
    print("✅ every service architecture pattern and its uses link each other.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
