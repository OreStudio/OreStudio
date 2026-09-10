#!/usr/bin/env python3
"""Make every structure note reachable from the pages it orders.

A structure note is a hub: it names a cluster and puts its pages in an
argued order. Reachability has to work both ways. A reader who finds the
hub gets the order; a reader who lands on one page by search needs to be
told the order exists, or the note only helps people who already knew
about it.

The hubs are the source of truth: whatever a hub links to, it orders. This
script reads each hub's outbound links and adds a back-link to the hub in
each ordered page's See also section, skipping pages that already carry
one.

Usage: python3 build/scripts/link_structure_notes.py [--check]
  --check  exit non-zero if an ordered page lacks its back-link.
"""
import argparse
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SEARCH = [ROOT / "doc", ROOT / "projects" / "modeling"]
TAG = "structure_note"

LINK = re.compile(r"\[\[id:([0-9A-Fa-f-]{36})\]\[([^\]]*)\]\]")


def org_files():
    for base in SEARCH:
        for p in base.rglob("*.org"):
            yield p


def frontmatter(text, field):
    m = re.search(rf"^#\+{field}:\s*(.+?)\s*$", text, re.M)
    return m.group(1) if m else ""


def doc_id(text):
    m = re.search(r"^:ID:\s*(\S+)\s*$", text, re.M)
    return m.group(1).upper() if m else None


def load():
    docs = {}
    for p in org_files():
        try:
            text = p.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        did = doc_id(text)
        if did:
            docs[did] = (p, text)
    return docs


def hubs(docs):
    return {did: (p, t) for did, (p, t) in docs.items()
            if TAG in frontmatter(t, r"filetags")}


# A hub's later sections point outwards: at the neighbouring cluster it
# excludes, and at the index and method pages every hub cites. Only the
# ordering sections name cluster members, so the scan stops at the first
# of these headings.
STOP_HEADINGS = ("* What this cluster does not cover", "* See also")


def ordering_section(hub_text):
    """The part of a hub that names its cluster: everything before the
    sections that deliberately point outside it."""
    end = len(hub_text)
    for heading in STOP_HEADINGS:
        i = hub_text.find("\n" + heading)
        if i != -1:
            end = min(end, i)
    return hub_text[:end]


def ordered_by(hub_text, docs, hub_id):
    """The cluster pages a hub orders, read from its ordering sections."""
    out = []
    for m in LINK.finditer(ordering_section(hub_text)):
        target = m.group(1).upper()
        if target == hub_id or target not in docs:
            continue
        if target not in out:
            out.append(target)
    return out


def add_backlink(path, text, hub_id, hub_title):
    if f"id:{hub_id}" in text or f"id:{hub_id.lower()}" in text:
        return None
    line = (f"- [[id:{hub_id}][{hub_title}]] — the structure note that "
            "orders this cluster, and where to read this page in it.")
    m = re.search(r"^\* See also\s*$", text, re.M)
    if m:
        insert = m.end()
        return text[:insert] + "\n\n" + line + text[insert:].rstrip("\n") + "\n" \
            if text[insert:].strip() == "" else \
            text[:insert] + "\n\n" + line + text[insert:]
    return text.rstrip("\n") + "\n\n* See also\n\n" + line + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="exit non-zero if a back-link is missing")
    args = ap.parse_args()

    docs = load()
    found = hubs(docs)
    if not found:
        print(f"❌ no document carries the {TAG} tag", file=sys.stderr)
        return 1

    missing = []
    for hub_id, (hub_path, hub_text) in sorted(found.items()):
        title = frontmatter(hub_text, "title")
        for target in ordered_by(hub_text, docs, hub_id):
            path, text = docs[target]
            if target in found:
                continue
            updated = add_backlink(path, text, hub_id, title)
            if updated is None:
                continue
            missing.append(path.relative_to(ROOT))
            if not args.check:
                path.write_text(updated, encoding="utf-8")
                docs[target] = (path, updated)

    if args.check:
        if missing:
            for m in missing:
                print(f"❌ {m} does not link its structure note", file=sys.stderr)
            print("   run: python3 build/scripts/link_structure_notes.py",
                  file=sys.stderr)
            return 1
        print(f"✅ every page ordered by a structure note links back to it "
              f"({len(found)} notes).")
        return 0

    print(f"✅ added {len(missing)} back-link(s) across {len(found)} "
          "structure note(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
