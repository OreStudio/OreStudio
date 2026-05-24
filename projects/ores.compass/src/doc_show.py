#!/usr/bin/env python3
"""
Show one doc: header metadata, blurb, outgoing links, incoming links.

Usage:
  doc_show.py <uuid-or-prefix>

A prefix of >= 6 hex chars is accepted as long as it resolves to exactly
one doc. Output sections:

  Title:        the doc's #+title
  Type:         the doc's #+type
  Path:         repo-relative path
  Tags:         comma-separated :filetags:
  Updated:      #+updated (if present)

  Blurb:        the paragraph between frontmatter and the first headline

  Outgoing:     every [[id:UUID]] reference in the body, resolved to
                "uuid | title (path)" — unresolved targets are flagged
                BROKEN.

  Incoming:     every doc whose body contains [[id:<this>...]], same shape.
"""

import argparse
import sys

from doc_index import (
    load_all, load_anchors, build_inbound,
    resolve_id, resolve_anchor, find_ambiguous,
)


def show_link_row(uuid: str, docs, anchors) -> str:
    target = docs.get(uuid)
    if target is not None:
        type_label = target.doctype or "?"
        return f"  {uuid}  | {type_label:<12} | {target.title}   ({target.rel_path})"
    anchor = anchors.get(uuid)
    if anchor is not None:
        parent = docs.get(anchor.parent_id)
        parent_label = parent.title if parent else anchor.rel_path
        return f"  {uuid}  | anchor       | {anchor.heading}  (in '{parent_label}')"
    return f"  {uuid}  [BROKEN — not in doc graph]"


def show_anchor(anchor, docs, inbound_idx, anchors) -> None:
    parent = docs.get(anchor.parent_id)
    print(f"Anchor (heading-level :ID:) inside a parent doc.")
    print()
    print(f"Heading:  {anchor.heading}")
    print(f"In doc:   {parent.title if parent else '(unknown)'}")
    print(f"Path:     {anchor.rel_path}")
    if parent:
        print(f"Parent:   {anchor.parent_id}")
    print()
    print("Section")
    print("-------")
    print(anchor.body if anchor.body else "(empty)")
    print()
    inbound = inbound_idx.get(anchor.id, [])
    print(f"Incoming links ({len(inbound)})")
    print("---------------")
    if not inbound:
        print("  (none)")
    else:
        for uuid in inbound:
            print(show_link_row(uuid, docs, anchors))


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(prog="compass show", description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("query", help="UUID or unambiguous prefix (>=6 hex chars)")
    args = ap.parse_args(argv)

    docs = load_all()
    inbound_idx = build_inbound(docs)
    doc = resolve_id(args.query, docs)
    if doc is None:
        # Try heading-level :ID:
        anchors_all = load_anchors(docs)
        anchor = resolve_anchor(args.query, anchors_all)
        if anchor is not None:
            show_anchor(anchor, docs, inbound_idx, anchors_all)
            return 0
        candidates = find_ambiguous(args.query, docs)
        if not candidates:
            print(f"No doc or anchor matches '{args.query}'.", file=sys.stderr)
        else:
            print(f"'{args.query}' is ambiguous, matches {len(candidates)} docs:",
                  file=sys.stderr)
            for d in candidates[:20]:
                print(f"  {d.id}  {d.title}", file=sys.stderr)
        return 1

    anchors_all = load_anchors(docs)

    print(f"Title:    {doc.title}")
    print(f"Type:     {doc.doctype or '(none)'}")
    print(f"Path:     {doc.rel_path}")
    if doc.tags:
        print(f"Tags:     {', '.join(doc.tags)}")
    if doc.updated:
        print(f"Updated:  {doc.updated}")
    if doc.description:
        print(f"Desc:     {doc.description}")
    print()

    print("Blurb")
    print("-----")
    print(doc.blurb if doc.blurb else "(no blurb)")
    print()

    print(f"Outgoing links ({len(doc.outbound)})")
    print("---------------")
    if not doc.outbound:
        print("  (none)")
    else:
        for uuid in doc.outbound:
            print(show_link_row(uuid, docs, anchors_all))
    print()

    inbound = inbound_idx.get(doc.id, [])
    print(f"Incoming links ({len(inbound)})")
    print("---------------")
    if not inbound:
        print("  (none)")
    else:
        for uuid in inbound:
            print(show_link_row(uuid, docs, anchors_all))

    return 0


if __name__ == "__main__":
    sys.exit(main())
