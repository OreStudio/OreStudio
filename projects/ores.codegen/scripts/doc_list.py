#!/usr/bin/env python3
"""
List every addressable doc in the repo as a single line per doc.

Each line:
  <uuid> | <type> | <title> — <description>   (rel/path)

Filters:
  --regex PATTERN   Match (case-insensitive) against title OR description.
  --tag TAG         Require this filetag (repeatable; tags must all match).
  --type TYPE       Require this #+type: (e.g. recipe, knowledge, task).
  --under PATH      Restrict to docs under repo path (e.g. doc/recipes,
                    doc/agile/versions/v0). Repeatable.
  --sort {title,updated,path}   Sort key (default: path).
  --count           Print only the match count.
  --paths           Print one path per line (no other fields). Useful for
                    piping into grep / xargs.

Examples:
  # All recipe docs mentioning "trade"
  doc_list.py --type recipe --regex trade

  # All docs under agile with the build_quality tag
  doc_list.py --under doc/agile --tag build_quality

  # Just paths of every knowledge doc, for use with grep
  doc_list.py --type knowledge --paths
"""

import argparse
import re
import sys

from doc_index import load_all, REPO_ROOT


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--regex", help="case-insensitive regex on title or description")
    ap.add_argument("--tag", action="append", default=[], help="require this filetag (repeatable)")
    ap.add_argument("--type", dest="doctype", help="require this #+type:")
    ap.add_argument("--under", action="append", default=[],
                    help="restrict to docs under this path (repeatable)")
    ap.add_argument("--sort", choices=["title", "updated", "path"], default="path")
    ap.add_argument("--count", action="store_true", help="print match count only")
    ap.add_argument("--paths", action="store_true", help="print only paths, one per line")
    args = ap.parse_args()

    pattern = re.compile(args.regex, re.IGNORECASE) if args.regex else None
    type_filter = args.doctype.lower() if args.doctype else None
    under = [str((REPO_ROOT / u).resolve()) for u in args.under]

    docs = load_all()
    results = []
    for d in docs.values():
        if pattern and not d.matches_text(pattern):
            continue
        if type_filter and d.doctype != type_filter:
            continue
        if args.tag and not all(d.has_tag(t) for t in args.tag):
            continue
        if under and not any(str(d.path.resolve()).startswith(u) for u in under):
            continue
        results.append(d)

    key_funcs = {
        "title":   lambda d: d.title.lower(),
        "updated": lambda d: d.updated or "",
        "path":    lambda d: d.rel_path,
    }
    results.sort(key=key_funcs[args.sort])

    if args.count:
        print(len(results))
        return 0
    if args.paths:
        for d in results:
            print(d.rel_path)
        return 0

    for d in results:
        type_label = d.doctype or "?"
        desc = d.description.replace("\n", " ").strip()
        title = d.title.replace("\n", " ").strip()
        line = f"{d.id} | {type_label:<14} | {title}"
        if desc:
            line += f"  —  {desc}"
        line += f"   ({d.rel_path})"
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
