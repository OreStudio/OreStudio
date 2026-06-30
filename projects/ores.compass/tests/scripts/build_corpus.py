#!/usr/bin/env python3
"""
Convert an org-roam file into a corpus document JSON.

Usage:
    python3 tests/scripts/build_corpus.py <path/to/file.org> [<file.org> ...]

Output:
    tests/data/corpus/<stem>.json  for each input file

The JSON format mirrors the fields that compass.py reads from doc_index.Doc
plus the FTS-indexed fields (title, description, body_excerpt).  It is the
canonical input representation for the search scorer test corpus.

To regenerate signals for queries after adding new corpus docs, run:
    python3 tests/scripts/build_signals.py

See tests/data/methodology.txt for the full procedure.
"""

import json
import re
import sys
from pathlib import Path

SRC_ROOT = Path(__file__).resolve().parents[2] / "src"
sys.path.insert(0, str(SRC_ROOT))

import doc_index  # noqa: E402 — needs sys.path set first

CORPUS_DIR = Path(__file__).resolve().parents[1] / "data" / "corpus"

ID_RE       = re.compile(r"^:ID:\s+([0-9A-Fa-f-]+)\s*$",   re.MULTILINE)
TITLE_RE    = re.compile(r"^#\+title:\s*(.*?)\s*$",         re.MULTILINE | re.IGNORECASE)
DESC_RE     = re.compile(r"^#\+description:\s*(.*?)\s*$",   re.MULTILINE | re.IGNORECASE)
TYPE_RE     = re.compile(r"^#\+type:\s*(\S+)",              re.MULTILINE | re.IGNORECASE)
FILETAGS_RE = re.compile(r"^#\+filetags:\s*:(.+?):\s*$",   re.MULTILINE | re.IGNORECASE)
UPDATED_RE  = re.compile(r"^#\+updated:\s*(\S+)",           re.MULTILINE | re.IGNORECASE)
LINK_RE     = re.compile(r"\[\[id:([0-9A-Fa-f-]+)")
HEADING_RE  = re.compile(r"^\*+\s", re.MULTILINE)

BODY_EXCERPT_CHARS = 400


def _body_excerpt(text: str) -> str:
    """Extract the blurb between frontmatter and first heading, then truncate."""
    heading_m = HEADING_RE.search(text)
    body = text[:heading_m.start()] if heading_m else text
    # Strip property drawer and keywords
    body = re.sub(r"^:PROPERTIES:.*?:END:\n?", "", body, flags=re.DOTALL)
    body = re.sub(r"^#\+\w+:.*$", "", body, flags=re.MULTILINE)
    body = body.strip()
    if len(body) > BODY_EXCERPT_CHARS:
        body = body[:BODY_EXCERPT_CHARS].rsplit(" ", 1)[0] + " …"
    return body


def org_to_corpus_doc(path: Path, repo_root: Path) -> dict:
    text = path.read_text(encoding="utf-8")

    id_m   = ID_RE.search(text)
    if not id_m:
        raise ValueError(f"No :ID: found in {path}")

    tags_m = FILETAGS_RE.search(text)
    tags   = tags_m.group(1).split(":") if tags_m else []

    outbound = list(dict.fromkeys(m.group(1).upper() for m in LINK_RE.finditer(text)))

    try:
        rel_path = str(path.relative_to(repo_root))
    except ValueError:
        rel_path = str(path)

    return {
        "id":           id_m.group(1).upper(),
        "title":        (TITLE_RE.search(text) or ["", ""])[1] if TITLE_RE.search(text) else "",
        "description":  (DESC_RE.search(text) or ["", ""])[1] if DESC_RE.search(text) else "",
        "doctype":      TYPE_RE.search(text).group(1).lower() if TYPE_RE.search(text) else None,
        "tags":         [t for t in tags if t],
        "rel_path":     rel_path,
        "updated":      UPDATED_RE.search(text).group(1) if UPDATED_RE.search(text) else None,
        "body_excerpt": _body_excerpt(text),
        "outbound_count": len(outbound),
    }


def main() -> None:
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    CORPUS_DIR.mkdir(parents=True, exist_ok=True)
    repo_root = Path(__file__).resolve().parents[4]

    for arg in sys.argv[1:]:
        path = Path(arg).resolve()
        try:
            doc = org_to_corpus_doc(path, repo_root)
        except ValueError as e:
            print(f"SKIP {path}: {e}", file=sys.stderr)
            continue

        out_name = path.stem + ".json"
        out_path = CORPUS_DIR / out_name
        out_path.write_text(json.dumps(doc, indent=4) + "\n", encoding="utf-8")
        print(f"wrote {out_path.relative_to(repo_root)}")


if __name__ == "__main__":
    main()
