#!/usr/bin/env python3
"""
Shared doc-index library used by doc_list.py / doc_show.py.

Walks every .org file in the repo (excluding build/external/vcpkg/.git/...),
extracts the minimal metadata needed to find documents:

  - :ID: from the leading :PROPERTIES: block (required — files without an
    :ID: are skipped, they are not part of the addressable doc graph)
  - #+title, #+description, #+type, #+filetags, #+updated
  - blurb (text between the frontmatter and the first * heading)
  - outbound [[id:UUID]] references

It also builds the inbound index (uuid -> list of uuids that link to it).
"""

import os
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Iterator


REPO_ROOT = Path(__file__).resolve().parents[3]
EXCLUDED_DIRS = {"build", ".packages", "vcpkg", "external", ".git", "node_modules", ".claude"}

ID_RE       = re.compile(r"^:ID:\s+([0-9A-Fa-f-]+)\s*$",   re.MULTILINE)
TITLE_RE    = re.compile(r"^#\+title:\s*(.*?)\s*$",        re.MULTILINE | re.IGNORECASE)
DESC_RE     = re.compile(r"^#\+description:\s*(.*?)\s*$",  re.MULTILINE | re.IGNORECASE)
TYPE_RE     = re.compile(r"^#\+type:\s*(\S+)",             re.MULTILINE | re.IGNORECASE)
FILETAGS_RE = re.compile(r"^#\+filetags:\s*(.+?)\s*$",     re.MULTILINE | re.IGNORECASE)
UPDATED_RE  = re.compile(r"^#\+updated:\s*(\S+)",          re.MULTILINE | re.IGNORECASE)
VERSION_RE  = re.compile(r"^#\+version:\s*(\S+)",          re.MULTILINE | re.IGNORECASE)
LINK_RE     = re.compile(r"\[\[id:([0-9A-Fa-f-]+)(?:\]\[([^\]]+))?\]\]")
HEADING_RE  = re.compile(r"^(\*+)\s+(.*?)\s*$")


@dataclass
class Doc:
    id: str
    title: str
    description: str
    doctype: str | None
    version: str | None
    tags: list[str]
    path: Path
    rel_path: str
    updated: str | None
    blurb: str
    outbound: list[str] = field(default_factory=list)

    def matches_text(self, pattern: re.Pattern) -> bool:
        return bool(pattern.search(self.title) or pattern.search(self.description))

    def has_tag(self, tag: str) -> bool:
        return tag.lower() in (t.lower() for t in self.tags)


@dataclass
class Anchor:
    """A heading-level :ID: inside a parent doc file."""
    id: str
    heading: str
    level: int
    parent_id: str
    rel_path: str
    body: str  # text from the heading down to the next sibling-or-higher heading


def parse_tags(raw: str) -> list[str]:
    raw = raw.strip()
    if raw.startswith(":") and raw.endswith(":"):
        return [t for t in raw.strip(":").split(":") if t]
    return raw.split()


def extract_blurb(text: str) -> str:
    """Text between end-of-frontmatter and the first * heading."""
    lines = text.splitlines()
    i, n = 0, len(lines)
    if i < n and lines[i].strip() == ":PROPERTIES:":
        while i < n and lines[i].strip() != ":END:":
            i += 1
        if i < n:
            i += 1
    while i < n:
        s = lines[i].strip()
        if s.startswith("#+") or s == "":
            i += 1
        else:
            break
    out = []
    while i < n:
        if lines[i].startswith("* "):
            break
        out.append(lines[i])
        i += 1
    return "\n".join(out).strip()


def parse_anchors(text: str, file_doc_id: str, rel_path: str) -> list[Anchor]:
    """Find every :ID: that belongs to a heading (not the file-level :PROPERTIES:)."""
    lines = text.splitlines()
    n = len(lines)
    anchors: list[Anchor] = []
    cur_heading: tuple[int, str, int] | None = None  # (level, text, line index)
    # Skip the file-level :PROPERTIES: block at the top so its :ID: is not double-counted.
    start = 0
    if start < n and lines[start].strip() == ":PROPERTIES:":
        while start < n and lines[start].strip() != ":END:":
            start += 1
        if start < n:
            start += 1
    i = start
    while i < n:
        line = lines[i]
        h = HEADING_RE.match(line)
        if h:
            cur_heading = (len(h.group(1)), h.group(2).strip(), i)
        elif line.strip() == ":PROPERTIES:" and cur_heading:
            # Walk to :END:, look for :ID: line
            j = i + 1
            heading_id = None
            while j < n and lines[j].strip() != ":END:":
                m = re.match(r":ID:\s+([0-9A-Fa-f-]+)\s*$", lines[j])
                if m:
                    heading_id = m.group(1).lower()
                j += 1
            if heading_id and heading_id != file_doc_id:
                # Body: from heading line down to next heading of same or higher level
                body_start = cur_heading[2]
                body_end = n
                k = j + 1
                while k < n:
                    h2 = HEADING_RE.match(lines[k])
                    if h2 and len(h2.group(1)) <= cur_heading[0]:
                        body_end = k
                        break
                    k += 1
                body = "\n".join(lines[body_start:body_end]).strip()
                anchors.append(Anchor(
                    id=heading_id,
                    heading=cur_heading[1],
                    level=cur_heading[0],
                    parent_id=file_doc_id,
                    rel_path=rel_path,
                    body=body,
                ))
            i = j  # jump past this :PROPERTIES: block
        i += 1
    return anchors


def parse_doc(path: Path) -> Doc | None:
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    id_m = ID_RE.search(text)
    if not id_m:
        return None
    doc_id = id_m.group(1).lower()

    title_m   = TITLE_RE.search(text)
    desc_m    = DESC_RE.search(text)
    type_m    = TYPE_RE.search(text)
    tags_m    = FILETAGS_RE.search(text)
    updated_m = UPDATED_RE.search(text)
    version_m = VERSION_RE.search(text)

    seen, outbound = {doc_id}, []
    for m in LINK_RE.finditer(text):
        u = m.group(1).lower()
        if u not in seen:
            seen.add(u)
            outbound.append(u)

    return Doc(
        id=doc_id,
        title=(title_m.group(1).strip() if title_m else ""),
        description=(desc_m.group(1).strip() if desc_m else ""),
        doctype=(type_m.group(1).strip().lower() if type_m else None),
        version=(version_m.group(1).strip() if version_m else None),
        tags=parse_tags(tags_m.group(1)) if tags_m else [],
        path=path,
        rel_path=str(path.relative_to(REPO_ROOT)),
        updated=(updated_m.group(1).strip() if updated_m else None),
        blurb=extract_blurb(text),
        outbound=outbound,
    )


def find_org_files(root: Path = REPO_ROOT) -> Iterator[Path]:
    """Walk root for *.org files, pruning EXCLUDED_DIRS so the walk never
    descends into them (build/ and friends can be huge and slow/IO-bound;
    filtering after Path.rglob had already visited every file inside)."""
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = sorted(d for d in dirnames if d not in EXCLUDED_DIRS)
        for name in sorted(filenames):
            if name.endswith(".org"):
                yield Path(dirpath) / name


def load_all() -> dict[str, Doc]:
    docs: dict[str, Doc] = {}
    for p in find_org_files():
        d = parse_doc(p)
        if d:
            docs[d.id] = d
    return docs


def load_anchors(docs: dict[str, Doc]) -> dict[str, Anchor]:
    """Heading-level :ID:s, keyed by uuid. Re-reads each doc's file once."""
    anchors: dict[str, Anchor] = {}
    for d in docs.values():
        try:
            text = d.path.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        for a in parse_anchors(text, d.id, d.rel_path):
            anchors[a.id] = a
    return anchors


def build_inbound(docs: dict[str, Doc]) -> dict[str, list[str]]:
    """For every uuid linked to, list ids of docs whose body contains the link."""
    inbound: dict[str, list[str]] = {}
    for d in docs.values():
        for target in d.outbound:
            inbound.setdefault(target, []).append(d.id)
    for k in inbound:
        inbound[k] = sorted(set(inbound[k]))
    return inbound


def resolve_id(query: str, docs: dict[str, Doc]) -> Doc | None:
    """Find a doc by full UUID or by unique prefix (>= 6 chars)."""
    q = query.lower().strip()
    if q in docs:
        return docs[q]
    if len(q) < 6:
        return None
    matches = [d for did, d in docs.items() if did.startswith(q)]
    if len(matches) == 1:
        return matches[0]
    return None


def resolve_anchor(query: str, anchors: dict[str, Anchor]) -> Anchor | None:
    q = query.lower().strip()
    if q in anchors:
        return anchors[q]
    if len(q) < 6:
        return None
    matches = [a for aid, a in anchors.items() if aid.startswith(q)]
    if len(matches) == 1:
        return matches[0]
    return None


def find_ambiguous(query: str, docs: dict[str, Doc]) -> list[Doc]:
    q = query.lower().strip()
    return [d for did, d in docs.items() if did.startswith(q)]
