#!/usr/bin/env python3
"""Generate release notes for a sprint from org-mode backlog data and PR records.

Reads the sprint's ``sprint.org`` and each story's ``story.org``, merges the
data with any available PR records from ``tmp/release_pr_data/``, and writes
``release_notes.md`` inside the sprint folder. Also appends a link to that
file from the sprint's ``sprint.org``.

No pip dependencies — stdlib only. The script auto-detects the latest sprint
under ``doc/agile/versions/v0/`` or accepts an explicit path via ``--sprint-dir``.
"""

import argparse
import json
import re
import sys
from datetime import datetime
from pathlib import Path
from textwrap import dedent


# ── Tag → section mapping (first match wins) ────────────────────────────────

TAG_TO_SECTION = [
    ({"windows", "msvc", "visibility", "cmake", "vcpkg"}, "Build & Portability"),
    ({"ore", "trading", "refdata", "fx", "equity", "conventions", "roundtrip"}, "Financial Features"),
    ({"isolation", "sql", "codegen", "workflow", "nats", "iam"}, "Service Architecture"),
    ({"workspace", "dq", "publish", "compute", "gleif"}, "New Features"),
    ({"qt", "ui"}, "Qt UI"),
    ({"documentation", "plantuml", "agile", "site"}, "Documentation & Tooling"),
    ({"dependencies", "housekeeping"}, "Maintenance"),
]
DEFAULT_SECTION = "Other"


# ── Org-mode parsing ─────────────────────────────────────────────────────────

def _split_sections(text: str, level: int = 1) -> dict[str, str]:
    """Return a dict mapping org heading text → body text at the given depth."""
    sections: dict[str, str] = {}
    current_heading: str | None = None
    buf: list[str] = []
    prefix = r"^\*{" + str(level) + r"}\s+(.+)"

    for line in text.splitlines():
        m = re.match(prefix, line)
        if m:
            if current_heading is not None:
                sections[current_heading] = "\n".join(buf).strip()
            current_heading = m.group(1).strip()
            buf = []
        else:
            if current_heading is not None:
                buf.append(line)

    if current_heading is not None:
        sections[current_heading] = "\n".join(buf).strip()

    return sections


def _extract_frontmatter(text: str) -> dict[str, str]:
    """Extract #+key: value pairs from org frontmatter."""
    meta: dict[str, str] = {}
    for line in text.splitlines():
        m = re.match(r"^\s*#\+([^:]+):\s*(.*)", line)
        if m:
            meta[m.group(1).strip().lower()] = m.group(2).strip()
        elif line.startswith(":ID:"):
            meta["id"] = line.split(":ID:", 1)[1].strip()
    return meta


def _extract_filetags(text: str) -> set[str]:
    """Return the set of tags from #+filetags: :a:b:c:"""
    meta = _extract_frontmatter(text)
    raw = meta.get("filetags", "")
    return {t.lower() for t in raw.strip(":").split(":") if t}


def _strip_org_links(text: str) -> str:
    """Replace [[id:...][label]] → label, [[file:...][label]] → label."""
    return re.sub(r"\[\[[^\]]*\]\[([^\]]*)\]\]", r"\1", text)


def _table_rows(text: str) -> list[list[str]]:
    """Return non-separator rows from an org table as lists of cell strings."""
    rows = []
    for line in text.splitlines():
        line = line.strip()
        if line.startswith("|") and not re.match(r"^\|[-+]+\|", line):
            cells = [c.strip() for c in line.strip("|").split("|")]
            rows.append(cells)
    return rows


# ── Sprint and story parsing ─────────────────────────────────────────────────

def parse_sprint(sprint_dir: Path) -> dict:
    """Parse sprint.org and return structured sprint data."""
    path = sprint_dir / "sprint.org"
    if not path.exists():
        sys.exit(f"[err] sprint.org not found at {path}")

    text = path.read_text(encoding="utf-8")
    meta = _extract_frontmatter(text)
    sections = _split_sections(text)

    title = meta.get("title", sprint_dir.name)
    mission = sections.get("Mission", "").strip()

    stories_raw: list[dict] = []
    stories_body = sections.get("Stories", "")
    for cells in _table_rows(stories_body):
        if len(cells) < 5:
            continue
        title_cell = _strip_org_links(cells[0]).strip()
        if not title_cell or title_cell.startswith("Story"):
            continue
        stories_raw.append({
            "title": title_cell,
            "state": cells[1].strip(),
            "start": cells[2].strip(),
            "end": cells[3].strip(),
            "theme": cells[4].strip(),
        })

    retro: dict[str, str] = {}
    for heading, body in sections.items():
        if "Retrospective" in heading:
            retro_secs = _split_sections(body, level=2)
            retro = {k: _strip_org_links(v) for k, v in retro_secs.items()}

    return {
        "title": title,
        "mission": _strip_org_links(mission),
        "stories": stories_raw,
        "retrospective": retro,
    }


def parse_story(story_dir: Path) -> dict:
    """Parse story.org and return structured story data."""
    path = story_dir / "story.org"
    if not path.exists():
        return {}

    text = path.read_text(encoding="utf-8")
    meta = _extract_frontmatter(text)
    sections = _split_sections(text)
    tags = _extract_filetags(text)

    title = meta.get("title", story_dir.name)
    if title.lower().startswith("story:"):
        title = title[len("story:"):].strip()

    goal = _strip_org_links(sections.get("Goal", "")).strip()

    tasks: list[str] = []
    tasks_body = sections.get("Tasks", "")
    for line in tasks_body.splitlines():
        m = re.match(r"^-\s+\[\[id:[^\]]+\]\[([^\]]+)\]\]", line)
        if m:
            tasks.append(m.group(1).strip())

    state_rows = _table_rows(sections.get("Status", ""))
    state = ""
    for row in state_rows:
        if row and row[0].strip() == "State":
            state = row[1].strip() if len(row) > 1 else ""
            break

    return {
        "slug": story_dir.name,
        "title": title,
        "goal": goal,
        "state": state,
        "tags": tags,
        "tasks": tasks,
    }


def assign_section(tags: set[str]) -> str:
    for tag_set, section in TAG_TO_SECTION:
        if tags & tag_set:
            return section
    return DEFAULT_SECTION


# ── PR data loading ──────────────────────────────────────────────────────────

def load_pr_summary(pr_dir: Path) -> dict:
    summary_path = pr_dir / "summary.json"
    if not summary_path.exists():
        return {}
    return json.loads(summary_path.read_text(encoding="utf-8"))


# ── Markdown generation ──────────────────────────────────────────────────────

def _wrap_para(text: str, width: int = 100) -> str:
    """Reflow a multi-line org paragraph into one wrapped Markdown paragraph."""
    words = text.split()
    lines: list[str] = []
    current: list[str] = []
    length = 0
    for w in words:
        if length + len(w) + 1 > width and current:
            lines.append(" ".join(current))
            current = [w]
            length = len(w)
        else:
            current.append(w)
            length += len(w) + 1
    if current:
        lines.append(" ".join(current))
    return "\n".join(lines)


def generate_notes(sprint: dict, stories: list[dict], pr_summary: dict) -> str:
    sprint_title = sprint["title"]

    # Sprint number for version tag guess
    m = re.search(r"(\d+)", sprint_title)
    sprint_num = int(m.group(1)) if m else 0

    # Date range from stories
    all_dates = []
    for s in sprint["stories"]:
        for d in (s["start"], s["end"]):
            if d and re.match(r"\d{4}-\d{2}-\d{2}", d):
                all_dates.append(d)
    sprint_start = min(all_dates) if all_dates else "?"
    sprint_end = max(all_dates) if all_dates else "?"
    try:
        month_year = datetime.strptime(sprint_end, "%Y-%m-%d").strftime("%B %Y")
    except ValueError:
        month_year = "2026"

    # PR stats
    pr_count = pr_summary.get("pr_count", 0)
    since_tag = pr_summary.get("since_tag", "previous release")
    pr_since = pr_summary.get("since_commit_iso", "")[:10]

    # Categorise stories
    by_section: dict[str, list[dict]] = {}
    story_map = {s["slug"]: s for s in stories if s}
    for entry in sprint["stories"]:
        # Match sprint table entry to parsed story by title
        match = None
        for s in stories:
            if s and s.get("title", "").lower() == entry["title"].lower():
                match = s
                break
        if match is None:
            # Fallback: create minimal record from sprint table
            match = {
                "slug": "",
                "title": entry["title"],
                "goal": entry["theme"],
                "state": entry["state"],
                "tags": set(),
                "tasks": [],
            }
        section = assign_section(match.get("tags", set()))
        by_section.setdefault(section, []).append({**entry, **match})

    # Decide section order
    section_order = [s for _, s in TAG_TO_SECTION] + [DEFAULT_SECTION]
    ordered_sections = [(sec, by_section[sec])
                        for sec in section_order if sec in by_section]

    # Highlights: from retrospective "What went well" — join multi-line bullets
    retro = sprint.get("retrospective", {})
    went_well = retro.get("What went well", "")
    highlights: list[str] = []
    current: list[str] = []
    for line in went_well.splitlines():
        stripped = line.strip()
        if stripped.startswith("-"):
            if current:
                highlights.append(" ".join(current))
            current = [stripped[1:].strip()]
        elif stripped and current:
            current.append(stripped)
    if current:
        highlights.append(" ".join(current))
    highlights = highlights[:6]

    # Known issues: ABANDONED or BLOCKED stories
    deferred: list[dict] = []
    for entry in sprint["stories"]:
        if entry["state"] not in ("DONE", "STARTED", ""):
            deferred.append(entry)

    # ── Build the document ────────────────────────────────────────────────

    lines: list[str] = []

    lines.append(f"# **ORE Studio {sprint_title} – Release Notes**")
    lines.append(f"*{month_year}*")
    lines.append("")
    lines.append(_wrap_para(sprint["mission"]))
    lines.append("")
    lines.append("---")
    lines.append("")

    # Highlights
    lines.append("## ✅ **Highlights**")
    lines.append("")
    if highlights:
        for h in highlights:
            lines.append(f"- {h}")
    else:
        lines.append("- All planned stories delivered.")
    lines.append("")

    # Key Improvements
    lines.append("## 🛠️ **Key Improvements**")
    lines.append("")
    for section_name, section_stories in ordered_sections:
        lines.append(f"### **{section_name}**")
        lines.append("")
        for s in section_stories:
            title = s.get("title", "")
            theme = s.get("theme", "")
            goal = s.get("goal", "")
            # Use goal if available; fall back to theme (from sprint table)
            detail = goal if goal else theme
            # Normalise whitespace and truncate to first sentence
            detail = re.sub(r"\s+", " ", detail).strip()
            first_sentence = re.split(r"(?<=[.!?])\s", detail)[0] if detail else ""
            lines.append(f"- **{title}**: {first_sentence}")
        lines.append("")

    # Known Issues
    lines.append("## ⚠️ **Known Issues & Postponed**")
    lines.append("")
    if deferred:
        for entry in deferred:
            lines.append(f"- **{entry['title']}** ({entry['state']}): deferred.")
    else:
        lines.append("- None — all stories completed.")
    lines.append("")

    # Time Summary
    lines.append("## 📊 **Time Summary**")
    lines.append("")
    lines.append("**Total effort**: not tracked")
    if pr_count:
        lines.append(
            f"**PRs merged**: {pr_count} (since {since_tag}"
            + (f", {pr_since} to {sprint_end}" if pr_since else "")
            + ")"
        )
    lines.append(f"**Sprint duration**: {sprint_start} → {sprint_end}")
    lines.append("")
    lines.append("---")
    lines.append("")
    lines.append("*Next sprint: TBD.*")
    lines.append("")

    return "\n".join(lines)


# ── Sprint org linking ────────────────────────────────────────────────────────

def link_release_notes(sprint_dir: Path) -> None:
    """Append a release-notes link to sprint.org if not already present."""
    path = sprint_dir / "sprint.org"
    text = path.read_text(encoding="utf-8")

    if "release_notes.md" in text:
        print("[info] sprint.org already links release_notes.md — skipping.")
        return

    # Append before the last non-empty line or at the end
    appendix = dedent("""\

        * Release Notes

        See [[file:release_notes.md][release_notes.md]] for the full narrative.
        """)
    path.write_text(text.rstrip() + "\n" + appendix, encoding="utf-8")
    print(f"[info] Linked release_notes.md in {path}")


# ── Auto-detect latest sprint ─────────────────────────────────────────────────

def latest_sprint_dir(repo_root: Path) -> Path:
    base = repo_root / "doc" / "agile" / "versions" / "v0"
    sprints = sorted(
        (d for d in base.iterdir()
         if d.is_dir() and d.name.startswith("sprint_")),
        key=lambda d: int(re.search(r"(\d+)", d.name).group(1))
    )
    if not sprints:
        sys.exit("[err] No sprint directories found under doc/agile/versions/v0/")
    return sprints[-1]


# ── Main ─────────────────────────────────────────────────────────────────────

def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--sprint-dir", default="",
                    help="Path to the sprint folder (default: latest sprint under "
                         "doc/agile/versions/v0/).")
    ap.add_argument("--pr-data-dir", default="tmp/release_pr_data",
                    help="Directory with PR data from collect_release_pr_data.py "
                         "(default: tmp/release_pr_data).")
    ap.add_argument("--out", default="",
                    help="Output path for release_notes.md (default: "
                         "<sprint-dir>/release_notes.md).")
    ap.add_argument("--no-link", action="store_true",
                    help="Do not update sprint.org with a link to release_notes.md.")
    args = ap.parse_args()

    repo_root = Path(__file__).resolve().parent.parent.parent

    sprint_dir = Path(args.sprint_dir) if args.sprint_dir else latest_sprint_dir(repo_root)
    sprint_dir = sprint_dir.resolve()
    print(f"[info] Sprint directory: {sprint_dir}")

    pr_dir = (repo_root / args.pr_data_dir).resolve()

    # Parse sprint
    sprint = parse_sprint(sprint_dir)
    print(f"[info] Sprint: {sprint['title']} ({len(sprint['stories'])} stories)")

    # Parse each story subdir
    stories: list[dict] = []
    for subdir in sorted(sprint_dir.iterdir()):
        if subdir.is_dir() and (subdir / "story.org").exists():
            s = parse_story(subdir)
            if s:
                stories.append(s)
    print(f"[info] Parsed {len(stories)} story files.")

    # Load PR summary
    pr_summary = load_pr_summary(pr_dir)
    if pr_summary:
        print(f"[info] PR data: {pr_summary.get('pr_count', 0)} PRs "
              f"since {pr_summary.get('since_tag', '?')}.")
    else:
        print("[info] No PR data found — continuing without it.")

    # Generate notes
    notes = generate_notes(sprint, stories, pr_summary)

    # Write output
    out_path = Path(args.out) if args.out else sprint_dir / "release_notes.md"
    out_path.write_text(notes, encoding="utf-8")
    print(f"[info] Wrote {out_path}")

    # Link from sprint.org
    if not args.no_link:
        link_release_notes(sprint_dir)


if __name__ == "__main__":
    main()
