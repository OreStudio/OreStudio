#!/usr/bin/env python3
"""Generate an org-mode release-notes document for a sprint.

Reads the sprint's ``sprint.org`` and each story's ``story.org``, merges the
data with any available PR records from ``tmp/release_pr_data/``, then renders
``projects/ores.codegen/library/templates/doc_release_notes.org.mustache``
via pystache and writes the result as ``release_notes.org`` inside the sprint
folder.  A link to that file is also appended to the sprint's ``sprint.org``.

After generation, open the file and fill in the summary blurb that sits between
the date line and the first ``-----`` rule — the placeholder says so explicitly.

Pystache is pulled from the ores.codegen venv (``projects/ores.codegen/venv``).
No other pip dependencies are required.

The script auto-detects the latest sprint under ``doc/agile/versions/v0/`` or
accepts an explicit path via ``--sprint-dir``.
"""

import argparse
import json
import re
import sys
import uuid
from datetime import date, datetime
from pathlib import Path
from textwrap import dedent


# ── Tag → section mapping (first match wins) ────────────────────────────────

TAG_TO_SECTION = [
    ({"windows", "msvc", "visibility", "cmake", "vcpkg"}, "Build & Portability"),
    ({"ore", "trading", "refdata", "fx", "equity", "conventions", "roundtrip",
      "marketdata", "market_data", "synthetic"}, "Financial Features"),
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
    meta: dict[str, str] = {}
    for line in text.splitlines():
        m = re.match(r"^\s*#\+([^:]+):\s*(.*)", line)
        if m:
            meta[m.group(1).strip().lower()] = m.group(2).strip()
        elif line.startswith(":ID:"):
            meta["id"] = line.split(":ID:", 1)[1].strip()
    return meta


def _extract_filetags(text: str) -> set[str]:
    meta = _extract_frontmatter(text)
    raw = meta.get("filetags", "")
    return {t.lower() for t in raw.strip(":").split(":") if t}


def _strip_org_links(text: str) -> str:
    return re.sub(r"\[\[[^\]]*\]\[([^\]]*)\]\]", r"\1", text)


def _table_rows(text: str) -> list[list[str]]:
    rows = []
    for line in text.splitlines():
        line = line.strip()
        if line.startswith("|") and not re.match(r"^\|[-+]+\|", line):
            cells = [c.strip() for c in line.strip("|").split("|")]
            rows.append(cells)
    return rows


# ── Sprint and story parsing ─────────────────────────────────────────────────

def parse_sprint(sprint_dir: Path) -> dict:
    path = sprint_dir / "sprint.org"
    if not path.exists():
        sys.exit(f"[err] sprint.org not found at {path}")

    text = path.read_text(encoding="utf-8")
    meta = _extract_frontmatter(text)
    sections = _split_sections(text)

    title = meta.get("title", sprint_dir.name)
    mission = sections.get("Mission", "").strip()

    stories_raw: list[dict] = []
    for cells in _table_rows(sections.get("Stories", "")):
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
            retro = {k: _strip_org_links(v)
                     for k, v in _split_sections(body, level=2).items()}

    return {
        "title": title,
        "mission": _strip_org_links(mission),
        "stories": stories_raw,
        "retrospective": retro,
    }


def parse_story(story_dir: Path) -> dict:
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


# ── Context building ─────────────────────────────────────────────────────────

def _wrap_para(text: str, width: int = 100) -> str:
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


def build_context(sprint: dict, stories: list[dict], pr_summary: dict,
                  sprint_dir: Path, repo_root: Path, screenshot_filename: str = "",
                  demo_video_id: str = "") -> dict:
    sprint_title = sprint["title"]

    # Sprint slug for filetags (e.g. sprint_17)
    sprint_slug = sprint_dir.name

    # repo-root-relative parent dir for proj: chart links (e.g.
    # doc/agile/versions/v0), so [[proj:{{parent_dir}}/{{slug}}/x.png]]
    # resolves to the sprint folder rather than the repo root.
    parent_dir = sprint_dir.relative_to(repo_root).parent.as_posix()

    # Date range
    all_dates = [
        d for entry in sprint["stories"]
        for d in (entry["start"], entry["end"])
        if d and re.match(r"\d{4}-\d{2}-\d{2}", d)
    ]
    sprint_start = min(all_dates) if all_dates else "?"
    sprint_end = max(all_dates) if all_dates else "?"
    try:
        month_year = datetime.strptime(sprint_end, "%Y-%m-%d").strftime("%B %Y")
    except ValueError:
        month_year = date.today().strftime("%B %Y")

    # PR stats
    pr_count = pr_summary.get("pr_count", 0)
    since_tag = pr_summary.get("since_tag", "")
    pr_since = pr_summary.get("since_commit_iso", "")[:10]

    # ── Highlights from retrospective "What went well" ──────────────────
    went_well = sprint.get("retrospective", {}).get("What went well", "")
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

    highlights_block = "\n".join(f"- {h}" for h in highlights) \
        if highlights else "- All planned stories delivered."

    # ── Categorise stories ───────────────────────────────────────────────
    by_section: dict[str, list[dict]] = {}
    for entry in sprint["stories"]:
        story = next(
            (s for s in stories if s and
             s.get("title", "").lower() == entry["title"].lower()),
            None,
        )
        if story is None:
            story = {
                "slug": "",
                "title": entry["title"],
                "goal": entry["theme"],
                "state": entry["state"],
                "tags": set(),
            }
        section = assign_section(story.get("tags", set()))
        detail = story.get("goal") or entry["theme"]
        detail = re.sub(r"\s+", " ", detail).strip()
        first_sentence = re.split(r"(?<=[.!?])\s", detail)[0] if detail else ""
        by_section.setdefault(section, []).append({
            "title": entry["title"],
            "detail": first_sentence,
        })

    section_order = [s for _, s in TAG_TO_SECTION] + [DEFAULT_SECTION]
    sections_lines: list[str] = []
    for sec in section_order:
        if sec not in by_section:
            continue
        sections_lines.append(f"** {sec}")
        sections_lines.append("")
        for s in by_section[sec]:
            sections_lines.append(f"- *{s['title']}*: {s['detail']}")
        sections_lines.append("")
    sections_block = "\n".join(sections_lines).rstrip()

    # ── Deferred / postponed ─────────────────────────────────────────────
    deferred = [e for e in sprint["stories"]
                if e["state"] not in ("DONE", "STARTED", "")]
    deferred_block = "\n".join(
        f"- *{e['title']}* ({e['state']}): deferred." for e in deferred
    ) if deferred else "- None — all stories completed."

    # ── Time summary ─────────────────────────────────────────────────────
    time_lines = ["- *Total effort*: not tracked"]
    if pr_count and since_tag:
        pr_line = f"{pr_count} (since {since_tag}"
        if pr_since:
            pr_line += f", {pr_since} to {sprint_end}"
        pr_line += ")"
        time_lines.append(f"- *PRs merged*: {pr_line}")
    time_lines.append(f"- *Sprint duration*: {sprint_start} → {sprint_end}")
    time_block = "\n".join(time_lines)

    # ── Filetags ─────────────────────────────────────────────────────────
    filetags = f":release_notes:{sprint_slug}:v0:"

    return {
        "id": str(uuid.uuid4()).upper(),
        "title": f"ORE Studio {sprint_title} – Release Notes",
        "sprint_title": sprint_title,
        "description": f"{sprint_title} release notes ({month_year}).",
        "filetags": filetags,
        "date": date.today().isoformat(),
        "month_year": month_year,
        "parent_dir": parent_dir,
        "slug": sprint_slug,
        "mission": _wrap_para(sprint["mission"]),
        "highlights_block": highlights_block,
        "sections_block": sections_block,
        "deferred_block": deferred_block,
        "time_block": time_block,
        "screenshot_filename": screenshot_filename,
        "demo_video_id": demo_video_id,
    }


# ── Rendering ────────────────────────────────────────────────────────────────

def _load_pystache(repo_root: Path):
    """Import pystache from the ores.codegen venv, or from the active environment."""
    try:
        import pystache
        return pystache
    except ImportError:
        pass
    import glob
    venv_root = repo_root / "projects" / "ores.codegen" / "venv"
    candidates = sorted(glob.glob(str(venv_root / "lib" / "python*" / "site-packages")))
    if not candidates:
        sys.exit(
            "[err] pystache not found. Activate the ores.codegen venv:\n"
            "  cd projects/ores.codegen && source venv/bin/activate\n"
            "  pip install -r requirements.txt"
        )
    sys.path.insert(0, candidates[-1])
    import pystache
    return pystache


def render_release_notes(context: dict, repo_root: Path) -> str:
    pystache = _load_pystache(repo_root)
    template_path = (
        repo_root / "projects" / "ores.codegen" / "library" / "templates"
        / "doc_release_notes.org.mustache"
    )
    template_text = template_path.read_text(encoding="utf-8")
    renderer = pystache.Renderer(escape=lambda v: v)
    return renderer.render(template_text, context)


# ── Sprint org linking ────────────────────────────────────────────────────────

def link_release_notes(sprint_dir: Path) -> None:
    """Append a release-notes link to sprint.org if not already present."""
    path = sprint_dir / "sprint.org"
    text = path.read_text(encoding="utf-8")

    if "release_notes.org" in text:
        print("[info] sprint.org already links release_notes.org — skipping.")
        return

    # Upgrade a legacy .md link written by a previous run of this script.
    if "release_notes.md" in text:
        text = text.replace(
            "[[file:release_notes.md][release_notes.md]]",
            "[[file:release_notes.org][release_notes.org]]",
        )
        path.write_text(text, encoding="utf-8")
        print(f"[info] Updated release_notes.md → release_notes.org link in {path}")
        return

    appendix = dedent("""\

        * Release Notes

        See [[file:release_notes.org][release_notes.org]] for the full narrative.
        """)
    path.write_text(text.rstrip() + "\n" + appendix, encoding="utf-8")
    print(f"[info] Linked release_notes.org in {path}")


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
                    help="Output path for release_notes.org (default: "
                         "<sprint-dir>/release_notes.org).")
    ap.add_argument("--no-link", action="store_true",
                    help="Do not update sprint.org with a link to release_notes.org.")
    ap.add_argument("--screenshot-filename", default="",
                    help="Filename under assets/images/ to embed at the top "
                         "(e.g. ore_studio-v0.0.18.png). Omit to skip.")
    ap.add_argument("--demo-video-id", default="",
                    help="YouTube video ID for the sprint demo "
                         "(e.g. NDxW12FNhrU). Omit to skip the demo section.")
    args = ap.parse_args()

    repo_root = Path(__file__).resolve().parent.parent.parent

    sprint_dir = Path(args.sprint_dir) if args.sprint_dir else latest_sprint_dir(repo_root)
    sprint_dir = sprint_dir.resolve()
    print(f"[info] Sprint directory: {sprint_dir}")

    pr_dir = (repo_root / args.pr_data_dir).resolve()

    sprint = parse_sprint(sprint_dir)
    print(f"[info] Sprint: {sprint['title']} ({len(sprint['stories'])} stories)")

    stories: list[dict] = []
    for subdir in sorted(sprint_dir.iterdir()):
        if subdir.is_dir() and (subdir / "story.org").exists():
            s = parse_story(subdir)
            if s:
                stories.append(s)
    print(f"[info] Parsed {len(stories)} story files.")

    pr_summary = load_pr_summary(pr_dir)
    if pr_summary:
        print(f"[info] PR data: {pr_summary.get('pr_count', 0)} PRs "
              f"since {pr_summary.get('since_tag', '?')}.")
    else:
        print("[info] No PR data found — continuing without it.")

    context = build_context(
        sprint, stories, pr_summary, sprint_dir, repo_root,
        screenshot_filename=args.screenshot_filename,
        demo_video_id=args.demo_video_id,
    )
    rendered = render_release_notes(context, repo_root)

    out_path = Path(args.out) if args.out else sprint_dir / "release_notes.org"
    out_path.write_text(rendered, encoding="utf-8")
    print(f"[info] Wrote {out_path}")
    print(f"[info] Open {out_path} and fill in the summary blurb placeholder.")

    if not args.no_link:
        link_release_notes(sprint_dir)


if __name__ == "__main__":
    main()
