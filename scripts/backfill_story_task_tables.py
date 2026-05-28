#!/usr/bin/env python3
"""
backfill_story_task_tables.py

Back-fills the `* Tasks` section in all sprint 18 story docs, replacing
bullet-list task entries with an org-mode table.

Usage:
    python3 scripts/backfill_story_task_tables.py [--dry-run]

Run from the repo root.
"""

import argparse
import os
import re
import sys
from pathlib import Path


DEFAULT_SPRINT_DIR = Path("doc/agile/versions/v0/sprint_18")
SKIP_STORIES = {"refactor_task_list_to_table"}

# Regex patterns
TASKS_SECTION_RE = re.compile(
    r"(^\* Tasks\n)(.*?)(?=^\* |\Z)",
    re.MULTILINE | re.DOTALL,
)
LINK_RE = re.compile(r"\[\[id:([A-F0-9\-]+)\]\[([^\]]+)\]\]", re.IGNORECASE)
STATE_ROW_RE = re.compile(r"^\| State\s*\|\s*(\S+)\s*\|", re.MULTILINE)
LAST_TOUCHED_RE = re.compile(r"^\| Last touched\s*\|\s*([^\|]+?)\s*\|", re.MULTILINE)
CREATED_RE = re.compile(r"^#\+created:\s*(\S+)", re.MULTILINE)
DESCRIPTION_RE = re.compile(r"^#\+description:\s*(.+)", re.MULTILINE)
ID_PROP_RE = re.compile(r":ID:\s+([A-F0-9\-]+)", re.IGNORECASE)

DONE_STATES = {"DONE", "ABANDONED"}


def find_task_doc(story_dir: Path, uuid: str) -> Path | None:
    """Find the task doc in story_dir whose :ID: property matches uuid."""
    uuid_upper = uuid.upper()
    for task_file in story_dir.glob("task_*.org"):
        content = task_file.read_text(encoding="utf-8")
        m = ID_PROP_RE.search(content)
        if m and m.group(1).upper() == uuid_upper:
            return task_file
    return None


def extract_task_fields(task_content: str) -> dict:
    """Extract State, Start (created), End, and Description from a task doc."""
    fields = {"state": "", "start": "", "end": "", "description": ""}

    m = STATE_ROW_RE.search(task_content)
    if m:
        fields["state"] = m.group(1).strip()

    m = CREATED_RE.search(task_content)
    if m:
        fields["start"] = m.group(1).strip()

    m = DESCRIPTION_RE.search(task_content)
    if m:
        desc = m.group(1).strip()
        # Strip "Initial task for:" prefix if present
        if desc.lower().startswith("initial task for:"):
            desc = desc[len("initial task for:"):].strip()
        fields["description"] = desc

    # End date: only if state is DONE or ABANDONED
    state = fields["state"].upper()
    if state in DONE_STATES:
        m = LAST_TOUCHED_RE.search(task_content)
        if m:
            fields["end"] = m.group(1).strip()

    return fields


def build_table(task_rows: list[dict]) -> str:
    """
    Build an org-mode table string.

    Each item in task_rows has keys: uuid, title, state, start, end, description.
    """
    header = "| Task | State | Start | End | Description |"
    sep = "|------+-------+-------+-----+-------------|"
    lines = [header, sep]
    for row in task_rows:
        link = f"[[id:{row['uuid']}][{row['title']}]]"
        lines.append(
            f"| {link} | {row['state']} | {row['start']} | {row['end']} | {row['description']} |"
        )
    return "\n".join(lines)


def process_story(story_file: Path, dry_run: bool) -> dict:
    """
    Process one story.org file.

    Returns a result dict with keys:
      slug, status (skipped/updated/no_links/error), rows_added, message
    """
    slug = story_file.parent.name
    result = {"slug": slug, "status": "ok", "rows_added": 0, "message": ""}

    if slug in SKIP_STORIES:
        result["status"] = "skipped"
        result["message"] = "in SKIP_STORIES"
        return result

    try:
        original = story_file.read_text(encoding="utf-8")
    except OSError as e:
        result["status"] = "error"
        result["message"] = str(e)
        return result

    # Skip if Tasks section already has a table
    if "| Task |" in original:
        result["status"] = "skipped"
        result["message"] = "already has task table"
        return result

    m = TASKS_SECTION_RE.search(original)
    if not m:
        result["status"] = "skipped"
        result["message"] = "no * Tasks section found"
        return result

    tasks_body = m.group(2)

    # Parse bullet items with [[id:...][...]] links
    links = LINK_RE.findall(tasks_body)

    if not links:
        # Pattern C — empty / placeholder — replace with empty table header only
        result["status"] = "no_links"
        result["message"] = "no task links; writing empty table"

    story_dir = story_file.parent
    task_rows = []

    for uuid, title in links:
        task_doc = find_task_doc(story_dir, uuid)
        row = {"uuid": uuid.upper(), "title": title, "state": "", "start": "", "end": "", "description": ""}
        if task_doc:
            content = task_doc.read_text(encoding="utf-8")
            fields = extract_task_fields(content)
            row.update(fields)
        else:
            result["message"] += f"[WARN: task doc not found for {uuid}] "
        task_rows.append(row)

    result["rows_added"] = len(task_rows)

    # Build the replacement Tasks section body
    if task_rows:
        new_body = "\n" + build_table(task_rows) + "\n\n"
    else:
        # Empty story — just a clean empty table header
        new_body = "\n| Task | State | Start | End | Description |\n|------+-------+-------+-----+-------------|\n\n"

    # Replace the Tasks section body (preserve the heading itself)
    new_content = TASKS_SECTION_RE.sub(
        lambda mo: mo.group(1) + new_body,
        original,
        count=1,
    )

    if new_content == original:
        result["status"] = "unchanged"
        result["message"] = "content unchanged after replacement"
        return result

    if not dry_run:
        story_file.write_text(new_content, encoding="utf-8")

    if result["status"] == "ok":
        result["status"] = "updated"

    return result


def main() -> None:
    parser = argparse.ArgumentParser(description="Back-fill story task tables for a sprint.")
    parser.add_argument("sprint_dir", nargs="?", type=Path, default=DEFAULT_SPRINT_DIR,
                        help="Path to sprint directory (default: sprint_18)")
    parser.add_argument("--dry-run", action="store_true", help="Show what would change without writing files.")
    args = parser.parse_args()

    sprint_dir = args.sprint_dir
    if not sprint_dir.exists():
        print(f"ERROR: Sprint 18 directory not found: {sprint_dir}", file=sys.stderr)
        sys.exit(1)

    story_files = sorted(sprint_dir.glob("*/story.org"))
    if not story_files:
        print("ERROR: No story.org files found.", file=sys.stderr)
        sys.exit(1)

    print(f"Found {len(story_files)} story files in {sprint_dir}")
    if args.dry_run:
        print("DRY RUN — no files will be written.\n")
    else:
        print()

    updated = []
    skipped = []
    no_links = []
    errors = []
    warnings = []

    for story_file in story_files:
        result = process_story(story_file, dry_run=args.dry_run)
        slug = result["slug"]
        status = result["status"]
        rows = result["rows_added"]
        msg = result["message"].strip()

        if status == "updated":
            verb = "Would update" if args.dry_run else "Updated"
            print(f"  {verb}: {slug} ({rows} task row{'s' if rows != 1 else ''})")
            updated.append(result)
        elif status == "no_links":
            verb = "Would write empty table for" if args.dry_run else "Wrote empty table for"
            print(f"  {verb}: {slug} (no task links)")
            no_links.append(result)
        elif status == "skipped":
            skipped.append(result)
        elif status == "error":
            print(f"  ERROR: {slug}: {msg}", file=sys.stderr)
            errors.append(result)
        elif status == "unchanged":
            print(f"  UNCHANGED: {slug}: {msg}")

        if msg and status == "updated" and "[WARN" in msg:
            print(f"    {msg}")
            warnings.append(result)

    print()
    print("=== Summary ===")
    print(f"  Updated:         {len(updated)}")
    print(f"  Empty tables:    {len(no_links)}")
    print(f"  Skipped:         {len(skipped)}")
    print(f"  Errors:          {len(errors)}")
    if warnings:
        print(f"  Warnings (missing task docs): {len(warnings)}")

    if skipped:
        print()
        print("Skipped stories:")
        for r in skipped:
            print(f"  {r['slug']}: {r['message']}")


if __name__ == "__main__":
    main()
