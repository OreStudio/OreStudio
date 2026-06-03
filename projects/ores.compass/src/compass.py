#!/usr/bin/env python3
"""
Compass: developer toolkit for ORE Studio — orient, scaffold, capture, and search.

Pillars:
  - Orient:   where we are (where/fleet/sprint/story) and what every worktree is doing.
  - Search:   fast NLP/FTS retrieval over Org-Roam notes (search/list/show).
  - Scaffold: create branches and agile artefacts in one step (goto/add).
  - Capture:  file and triage product backlog ideas (capture/inbox/next/backlog).
  - Journal:  per-worktree session log for restart recovery and overlap detection.
"""

import argparse
import csv
import datetime
import json
import os
import re
import shutil
import sqlite3
import subprocess
import sys
import time
from collections import defaultdict
from pathlib import Path

# Make the bundled doc-graph modules importable whether compass.py is run as a
# script (src/ is already on sys.path) or imported from elsewhere. The sys.path
# setup must precede these imports, hence the E402 suppressions.
sys.path.insert(0, str(Path(__file__).resolve().parent))
import doc_index  # noqa: E402
import doc_list   # noqa: E402
import doc_show   # noqa: E402

# --- Dynamic Path Resolution ---
def find_git_root():
    """Walk up directories from the script's location until a .git folder is found."""
    current = Path(__file__).resolve().parent
    for _ in range(10):  # Safety limit
        if (current / ".git").exists():
            return current
        if current == current.parent:
            break
        current = current.parent
    return None

PROJECT_ROOT = find_git_root()
if PROJECT_ROOT is None:
    print("❌ Error: Could not find project root (no .git directory found).", file=sys.stderr)
    print("   Please run this script from within your Org-roam repository.", file=sys.stderr)
    sys.exit(1)

# Normalize to absolute path
PROJECT_ROOT = PROJECT_ROOT.resolve()

ORG_ROAM_DB = str(PROJECT_ROOT / "org-roam.db")
if not os.path.exists(ORG_ROAM_DB):
    ORG_ROAM_DB = str(PROJECT_ROOT / ".org-roam.db")

COMPASS_DB   = str(PROJECT_ROOT / ".compass.db")
JOURNAL_FILE = PROJECT_ROOT / ".journal.org"

def strip_quotes(path):
    """Remove surrounding quotes from a path string."""
    if not path:
        return path
    return path.strip('"').strip("'")

def resolve_path(db_path):
    """Resolve DB paths that may be stale due to symlinks or directory moves."""
    if os.path.exists(db_path):
        return db_path
    repo_name = PROJECT_ROOT.name
    if repo_name and repo_name in db_path:
        parts = db_path.split(repo_name)
        if len(parts) >= 2:
            relative_path = parts[-1].lstrip("/").lstrip("\\")
            candidate = str(PROJECT_ROOT / relative_path)
            if os.path.exists(candidate):
                return candidate
    return db_path

def parse_org_roam_mtime(mtime_value):
    """
    Parse org-roam's mtime format to a Unix timestamp.
    The format appears to be: (seconds nanoseconds ? ?)
    """
    if isinstance(mtime_value, (int, float)):
        return float(mtime_value)

    if isinstance(mtime_value, str):
        # Remove parentheses and split
        cleaned = mtime_value.strip('()')
        parts = [int(x) for x in cleaned.split()]
        if parts:
            # First part is usually seconds since epoch
            return float(parts[0])

    # Fallback: try to convert directly
    try:
        return float(mtime_value)
    except (ValueError, TypeError):
        return 0

def validate_paths(command):
    if not os.path.exists(ORG_ROAM_DB):
        print(f"❌ Error: org-roam.db not found at project root: {ORG_ROAM_DB}", file=sys.stderr)
        print("   Make sure you run 'M-x org-roam-db-sync' in Emacs first.", file=sys.stderr)
        sys.exit(1)

def get_roam_conn():
    uri = f"file:{ORG_ROAM_DB}?mode=ro"
    conn = sqlite3.connect(uri, uri=True)
    conn.row_factory = sqlite3.Row
    return conn

def get_compass_conn():
    conn = sqlite3.connect(COMPASS_DB)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode=WAL;")
    conn.execute("""
        CREATE VIRTUAL TABLE IF NOT EXISTS compass_fts USING fts5(
            roam_id UNINDEXED, file_path UNINDEXED, title, description, tags, content, olp
        );
    """)

    # Check if compass_meta exists and has the right schema
    cursor = conn.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='compass_meta'")
    table_exists = cursor.fetchone()

    if table_exists:
        # Check existing columns
        cursor = conn.execute("PRAGMA table_info(compass_meta)")
        columns = [col['name'] for col in cursor.fetchall()]

        # If old schema (just file_path and mtime), drop and recreate
        if 'mtime' in columns and 'org_roam_mtime' not in columns:
            print("⚠️  Upgrading compass_meta table schema...")
            conn.execute("DROP TABLE compass_meta")
            table_exists = False

    if not table_exists:
        conn.execute("""
            CREATE TABLE compass_meta (
                file_path TEXT PRIMARY KEY,
                org_roam_mtime REAL,
                fs_mtime REAL
            )
        """)

    conn.commit()
    return conn

def cmd_index(args):
    print("Starting Compass index...")
    print(f"📂 Using org-roam.db: {ORG_ROAM_DB}")
    print(f"🌳 Project root:      {PROJECT_ROOT}")

    roam_conn = get_roam_conn()
    file_count = roam_conn.execute("SELECT COUNT(*) FROM files").fetchone()[0]
    print(f"📝 Found {file_count} files registered in org-roam.db")

    if file_count == 0:
        print("⚠️  org-roam.db is empty. Run 'M-x org-roam-db-sync' in Emacs.")
        roam_conn.close()
        sys.exit(1)

    roam_files = roam_conn.execute("SELECT file, mtime FROM files").fetchall()

    compass_conn = get_compass_conn()

    # Get existing indexed files with their mtimes
    indexed_info = {}
    for row in compass_conn.execute("SELECT file_path, org_roam_mtime, fs_mtime FROM compass_meta").fetchall():
        indexed_info[row['file_path']] = {
            'org_roam_mtime': row['org_roam_mtime'],
            'fs_mtime': row['fs_mtime']
        }

    # Clear if rebuilding
    if args.rebuild:
        print("🔄 Rebuilding index from scratch...")
        compass_conn.execute("DELETE FROM compass_fts")
        compass_conn.execute("DELETE FROM compass_meta")
        compass_conn.commit()
        indexed_info = {}

    updated_count = 0
    skipped_count = 0
    missing_files = 0
    total_chunks = 0

    for idx, roam_file in enumerate(roam_files):
        if idx % 100 == 0:
            print(f"  Progress: {idx}/{file_count} files...", end='\r')

        # Strip quotes and resolve path
        original_path = strip_quotes(roam_file['file'])
        file_path = resolve_path(original_path)

        # Parse org-roam mtime
        org_roam_mtime = parse_org_roam_mtime(roam_file['mtime'])

        # Check if file exists on disk
        if not os.path.exists(file_path):
            missing_files += 1
            # If it was indexed but now missing, remove from index
            if file_path in indexed_info:
                compass_conn.execute("DELETE FROM compass_fts WHERE file_path = ?", (file_path,))
                compass_conn.execute("DELETE FROM compass_meta WHERE file_path = ?", (file_path,))
                updated_count += 1
            continue

        # Get filesystem mtime
        fs_mtime = os.path.getmtime(file_path)

        # Check if file needs updating
        needs_update = False
        if args.rebuild:
            needs_update = True
        elif file_path not in indexed_info:
            needs_update = True
        else:
            # Compare mtimes - if either changed, update
            stored_org_mtime = indexed_info[file_path]['org_roam_mtime']
            stored_fs_mtime = indexed_info[file_path]['fs_mtime']

            # Allow small floating point differences
            if abs(org_roam_mtime - stored_org_mtime) > 0.1 or abs(fs_mtime - stored_fs_mtime) > 0.1:
                needs_update = True

        if not needs_update:
            skipped_count += 1
            continue

        # Query nodes using the quoted path from the database
        nodes = roam_conn.execute("""
            SELECT id, level, pos, title, properties, olp,
                   (SELECT GROUP_CONCAT(t.tag, ' ') FROM tags t WHERE t.node_id = nodes.id) as tags
            FROM nodes
            WHERE file = ?
            ORDER BY pos ASC
        """, (roam_file['file'],)).fetchall()

        if not nodes:
            if updated_count < 3:  # Only show first few missing
                print(f"\n  [!] No nodes for: {Path(file_path).name}")
            continue

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                raw_text = f.read()
        except Exception as e:
            print(f"\n  [!] Could not read {file_path}: {e}")
            continue

        # Get description from level 0 node
        description = ""
        for node in nodes:
            if node['level'] == 0 and node['properties']:
                try:
                    props = json.loads(node['properties'])
                    description = props.get('DESCRIPTION', '')
                except (json.JSONDecodeError, TypeError, AttributeError):
                    pass
                break

        # Delete old entries for this file
        compass_conn.execute("DELETE FROM compass_fts WHERE file_path = ?", (file_path,))

        file_chunks = 0

        for i, node in enumerate(nodes):
            # Calculate content range for this node
            start_idx = max(0, node['pos'] - 1)
            if i + 1 < len(nodes):
                end_idx = max(0, nodes[i+1]['pos'] - 1)
            else:
                end_idx = len(raw_text)

            chunk_text = raw_text[start_idx:end_idx].strip()

            if chunk_text:
                tags_str = (node['tags'] or "").strip()
                olp_str = (node['olp'] or "").strip()
                if olp_str.startswith('['):
                    try:
                        olp_str = " > ".join(json.loads(olp_str))
                    except (json.JSONDecodeError, TypeError):
                        pass

                # Use node title, or filename for level 0 if no title
                title = node['title']
                if not title and node['level'] == 0:
                    title = Path(file_path).stem

                compass_conn.execute("""
                    INSERT INTO compass_fts (roam_id, file_path, title, description, tags, content, olp)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                """, (node['id'], file_path, title or "", description, tags_str, chunk_text, olp_str))
                file_chunks += 1

        if file_chunks > 0:
            # Store both mtimes for future comparison
            compass_conn.execute("""
                INSERT OR REPLACE INTO compass_meta (file_path, org_roam_mtime, fs_mtime)
                VALUES (?, ?, ?)
            """, (file_path, org_roam_mtime, fs_mtime))
            updated_count += 1
            total_chunks += file_chunks

            if updated_count <= 5:
                print(f"\n  [+] Indexed: {Path(file_path).name} ({file_chunks} chunks)")

    compass_conn.commit()
    roam_conn.close()
    compass_conn.close()

    print(f"\n\n✅ Done.")
    print(f"   Files updated: {updated_count}")
    print(f"   Files skipped (up to date): {skipped_count}")
    print(f"   Total content chunks: {total_chunks}")
    if missing_files > 0:
        print(f"   Missing on disk: {missing_files}")

def cmd_search(args):
    query = args.query
    if not query.strip():
        print("Please provide a search query.")
        return

    compass_conn = get_compass_conn()

    # Check if index has data
    count = compass_conn.execute("SELECT COUNT(*) as cnt FROM compass_fts").fetchone()['cnt']
    if count == 0:
        print("❌ Index is empty. Run './compass.sh index --rebuild' first.")
        compass_conn.close()
        return

    # Build FTS query
    if '"' not in query and '*' not in query and 'AND' not in query.upper() and 'OR' not in query.upper():
        fts_query = " OR ".join([f"{word}*" for word in query.split()])
    else:
        fts_query = query

    sql = """
        SELECT roam_id, file_path, title, olp, tags,
               snippet(compass_fts, 5, '>>>', '<<<', '...', 20) as snippet
        FROM compass_fts
        WHERE compass_fts MATCH ?
        ORDER BY rank
        LIMIT ?
    """

    try:
        results = compass_conn.execute(sql, (fts_query, args.limit)).fetchall()
    except sqlite3.OperationalError as e:
        print(f"Search syntax error: {e}")
        return

    if not results:
        print("No results found.")
        return

    # Handle different output formats
    if args.format == "json":
        output = []
        for r in results:
            # Get relative path
            try:
                rel_path = Path(r['file_path']).relative_to(PROJECT_ROOT)
            except ValueError:
                rel_path = Path(r['file_path'])

            output.append({
                "uuid": r['roam_id'],
                "path": str(rel_path),
                "title": r['title'] or "Untitled",
                "olp": r['olp'],
                "tags": r['tags'].split() if r['tags'] else [],
                "snippet": r['snippet'].replace('\n', ' ').strip() if r['snippet'] else ""
            })
        print(json.dumps(output, indent=2))

    elif args.format == "line":
        # Single line format: UUID "relative/path" "match text"
        for r in results:
            # Get relative path
            try:
                rel_path = Path(r['file_path']).relative_to(PROJECT_ROOT)
            except ValueError:
                rel_path = Path(r['file_path'])

            snippet = r['snippet'].replace('\n', ' ').strip() if r['snippet'] else ""
            # Extract the matched text (between >>> and <<<)
            match = re.search(r'>>>([^<]+)<<<', snippet)
            matched_text = match.group(1) if match else query

            print(f'{r["roam_id"]} "{rel_path}" "{matched_text}"')

    else:  # Pretty format (default)
        print(f"\nFound {len(results)} results for '{query}':\n" + "-"*50)
        for r in results:
            title = r['title'] or "Untitled"
            olp = r['olp']
            tags = r['tags']

            # Get relative path
            try:
                rel_path = Path(r['file_path']).relative_to(PROJECT_ROOT)
            except ValueError:
                rel_path = Path(r['file_path'])

            header = f"📄 {title}"
            if olp: header += f"  📍 {olp}"
            if tags: header += f"  🏷️  {tags}"

            print(header)
            print(f"   {rel_path}")
            if r['snippet']:
                # Clean up the snippet for display
                clean_snippet = r['snippet'].replace('\n', ' ').strip()
                # Remove the >>> and <<< markers for display
                clean_snippet = re.sub(r'>>>([^<]+)<<<', r'\1', clean_snippet)
                print(f"   ...{clean_snippet}...")
            print("-" * 50)

    compass_conn.close()

def cmd_debug(args):
    """Debug command to inspect the index contents"""
    print("=== Compass Debug Information ===\n")

    if not os.path.exists(COMPASS_DB):
        print(f"❌ Index not found at {COMPASS_DB}")
        return

    compass_conn = get_compass_conn()
    roam_conn = get_roam_conn()

    print("📊 INDEX STATISTICS:")
    fts_count = compass_conn.execute("SELECT COUNT(*) as cnt FROM compass_fts").fetchone()['cnt']
    meta_count = compass_conn.execute("SELECT COUNT(*) as cnt FROM compass_meta").fetchone()['cnt']
    print(f"   Total rows in FTS table: {fts_count}")
    print(f"   Total files in metadata: {meta_count}")

    if args.file:
        print(f"\n🔍 CHECKING FILE: {args.file}")
        file_entry = compass_conn.execute("""
            SELECT file_path, COUNT(*) as chunk_count
            FROM compass_fts
            WHERE file_path LIKE ?
            GROUP BY file_path
        """, (f'%{args.file}%',)).fetchall()

        if file_entry:
            for fe in file_entry:
                try:
                    rel_path = Path(fe['file_path']).relative_to(PROJECT_ROOT)
                except ValueError:
                    rel_path = Path(fe['file_path'])
                print(f"   ✓ {rel_path}: {fe['chunk_count']} chunks")
        else:
            print(f"   ❌ No entries found in compass index")

            # Check if it's in org-roam.db
            roam_check = roam_conn.execute("SELECT COUNT(*) as cnt FROM files WHERE file LIKE ?",
                                          (f'%{args.file}%',)).fetchone()
            if roam_check['cnt'] > 0:
                print(f"   ✓ File IS in org-roam.db but not in compass index")
                print(f"   → Run './compass.sh index --rebuild' to index it")

    compass_conn.close()
    roam_conn.close()

# --- Locate pillar: "where are we in time?" ---
#
# Source of truth: the agile document tree under doc/agile/versions/. Locate
# uses the bundled doc_index (the canonical org-frontmatter parser, which also
# backs the `list` and `show` commands) for discovery, and adds the one field
# it does not expose — the current State, which lives in each doc's "* Status"
# table as a "| State | <STATE> |" row. This keeps Locate dependency-free of
# org-roam.db (no M-x org-roam-db-sync needed) and always fresh against the
# working tree.

STATE_RE = re.compile(r"^\|\s*State\s*\|\s*([A-Z]+)\s*\|", re.MULTILINE)
IN_FLIGHT_STATE = "STARTED"

# Matches the body rows of a "* PRs" table. The header row (PR | Title) and
# separator row are consumed by the leading anchors; each data row must have a
# numeric PR number in the first cell and a non-empty title in the second.
_PRS_SECTION_RE = re.compile(r"^\* PRs\s*\n", re.MULTILINE)
_PRS_ROW_RE = re.compile(r"^\|\s*(\d+)\s*\|\s*([^|\n]+?)\s*\|", re.MULTILINE)

def read_state(path):
    """Return the State value from a doc's Status table, or None if absent."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return None
    match = STATE_RE.search(text)
    return match.group(1) if match else None

def parse_prs_table(path):
    """Return list of (pr_number: int, title: str) from the task's * PRs table."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return []
    section_match = _PRS_SECTION_RE.search(text)
    if not section_match:
        return []
    # Slice from the section heading to the next heading (or EOF)
    section_start = section_match.end()
    next_heading = re.search(r"^\*+ ", text[section_start:], re.MULTILINE)
    section_text = text[section_start: section_start + next_heading.start()] if next_heading else text[section_start:]
    prs = []
    for m in _PRS_ROW_RE.finditer(section_text):
        try:
            num = int(m.group(1))
        except ValueError:
            continue
        title = m.group(2).strip()
        if num and title:
            prs.append((num, title))
    return prs

def fetch_pr_statuses(pr_numbers):
    """
    Fetch live PR state from GitHub for each PR number.
    Returns {pr_number: {"title": str, "state": str, "url": str}}.
    Falls back gracefully if gh is unavailable.
    """
    import subprocess
    result = {}
    for num in sorted(set(pr_numbers)):
        try:
            proc = subprocess.run(
                ["gh", "pr", "view", str(num), "--json", "number,title,state,url"],
                capture_output=True, text=True, timeout=15,
            )
            if proc.returncode == 0:
                data = json.loads(proc.stdout)
                result[num] = {"title": data.get("title", ""), "state": data.get("state", ""), "url": data.get("url", "")}
            else:
                result[num] = {"title": "", "state": "UNKNOWN", "url": ""}
        except Exception:
            result[num] = {"title": "", "state": "UNKNOWN", "url": ""}
    return result

def _parent_dir(rel_path):
    """Directory holding a doc, as a forward-slash relative string."""
    return Path(rel_path).parent.as_posix()

def _seq_key(doc):
    """Ordering key from a version/sprint folder name (e.g. v0, sprint_18).

    Sorts by the trailing integer so v10 > v9 and sprint_100 > sprint_99,
    rather than lexicographically. Falls back to the path for stability.
    """
    name = Path(doc.rel_path).parent.name
    match = re.search(r"(\d+)", name)
    return (int(match.group(1)) if match else -1, doc.rel_path)

def _strip_type_prefix(title):
    """Drop the codegen 'Story: ' / 'Task: ' prefix for display next to a type column."""
    for prefix in ("Story: ", "Task: "):
        if title.startswith(prefix):
            return title[len(prefix):]
    return title

def current_version_sprint(docs):
    """Return (current_version_doc, current_sprint_doc) from a loaded index.

    "Current" = the version/sprint folder with the highest sequence number
    (v0, v1, …, v10; sprint_NN), matching the rule the agile product-owner
    skill uses. Either may be None if no such docs exist.
    """
    versions = [d for d in docs.values() if d.doctype == "version"]
    sprints  = [d for d in docs.values() if d.doctype == "sprint"]
    if not versions or not sprints:
        return None, None
    current_version = max(versions, key=_seq_key)
    version_dir = _parent_dir(current_version.rel_path)
    in_version = [d for d in sprints if d.rel_path.startswith(version_dir + "/")]
    current_sprint = max(in_version or sprints, key=_seq_key)
    return current_version, current_sprint

# --- Branch staleness helpers (Orient pillar) ---

def _git_out(*cmd, cwd):
    """Run a git command; return stripped stdout or None on failure."""
    try:
        p = subprocess.run(["git"] + list(cmd), capture_output=True, text=True,
                           cwd=str(cwd), timeout=5)
        return p.stdout.strip() if p.returncode == 0 else None
    except (OSError, subprocess.SubprocessError):
        return None

def branch_staleness(root):
    """Compute how far the current branch has drifted from origin/main.

    No network call — reads cached origin/main and .git/FETCH_HEAD only.
    Returns a dict:
      branch      - current branch name, or None (detached HEAD)
      ahead       - commits ahead of origin/main
      behind      - commits behind origin/main
      base_age_s  - seconds since merge-base diverged (float), or None
      fetch_age_s - seconds since last git fetch (float), or None
      error       - None | "detached" | "no-remote"
    """
    info = {"branch": None, "ahead": 0, "behind": 0,
            "base_age_s": None, "fetch_age_s": None, "error": None}

    branch = _git_out("symbolic-ref", "--short", "HEAD", cwd=root)
    if branch is None:
        info["error"] = "detached"
        return info
    info["branch"] = branch

    if _git_out("rev-parse", "--verify", "origin/main", cwd=root) is None:
        info["error"] = "no-remote"
        return info

    counts = _git_out("rev-list", "--left-right", "--count", "origin/main...HEAD", cwd=root)
    if counts:
        parts = counts.split()
        if len(parts) == 2 and parts[0].isdigit() and parts[1].isdigit():
            info["behind"] = int(parts[0])
            info["ahead"]  = int(parts[1])

    base = _git_out("merge-base", "HEAD", "origin/main", cwd=root)
    if base:
        ts = _git_out("log", "-1", "--format=%ct", base, cwd=root)
        if ts and ts.isdigit():
            info["base_age_s"] = time.time() - int(ts)

    # Resolve the worktree-specific git dir directly — avoids a subprocess.
    # .git is a directory in the main checkout and a "gitdir: <path>" file in
    # secondary worktrees; FETCH_HEAD lives in whichever dir git fetch writes to.
    dot_git = Path(root) / ".git"
    if dot_git.is_dir():
        gitdir = dot_git
    elif dot_git.is_file():
        content = dot_git.read_text().strip()
        if content.startswith("gitdir:"):
            gp = Path(content.split(":", 1)[1].strip())
            gitdir = gp if gp.is_absolute() else Path(root) / gp
        else:
            gitdir = None
    else:
        gitdir = None
    if gitdir:
        fh = gitdir / "FETCH_HEAD"
        if fh.exists():
            info["fetch_age_s"] = time.time() - fh.stat().st_mtime

    return info

def _staleness_severity(info):
    """Return 'ok', 'warn', or 'stale'."""
    if info.get("error"):
        return "ok"
    behind     = info.get("behind", 0)
    fetch_days = (info.get("fetch_age_s") or 0) / 86400
    if behind >= 25 or fetch_days > 2:
        return "stale"
    if behind >= 10 or fetch_days > 1:
        return "warn"
    return "ok"

def _age_human(seconds):
    """Convert seconds to a compact label: 30m, 4h, 2d."""
    if seconds is None:
        return "?"
    s = int(seconds)
    if s < 3600:
        return f"{s // 60}m"
    if s < 86400:
        return f"{s // 3600}h"
    return f"{s // 86400}d"

_C_GREEN  = "\033[32m"
_C_YELLOW = "\033[33m"
_C_RED    = "\033[31m"
_C_RESET  = "\033[0m"
_C_SEV    = {"ok": _C_GREEN, "warn": _C_YELLOW, "stale": _C_RED}

def staleness_lines(info):
    """Return (chip_line, warning_line_or_None) for a branch_staleness dict."""
    error  = info.get("error")
    branch = info.get("branch") or "(detached)"

    if error == "detached":
        return "⎇  (detached HEAD)", None
    if error == "no-remote":
        return f"⎇  {branch}  (no origin/main)", None

    ahead      = info.get("ahead", 0)
    behind     = info.get("behind", 0)
    base_age   = _age_human(info.get("base_age_s"))
    fetch_age  = _age_human(info.get("fetch_age_s"))
    severity   = _staleness_severity(info)
    col        = _C_SEV[severity]

    if branch == "main":
        if behind == 0:
            chip = f"{col}⎇  main  (up to date, fetched {fetch_age} ago){_C_RESET}"
        else:
            chip = f"{col}⎇  main  ↓{behind}  (fetched {fetch_age} ago){_C_RESET}"
        warning = (f"{_C_YELLOW}⚠  main is behind origin — run: git fetch{_C_RESET}"
                   if behind > 0 else None)
        return chip, warning

    chip = (f"{col}⎇  {branch}  ↑{ahead} ↓{behind}"
            f"  (diverged {base_age} ago, fetched {fetch_age} ago){_C_RESET}")
    if severity == "stale":
        warning = f"{_C_RED}⚠  Branch is stale — run: git fetch && git rebase origin/main{_C_RESET}"
    elif severity == "warn":
        warning = f"{_C_YELLOW}⚠  Branch is drifting — consider: git fetch && git rebase origin/main{_C_RESET}"
    else:
        warning = None
    return chip, warning

def cmd_where(args):
    docs = doc_index.load_all()
    current_version, current_sprint = current_version_sprint(docs)
    if current_version is None or current_sprint is None:
        print("❌ No version/sprint documents found under doc/agile/versions/.",
              file=sys.stderr)
        sys.exit(1)
    sprint_dir = _parent_dir(current_sprint.rel_path)

    # In-flight = stories/tasks under the current sprint with State == STARTED.
    in_flight = []
    for d in docs.values():
        if d.doctype in ("story", "task") and d.rel_path.startswith(sprint_dir + "/"):
            state = read_state(d.path)
            if state == IN_FLIGHT_STATE:
                in_flight.append((d, state))
    in_flight.sort(key=lambda pair: (pair[0].doctype, pair[0].rel_path))

    # --prs: collect PRs from all task files in the current sprint (any state).
    sprint_prs = []   # list of (pr_number, title, task_doc)
    pr_statuses = {}  # pr_number -> {title, state, url}
    if getattr(args, "prs", False):
        all_tasks = [d for d in docs.values()
                     if d.doctype == "task" and d.rel_path.startswith(sprint_dir + "/")]
        all_tasks.sort(key=lambda d: d.rel_path)
        for d in all_tasks:
            for pr_num, pr_title in parse_prs_table(d.path):
                sprint_prs.append((pr_num, pr_title, d))
        if sprint_prs:
            all_nums = [pr_num for pr_num, _, _ in sprint_prs]
            pr_statuses = fetch_pr_statuses(all_nums)

    if args.format == "json":
        def entry(d):
            return {"id": d.id.upper(), "title": d.title, "path": d.rel_path}
        out = {
            "version": entry(current_version),
            "sprint": entry(current_sprint),
            "in_flight": [
                {"type": d.doctype, "id": d.id.upper(), "title": _strip_type_prefix(d.title),
                 "state": state, "path": d.rel_path}
                for d, state in in_flight
            ],
        }
        if getattr(args, "prs", False):
            out["prs"] = [
                {
                    "pr": pr_num,
                    "task_id": td.id.upper(),
                    "task_title": _strip_type_prefix(td.title),
                    "title": pr_statuses.get(pr_num, {}).get("title") or pr_title,
                    "state": pr_statuses.get(pr_num, {}).get("state", "UNKNOWN"),
                    "url": pr_statuses.get(pr_num, {}).get("url", ""),
                }
                for pr_num, pr_title, td in sprint_prs
            ]
        print(json.dumps(out, indent=2))
        return

    print("🧭 ores.compass — where are we?\n")
    chip, warning = staleness_lines(branch_staleness(PROJECT_ROOT))
    print(chip)
    if warning:
        print(warning)
    print()
    print(f"Version:  {current_version.title}  [{current_version.id.upper()}]")
    print(f"          {current_version.rel_path}")
    print(f"Sprint:   {current_sprint.title}  [{current_sprint.id.upper()}]")
    print(f"          {current_sprint.rel_path}")
    print(f"\nIn flight ({IN_FLIGHT_STATE}):")
    if not in_flight:
        print("  (nothing in flight)")
    else:
        for d, state in in_flight:
            print(f"  {d.doctype:<5} {_strip_type_prefix(d.title)}")
            print(f"        [{d.id.upper()}]  {d.rel_path}")

    if getattr(args, "prs", False):
        print(f"\nPRs (sprint {current_sprint.title}):")
        if not sprint_prs:
            print("  (no PRs recorded in task * PRs tables)")
        else:
            for pr_num, pr_title, td in sprint_prs:
                info = pr_statuses.get(pr_num, {})
                live_title = info.get("title") or pr_title
                state_label = info.get("state", "UNKNOWN")
                url = info.get("url", "")
                task_label = _strip_type_prefix(td.title)
                print(f"  #{pr_num:<5} [{state_label:<6}] {live_title}")
                print(f"          task: {task_label}")
                if url:
                    print(f"          {url}")

# --- Fleet: what is every worktree doing? ---
#
# Coordination view across parallel checkouts: for each git worktree, show its
# branch, the task/story it maps to (via the task's #+branch field), and the
# open PR (live state via gh). Each worktree is read in its own tree, so it
# reports its own truth even on branches not yet merged.

_BRANCH_FIELD_RE = re.compile(r"^#\+branch:\s*(\S+)\s*$", re.MULTILINE)
_TITLE_FIELD_RE  = re.compile(r"^#\+title:\s*(.+?)\s*$",   re.MULTILINE)

# --- Journal pillar: per-worktree .journal.org ---

_ORG_LINK_RE = re.compile(r"\[\[id:([A-F0-9-]+)\]\[([^\]]+)\]\]", re.IGNORECASE)

def _lookup_title(uuid):
    """Return the title for a UUID from the doc index, or the UUID itself if not found."""
    try:
        docs = doc_index.load_all()
        doc = docs.get(uuid.lower()) or docs.get(uuid.upper())
        if doc:
            return _strip_type_prefix(doc.title)
    except Exception:
        pass
    return uuid

def _journal_entries(text):
    """Parse .journal.org text into a list of entry dicts."""
    entries = []
    blocks = re.split(r"\n(?=\* \d{4}-)", "\n" + text)
    for block in blocks:
        block = block.strip()
        if not block:
            continue
        m = re.match(r"^\* (\d{4}-\d{2}-\d{2} \d{2}:\d{2}) — (.+)$", block, re.MULTILINE)
        if not m:
            continue
        entry = {"timestamp": m.group(1), "story_link": m.group(2).strip()}
        for field in ("Task", "State", "Branch", "PR"):
            fm = re.search(rf"^\s+- {field} ::\s+(.+)$", block, re.MULTILINE)
            entry[field.lower()] = fm.group(1).strip() if fm else None
        entries.append(entry)
    return entries

def _print_entry(entry):
    """Print one journal entry in the standard fleet-style format."""
    sl = entry["story_link"]
    sm = _ORG_LINK_RE.match(sl)
    story_display = f"{sm.group(2)} ({sm.group(1)[:8]})" if sm else sl

    tl = entry.get("task") or ""
    tm = _ORG_LINK_RE.match(tl)
    task_display = f"{tm.group(2)} ({tm.group(1)[:8]})" if tm else (tl or "—")

    print(f"  ● {entry['timestamp']} — {story_display}")
    print(f"    Task:   {task_display} — {entry.get('state') or '?'}")
    print(f"    Branch: {entry.get('branch') or '—'}")
    print(f"    PR:     {entry.get('pr') or 'none'}")

def _journal_update(args):
    from datetime import datetime
    story_title = _lookup_title(args.story)
    task_title  = _lookup_title(args.task)
    clean_pr = args.pr.lstrip("#") if args.pr else ""
    pr_val = f"#{clean_pr}" if clean_pr.isdigit() else (args.pr or "none")
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M")
    entry = (
        f"* {timestamp} — [[id:{args.story.upper()}][{story_title}]]\n"
        f"  - Task :: [[id:{args.task.upper()}][{task_title}]]\n"
        f"  - State :: {args.state}\n"
        f"  - Branch :: {args.branch}\n"
        f"  - PR :: {pr_val}\n"
    )
    existing = JOURNAL_FILE.read_text(encoding="utf-8") if JOURNAL_FILE.exists() else ""
    new_content = (existing.rstrip("\n") + "\n\n" + entry) if existing.strip() else entry
    tmp = JOURNAL_FILE.with_suffix(".tmp")
    tmp.write_text(new_content, encoding="utf-8")
    tmp.replace(JOURNAL_FILE)
    print(f"📓 journal updated: {timestamp} — {story_title} / {task_title}")
    return 0

def _journal_where():
    if not JOURNAL_FILE.exists() or not JOURNAL_FILE.stat().st_size:
        print("No .journal.org found. Run 'compass journal update' when you pick up a task.")
        return 0
    entries = _journal_entries(JOURNAL_FILE.read_text(encoding="utf-8"))
    if not entries:
        print("No entries in .journal.org.")
        return 0
    _print_entry(entries[-1])
    return 0

def _journal_log():
    if not JOURNAL_FILE.exists() or not JOURNAL_FILE.stat().st_size:
        print("No .journal.org found.")
        return 0
    entries = _journal_entries(JOURNAL_FILE.read_text(encoding="utf-8"))
    if not entries:
        print("No entries in .journal.org.")
        return 0
    print(f"📓 ores.compass — session journal ({len(entries)} {'entry' if len(entries) == 1 else 'entries'})\n")
    for entry in entries:
        _print_entry(entry)
        print()
    return 0

def cmd_journal(argv):
    """compass journal — read and write the per-worktree session journal (.journal.org)."""
    ap = argparse.ArgumentParser(
        prog="compass journal",
        description="Read and write .journal.org — the per-worktree Claude session journal.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    up = sub.add_parser("update", help="Append a new entry to .journal.org")
    up.add_argument("--story",  required=True, help="Story UUID")
    up.add_argument("--task",   required=True, help="Task UUID")
    up.add_argument("--branch", required=True, help="Current git branch name")
    up.add_argument("--state",  default="STARTED", help="Task state (default: STARTED)")
    up.add_argument("--pr",     default="none",    help="PR number or 'none' (default: none)")

    sub.add_parser("where", help="Show the last journal entry (where was I?)")
    sub.add_parser("log",   help="Show all journal entries in chronological order")

    args = ap.parse_args(argv)
    if args.subcmd == "update":
        return _journal_update(args)
    if args.subcmd == "where":
        return _journal_where()
    if args.subcmd == "log":
        return _journal_log()

def list_worktrees():
    """Return [(path, branch_or_None)] from `git worktree list --porcelain`."""
    try:
        out = subprocess.run(["git", "worktree", "list", "--porcelain"],
                             capture_output=True, text=True, cwd=str(PROJECT_ROOT),
                             timeout=15)
    except (OSError, subprocess.SubprocessError):
        return []
    worktrees, path, branch = [], None, None
    for line in out.stdout.splitlines():
        if line.startswith("worktree "):
            if path is not None:
                worktrees.append((path, branch))
            path, branch = line[len("worktree "):], None
        elif line.startswith("branch "):
            branch = re.sub(r"^refs/heads/", "", line[len("branch "):])
    if path is not None:
        worktrees.append((path, branch))
    return worktrees

def open_prs_by_branch():
    """Map head branch -> {number,title,state,url} for open PRs, via gh. {} on failure."""
    try:
        out = subprocess.run(
            ["gh", "pr", "list", "--state", "open", "--limit", "200",
             "--json", "number,title,state,url,headRefName"],
            capture_output=True, text=True, cwd=str(PROJECT_ROOT), timeout=20)
        if out.returncode != 0:
            return {}
        prs = json.loads(out.stdout)
    except (OSError, subprocess.SubprocessError, json.JSONDecodeError):
        return {}
    return {pr["headRefName"]: pr for pr in prs if pr.get("headRefName")}

def task_for_branch(worktree_path, branch):
    """Find a task in this worktree's tree whose #+branch == branch.

    Returns (task_title, story_title) or (None, None). Read per-worktree so a
    branch's task is found even if the doc only exists on that branch.
    """
    base = Path(worktree_path) / "doc" / "agile" / "versions"
    if not base.exists():
        return None, None
    for task_file in sorted(base.glob("*/sprint_*/**/task_*.org")):
        try:
            text = task_file.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        m = _BRANCH_FIELD_RE.search(text)
        if not m or m.group(1) != branch:
            continue
        tm = _TITLE_FIELD_RE.search(text)
        task_title = _strip_type_prefix(tm.group(1)) if tm else task_file.stem
        story_title = None
        story_file = task_file.parent / "story.org"
        if story_file.exists():
            try:
                sm = _TITLE_FIELD_RE.search(story_file.read_text(encoding="utf-8"))
                story_title = _strip_type_prefix(sm.group(1)) if sm else None
            except (OSError, UnicodeDecodeError):
                pass
        return task_title, story_title
    return None, None

def _journal_last_entry(worktree_path):
    """Return the last entry dict from a worktree's .journal.org, or None."""
    jf = Path(worktree_path) / ".journal.org"
    try:
        if not jf.exists() or not jf.stat().st_size:
            return None
        entries = _journal_entries(jf.read_text(encoding="utf-8"))
        return entries[-1] if entries else None
    except (OSError, UnicodeDecodeError):
        return None

_STATE_RE  = re.compile(r"^\| State[^|]*\|\s*([A-Z]+)", re.MULTILINE)
_TITLE_RE2 = re.compile(r"^#\+title:\s*(?:Story:\s*)?(.+?)\s*$", re.MULTILINE | re.IGNORECASE)

_BRANCH_FIELD_RE2 = re.compile(r"^#\+branch:[ \t\r]*(\S+)[ \t\r]*$", re.MULTILINE)
_PR_FIELD_RE      = re.compile(r"^#\+pr:[ \t\r]*(\S+)[ \t\r]*$", re.MULTILINE)

def _read_task_detail(task_file):
    """Return (title, state, uuid, branch, pr) from a task.org file."""
    try:
        text = task_file.read_text(encoding="utf-8")
        tm = _TITLE_RE2.search(text)
        sm = _STATE_RE.search(text)
        im = _ORG_ID_RE.search(text)
        bm = _BRANCH_FIELD_RE2.search(text)
        pm = _PR_FIELD_RE.search(text)
        title  = _strip_type_prefix(tm.group(1)) if tm else task_file.stem
        state  = sm.group(1).upper() if sm else "UNKNOWN"
        uuid   = im.group(1) if im else None
        branch = bm.group(1) if bm else None
        pr     = pm.group(1) if pm else None
        return title, state, uuid, branch, pr
    except (OSError, UnicodeDecodeError):
        return task_file.stem, "UNKNOWN", None, None, None

def _read_story_state(story_file):
    """Return (title, state, uuid) from a story.org file, or (stem, 'UNKNOWN', None)."""
    try:
        text = story_file.read_text(encoding="utf-8")
        tm = _TITLE_RE2.search(text)
        sm = _STATE_RE.search(text)
        im = _ORG_ID_RE.search(text)
        title = tm.group(1) if tm else story_file.parent.name
        state = sm.group(1).upper() if sm else "UNKNOWN"
        uuid  = im.group(1) if im else None
        return title, state, uuid
    except (OSError, UnicodeDecodeError):
        return story_file.parent.name, "UNKNOWN", None

def _task_counts(story_dir):
    """Return (done, total) task count for a story directory."""
    done = total = 0
    for tf in sorted(story_dir.glob("task_*.org")):
        try:
            text = tf.read_text(encoding="utf-8")
            m = _STATE_RE.search(text)
            state = m.group(1).upper() if m else "UNKNOWN"
            total += 1
            if state == "DONE":
                done += 1
        except (OSError, UnicodeDecodeError):
            pass
    return done, total

# --- Sprint charts helpers ---

def _sc_parse_date(s):
    parts = s.strip().split("-")
    return datetime.date(int(parts[0]), int(parts[1]), int(parts[2]))


def _sc_get_sprint_range(sprint):
    sprint_file = PROJECT_ROOT / f"doc/agile/versions/v0/sprint_{sprint:02d}/sprint.org"
    if not sprint_file.exists():
        now = datetime.date.today()
        return now - datetime.timedelta(days=14), now
    text = sprint_file.read_text(encoding="utf-8")
    start = end = None
    for line in text.splitlines():
        if line.startswith("| Start") and "|" in line:
            m = re.search(r"\d{4}-\d{2}-\d{2}", line)
            if m:
                start = _sc_parse_date(m.group())
        if (line.startswith("| End") or "End (expected)" in line) and "|" in line:
            m = re.search(r"\d{4}-\d{2}-\d{2}", line)
            if m:
                end = _sc_parse_date(m.group())
    if start and end:
        return start, end
    if start is None:
        m = re.search(r"#\+created:\s*(\d{4}-\d{2}-\d{2})", text)
        start = _sc_parse_date(m.group(1)) if m else (datetime.date.today() - datetime.timedelta(days=7))
    end = end or (start + datetime.timedelta(days=7))
    return start, end


def _sc_collect_daily_metrics(start, end):
    metrics = defaultdict(lambda: {"commits": 0, "merges": 0, "added": 0, "deleted": 0})
    end_inc = end + datetime.timedelta(days=1)
    log = subprocess.check_output([
        "git", "log", f"--after={start}", f"--before={end_inc}",
        "--format=COMMIT %ai", "--numstat", "--reverse", "origin/main",
    ], text=True, cwd=PROJECT_ROOT).strip()
    current_date = None
    for line in log.splitlines():
        if line.startswith("COMMIT "):
            current_date = _sc_parse_date(line.split()[1])
            if current_date:
                metrics[current_date]["commits"] += 1
        elif current_date and "\t" in line:
            parts = line.split("\t")
            if len(parts) == 3 and parts[0] not in ("-", ""):
                try:
                    metrics[current_date]["added"] += int(parts[0])
                    metrics[current_date]["deleted"] += int(parts[1])
                except ValueError:
                    pass
    log = subprocess.check_output([
        "git", "log", "--merges", f"--after={start}", f"--before={end_inc}",
        "--format=%ai", "--reverse", "origin/main",
    ], text=True, cwd=PROJECT_ROOT).strip()
    for line in log.splitlines():
        if line.strip():
            metrics[_sc_parse_date(line.split()[0])]["merges"] += 1
    return metrics


def _sc_get_merged_prs(start, end):
    if not shutil.which("gh"):
        print("Warning: gh CLI not found — skipping PR cycle time data", file=sys.stderr)
        return []
    try:
        output = subprocess.check_output([
            "gh", "pr", "list", "--state", "merged",
            "--search", f"merged:>={start}",
            "--json", "number,title,createdAt,mergedAt",
            "--limit", "200",
        ], text=True).strip()
    except subprocess.CalledProcessError as e:
        print(f"Warning: gh pr list failed: {e}", file=sys.stderr)
        return []
    prs = []
    for pr in json.loads(output):
        merged_str = pr.get("mergedAt", "")
        if not merged_str:
            continue
        merged_dt = datetime.datetime.fromisoformat(merged_str.replace("Z", "+00:00"))
        if merged_dt.date() > end:
            continue
        created_dt = datetime.datetime.fromisoformat(pr["createdAt"].replace("Z", "+00:00"))
        cycle_hours = round((merged_dt - created_dt).total_seconds() / 3600, 1)
        prs.append({
            "number": pr["number"],
            "title": (pr.get("title") or "")[:60],
            "merged_day": merged_dt.date(),
            "cycle_hours": cycle_hours,
        })
    return prs


def _sc_parse_sprint_stories(sprint, start, end):
    stories_file = PROJECT_ROOT / f"doc/agile/versions/v0/sprint_{sprint:02d}/sprint.org"
    if not stories_file.exists():
        return defaultdict(int)
    text = stories_file.read_text(encoding="utf-8")
    cumulative = defaultdict(int)
    in_table = False
    for line in text.splitlines():
        if line.startswith("| Story"):
            in_table = True
            continue
        if in_table and line.strip() == "":
            in_table = False
            continue
        if in_table and "|" in line and line.count("|") >= 5:
            parts = [p.strip() for p in line.split("|")]
            if len(parts) >= 6:
                state, end_str = parts[2], parts[4]
                if state:
                    m = re.search(r"\d{4}-\d{2}-\d{2}", end_str)
                    if m:
                        try:
                            end_d = _sc_parse_date(m.group())
                            if start <= end_d <= end:
                                cumulative[end_d] += 1
                        except Exception:
                            pass
    return cumulative


def _sc_write_activity(metrics, output_dir):
    path = output_dir / "sprint_activity.csv"
    with open(path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["day", "merges", "commits", "added", "deleted"])
        for d in sorted(metrics):
            m = metrics[d]
            w.writerow([d.isoformat(), m["merges"], m["commits"], m["added"], m["deleted"]])
    print(f"Wrote {path}")


def _sc_write_progress(cumulative, prs, output_dir):
    path = output_dir / "sprint_progress.csv"
    with open(path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["day", "cumulative_done"])
        running = 0
        for d in sorted(cumulative):
            running += cumulative[d]
            w.writerow([d.isoformat(), running])
        if not cumulative:
            w.writerow(["2000-01-01", 0])
    print(f"Wrote {path}")
    cycle_path = output_dir / "pr_cycle_times.csv"
    meaningful = sorted(
        [pr for pr in prs if pr["cycle_hours"] > 0],
        key=lambda x: x["cycle_hours"], reverse=True,
    )[:20]
    with open(cycle_path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["pr_number", "title", "day", "cycle_hours"])
        for pr in sorted(meaningful, key=lambda x: x["merged_day"]):
            w.writerow([pr["number"], pr["title"], pr["merged_day"].isoformat(), pr["cycle_hours"]])
    print(f"Wrote {cycle_path}")


def _sc_sprint_number_from_doc(sprint_doc):
    """Extract the integer sprint number from a sprint doc path (sprint_19 → 19)."""
    m = re.search(r"sprint_(\d+)", sprint_doc.rel_path)
    return int(m.group(1)) if m else None


_SC_GNUPLOT_SCRIPTS = [
    "sprint_prs_commits.gnuplot",
    "sprint_line_churn.gnuplot",
    "sprint_pr_cycle.gnuplot",
    "sprint_stories_done.gnuplot",
]


def cmd_sprint_charts(args):
    """Implement compass sprint charts."""
    if not shutil.which("gnuplot"):
        print("❌ gnuplot not found on PATH. Install gnuplot to render sprint charts.",
              file=sys.stderr)
        return 1

    if args.sprint:
        sprint = args.sprint
    else:
        _, current_sprint = current_version_sprint(doc_index.load_all())
        if current_sprint is None:
            print("❌ No current sprint found.", file=sys.stderr)
            return 1
        sprint = _sc_sprint_number_from_doc(current_sprint)
        if sprint is None:
            print("❌ Could not determine sprint number from path.", file=sys.stderr)
            return 1
        print(f"Auto-detected sprint: {sprint}")

    start, end = _sc_get_sprint_range(sprint)
    if args.start_date:
        try:
            start = _sc_parse_date(args.start_date)
        except (ValueError, IndexError):
            print(f"❌ Invalid --start-date: {args.start_date!r} (expected YYYY-MM-DD)", file=sys.stderr)
            return 1
    if args.end_date:
        try:
            end = _sc_parse_date(args.end_date)
        except (ValueError, IndexError):
            print(f"❌ Invalid --end-date: {args.end_date!r} (expected YYYY-MM-DD)", file=sys.stderr)
            return 1

    print(f"Sprint {sprint}: {start} → {end}")

    output_dir = Path(args.output_dir) if args.output_dir else \
        PROJECT_ROOT / f"build/output/sprint_{sprint:02d}"
    output_dir.mkdir(parents=True, exist_ok=True)

    daily = _sc_collect_daily_metrics(start, end)
    prs = _sc_get_merged_prs(start, end)
    cumulative = _sc_parse_sprint_stories(sprint, start, end)

    _sc_write_activity(daily, output_dir)
    _sc_write_progress(cumulative, prs, output_dir)

    print(f"Found {len(daily)} days of activity, {len(prs)} merged PRs, "
          f"{sum(cumulative.values())} story transitions")

    print("Rendering charts with gnuplot...")
    for script in _SC_GNUPLOT_SCRIPTS:
        script_path = PROJECT_ROOT / "scripts" / script
        if not script_path.exists():
            print(f"  ⚠️  skipping {script} (not found)", file=sys.stderr)
            continue
        subprocess.check_call(
            ["gnuplot", "-e", f"sprint={sprint}", str(script_path)],
            cwd=PROJECT_ROOT,
        )
        print(f"  Rendered {script}")

    return 0


def cmd_sprint(argv):
    """compass sprint — sprint-level operations."""
    ap = argparse.ArgumentParser(prog="compass sprint",
                                 description="Sprint-level operations.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    st = sub.add_parser("status", help="All stories in the current sprint grouped by state")
    st.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty")
    st.add_argument("--uuids", action="store_true",
                    help="Show story UUIDs alongside titles for use with other commands")

    ch = sub.add_parser("charts",
                        help="Generate sprint metric CSVs for gnuplot chart rendering")
    ch.add_argument("--sprint", type=int, default=None,
                    help="Sprint number (default: auto-detect current sprint)")
    ch.add_argument("--start-date", default=None,
                    help="Override sprint start date (YYYY-MM-DD)")
    ch.add_argument("--end-date", default=None,
                    help="Override sprint end date (YYYY-MM-DD)")
    ch.add_argument("--output-dir", default=None,
                    help="Output directory (default: build/output/sprint_NN/)")

    args = ap.parse_args(argv)

    if args.subcmd == "charts":
        return cmd_sprint_charts(args)

    if args.subcmd == "status":
        _, current_sprint = current_version_sprint(doc_index.load_all())
        if current_sprint is None:
            print("❌ No current sprint found.", file=sys.stderr)
            return 1
        sprint_dir = Path(PROJECT_ROOT) / _parent_dir(current_sprint.rel_path)
        if not sprint_dir.exists():
            print(f"❌ Sprint directory not found: {sprint_dir}", file=sys.stderr)
            return 1

        stories = []
        for sf in sorted(sprint_dir.glob("*/story.org")):
            title, state, uuid = _read_story_state(sf)
            done, total = _task_counts(sf.parent)
            stories.append({"title": title, "state": state, "uuid": uuid,
                            "tasks_done": done, "tasks_total": total,
                            "path": str(sf.relative_to(PROJECT_ROOT))})

        if args.format == "json":
            print(json.dumps({"sprint": current_sprint.title, "stories": stories}, indent=2))
            return 0

        order = {"DONE": 0, "STARTED": 1, "BLOCKED": 2, "BACKLOG": 3}
        stories.sort(key=lambda s: (order.get(s["state"], 9), s["state"], s["title"]))

        print(f"🧭 ores.compass — sprint status: {current_sprint.title}\n")
        current_state = None
        for s in stories:
            if s["state"] != current_state:
                current_state = s["state"]
                print(f"  {current_state}")
            tasks = f"{s['tasks_done']}/{s['tasks_total']}" if s["tasks_total"] else "—"
            uuid_suffix = f"  [{s['uuid']}]" if args.uuids and s["uuid"] else ""
            print(f"    [{tasks}] {s['title']}{uuid_suffix}")
        return 0

def _org_link_text(link_str):
    """Extract plain title from an org-roam [[id:...][Title]] link, or return as-is."""
    m = _ORG_LINK_RE.match(link_str or "")
    return m.group(2) if m else link_str

def cmd_fleet(args):
    worktrees = list_worktrees()
    if not worktrees:
        print("❌ Could not enumerate git worktrees.", file=sys.stderr)
        sys.exit(1)
    here = str(PROJECT_ROOT)
    pr_map = open_prs_by_branch()

    rows = []
    for path, branch in worktrees:
        journal = _journal_last_entry(path)
        if journal:
            story_title = _org_link_text(journal.get("story_link"))
            task_title  = _org_link_text(journal.get("task"))
            task_state  = journal.get("state")
            journal_branch = journal.get("branch")
        else:
            task_title = story_title = task_state = journal_branch = None
            if branch:
                task_title, story_title = task_for_branch(path, branch)

        pr = pr_map.get(branch) if branch else None
        rows.append({
            "worktree": Path(path).name,
            "path": path,
            "current": os.path.realpath(path) == os.path.realpath(here),
            "branch": branch,
            "story": story_title,
            "task": task_title,
            "task_state": task_state,
            "journal_branch": journal_branch,
            "journal": bool(journal),
            "pr": ({"number": pr["number"], "state": pr["state"], "url": pr["url"]}
                   if pr else None),
            "staleness": branch_staleness(path),
        })

    if args.format == "json":
        print(json.dumps(rows, indent=2))
        return

    print(f"🧭 ores.compass — fleet ({len(rows)} worktrees)\n")
    for r in rows:
        mark = "→" if r["current"] else " "
        branch = r["branch"] or "(detached)"
        chip, warning = staleness_lines(r["staleness"])
        print(f"{mark} {r['worktree']}   {branch}")
        print(f"      {chip}")
        if warning:
            print(f"      {warning}")
        if r["task"] or r["story"]:
            state_suffix = f" [{r['task_state']}]" if r["task_state"] else ""
            source = "" if r["journal"] else " (from branch)"
            print(f"      story: {r['story'] or '—'}{source}")
            print(f"      task:  {r['task'] or '—'}{state_suffix}")
        elif r["branch"] and r["branch"] != "main":
            print("      (no journal or task records this branch)")
        if r["pr"]:
            print(f"      PR:    #{r['pr']['number']} [{r['pr']['state']}]  {r['pr']['url']}")

# --- Scaffold pillar: create agile/doc artefacts ---
#
# Generation belongs to ores.codegen, not compass: `add` calls codegen's
# doc_generate *as a library* (no shelling out, no copied generator). For
# the unambiguous cases it fills --parent-dir from the current sprint/version
# via Locate, so `compass add story --title ...` lands in the open sprint.

# Types whose parent compass can resolve from "where we are".
_DEFAULTABLE_PARENT = {"story": "sprint", "sprint": "version"}

def _default_parent_dir(doc_type):
    """Parent dir derived from the current sprint/version, or None."""
    parent_kind = _DEFAULTABLE_PARENT.get(doc_type)
    if parent_kind is None:
        return None
    current_version, current_sprint = current_version_sprint(doc_index.load_all())
    if parent_kind == "version" and current_version is not None:
        return _parent_dir(current_version.rel_path)
    if parent_kind == "sprint" and current_sprint is not None:
        return _parent_dir(current_sprint.rel_path)
    return None

BACKLOG_BUCKETS = ("next", "deferred", "discarded")
BACKLOG_ROOT = Path("doc") / "agile" / "product_backlog"

ALL_BUCKETS = ("inbox", *BACKLOG_BUCKETS)

def _inbox_dir():
    """Return the absolute path to the inbox/ backlog bucket."""
    return Path(PROJECT_ROOT) / BACKLOG_ROOT / "inbox"

def cmd_backlog(bucket, extra_args=None):
    """List captures in a product backlog bucket (inbox/next/deferred/discarded)."""
    under = str(BACKLOG_ROOT / bucket)
    args = ["--type", "capture", "--under", under, "--sort", "title"]
    if extra_args:
        args.extend(extra_args)
    try:
        return doc_list.main(args)
    except SystemExit as e:
        return e.code

def _backlog_dir(bucket):
    return Path(PROJECT_ROOT) / "doc" / "agile" / "product_backlog" / bucket

def _find_capture(slug):
    """Find a capture by slug in inbox/ or any backlog bucket. Returns Path or None."""
    backlog_root = Path(PROJECT_ROOT) / BACKLOG_ROOT
    for bucket in ("inbox", *BACKLOG_BUCKETS):
        candidate = backlog_root / bucket / f"{slug}.org"
        if candidate.exists():
            return candidate
    return None

def cmd_capture(argv):
    """Manage product backlog captures.

    Subcommands:
      (default)  --note "..."  [--slug <slug>]        Create a capture in inbox/.
      file <slug> next|deferred|discarded             Move an inbox capture to a bucket.
      promote <slug>                                  Show instructions to promote to story/task.
    """
    if not argv or argv[0] in ("-h", "--help"):
        print(
            "usage:\n"
            "  compass capture --note \"...\" [--slug <slug>]\n"
            "      Create a capture in doc/agile/product_backlog/inbox/.\n"
            "  compass capture file <slug> next|deferred|discarded\n"
            "      Move an inbox capture to a triaged product backlog bucket.\n"
            "  compass capture promote <slug>\n"
            "      Print instructions for promoting a capture to a story or task.\n"
        )
        return 0

    subcommand = argv[0]

    # --- file subcommand ---
    if subcommand == "file":
        if len(argv) < 3:
            print("❌ usage: compass capture file <slug> next|deferred|discarded", file=sys.stderr)
            return 1
        slug, bucket = argv[1], argv[2]
        if bucket not in BACKLOG_BUCKETS:
            print(f"❌ bucket must be one of: {', '.join(BACKLOG_BUCKETS)}", file=sys.stderr)
            return 1
        src = _find_capture(slug)
        if src is None:
            print(f"❌ No capture found for slug '{slug}'.", file=sys.stderr)
            return 1
        dst = _backlog_dir(bucket) / f"{slug}.org"
        if dst.exists():
            print(f"❌ {dst} already exists.", file=sys.stderr)
            return 1
        # Update bucket tag in filetags to reflect new location, then git mv.
        text = src.read_text(encoding="utf-8")
        # Replace any existing bucket tag with the new one.
        for old_bucket in ("inbox", *BACKLOG_BUCKETS):
            text = re.sub(rf":{re.escape(old_bucket)}:", f":{bucket}:", text)
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_text(text, encoding="utf-8")
        result = subprocess.run(
            ["git", "mv", str(src), str(dst)],
            cwd=str(PROJECT_ROOT), capture_output=True, text=True)
        if result.returncode != 0:
            # git mv failed (e.g. file not tracked yet); fall back to plain move.
            src.unlink()
        print(f"✅ Filed '{slug}' to {bucket} backlog: {dst.relative_to(PROJECT_ROOT)}")
        print("ℹ️  Re-run regenerate_backlog_indexes.py to update the bucket index.")
        return 0

    # --- promote subcommand ---
    if subcommand == "promote":
        if len(argv) < 2:
            print("❌ usage: compass capture promote <slug>", file=sys.stderr)
            return 1
        slug = argv[1]
        src = _find_capture(slug)
        if src is None:
            print(f"❌ No capture found for slug '{slug}'.", file=sys.stderr)
            return 1
        print(f"Capture: {src.relative_to(PROJECT_ROOT)}")
        print("\nTo promote to a new story:")
        print(f"  compass goto --slug {slug} --title \"<title>\" --description \"<desc>\" --tags \"<tags>\"")
        print(f"  Then delete: {src.relative_to(PROJECT_ROOT)}")
        print("\nTo promote to a task on an existing story:")
        print(f"  compass goto --story <id-or-slug> --task-slug {slug} --task \"<title>\"")
        print(f"  Then delete: {src.relative_to(PROJECT_ROOT)}")
        return 0

    # --- default: create a new capture in inbox ---
    ap = argparse.ArgumentParser(prog="compass capture",
                                 description="Create a capture in the product backlog inbox.")
    ap.add_argument("--note", required=True, help="The note text (used as description and body).")
    ap.add_argument("--slug", default="", help="Snake_case slug (auto-generated from note if omitted).")
    ap.add_argument("--title", default="", help="Title (defaults to first 60 chars of note).")
    ap.add_argument("--tags", default="", help="Comma-separated tags.")
    ap.add_argument("--dry-run", action="store_true", help="Print plan without creating the file.")
    args = ap.parse_args(argv)

    inbox_dir = _inbox_dir()
    slug = args.slug or re.sub(r"[^a-z0-9]+", "_", args.note[:50].lower()).strip("_")
    title = args.title or (args.note[:60] + ("…" if len(args.note) > 60 else ""))
    out_file = inbox_dir / f"{slug}.org"

    if args.dry_run:
        print(f"compass capture — plan (dry run):")
        print(f"  file:  {out_file.relative_to(PROJECT_ROOT)}")
        print(f"  slug:  {slug}")
        print(f"  title: {title}")
        return 0

    gen = _import_generator()
    inbox_dir.mkdir(parents=True, exist_ok=True)
    try:
        rc = gen.main(["--type", "capture", "--slug", slug,
                       "--parent-dir", str(inbox_dir.relative_to(PROJECT_ROOT)),
                       "--title", title, "--description", args.note,
                       "--tags", args.tags or "capture"])
    except SystemExit as exc:
        rc = exc.code
    if rc in (None, 0):
        print(f"✅ Product backlog note created: {out_file.relative_to(PROJECT_ROOT)}")
        print("ℹ️  Use 'compass capture file <slug> next|deferred|discarded' to move to backlog,")
        print("   or 'compass capture promote <slug>' for instructions to make it a story/task.")
        return 0
    return rc or 1

_PUML_HEADER_TEMPLATE = """\
' -*- mode: plantuml; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
'
' Copyright (C) {year} Marco Craveiro <marco.craveiro@gmail.com>
'
' This program is free software; you can redistribute it and/or modify it under
' the terms of the GNU General Public License as published by the Free Software
' Foundation; either version 3 of the License, or (at your option) any later
' version.
'
' This program is distributed in the hope that it will be useful, but WITHOUT
' ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
' FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License along with
' this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
' Street, Fifth Floor, Boston, MA 02110-1301, USA.
'
@startuml

title {title}

@enduml
"""


def cmd_add_diagram(argv):
    """Scaffold a .puml file with the standard licence header.

    Usage: compass add diagram <slug> [--parent-dir DIR] [--title TITLE]
    """
    import argparse as _ap
    p = _ap.ArgumentParser(prog="compass add diagram")
    p.add_argument("slug", help="snake_case slug; output will be <slug>.puml")
    p.add_argument("--parent-dir", default=".", metavar="DIR",
                   help="Directory to write into (default: current directory)")
    p.add_argument("--title", default="", metavar="TITLE",
                   help="Title line inside the diagram (default: derived from slug)")
    try:
        args = p.parse_args(argv)
    except SystemExit as exc:
        return exc.code or 0

    from datetime import date as _date
    year = _date.today().year

    parent = Path(args.parent_dir)
    if not parent.is_dir():
        print(f"❌ Directory not found: {parent}", file=sys.stderr)
        return 1

    out = parent / f"{args.slug}.puml"
    if out.exists():
        print(f"❌ Refusing to overwrite {out}", file=sys.stderr)
        return 1

    title = args.title or args.slug.replace("_", " ").replace("-", " ").title()
    content = _PUML_HEADER_TEMPLATE.format(year=year, title=title)
    out.write_text(content, encoding="utf-8")
    try:
        display = out.relative_to(PROJECT_ROOT)
    except ValueError:
        display = out
    print(f"✅ Diagram scaffolded: {display}")
    print(f"   Edit {out.name}, then run: plantuml {out.name}")
    return 0


def cmd_add(argv):
    """Create a doc by calling ores.codegen as a library.

    argv is [<type>, <generate_doc flags...>]. When --parent-dir is
    omitted, default it from the current sprint (story) or version (sprint).
    """
    if not argv or argv[0] in ("-h", "--help"):
        print("usage: compass add <type> [generate_doc options]\n"
              "  types: story task sprint version recipe knowledge component\n"
              "         capture memory investigation product_identity skill\n"
              "         diagram entity_org dataset_overview\n"
              "  --parent-dir defaults to the current sprint (story) or\n"
              "  version (sprint), or doc/llm/skills (skill); required otherwise.\n"
              "  diagram: scaffolds a .puml file with the standard licence header.\n"
              "  entity_org: scaffolds projects/ores.<component>/modeling/\n"
              "    ores.<component>.<slug>.org; requires --component, --slug,\n"
              "    --description.\n"
              "  dataset_overview: scaffolds projects/ores.seeder/datasets/<name>/\n"
              "    dataset_overview.org; requires --dataset, --description.\n"
              "  remaining flags are passed through to ores.codegen "
              "(see 'How do I create a new v2 doc?').")
        return 0

    doc_type = argv[0]
    rest = list(argv[1:])

    if doc_type == "diagram":
        return cmd_add_diagram(rest)

    has_parent = any(a == "--parent-dir" or a.startswith("--parent-dir=")
                     for a in rest)
    if not has_parent:
        if doc_type == "skill":
            default_parent = "doc/llm/skills"
            rest += ["--parent-dir", default_parent]
            print(f"ℹ️  --parent-dir not given; using default: {default_parent}",
                  file=sys.stderr)
        else:
            default_parent = _default_parent_dir(doc_type)
            if default_parent:
                rest += ["--parent-dir", default_parent]
                print(f"ℹ️  --parent-dir not given; using current "
                      f"{_DEFAULTABLE_PARENT[doc_type]}: {default_parent}", file=sys.stderr)

    # Lazy, optional import: the generator lives in ores.codegen and needs
    # pystache. Only `add` / `goto` require it; the other commands stay
    # dependency-free.
    doc_generate = _import_generator()

    # doc_generate.main returns 0 on success but may sys.exit on error;
    # catch SystemExit so the reminder prints only on success and the
    # generator's own error message/code is preserved.
    try:
        rc = doc_generate.main(["--type", doc_type, *rest])
    except SystemExit as exc:
        rc = exc.code
    if rc in (None, 0):
        print("ℹ️  Remember to wire the new artefact into its parent "
              "(e.g. the sprint's * Stories table) where needed.", file=sys.stderr)
        return 0
    if isinstance(rc, str):   # sys.exit("message") — surface it
        print(rc, file=sys.stderr)
        return 1
    return rc

def _import_generator():
    """Import ores.codegen's doc_generate (needs pystache). Exit on failure."""
    sys.path.insert(0, str(PROJECT_ROOT / "projects" / "ores.codegen" / "src"))
    try:
        import doc_generate
        return doc_generate
    except ImportError as exc:
        print(f"❌ This needs ores.codegen's generator ({exc}). Install its "
              f"dependency into the compass venv: pip install pystache",
              file=sys.stderr)
        sys.exit(1)

def _set_frontmatter_branch(path, branch):
    """Set the #+branch field of a task doc (so fleet/where pick it up)."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except OSError:
        return
    new, count = re.subn(r"^#\+branch:.*$", f"#+branch: {branch}", text, count=1,
                         flags=re.MULTILINE)
    if count == 0:                       # field absent — append rather than no-op
        new = text.rstrip() + f"\n#+branch: {branch}\n"
    if new != text:                      # avoid a redundant write
        Path(path).write_text(new, encoding="utf-8")

_ORG_ID_RE = re.compile(r"^[ \t]*:ID:\s+(\S+)\s*$", re.MULTILINE)

def _read_org_id(path):
    """Return the :ID: value from an org file, or None if absent/unreadable."""
    try:
        text = Path(path).read_text(encoding="utf-8")
        m = _ORG_ID_RE.search(text)
        return m.group(1) if m else None
    except (OSError, UnicodeDecodeError):
        return None

def resolve_story(ident):
    """Resolve a story identifier to (story_dir, title), or (None, None).

    `ident` is matched against story docs by :ID: (exact, else unique prefix)
    or by folder slug. Returns (None, None) if there is no unique match.
    """
    stories = [d for d in doc_index.load_all().values() if d.doctype == "story"]
    il = ident.lower()
    exact = [d for d in stories if d.id.lower() == il]
    if exact:
        cands = exact
    else:
        # Combine id-prefix and (case-insensitive) folder-slug matches and
        # require a single unique match — so an id-prefix and a different
        # story's slug don't silently resolve to the id one.
        prefix = [d for d in stories if d.id.lower().startswith(il)]
        slug = [d for d in stories if Path(d.rel_path).parent.name.lower() == il]
        cands = list({d.id: d for d in prefix + slug}.values())
    if len(cands) == 1:
        d = cands[0]
        return _parent_dir(d.rel_path), _strip_type_prefix(d.title)
    return None, None

def _scaffold_and_branch(sprint_dir, story_dir, story_title, new_story,
                          task_slug, task_title, task_desc, branch, base, dry_run,
                          current_sprint):
    """Shared implementation for compass story new and compass task new."""
    task_path = Path(PROJECT_ROOT) / story_dir / f"task_{task_slug}.org"

    if dry_run:
        cmd = "story new" if new_story else "task new"
        print(f"compass {cmd} — plan (dry run):")
        print(f"  mode:    {'new story' if new_story else 'new task on existing story'}")
        print(f"  branch:  {branch}   (base {base})")
        print(f"  story:   {story_dir}/story.org   "
              f"({'new' if new_story else f'existing: {story_title}'})")
        print(f"  task:    {story_dir}/task_{task_slug}.org   (#+branch: {branch})")
        print(f"  sprint:  {current_sprint.title}")
        return 0

    gen = _import_generator()

    # 1. fetch + new branch off base
    subprocess.run(["git", "fetch", "origin"], cwd=str(PROJECT_ROOT), check=False)
    sw = subprocess.run(["git", "switch", "-c", branch, base],
                        cwd=str(PROJECT_ROOT), capture_output=True, text=True)
    if sw.returncode != 0:
        print(f"❌ git switch -c {branch} {base} failed:\n{sw.stderr.strip()}",
              file=sys.stderr)
        return 1
    print(f"✅ created and switched to {branch} (off {base})")

    # 2. scaffold story (new mode only) + task via codegen
    try:
        if new_story:
            slug, title, desc, tags = new_story
            rc = gen.main(["--type", "story", "--slug", slug, "--parent-dir", sprint_dir,
                           "--title", title, "--description", desc, "--tags", tags])
            if rc:
                return rc
        rc = gen.main(["--type", "task", "--slug", task_slug, "--parent-dir", story_dir,
                       "--title", task_title, "--description", task_desc])
        if rc:
            return rc
    except SystemExit as exc:
        if exc.code:
            print(f"❌ scaffolding failed: {exc.code}", file=sys.stderr)
        return exc.code or 0

    # 3. record branch on task so fleet/where can map this worktree
    _set_frontmatter_branch(task_path, branch)

    # 4. next steps
    print("\nNext steps:")
    if new_story:
        print(f"  - wire the story into {sprint_dir}/sprint.org (* Stories table)")
    print(f"  - compass task start {task_path.stem.removeprefix('task_')}   # clock on + stamp journal")
    print(f"  - git push -u origin {branch}   &&   open a PR")
    return 0


def cmd_story(argv):
    """compass story — story-level operations."""
    ap = argparse.ArgumentParser(prog="compass story",
                                 description="Story-level operations.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    new_p = sub.add_parser("new", help="Create a story, branch, and first task in the current sprint")
    new_p.add_argument("--slug",        required=True, help="snake_case slug (drives branch + folder)")
    new_p.add_argument("--title",       required=True, help="Story title")
    new_p.add_argument("--description", required=True, help="Story description")
    new_p.add_argument("--tags",        default="",    help="Comma-separated content tags")
    new_p.add_argument("--task",        default="",    help="First task title (default: 'Implement <title>')")
    new_p.add_argument("--base",        default="origin/main")
    new_p.add_argument("--kind",        default="feature", choices=["feature", "hotfix"])
    new_p.add_argument("--dry-run",     action="store_true")

    st = sub.add_parser("status", help="All tasks in a story grouped by state with branch and PR")
    st.add_argument("story", help="Story UUID/prefix or folder slug")
    st.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty")
    st.add_argument("--uuids", action="store_true",
                    help="Show task UUIDs alongside titles")

    args = ap.parse_args(argv)

    if args.subcmd == "new":
        _, current_sprint = current_version_sprint(doc_index.load_all())
        if current_sprint is None:
            print("❌ No current sprint found.", file=sys.stderr)
            return 1
        sprint_dir = _parent_dir(current_sprint.rel_path)
        story_dir  = f"{sprint_dir}/{args.slug}"
        task_slug  = "implement_" + args.slug
        task_title = args.task or f"Implement {args.title}"
        branch     = args.kind + "/" + args.slug.replace("_", "-")
        return _scaffold_and_branch(
            sprint_dir, story_dir, args.title,
            (args.slug, args.title, args.description, args.tags),
            task_slug, task_title, f"Initial task for: {args.title}",
            branch, args.base, args.dry_run, current_sprint)

    if args.subcmd == "status":
        story_dir, story_title = resolve_story(args.story)
        if story_dir is None:
            print(f"❌ Could not resolve story '{args.story}'.", file=sys.stderr)
            return 1
        story_path = Path(PROJECT_ROOT) / story_dir
        if not story_path.exists():
            print(f"❌ Story directory not found: {story_path}", file=sys.stderr)
            return 1

        tasks = []
        for tf in sorted(story_path.glob("task_*.org")):
            title, state, uuid, branch, pr = _read_task_detail(tf)
            tasks.append({"title": title, "state": state, "uuid": uuid,
                          "branch": branch, "pr": pr,
                          "path": str(tf.relative_to(PROJECT_ROOT))})

        if args.format == "json":
            print(json.dumps({"story": story_title, "tasks": tasks}, indent=2))
            return 0

        order = {"DONE": 0, "STARTED": 1, "BLOCKED": 2, "BACKLOG": 3}
        tasks.sort(key=lambda t: (order.get(t["state"], 9), t["state"], t["title"]))

        print(f"🧭 ores.compass — story status: {story_title}\n")
        current_state = None
        for t in tasks:
            if t["state"] != current_state:
                current_state = t["state"]
                print(f"  {current_state}")
            uuid_suffix   = f"  [{t['uuid']}]" if args.uuids and t["uuid"] else ""
            branch_info   = f"  {t['branch']}" if t["branch"] else ""
            pr_info       = f"  PR:{t['pr']}" if t["pr"] and t["pr"] != "none" else ""
            print(f"    {t['title']}{uuid_suffix}{branch_info}{pr_info}")
        return 0


def _cmd_task_start(task_ident):
    """compass task start <slug-or-uuid> — clock on to an existing task."""
    docs = doc_index.load_all()
    il   = task_ident.lower()

    # Resolve by UUID (exact or prefix), slug, or branch suffix.
    tasks = [d for d in docs.values() if d.doctype == "task"]
    exact  = [d for d in tasks if d.id and d.id.lower() == il]
    prefix = [d for d in tasks if d.id and d.id.lower().startswith(il)]
    slug   = [d for d in tasks if Path(d.rel_path).stem.lower() == f"task_{il}"
                                or Path(d.rel_path).stem.lower() == il]
    cands  = list({d.rel_path: d for d in (exact or prefix) + slug}.values())

    if not cands:
        print(f"❌ No task matching '{task_ident}'.", file=sys.stderr)
        return 1
    if len(cands) > 1:
        print(f"❌ Ambiguous — {len(cands)} tasks match '{task_ident}':", file=sys.stderr)
        for c in cands:
            print(f"   {c.id}  {c.rel_path}", file=sys.stderr)
        return 1

    task_doc  = cands[0]
    task_path = Path(PROJECT_ROOT) / task_doc.rel_path

    # Read branch from #+branch: frontmatter.
    branch = _read_frontmatter_field(task_path, "branch")
    if not branch:
        print(f"❌ Task has no #+branch: set. Run 'compass task new' first.", file=sys.stderr)
        return 1

    # Find the parent story (one directory up, story.org).
    story_path = task_path.parent / "story.org"
    story_uuid = _read_org_id(story_path) if story_path.exists() else None

    # Switch to the task branch (create off origin/main if absent).
    result = subprocess.run(["git", "switch", branch], capture_output=True, text=True,
                            cwd=PROJECT_ROOT)
    if result.returncode != 0:
        result2 = subprocess.run(
            ["git", "switch", "-c", branch, "origin/main"],
            capture_output=True, text=True, cwd=PROJECT_ROOT)
        if result2.returncode != 0:
            print(f"❌ git switch failed:\n{result2.stderr.strip()}", file=sys.stderr)
            return 1
        print(f"✅ created and switched to {branch} (off origin/main)")
    else:
        print(f"✅ switched to {branch}")

    # Flip BACKLOG → STARTED in the task file if needed.
    text = task_path.read_text(encoding="utf-8")
    new_text, n = re.subn(r'(\| State\s+\|) BACKLOG', r'\1 STARTED', text, count=1)
    if n:
        task_path.write_text(new_text, encoding="utf-8")
        print(f"📝 state: BACKLOG → STARTED")

    # Stamp the journal.
    try:
        task_uuid = _read_org_id(task_path)
        if story_uuid and task_uuid:
            _journal_update(argparse.Namespace(
                story=story_uuid, task=task_uuid, branch=branch,
                state="STARTED", pr="none"))
    except Exception as e:
        print(f"⚠️  journal update failed: {e}", file=sys.stderr)

    return 0


def _read_frontmatter_field(path, field):
    """Return the value of #+<field>: from an org file, or empty string."""
    prefix = f"#+{field}:"
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.lower().startswith(prefix.lower()):
            return line[len(prefix):].strip()
    return ""


def cmd_task(argv):
    """compass task — task-level operations."""
    ap = argparse.ArgumentParser(prog="compass task",
                                 description="Task-level operations.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    new_p = sub.add_parser("new", help="Add a task to an existing story and create a branch")
    new_p.add_argument("--story",     required=True, help="Story UUID/prefix or folder slug")
    new_p.add_argument("--slug",      required=True, dest="task_slug", help="Task slug (drives branch)")
    new_p.add_argument("--title",     required=True, help="Task title")
    new_p.add_argument("--description", default="",  help="Task description")
    new_p.add_argument("--base",      default="origin/main")
    new_p.add_argument("--kind",      default="feature", choices=["feature", "hotfix"])
    new_p.add_argument("--dry-run",   action="store_true")

    start_p = sub.add_parser(
        "start",
        help="Clock on to a task: switch branch, flip to STARTED, stamp journal",
        description=(
            "Clock on to an existing task — covers all three use cases:\n"
            "  1. new task just scaffolded (compass task new)\n"
            "  2. BACKLOG task being picked up for the first time\n"
            "  3. STARTED task being resumed after a restart\n\n"
            "Switches to the task's branch, flips BACKLOG→STARTED if needed,\n"
            "and appends a timestamped entry to .journal.org so 'compass journal\n"
            "where' and 'compass fleet' show the current context.\n\n"
            "This is the required first step in both the new-story and\n"
            "work-task-to-merged-PR runbooks."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    start_p.add_argument("task", help="Task slug, UUID/prefix, or branch suffix")

    args = ap.parse_args(argv)

    if args.subcmd == "new":
        _, current_sprint = current_version_sprint(doc_index.load_all())
        if current_sprint is None:
            print("❌ No current sprint found.", file=sys.stderr)
            return 1
        story_dir, story_title = resolve_story(args.story)
        if story_dir is None:
            print(f"❌ Could not resolve story '{args.story}'.", file=sys.stderr)
            return 1
        sprint_dir = _parent_dir(current_sprint.rel_path)
        task_desc  = args.description or f"Task for: {story_title}"
        branch     = args.kind + "/" + args.task_slug.replace("_", "-")
        return _scaffold_and_branch(
            sprint_dir, story_dir, story_title, None,
            args.task_slug, args.title, task_desc,
            branch, args.base, args.dry_run, current_sprint)

    if args.subcmd == "start":
        return _cmd_task_start(args.task)

def cmd_env(argv):
    """compass env — Provision pillar: environment setup."""
    if argv and argv[0] == "init":
        import env_init
        return env_init.run(argv[1:], PROJECT_ROOT)
    if argv and argv[0] == "diff":
        import env_init
        return env_init.diff(PROJECT_ROOT)
    if argv and argv[0] == "list":
        import env_init
        lp = argparse.ArgumentParser(prog="compass env list",
                                     description="List .env variables grouped; secrets masked.")
        lp.add_argument("--show-secrets", action="store_true",
                        help="Reveal secret values (passwords, JWT key) instead of masking them")
        largs = lp.parse_args(argv[1:])
        return env_init.list_env(PROJECT_ROOT, largs.show_secrets)
    if argv and argv[0] == "version":
        import env_init
        # `version new "<desc>"` records a new version; bare `version` prints it.
        if len(argv) >= 2 and argv[1] == "new":
            np = argparse.ArgumentParser(prog="compass env version new",
                                         description="Record a new .env-format version.")
            np.add_argument("description",
                            help="What changed in the .env (becomes the log row's description)")
            nargs = np.parse_args(argv[2:])
            return env_init.new_version(PROJECT_ROOT, nargs.description)
        print(env_init.current_version(PROJECT_ROOT))
        return 0
    # No/unknown subcommand: render help (and error on unknown).
    ap = argparse.ArgumentParser(prog="compass env",
                                 description="Provision: checkout environment setup.")
    sub = ap.add_subparsers(dest="subcmd", required=True)
    sub.add_parser("init", help="Generate .env + NATS certs + IAM key "
                                "(reuses existing secrets; --with-diff to show changes)")
    sub.add_parser("diff", help="Unified diff of .env.old vs .env")
    sub.add_parser("list", help="List .env vars grouped (secrets masked; --show-secrets to reveal)")
    sub.add_parser("version", help="Show the .env-format version; 'version new <desc>' records a new one")
    ap.parse_args(argv or ["--help"])
    return 0


# --- Test pillar ---

def _tr_read_preset() -> str:
    """Read ORES_PRESET from .env; return empty string if not found."""
    env_file = PROJECT_ROOT / ".env"
    if not env_file.is_file():
        return ""
    for line in env_file.read_text(encoding="utf-8").splitlines():
        if not line or line.lstrip().startswith("#") or "=" not in line:
            continue
        key, _, val = line.partition("=")
        if key.strip() == "ORES_PRESET":
            return val.strip()
    return ""


def _tr_parse_xml(xml_file):
    import xml.etree.ElementTree as ET
    try:
        tree = ET.parse(xml_file)
        return tree.getroot(), None
    except ET.ParseError as e:
        return None, f"XML parse error: {e}"
    except Exception as e:
        return None, f"Unexpected error: {e}"


def _tr_suite_stats(root, xml_file):
    stats = {"name": root.get("name", "Unknown"),
             "catch2_version": root.get("catch2-version", ""),
             "rng_seed": root.get("rng-seed", ""),
             "total": 0, "passed": 0, "failed": 0, "skipped": 0,
             "duration": 0.0}
    try:
        import os
        stats["mtime"] = datetime.datetime.fromtimestamp(
            os.path.getmtime(xml_file)).strftime("%Y-%m-%d %H:%M:%S")
    except OSError:
        stats["mtime"] = "unknown"
    for tc in root.findall(".//TestCase"):
        r = tc.find("./OverallResult")
        if r is None:
            continue
        stats["total"] += 1
        if r.get("success") == "true":
            stats["passed"] += 1
        else:
            stats["failed"] += 1
        skips = r.get("skips", "0")
        try:
            stats["skipped"] += int(skips)
        except ValueError:
            pass
        dur = r.get("durationInSeconds")
        if dur:
            try:
                stats["duration"] += float(dur)
            except ValueError:
                pass
    return stats


def _tr_failed_tests(root):
    failures = []
    for tc in root.findall(".//TestCase"):
        r = tc.find("./OverallResult")
        if r is not None and r.get("success") == "false":
            exc = tc.find("./Exception")
            exc_text = ""
            if exc is not None:
                parts = []
                if exc.get("filename") and exc.get("line"):
                    parts.append(f"[{exc.get('filename')}:{exc.get('line')}]")
                if exc.text:
                    parts.append(exc.text.strip())
                exc_text = " ".join(parts)
            failures.append({
                "name": tc.get("name", ""),
                "tags": tc.get("tags", ""),
                "filename": tc.get("filename", ""),
                "line": tc.get("line", ""),
                "duration": r.get("durationInSeconds"),
                "exception": exc_text,
            })
    return failures


def _tr_grep_log(log_path, patterns=("ERROR", "WARN")):
    import os as _os
    if not _os.path.exists(log_path):
        return []
    matches = []
    try:
        with open(log_path, encoding="utf-8", errors="ignore") as f:
            for n, line in enumerate(f, 1):
                if any(p in line for p in patterns):
                    matches.append(f"Line {n}: {line.rstrip()}")
    except OSError:
        pass
    return matches


def _tr_find_suite_log(log_dir, project_name):
    import os as _os
    candidate = _os.path.join(log_dir, f"{project_name}.tests",
                              f"{project_name}.tests.log")
    if _os.path.exists(candidate):
        return candidate
    for root_d, _dirs, files in _os.walk(log_dir):
        for f in files:
            if f.endswith(".log") and project_name in f:
                return _os.path.join(root_d, f)
    return None


def cmd_test(argv):
    """compass test — test pillar: inspect build test results."""
    ap = argparse.ArgumentParser(
        prog="compass test",
        description="Test pillar: parse and display Catch2 test results.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    rp = sub.add_parser("results",
                        help="Parse test-results*.xml and show an overview of the last test run")
    rp.add_argument("--preset", default="",
                    help="CMake preset name (default: read ORES_PRESET from .env)")

    args = ap.parse_args(argv)

    if args.subcmd == "results":
        return _cmd_test_results(args)


def _cmd_test_results(args):
    import glob as _glob
    import re as _re
    import os as _os

    preset = args.preset or _tr_read_preset()
    if not preset:
        print("❌ No preset supplied and ORES_PRESET not set in .env.\n"
              "   Pass --preset <name> or run compass env init.", file=sys.stderr)
        return 1

    bin_dir = PROJECT_ROOT / "build" / "output" / preset / "publish" / "bin"
    if not bin_dir.exists():
        print(f"❌ Build output not found: {bin_dir}", file=sys.stderr)
        print(f"   Have you built and run tests?\n"
              f"   cmake --build --preset {preset} --target rat", file=sys.stderr)
        return 1

    xml_files = sorted(_glob.glob(str(bin_dir / "test-results*.xml")))
    if not xml_files:
        print(f"❌ No test-results*.xml files found in {bin_dir}", file=sys.stderr)
        print(f"   Have you run tests?\n"
              f"   cmake --build --preset {preset} --target rat", file=sys.stderr)
        return 1

    # Locate log directory (try several candidates)
    log_dir = None
    for candidate in [
        PROJECT_ROOT / "build" / "output" / preset / "log",
        PROJECT_ROOT / "build" / "output" / "log",
    ]:
        if candidate.exists():
            log_dir = str(candidate)
            break

    print(f"🧭 ores.compass — test results: preset={preset}\n")
    print(f"Found {len(xml_files)} test-results file(s)  |  "
          f"Logs: {log_dir or 'none (run with -DORES_TEST_LOG_LEVEL=debug to enable)'}\n")

    if log_dir is None:
        print("⚠️  No log directory found. To enable logging, reconfigure CMake:")
        print(f"   cmake --preset {preset} -DORES_TEST_LOG_LEVEL=debug\n")

    total_stats = {"total": 0, "passed": 0, "failed": 0,
                   "skipped": 0, "duration": 0.0}
    total_failures = 0

    for xml_file in xml_files:
        fname = _os.path.basename(xml_file)
        print(f"{'='*72}")
        print(f"  {fname}")
        print(f"{'='*72}")

        # Derive project name from filename: test-results-ores.foo.tests.xml → ores.foo
        m = _re.match(r"test-results-(.+)\.tests\.xml$", fname)
        project_name = m.group(1) if m else fname

        root, err = _tr_parse_xml(xml_file)
        suite_log = _tr_find_suite_log(log_dir, project_name) if log_dir else None

        if root is None:
            print(f"  ❌ Could not parse XML: {err}")
            if suite_log:
                print(f"  Suite log: {suite_log}")
                with open(suite_log, encoding="utf-8", errors="ignore") as f:
                    for line in f:
                        print(f"    {line}", end="")
            print()
            continue

        stats = _tr_suite_stats(root, xml_file)
        print(f"  Suite:    {stats['name']}")
        print(f"  Run at:   {stats['mtime']}")
        print(f"  Catch2:   {stats['catch2_version'] or 'n/a'}  "
              f"  seed={stats['rng_seed'] or 'n/a'}")
        print(f"  Results:  {stats['total']} total  "
              f"✓{stats['passed']} passed  "
              f"✗{stats['failed']} failed  "
              f"~{stats['skipped']} skipped  "
              f"({stats['duration']:.2f}s)")

        for k in ("total", "passed", "failed", "skipped"):
            total_stats[k] += stats[k]
        total_stats["duration"] += stats["duration"]

        if suite_log:
            matches = _tr_grep_log(suite_log)
            if matches:
                print(f"\n  ⚠️  Errors/warnings in suite log ({len(matches)} lines):")
                for line in matches[:10]:
                    print(f"    {line}")
                if len(matches) > 10:
                    print(f"    … and {len(matches) - 10} more")

        failures = _tr_failed_tests(root)
        if not failures:
            print("  ✓ No failures.")
        else:
            print(f"\n  {len(failures)} failure(s):")
            for i, f in enumerate(failures, 1):
                total_failures += 1
                print(f"\n  FAIL #{total_failures}: {f['name']}")
                if f["tags"]:
                    print(f"    Tags:     {f['tags']}")
                if f["filename"]:
                    print(f"    Location: {f['filename']}:{f['line']}")
                if f["duration"]:
                    print(f"    Duration: {f['duration']}s")
                if f["exception"]:
                    print(f"    Exception: {f['exception'][:300]}")
        print()

    print(f"{'='*72}")
    print(f"SUMMARY  preset={preset}")
    print(f"{'='*72}")
    print(f"  Suites:   {len(xml_files)}")
    print(f"  Total:    {total_stats['total']}  "
          f"✓{total_stats['passed']} passed  "
          f"✗{total_stats['failed']} failed  "
          f"~{total_stats['skipped']} skipped")
    if total_stats["total"] > 0:
        pct = total_stats["passed"] / total_stats["total"] * 100
        print(f"  Pass rate: {pct:.1f}%")
    print(f"  Duration:  {total_stats['duration']:.2f}s")
    print(f"  Failures:  {total_failures}")
    return 0 if total_stats["failed"] == 0 else 1


# --- Bearings: cold-start orientation ---

_SEP = "─" * 66

def _bearings_section(icon, title):
    print(f"\n{_SEP}")
    print(f"{icon}  {title}")
    print(_SEP)


def cmd_bearings(argv):
    """compass bearings — cold-start orientation for LLMs and new contributors."""
    import argparse as _ap
    ap = _ap.ArgumentParser(
        prog="compass bearings",
        description="Cold-start orientation: project identity, where we are, "
                    "last session, key recipes, and project memories.")
    ap.parse_args(argv)

    docs = doc_index.load_all()

    # ── Section 0: What is ORE Studio? ──────────────────────────────────────
    _bearings_section("🏢", "What is ORE Studio?")
    identity_docs = [d for d in docs.values() if d.doctype == "product_identity"]
    if identity_docs:
        pi = identity_docs[0]
        print(f"  {pi.description}")
        print(f"\n  compass show {pi.id.upper()}")
    else:
        print("  ❌ No product_identity document found.")

    # ── Section 1: Where we are ─────────────────────────────────────────────
    _bearings_section("📍", "Where we are")
    current_version, current_sprint = current_version_sprint(docs)
    if current_version is None or current_sprint is None:
        print("  ❌ No version/sprint documents found.")
    else:
        chip, warning = staleness_lines(branch_staleness(PROJECT_ROOT))
        print(f"  {chip}")
        if warning:
            print(f"  {warning}")
        print()
        print(f"  Version:  {current_version.title}  [{current_version.id.upper()}]")
        print(f"  Sprint:   {current_sprint.title}  [{current_sprint.id.upper()}]")
        sprint_dir = _parent_dir(current_sprint.rel_path)
        in_flight = [
            d for d in docs.values()
            if d.doctype in ("story", "task")
            and d.rel_path.startswith(sprint_dir + "/")
            and read_state(d.path) == IN_FLIGHT_STATE
        ]
        in_flight.sort(key=lambda d: (d.doctype, d.rel_path))
        print(f"\n  In flight ({IN_FLIGHT_STATE}):")
        if not in_flight:
            print("  (nothing in flight)")
        else:
            for d in in_flight:
                print(f"    {d.doctype:<5}  {_strip_type_prefix(d.title)}")
                print(f"           [{d.id.upper()}]")

    # ── Section 2: Last session ──────────────────────────────────────────────
    _bearings_section("📓", "Last session")
    _journal_where()

    # ── Section 3: Key recipes ───────────────────────────────────────────────
    _bearings_section("📖", "Key recipes")
    recipes = sorted(
        [d for d in docs.values()
         if d.doctype == "recipe" and d.has_tag("bearings")],
        key=lambda d: d.title,
    )
    if not recipes:
        print("  ❌ No bearings-tagged recipes found — configuration error.")
        print("     Tag any recipe with :bearings: to include it here.")
    else:
        for r in recipes:
            print(f"\n  • {r.title}")
            if r.description:
                print(f"    {r.description}")
            print(f"    compass show {r.id.upper()}")

    # ── Section 4: Memories ──────────────────────────────────────────────────
    _bearings_section("🧠", "Memories")
    memories = sorted(
        [d for d in docs.values()
         if d.doctype == "memory" and d.has_tag("bearings")],
        key=lambda d: d.title,
    )
    if not memories:
        print("  ❌ No bearings-tagged memories found — configuration error.")
        print("     Tag any memory with :bearings: to include it here.")
    else:
        for m in memories:
            print(f"\n  • {m.title}")
            if m.description:
                print(f"    {m.description}")
            print(f"    compass show {m.id.upper()}")

    print(f"\n{_SEP}\n")
    return 0


def main():
    # `list` and `show` pass every remaining argument straight through to the
    # bundled doc tools (full flag compatibility, including their own --help).
    # Short-circuit before argparse so leading options like `--type` are not
    # swallowed by this parser.
    if len(sys.argv) >= 2 and sys.argv[1] == "list":
        sys.exit(doc_list.main(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "show":
        sys.exit(doc_show.main(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "add":
        sys.exit(cmd_add(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "sprint":
        sys.exit(cmd_sprint(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "story":
        sys.exit(cmd_story(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "task":
        sys.exit(cmd_task(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "capture":
        sys.exit(cmd_capture(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "journal":
        sys.exit(cmd_journal(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "env":
        sys.exit(cmd_env(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "test":
        sys.exit(cmd_test(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] in ("bearings", "orient"):
        sys.exit(cmd_bearings(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] in ALL_BUCKETS:
        sys.exit(cmd_backlog(sys.argv[1], sys.argv[2:]))

    _EPILOG = (
        "Pillars:\n"
        "  Orient:    where, fleet\n"
        "  Search:    search (find), list, show\n"
        "  Scaffold:  story, task, add\n"
        "  Capture:   capture, inbox, next, deferred, discarded, backlog\n"
        "  Journal:   journal\n"
        "  Provision: env\n"
        "  Test:      test\n"
        "  Bearings:  bearings (alias: orient)\n"
        "\n"
        "Entity commands (sub-subcommands span pillars):\n"
        "  sprint:   status (orient)\n"
        "  story:    new (scaffold) | status (orient)\n"
        "  task:     new (scaffold)\n"
        "  env:      init | diff | list | version [new] (provision)\n"
    )
    parser = argparse.ArgumentParser(
        description="Compass: developer toolkit for ORE Studio — orient, scaffold, capture, and search.",
        epilog=_EPILOG,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    index_parser = subparsers.add_parser("index", help="Index or update notes from org-roam.db")
    index_parser.add_argument("--rebuild", action="store_true", help="Rebuild the entire index from scratch")

    search_parser = subparsers.add_parser("search", aliases=["find"], help="Search your notes")
    search_parser.add_argument("query", type=str, help="The search query")
    search_parser.add_argument("-l", "--limit", type=int, default=10, help="Max results (default 10)")
    search_parser.add_argument("-f", "--format", choices=["pretty", "line", "json"], default="pretty",
                              help="Output format: pretty (default), line (UUID path match), or json")

    debug_parser = subparsers.add_parser("debug", help="Debug the index contents")
    debug_parser.add_argument("-f", "--file", type=str, help="Check a specific file (partial match)")

    where_parser = subparsers.add_parser("where", aliases=["status"],
                                         help="Show where we are: current version, sprint, in-flight work")
    where_parser.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty",
                              help="Output format: pretty (default) or json")
    where_parser.add_argument("--prs", action="store_true",
                              help="Also show PRs from all sprint tasks (fetches live status via gh)")

    fleet_parser = subparsers.add_parser("fleet",
                                         help="Show what every git worktree is doing: branch, story, task, PR")
    fleet_parser.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty",
                              help="Output format: pretty (default) or json")

    # Registered for discoverability in --help; actually handled by the
    # short-circuit above (which forwards all args to their handlers).
    subparsers.add_parser("list",
                          help="List/filter docs (--regex/--tag/--type/--under/...); 'list --help' for filters")
    subparsers.add_parser("show",
                          help="Show one doc by UUID/prefix: metadata, blurb, in/out links")
    subparsers.add_parser("add",
                          help="Create a v2 doc via ores.codegen (low-level scaffold); 'add --help' for usage")
    subparsers.add_parser("sprint",
                          help="Sprint-level operations: 'sprint status' for all stories by state; 'sprint --help'")
    subparsers.add_parser("story",
                          help="Story-level operations: 'story new' to create story+branch+task; 'story --help'")
    subparsers.add_parser("task",
                          help="Task-level operations: 'task new' to add task+branch to existing story; 'task --help'")
    subparsers.add_parser("journal",
                          help="Read/write the per-worktree session journal; 'journal --help' for subcommands")
    subparsers.add_parser("env",
                          help="Provision: 'env init' generates .env + certs + IAM key; 'env diff'; 'env --help'")
    subparsers.add_parser("test",
                          help="Test: 'test results' parses Catch2 XML and shows an overview; 'test --help'")
    subparsers.add_parser("bearings",
                          help="Cold-start orientation: identity, where, last session, recipes, memories")
    subparsers.add_parser("orient",
                          help="Alias for bearings")
    subparsers.add_parser("inbox",     help="List captures in the product backlog inbox/")
    subparsers.add_parser("next",      help="List captures in the product backlog next/")
    subparsers.add_parser("deferred",  help="List captures in the product backlog deferred/")
    subparsers.add_parser("discarded", help="List captures in the product backlog discarded/")

    args = parser.parse_args()

    # Only the org-roam-backed commands need org-roam.db; the agile/doc-graph
    # commands read the working tree directly.
    if args.command in ("index", "search", "find", "debug"):
        validate_paths(args.command)

    if args.command == "index":
        cmd_index(args)
    elif args.command in ("search", "find"):
        cmd_search(args)
    elif args.command == "debug":
        cmd_debug(args)
    elif args.command in ("where", "status"):
        cmd_where(args)
    elif args.command == "fleet":
        cmd_fleet(args)

if __name__ == "__main__":
    main()
