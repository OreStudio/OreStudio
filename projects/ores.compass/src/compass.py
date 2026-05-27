#!/usr/bin/env python3
"""
Compass: a repository orientation tool for ORE Studio.

Pillars:
  - Search: fast NLP/FTS retrieval over Org-Roam notes (index/search/debug).
  - Locate: where are we in time — current version, sprint, in-flight work
    (where/status), read from the agile document tree.
  - Navigate: list/filter the doc graph (list) and inspect a single doc with
    its inbound/outbound links (show).
  - Scaffold: create agile/doc artefacts (add) by calling ores.codegen as a
    library — generation stays in codegen, not duplicated here.
  - Fleet: what every git worktree is doing — branch, story, task, PR
    (fleet), so parallel checkouts/agents don't collide.
  - Goto: start a unit of work in one step — fetch main, branch, scaffold
    a linked story+task, print next steps (goto).
"""

import argparse
import json
import os
import re
import sqlite3
import subprocess
import sys
import time
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

COMPASS_DB = str(PROJECT_ROOT / ".compass.db")

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

def cmd_fleet(args):
    worktrees = list_worktrees()
    if not worktrees:
        print("❌ Could not enumerate git worktrees.", file=sys.stderr)
        sys.exit(1)
    here = str(PROJECT_ROOT)
    pr_map = open_prs_by_branch()

    rows = []
    for path, branch in worktrees:
        task_title = story_title = None
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
            "pr": ({"number": pr["number"], "state": pr["state"], "url": pr["url"]}
                   if pr else None),
        })

    if args.format == "json":
        print(json.dumps(rows, indent=2))
        return

    print(f"🧭 ores.compass — fleet ({len(rows)} worktrees)\n")
    for r in rows:
        mark = "→" if r["current"] else " "
        branch = r["branch"] or "(detached)"
        print(f"{mark} {r['worktree']}   {branch}")
        if r["task"] or r["story"]:
            print(f"      story: {r['story'] or '—'}")
            print(f"      task:  {r['task'] or '—'}")
        elif r["branch"] and r["branch"] != "main":
            print("      (no task records this branch)")
        if r["pr"]:
            print(f"      PR:    #{r['pr']['number']} [{r['pr']['state']}]  {r['pr']['url']}")

# --- Scaffold pillar: create agile/doc artefacts ---
#
# Generation belongs to ores.codegen, not compass: `add` calls codegen's
# v2_doc_generate *as a library* (no shelling out, no copied generator). For
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

def _inbox_dir():
    """Return the absolute path to the inbox/ backlog bucket."""
    return Path(PROJECT_ROOT) / BACKLOG_ROOT / "inbox"

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
        print(f"✅ Sprint note created: {out_file.relative_to(PROJECT_ROOT)}")
        print("ℹ️  Use 'compass capture file <slug> near|far|discarded' to move to backlog,")
        print("   or 'compass capture promote <slug>' for instructions to make it a story/task.")
        return 0
    return rc or 1

def cmd_add(argv):
    """Create a v2 doc by calling ores.codegen as a library.

    argv is [<type>, <generate_v2_doc flags...>]. When --parent-dir is
    omitted, default it from the current sprint (story) or version (sprint).
    """
    if not argv or argv[0] in ("-h", "--help"):
        print("usage: compass add <type> [generate_v2_doc options]\n"
              "  types: story task sprint version recipe knowledge component\n"
              "         capture memory investigation product_identity\n"
              "  --parent-dir defaults to the current sprint (story) or "
              "version (sprint); required otherwise.\n"
              "  remaining flags are passed through to ores.codegen "
              "(see 'How do I create a new v2 doc?').")
        return 0

    doc_type = argv[0]
    rest = list(argv[1:])

    has_parent = any(a == "--parent-dir" or a.startswith("--parent-dir=")
                     for a in rest)
    if not has_parent:
        default_parent = _default_parent_dir(doc_type)
        if default_parent:
            rest += ["--parent-dir", default_parent]
            print(f"ℹ️  --parent-dir not given; using current "
                  f"{_DEFAULTABLE_PARENT[doc_type]}: {default_parent}", file=sys.stderr)

    # Lazy, optional import: the generator lives in ores.codegen and needs
    # pystache. Only `add` / `goto` require it; the other commands stay
    # dependency-free.
    v2_doc_generate = _import_generator()

    # v2_doc_generate.main returns 0 on success but may sys.exit on error;
    # catch SystemExit so the reminder prints only on success and the
    # generator's own error message/code is preserved.
    try:
        rc = v2_doc_generate.main(["--type", doc_type, *rest])
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
    """Import ores.codegen's v2_doc_generate (needs pystache). Exit on failure."""
    sys.path.insert(0, str(PROJECT_ROOT / "projects" / "ores.codegen" / "src"))
    try:
        import v2_doc_generate
        return v2_doc_generate
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

def cmd_goto(argv):
    """Start a unit of work in one command. Two modes:

      new story:      --slug --title --description [--tags] [--task]
      existing story: --story <id-or-slug> --task-slug --task [--description]

    Both fetch main, create a feature branch, scaffold the task (and the
    story, in new-story mode) via codegen, set the task #+branch, and print
    the remaining manual steps."""
    ap = argparse.ArgumentParser(prog="compass goto",
                                 description="Fetch main, branch, scaffold a story/task, print next steps.")
    ap.add_argument("--story", default="", help="existing story (UUID/prefix or folder slug) — existing-story mode")
    ap.add_argument("--slug", default="", help="new-story mode: snake_case slug (drives branch + folder)")
    ap.add_argument("--title", default="", help="new-story mode: story title")
    ap.add_argument("--description", default="", help="story description (new mode) or task description (existing mode)")
    ap.add_argument("--tags", default="", help="new-story mode: comma-separated content tags")
    ap.add_argument("--task", default="", help="task title (default in new mode: 'Implement <title>')")
    ap.add_argument("--task-slug", default="", help="existing-story mode: task slug (drives the branch)")
    ap.add_argument("--base", default="origin/main", help="branch base (default: origin/main)")
    ap.add_argument("--dry-run", action="store_true", help="print the plan without executing")
    args = ap.parse_args(argv)

    _, current_sprint = current_version_sprint(doc_index.load_all())
    if current_sprint is None:
        print("❌ No current sprint found under doc/agile/versions/.", file=sys.stderr)
        return 1
    sprint_dir = _parent_dir(current_sprint.rel_path)

    if args.story:
        # --- existing-story mode ---
        if args.slug or args.title:
            print("❌ --story cannot be combined with --slug/--title (new-story mode).", file=sys.stderr)
            return 1
        if not (args.task_slug and args.task):
            print("❌ --story requires --task-slug and --task.", file=sys.stderr)
            return 1
        story_dir, story_title = resolve_story(args.story)
        if story_dir is None:
            print(f"❌ Could not resolve a unique story for '{args.story}'.", file=sys.stderr)
            return 1
        new_story = None
        task_slug, task_title = args.task_slug, args.task
        task_desc = args.description or f"Task for: {story_title}"
        branch = "feature/" + task_slug.replace("_", "-")
    else:
        # --- new-story mode ---
        if not (args.slug and args.title and args.description):
            print("❌ new-story mode needs --slug, --title and --description "
                  "(or use --story for an existing story).", file=sys.stderr)
            return 1
        story_dir = f"{sprint_dir}/{args.slug}"
        story_title = args.title
        new_story = (args.slug, args.title, args.description, args.tags)
        task_slug = "implement_" + args.slug
        task_title = args.task or f"Implement {args.title}"
        task_desc = f"Initial task for: {args.title}"
        branch = "feature/" + args.slug.replace("_", "-")

    task_path = Path(PROJECT_ROOT) / story_dir / f"task_{task_slug}.org"

    if args.dry_run:
        print("compass goto — plan (dry run):")
        print(f"  mode:    {'new story' if new_story else 'existing story'}")
        print(f"  branch:  {branch}   (base {args.base})")
        print(f"  story:   {story_dir}/story.org   "
              f"({'new' if new_story else f'existing: {story_title}'})")
        print(f"  task:    {story_dir}/task_{task_slug}.org   (#+branch: {branch})")
        print(f"  sprint:  {current_sprint.title}")
        return 0

    gen = _import_generator()

    # 1. fetch + new branch off base
    subprocess.run(["git", "fetch", "origin"], cwd=str(PROJECT_ROOT), check=False)
    sw = subprocess.run(["git", "switch", "-c", branch, args.base],
                        cwd=str(PROJECT_ROOT), capture_output=True, text=True)
    if sw.returncode != 0:
        print(f"❌ git switch -c {branch} {args.base} failed:\n{sw.stderr.strip()}",
              file=sys.stderr)
        return 1
    print(f"✅ created and switched to {branch} (off {args.base})")

    # 2. scaffold the story (new mode only) + the task via codegen, stopping on
    # the first failure (gen.main returns 0 on success but may sys.exit on error).
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

    # 3. record the branch on the task so fleet/where can map this worktree
    _set_frontmatter_branch(task_path, branch)

    # 4. next steps (deliberately manual — needs judgement)
    print("\nNext steps:")
    if new_story:
        print(f"  - wire the story into {sprint_dir}/sprint.org (* Stories table)")
    print(f"  - flip the {'story and task' if new_story else 'task'} State to STARTED when you begin")
    print(f"  - git push -u origin {branch}   &&   open a PR")
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
    if len(sys.argv) >= 2 and sys.argv[1] == "goto":
        sys.exit(cmd_goto(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "capture":
        sys.exit(cmd_capture(sys.argv[2:]))

    parser = argparse.ArgumentParser(description="Compass: orientation tool for ORE Studio")
    subparsers = parser.add_subparsers(dest="command", required=True)

    index_parser = subparsers.add_parser("index", help="Index or update notes from org-roam.db")
    index_parser.add_argument("--rebuild", action="store_true", help="Rebuild the entire index from scratch")

    search_parser = subparsers.add_parser("search", help="Search your notes")
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
                          help="Create a v2 doc via ores.codegen (story/task/sprint/...); 'add --help' for usage")
    subparsers.add_parser("goto",
                          help="Start work: fetch main, branch, scaffold story+task, print next steps ('goto --help')")

    args = parser.parse_args()

    # Only the org-roam-backed commands need org-roam.db; the agile/doc-graph
    # commands read the working tree directly.
    if args.command in ("index", "search", "debug"):
        validate_paths(args.command)

    if args.command == "index":
        cmd_index(args)
    elif args.command == "search":
        cmd_search(args)
    elif args.command == "debug":
        cmd_debug(args)
    elif args.command in ("where", "status"):
        cmd_where(args)
    elif args.command == "fleet":
        cmd_fleet(args)

if __name__ == "__main__":
    main()
