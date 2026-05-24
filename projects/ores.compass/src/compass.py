#!/usr/bin/env python3
"""
Compass: a repository orientation tool for ORE Studio.

Pillars:
  - Search: fast NLP/FTS retrieval over Org-Roam notes (index/search/debug).
  - Locate: where are we in time — current version, sprint, in-flight work
    (where/status), read from the agile document tree.
"""

import argparse
import json
import os
import re
import sqlite3
import sys
import time
from pathlib import Path

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
# Source of truth: the agile document tree under doc/agile/versions/. We reuse
# ores.codegen's doc_index (the canonical org-frontmatter parser) for discovery
# rather than re-implementing parsing, and add the one field it does not expose
# — the current State, which lives in each doc's "* Status" table as a
# "| State | <STATE> |" row. This keeps Locate dependency-free of org-roam.db
# (no M-x org-roam-db-sync needed) and always fresh against the working tree.

STATE_RE = re.compile(r"^\|\s*State\s*\|\s*([A-Z]+)\s*\|", re.MULTILINE)
IN_FLIGHT_STATE = "STARTED"

def load_doc_index():
    """Import ores.codegen's doc_index (canonical org parser). None if absent."""
    scripts_dir = PROJECT_ROOT / "projects" / "ores.codegen" / "scripts"
    if str(scripts_dir) not in sys.path:
        sys.path.insert(0, str(scripts_dir))
    try:
        import doc_index
        return doc_index
    except ImportError:
        return None

def read_state(path):
    """Return the State value from a doc's Status table, or None if absent."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except OSError:
        return None
    match = STATE_RE.search(text)
    return match.group(1) if match else None

def _parent_dir(rel_path):
    """Directory holding a doc, as a forward-slash relative string."""
    return Path(rel_path).parent.as_posix()

def _strip_type_prefix(title):
    """Drop the codegen 'Story: ' / 'Task: ' prefix for display next to a type column."""
    for prefix in ("Story: ", "Task: "):
        if title.startswith(prefix):
            return title[len(prefix):]
    return title

def cmd_where(args):
    di = load_doc_index()
    if di is None:
        print("❌ Could not import ores.codegen doc_index "
              "(expected under projects/ores.codegen/scripts/).", file=sys.stderr)
        sys.exit(1)

    docs = di.load_all()
    versions = [d for d in docs.values() if d.doctype == "version"]
    sprints  = [d for d in docs.values() if d.doctype == "sprint"]
    if not versions or not sprints:
        print("❌ No version/sprint documents found under doc/agile/versions/.",
              file=sys.stderr)
        sys.exit(1)

    # "Current" = lexicographically-highest folder (sprint_NN is zero-padded),
    # matching the rule the agile product-owner skill already uses.
    current_version = max(versions, key=lambda d: d.rel_path)
    version_dir = _parent_dir(current_version.rel_path)
    in_version = [d for d in sprints if d.rel_path.startswith(version_dir + "/")]
    current_sprint = max(in_version or sprints, key=lambda d: d.rel_path)
    sprint_dir = _parent_dir(current_sprint.rel_path)

    # In-flight = stories/tasks under the current sprint with State == STARTED.
    in_flight = []
    for d in docs.values():
        if d.doctype in ("story", "task") and d.rel_path.startswith(sprint_dir + "/"):
            state = read_state(d.path)
            if state == IN_FLIGHT_STATE:
                in_flight.append((d, state))
    in_flight.sort(key=lambda pair: (pair[0].doctype, pair[0].rel_path))

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

def main():
    parser = argparse.ArgumentParser(description="Compass: NLP/FTS search for Org-Roam")
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

    args = parser.parse_args()

    # Only the org-roam-backed commands need org-roam.db; Locate reads the
    # agile tree directly.
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

if __name__ == "__main__":
    main()
