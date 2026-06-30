#!/usr/bin/env python3
"""
Compass: developer toolkit for ORE Studio — orient, scaffold, capture, and search.

Pillars:
  - Orient:   where we are (where/fleet/sprint/story) and what every worktree is doing.
  - Search:   fast NLP/FTS retrieval over Org-Roam notes (search/list/show).
  - Scaffold: create branches and agile artefacts in one step (story new/task new/add).
  - Capture:  file and triage product backlog ideas (capture/inbox/next/backlog).
  - Journal:  per-worktree session log for restart recovery and overlap detection.
"""

import argparse
import csv
import datetime
import functools
import http.server
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
import doc_index      # noqa: E402
import doc_list        # noqa: E402
import doc_show        # noqa: E402
import search_scorer   # noqa: E402
import ui              # noqa: E402

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
        print("   Run: compass build --direct org-roam-db-sync", file=sys.stderr)
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

    if getattr(args, "org_roam_db_sync", False):
        script = (PROJECT_ROOT / "projects" / "ores.lisp" / "src"
                  / "ores-sync-org-roam.el")
        print(f"🔄 Syncing org-roam db first: emacs -Q --script "
              f"{script.relative_to(PROJECT_ROOT)}")
        rc = subprocess.run(
            ["emacs", "-Q", "--script", str(script)],
            cwd=PROJECT_ROOT).returncode
        if rc != 0:
            print(f"❌ org-roam db sync failed (exit code {rc}); "
                  f"indexing aborted.", file=sys.stderr)
            sys.exit(rc)
        validate_paths("index")

    roam_conn = get_roam_conn()
    file_count = roam_conn.execute("SELECT COUNT(*) FROM files").fetchone()[0]
    print(f"📝 Found {file_count} files registered in org-roam.db")

    if file_count == 0:
        print("⚠️  org-roam.db is empty. Run: compass build --direct org-roam-db-sync")
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

_QUESTION_WORDS = ("how", "what", "where", "when", "why", "which",
                   "who", "can", "do", "does", "is", "are", "should")

# Words carrying no search signal in a natural-language question; the
# title-scoped pass keeps only the content words ("How do I create a
# PR?" → create, pr).
# Intentionally smaller than search_scorer.STOPWORDS — covers only
# question starters and common articles/pronouns, not logical connectors.
_STOPWORDS = frozenset(
    _QUESTION_WORDS
    + ("i", "a", "an", "the", "to", "of", "in", "on", "for", "with",
       "we", "it", "my", "our", "you", "your"))


def _is_question(query: str) -> bool:
    """A query is question-shaped when it reads like a natural question."""
    q = query.strip().lower()
    if not q:
        return False
    return q.endswith("?") or q.split()[0] in _QUESTION_WORDS


def _is_folder_slug(query: str) -> bool:
    """True when query looks like a story/task folder slug (snake_case, no spaces)."""
    return bool(re.match(r'^[a-z][a-z0-9_]*$', query) and '_' in query)


def _answer_extract(file_path: str, max_len: int = 200) -> str:
    """First prose of a recipe's * Answer section, clipped to MAX_LEN."""
    path = Path(file_path)
    if not path.is_absolute():
        path = PROJECT_ROOT / path
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return ""
    m = re.search(r"^\* Answer\s*\n(.*?)(?=^\* |\Z)", text, re.M | re.S)
    if not m:
        return ""
    # Take prose lines only — stop at the first src block.
    lines = []
    for line in m.group(1).splitlines():
        if line.strip().startswith("#+begin_src"):
            break
        if line.strip():
            lines.append(line.strip())
    extract = " ".join(lines)
    if len(extract) > max_len:
        extract = extract[:max_len].rsplit(" ", 1)[0] + "…"
    return extract


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

    if args.limit <= 0:
        print("❌ --limit must be a positive integer.")
        return

    # Build FTS query. Plain queries are tokenised to bare words (so
    # punctuation like the '?' of a question cannot break FTS5 syntax)
    # and OR-combined as prefix terms; queries using explicit FTS syntax
    # are passed through verbatim.
    words = []
    folder_slug = None
    if ('"' not in query and '*' not in query
            and not re.search(r"\b(AND|OR|NOT)\b", query)):
        words = re.findall(r"\w+", query)
        # Folder-slug queries (e.g. user_manual_pdf_build): the underscore is a
        # \w character so the whole slug becomes one FTS term, which the unicode61
        # tokeniser reads as a phrase — killing recall.  Split on _ instead so
        # each part becomes an independent OR prefix term.
        if len(words) == 1 and _is_folder_slug(words[0]):
            folder_slug = words[0]
            fts_query = " OR ".join(f"{w}*" for w in folder_slug.split("_"))
        else:
            # Expand synonyms for the OR body query to improve recall.
            _expanded = search_scorer.expand_synonyms(
                [w.lower() for w in words])
            fts_query = " OR ".join(f"{word}*" for word in _expanded)
        if not fts_query:
            print("Please provide a search query.")
            return
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

    _qplan_early = search_scorer.QueryPlan.from_query(query)
    question = _qplan_early.is_question
    how_do = _qplan_early.is_how_do

    # Question-shaped plain queries get a title-scoped first pass:
    # recipes are titled as the question they answer, so a doc whose
    # title carries every content word of the question is almost always
    # the doc being asked for. Recipes float above other title matches;
    # general full-text hits fill the remaining slots.
    rows = []
    if question and words:
        content_words = [w for w in words if w.lower() not in _STOPWORDS]
        if content_words:
            title_q = ("title : ("
                       + " AND ".join(f"{w}*" for w in content_words) + ")")
            try:
                trows = compass_conn.execute(sql, (title_q, 50)).fetchall()
                rows.extend(sorted(
                    trows,
                    key=lambda r: 0 if "recipes/" in (r['file_path'] or "")
                    else 1))
            except sqlite3.OperationalError:
                pass  # fall through to the general pass

    # Over-fetch a large pool then dedup — the index can carry the same node
    # twice (relative + absolute path) and a doc can match on several chunks.
    # We need enough raw hits to fill every bucket independently, so fetch
    # well beyond --limit; post-filter and --type queries need even more.
    _has_filter = bool(args.under or args.doctype)
    _fetch_cap  = min(max(args.limit * 100, 500), 2000) if _has_filter \
                  else min(max(args.limit * 20, 200), 1000)
    try:
        rows.extend(compass_conn.execute(
            sql, (fts_query, _fetch_cap)).fetchall())
    except sqlite3.OperationalError as e:
        print(f"Search syntax error: {e}")
        return

    def _unquote(value):
        """org-roam stores strings with embedded quotes ('\"Task: …\"')."""
        value = (value or "").strip()
        if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
            value = value[1:-1]
        return value

    results, seen = [], set()
    for r in rows:
        rid = _unquote(r['roam_id']).upper()
        if rid in seen:
            continue
        seen.add(rid)
        results.append({
            'roam_id': rid,
            'file_path': _unquote(r['file_path']),
            'title': _unquote(r['title']),
            'olp': _unquote(r['olp']),
            'tags': r['tags'],
            'snippet': r['snippet'],
        })
        # No early clip — buckets each take up to --limit independently.

    # "how do …" queries: recipe docs almost always carry the answer, so
    # float them to the top regardless of FTS rank before any other re-ranking.
    if how_do:
        recipe_hits = [r for r in results if "recipes/" in (r['file_path'] or "")]
        other_hits  = [r for r in results if "recipes/" not in (r['file_path'] or "")]
        results = recipe_hits + other_hits

    # Load doc index once; used for inbound boost, sprint detection, and scoring.
    _LINK_BOOST_CAP = 4
    _link_docs = doc_index.load_all()
    _inbound = doc_index.build_inbound(_link_docs)

    # Compute sprint prefix for past-sprint exclusion and bucket labelling.
    _, _current_sprint_doc = current_version_sprint(_link_docs)
    _sprint_prefix_early = (
        _parent_dir(_current_sprint_doc.rel_path) + "/"
        if _current_sprint_doc else ""
    )
    _scored = [
        (max(0, i - min(len(_inbound.get(r['roam_id'], [])), _LINK_BOOST_CAP)), i, r)
        for i, r in enumerate(results)
    ]
    _scored.sort(key=lambda x: (x[0], x[1]))
    results = [r for _, _, r in _scored]

    # For folder-slug queries, guarantee that docs living inside /<slug>/
    # appear at the top (story.org first, then tasks).  FTS ranking can miss
    # the story entirely when the component words are very common, so we do a
    # direct doc-index lookup and inject any absent folder members.
    if folder_slug:
        slug_fragment = f"/{folder_slug}/"
        seen_ids = {r['roam_id'] for r in results}
        in_folder  = [r for r in results if slug_fragment in r['file_path']]
        out_folder = [r for r in results if slug_fragment not in r['file_path']]

        all_docs_fs = doc_index.load_all()
        for d in all_docs_fs.values():
            abs_path = str(Path(PROJECT_ROOT) / d.rel_path)
            if slug_fragment not in abs_path:
                continue
            if d.id.upper() in seen_ids:
                continue
            in_folder.append({
                'roam_id': d.id.upper(),
                'file_path': abs_path,
                'title': d.title or "",
                'olp': "",
                'tags': " ".join(d.tags) if d.tags else "",
                'snippet': "",
            })
            seen_ids.add(d.id.upper())

        story_hits = [r for r in in_folder if r['file_path'].endswith("/story.org")]
        task_hits  = [r for r in in_folder if not r['file_path'].endswith("/story.org")]
        results = story_hits + task_hits + out_folder

    # Apply --under path-prefix filter.
    if args.under:
        def _under_any(file_path, prefixes):
            try:
                rel = str(Path(file_path).relative_to(PROJECT_ROOT))
                for p in prefixes:
                    p = p.strip("/")
                    if rel.startswith(p + "/") or rel == p:
                        return True
            except ValueError:
                pass
            for p in prefixes:
                p = p.strip("/")
                if ("/" + p + "/") in file_path or file_path.endswith("/" + p):
                    return True
            return False
        results = [r for r in results if _under_any(r['file_path'], args.under)]

    # Apply --type doctype filter.
    if args.doctype:
        _all_docs = doc_index.load_all()
        _type_ids = {d.id.upper() for d in _all_docs.values()
                     if d.doctype == args.doctype}
        results = [r for r in results if r['roam_id'] in _type_ids]

    # When --type or --under is active, apply the original single-list clip.
    if args.under or args.doctype:
        results = results[:args.limit]

    # Past-sprint exclusion (default behaviour): story and task docs from
    # previous sprints are almost never what the user wants when searching —
    # they bloat results and hide current content.  Exclude them unless
    # --history is explicitly requested.
    if not getattr(args, 'history', False):
        _agile_types = {"story", "task"}
        def _is_past_sprint(r):
            doc = _link_docs.get(r['roam_id'].lower()) or _link_docs.get(r['roam_id'])
            if doc is None:
                return False
            if doc.doctype not in _agile_types:
                return False
            if not _sprint_prefix_early:
                return False
            return not doc.rel_path.startswith(_sprint_prefix_early)
        results = [r for r in results if not _is_past_sprint(r)]

    # Heading-anchor exclusion: nodes with a non-empty olp but no #+type: are
    # subsections of another document.  The parent's body text is already in
    # the index, so these are duplicate signals — exclude by default.
    # --all-buckets restores them for completeness.
    if not getattr(args, 'all_buckets', False):
        def _is_heading_anchor(r):
            if not r.get('olp', '').strip():
                return False
            doc = _link_docs.get(r['roam_id'].lower()) or _link_docs.get(r['roam_id'])
            return not doc or not doc.doctype
        results = [r for r in results if not _is_heading_anchor(r)]

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

    elif args.verbose:  # Verbose pretty format (-v): path, location, snippet
        print_db_freshness()
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
            print(f"   {ui.ycmd('compass show ' + r['roam_id'])}")
            if r['snippet']:
                # Clean up the snippet for display
                clean_snippet = r['snippet'].replace('\n', ' ').strip()
                # Remove the >>> and <<< markers for display
                clean_snippet = re.sub(r'>>>([^<]+)<<<', r'\1', clean_snippet)
                print(f"   ...{clean_snippet}...")
            print("-" * 50)

    else:  # Pretty format (default): bucketed, top-N per bucket independently
        # Reuse the doc index already loaded for inbound scoring.
        docs = {d.id.upper(): d for d in _link_docs.values()}

        # ── Helpers ───────────────────────────────────────────────────────────
        def _dt(r):
            doc = docs.get(r['roam_id'])
            return ((doc.doctype or "") if doc else "").lower()

        def _rel(r):
            doc = docs.get(r['roam_id'])
            return (doc.rel_path if doc else "") or ""

        # ── Scoring pipeline ──────────────────────────────────────────────────
        # Two FTS passes produce title-match rank positions (AND semantics so
        # every query word must appear in title or description):
        #   core pass  — question verbs stripped; targets the topic noun phrase.
        #   full pass  — all content words; catches question-form titles.
        # Signals are fed into search_scorer.score_document() which combines
        # them with explicit named weights and returns a percentage + breakdown.
        # The bucket classifier reads ui.BUCKET_AFFINITY so new doctypes are
        # automatically picked up without changes here.

        _qplan = _qplan_early  # reuse the QueryPlan already built at query start
        _w_overrides = {}
        if getattr(args, 'floor', None) is not None:
            _w_overrides['threshold_pct'] = args.floor
        if getattr(args, 'dropout', None) is not None:
            _w_overrides['dropout_ratio'] = args.dropout
        _scorer_weights = search_scorer.Weights(**_w_overrides)

        def _title_fts_ranks(word_list: list[str]) -> dict[str, int]:
            if not word_list:
                return {}
            q = " AND ".join(
                f"{{title description}} : {w}*" for w in word_list)
            ranks: dict[str, int] = {}
            try:
                rows = compass_conn.execute(
                    "SELECT roam_id FROM compass_fts"
                    " WHERE compass_fts MATCH ? ORDER BY rank LIMIT 500",
                    (q,)
                ).fetchall()
                for i, row in enumerate(rows):
                    rid = _unquote(row['roam_id']).upper()
                    if rid not in ranks:
                        ranks[rid] = i
            except sqlite3.OperationalError:
                pass
            return ranks

        _core_rank = _title_fts_ranks(_qplan.core_words)
        _full_rank = _title_fts_ranks(_qplan.full_words)

        # Derive affinity sets from ui.BUCKET_AFFINITY so new doctypes
        # registered there are automatically included here.
        _HOW_TO_TYPES    = frozenset(
            dt for dt, b in ui.BUCKET_AFFINITY.items() if b == "how_to")
        _KNOWLEDGE_TYPES = frozenset(
            dt for dt, b in ui.BUCKET_AFFINITY.items() if b == "knowledge")

        # Score every result: pool position (BM25 proxy) + title ranks + signals.
        _pool_pos = {r['roam_id']: i for i, r in enumerate(results)}
        _doc_scores: dict[str, search_scorer.ScoreResult] = {}
        for r in results:
            rid   = r['roam_id']
            doc   = docs.get(rid)
            dt    = ((doc.doctype or "") if doc else "").lower()
            is_recipe = dt in _HOW_TO_TYPES
            sigs  = search_scorer.DocSignals(
                core_rank          = _core_rank.get(rid),
                full_rank          = _full_rank.get(rid),
                body_bm25          = -(_pool_pos.get(rid, 200) / 10.0),
                inbound_count      = len(_inbound.get(rid, [])),
                is_recipe_type     = is_recipe,
                recipe_for_question= question and is_recipe,
                pool_position      = _pool_pos.get(rid, 0),
            )
            _doc_scores[rid] = search_scorer.score_document(sigs, _scorer_weights)

        # Global relative floor: max_score * dropout_ratio, floored at the
        # absolute threshold.  All buckets use the same floor so a strong
        # Recipes match (80%) suppresses weak Knowledge hits (15%) that would
        # only pollute LLM context.  --all-buckets disables the ratio.
        _all_buckets = getattr(args, 'all_buckets', False)
        _floor = search_scorer.global_floor(
            _doc_scores, _scorer_weights, all_buckets=_all_buckets)
        _filtered = [r for r in results
                     if _doc_scores[r['roam_id']].pct >= _floor]

        # Fallback: if the floor filters everything out (body-only matches
        # can't reach the absolute minimum when there are no title hits),
        # relax to the top --limit results and surface a low-confidence note.
        _floor_relaxed = False
        if not _filtered and results:
            _filtered = results[:args.limit]
            _floor_relaxed = True
        results = _filtered

        def _rank_bucket(hits: list) -> list:
            """Sort by composite score descending."""
            return sorted(hits,
                          key=lambda r: -_doc_scores[r['roam_id']].total)

        def _print_hit(r):
            rid = r['roam_id']
            doc = docs.get(rid)
            doctype = doc.doctype if doc else ""
            title = _strip_type_prefix(
                (doc.title if doc else "") or r['title'] or "Untitled")
            description = (doc.description if doc else "") or ""
            score_res = _doc_scores.get(rid)
            score_lbl = score_res.label if score_res else "?"

            # Tags: prefer the doc-index list; fall back to FTS column.
            if doc and doc.tags:
                _tags = [t.strip('"') for t in doc.tags if t.strip('"')]
            elif r.get('tags'):
                _tags = [t.strip('"') for t in r['tags'].split()
                         if t.strip('"')]
            else:
                _tags = []

            # Heading anchors have no doctype — show parent path + section.
            _context = ""
            if not doctype:
                try:
                    rel = str(Path(r['file_path']).relative_to(PROJECT_ROOT))
                except ValueError:
                    rel = r['file_path']
                olp = r.get('olp', '').strip('(")')
                _context = rel + (f" § {olp}" if olp else "")

            line = f"{ui.icon_for_doc(doctype, doc.path if doc else None)}  "
            if doctype:
                line += f"{doctype}: "
            line += ui.header(title)
            line += f"  {ui.CYAN}[{score_lbl}]{ui.RESET}"
            print(line)
            if description:
                print(f"    {description}")
            if _context:
                print(f"    📍 {_context}")
            if _tags:
                print(f"    🏷  {', '.join(_tags)}")
            print(f"    {ui.ycmd('compass show ' + rid)}")

            if question and doctype in _HOW_TO_TYPES:
                answer = _answer_extract(r['file_path'])
                if answer:
                    print(f"    💬 {answer}")

            if getattr(args, 'related', False) and doc:
                _RELATED_N = getattr(args, 'related_limit', 3)
                # Outgoing: links from this doc to others, ranked by whether
                # the neighbour's title/description scores against the query.
                _out_docs = [
                    docs.get(uid.upper())
                    for uid in (doc.outbound or [])
                ]
                _out_docs = [d for d in _out_docs if d and d.id.upper() != rid]
                # Score each outgoing neighbour by its own FTS score if in
                # results pool, else fall back to inbound-count as a proxy.
                def _out_score(d):
                    sc = _doc_scores.get(d.id.upper())
                    return sc.total if sc else len(_inbound.get(d.id.lower(), []))
                _out_docs.sort(key=_out_score, reverse=True)
                _out_top = _out_docs[:_RELATED_N]

                # Incoming: docs that link to this one, ranked by their own
                # inbound-link count (cheap PageRank proxy). Dedup against
                # outgoing before slicing so we backfill from lower-ranked
                # candidates when the top entries overlap.
                _in_ids = _inbound.get(rid.lower(), [])
                _in_docs = [docs.get(uid.upper()) for uid in _in_ids]
                _in_docs = [d for d in _in_docs if d]
                _in_docs.sort(key=lambda d: -len(_inbound.get(d.id.lower(), [])))
                _out_ids = {x.id.upper() for x in _out_top}
                _in_top = [d for d in _in_docs
                           if d.id.upper() not in _out_ids][:_RELATED_N]

                if _out_top or _in_top:
                    print("    🔗  Related")
                    for nd in _out_top:
                        nt = _strip_type_prefix(nd.title or "")
                        _ndt = f"{nd.doctype}: " if nd.doctype else ""
                        print(f"       ↳ {ui.icon_for(nd.doctype)}  {_ndt}"
                              f"{ui.CYAN}{nt}{ui.RESET}"
                              f"  {ui.ycmd('compass show ' + nd.id.upper())}")
                    for nd in _in_top:
                        nt = _strip_type_prefix(nd.title or "")
                        _ndt = f"{nd.doctype}: " if nd.doctype else ""
                        print(f"       ← {ui.icon_for(nd.doctype)}  {_ndt}"
                              f"{ui.CYAN}{nt}{ui.RESET}"
                              f"  {ui.ycmd('compass show ' + nd.id.upper())}")

            print()

        def _print_bucket(label: str, hits: list) -> None:
            shown = hits[:bucket_limit]
            if not shown:
                return
            overflow = len(hits) - len(shown)
            print(label)
            print()
            for r in shown:
                _print_hit(r)
            if overflow:
                print(f"    … {overflow} more (increase --limit to see them)")
                print()

        # ── Fixed buckets — curated by semantic intent ────────────────────────
        # Temporal types (story, task, capture) use sprint-membership logic;
        # all others are driven by ui.BUCKET_AFFINITY above.
        def _in_current_sprint(r):
            return bool(_sprint_prefix_early) and _rel(r).startswith(_sprint_prefix_early)

        FIXED_BUCKETS = [
            ("📜  Recipes & how-to",
             lambda r: _dt(r) in _HOW_TO_TYPES),
            ("📚  Knowledge",
             lambda r: _dt(r) in _KNOWLEDGE_TYPES),
            ("🔵  Now — current sprint & captures",
             lambda r: _dt(r) == "capture"
                       or (_dt(r) in ("story", "task") and _in_current_sprint(r))),
            ("🗄  Past sprints",
             lambda r: _dt(r) in ("story", "task") and not _in_current_sprint(r)),
        ]

        bucket_limit = args.limit
        print(ui.header(f"🧭 ores.compass — search: '{query}'"))
        print()

        # ── Configuration block ───────────────────────────────────────────────
        print(f"{ui.BOLD}⚙  Configuration{ui.RESET}")
        if _sprint_prefix_early:
            _sprint_name = _sprint_prefix_early.rstrip("/").split("/")[-1].replace("_", " ")
            _hist_included = getattr(args, 'history', False)
            print(f"   • Sprint:   {_sprint_name}")
            if _hist_included:
                print(f"   • History:  included")
            else:
                print(f"   • History:  past-sprint docs excluded  (--history to include)")
        _ab_note = "  (ratio disabled)" if _all_buckets else \
                   "  (--all-buckets to disable)"
        print(f"   • Floor:    ≥{_floor}%{_ab_note}")
        print(f"   • Dropout:  {_scorer_weights.dropout_ratio}"
              f"  (keep results ≥ best × ratio)")
        if not getattr(args, 'related', False):
            print(f"   • Related:  link-graph neighbours hidden  (--related to show)")
        if _floor_relaxed:
            print(f"\n{ui.YELLOW}⚠  No results reached the score floor — "
                  f"showing best available (low confidence){ui.RESET}")
        print()

        assigned: set[str] = set()
        for label, pred in FIXED_BUCKETS:
            hits = _rank_bucket(
                [r for r in results if r['roam_id'] not in assigned and pred(r)])
            for r in hits:   # claim ALL predicate-matched items, not just the shown slice
                assigned.add(r['roam_id'])
            _print_bucket(label, hits)

        # ── Dynamic tail — any remaining doctype, labelled from ui.TYPE_ICONS ─
        # New doctypes in ui.TYPE_ICONS get an automatic bucket here with no
        # changes required to this file.
        dynamic_by_type: dict[str, list] = {}
        for r in results:
            if r['roam_id'] in assigned:
                continue
            dt = _dt(r) or "other"
            dynamic_by_type.setdefault(dt, []).append(r)

        for dt in sorted(dynamic_by_type):
            icon  = ui.icon_for(dt)
            label = f"{icon}  {dt.replace('_', ' ').capitalize()}"
            _print_bucket(label, _rank_bucket(dynamic_by_type[dt]))

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

IN_FLIGHT_STATE = "STARTED"

# Matches the body rows of a "* PRs" table. The header row (PR | Title) and
# separator row are consumed by the leading anchors; each data row must have a
# numeric PR number in the first cell and a non-empty title in the second.
_PRS_SECTION_RE = re.compile(r"^\* PRs\s*\n", re.MULTILINE)
_PRS_ROW_RE = re.compile(r"^\|\s*(\d+)\s*\|\s*([^|\n]+?)\s*\|", re.MULTILINE)

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

_IS_TTY    = sys.stdout.isatty()
_C_GREEN   = "\033[32m" if _IS_TTY else ""
_C_YELLOW  = "\033[33m" if _IS_TTY else ""
_C_RED     = "\033[31m" if _IS_TTY else ""
_C_CYAN    = "\033[36m" if _IS_TTY else ""
_C_MAGENTA = "\033[35m" if _IS_TTY else ""
_C_BOLD    = "\033[1m"  if _IS_TTY else ""
_C_RESET   = "\033[0m"  if _IS_TTY else ""
_C_SEV    = {"ok": _C_GREEN, "warn": _C_YELLOW, "stale": _C_RED}

# Freshness bands for the search/index databases, in seconds.
_FRESH_GREEN    = 3600       # < 1h: green, fresh
_FRESH_WARNING  = 5 * 3600   # > 5h: yellow, warning
_FRESH_CRITICAL = 24 * 3600  # > 24h: red, critical


def db_freshness_line(label, path, refresh_hint):
    """One-line freshness report for a database file.

    < 1h green (info), 1h–5h plain (info), > 5h yellow (warning),
    > 24h red (critical). Missing file is always critical.
    """
    if not os.path.exists(path):
        return (f"{_C_RED}🛑 {label}: missing — "
                f"run: {refresh_hint}{_C_RESET}")
    age = time.time() - os.path.getmtime(path)
    human = _age_human(age)
    if age > _FRESH_CRITICAL:
        return (f"{_C_RED}🛑 {label}: {human} old (critical) — "
                f"run: {refresh_hint}{_C_RESET}")
    if age > _FRESH_WARNING:
        return (f"{_C_YELLOW}⚠  {label}: {human} old (stale) — "
                f"consider: {refresh_hint}{_C_RESET}")
    if age < _FRESH_GREEN:
        return f"{_C_GREEN}✅ {label}: {human} old (fresh){_C_RESET}"
    return f"ℹ️  {label}: {human} old"


def print_db_freshness():
    """Print freshness of the compass index and the org-roam db."""
    print(db_freshness_line("index (.compass.db)", COMPASS_DB,
                            "compass index"))
    print(db_freshness_line("org-roam (.org-roam.db)", ORG_ROAM_DB,
                            "compass index --org-roam-db-sync"))


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
            state = ui.read_state(d.path)
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

    print(ui.header("🧭 ores.compass — where are we?"))
    print()
    chip, warning = staleness_lines(branch_staleness(PROJECT_ROOT))
    print(chip)
    if warning:
        print(warning)
    print()

    print("📍  Context")
    print()
    v_icon = ui.icon_for(current_version.doctype)
    s_icon = ui.icon_for(current_sprint.doctype)
    print(f"{v_icon}  version: {ui.header(current_version.title)}")
    print(f"    {ui.ycmd(f'compass show {current_version.id.upper()}')}")
    print()
    print(f"{s_icon}  sprint: {ui.header(current_sprint.title)}")
    print(f"    {ui.ycmd(f'compass show {current_sprint.id.upper()}')}")
    print()

    print(f"🔵  In flight ({IN_FLIGHT_STATE})")
    print()
    if not in_flight:
        print("    (nothing in flight)")
        print()
    else:
        for d, state in in_flight:
            icon = ui.icon_for_doc(d.doctype, d.path)
            title = _strip_type_prefix(d.title or "")
            try:
                _env_m = _ENV_FIELD_RE.search(Path(d.path).read_text(encoding="utf-8"))
                _env = _env_m.group(1) if _env_m else ""
            except (OSError, UnicodeDecodeError):
                _env = ""
            _env_note = f"  {ui.CYAN}[{_env}]{ui.RESET}" if _env else ""
            print(f"{icon}  {d.doctype}: {ui.header(title)}{_env_note}")
            print(f"    {ui.ycmd(f'compass show {d.id.upper()}')}")
            print()

    # ── Blocked dependency trees ───────────────────────────────────────────────
    # For every BLOCKED task in the sprint, walk the #+blocked_on: chain to
    # the root blocker and render an indented dependency tree.

    def _read_blocked_fields(path):
        """Return (blocked_on_uuid_or_none, blocked_since_or_none) from an org file."""
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            return None, None
        bm = _BLOCKED_ON_RE.search(text)
        sm = _BLOCKED_SINCE_RE.search(text)
        raw = bm.group(1) if bm else ""
        blocked_on = raw if raw and raw.lower() != "none" else None
        blocked_since = sm.group(1) if sm else None
        return blocked_on, blocked_since

    # Build UUID → doc map for the whole doc index (for chain resolution).
    uuid_map = {d.id.lower(): d for d in docs.values() if d.id}

    # Collect BLOCKED tasks in the current sprint.
    blocked_tasks = []
    for d in docs.values():
        if (d.doctype == "task"
                and d.rel_path.startswith(sprint_dir + "/")
                and ui.read_state(d.path) == "BLOCKED"):
            blocked_tasks.append(d)
    blocked_tasks.sort(key=lambda d: d.rel_path)

    if blocked_tasks:
        print(f"🔴  Blocked ({len(blocked_tasks)})")
        print()
        for d in blocked_tasks:
            blocked_on_uuid, blocked_since = _read_blocked_fields(d.path)
            title = _strip_type_prefix(d.title or "")
            since_note = f"  {ui.CYAN}(since {blocked_since}){ui.RESET}" if blocked_since else ""
            print(f"🔴  task: {ui.header(title)}{since_note}")
            print(f"    {ui.ycmd(f'compass show {d.id.upper()}')}")
            # Walk the blocker chain, up to 8 hops to guard against cycles.
            chain_uuid = blocked_on_uuid
            depth = 1
            seen = {d.id.lower()}
            while chain_uuid and depth <= 8:
                chain_lower = chain_uuid.lower()
                if chain_lower in seen:
                    print(f"    {'  ' * depth}⚠️  cycle detected at {chain_uuid[:8]}")
                    break
                seen.add(chain_lower)
                blocker = uuid_map.get(chain_lower)
                if blocker:
                    b_state = ui.read_state(blocker.path) or "?"
                    b_title = _strip_type_prefix(blocker.title or "")
                    b_icon = ui.icon_for_doc(blocker.doctype, blocker.path)
                    indent = "    " + "  " * depth
                    print(f"{indent}↳ {b_icon}  {blocker.doctype}: {ui.header(b_title)}  {ui.CYAN}[{b_state}]{ui.RESET}")
                    print(f"{indent}   {ui.ycmd(f'compass show {blocker.id.upper()}')}")
                    if b_state == "BLOCKED":
                        next_uuid, _ = _read_blocked_fields(blocker.path)
                        chain_uuid = next_uuid
                    else:
                        chain_uuid = None
                else:
                    print(f"    {'  ' * depth}↳ ❓ {chain_uuid[:8]}… (not found in index)")
                    chain_uuid = None
                depth += 1
            print()

    if getattr(args, "prs", False):
        print(f"🔀  PRs (sprint {current_sprint.title})")
        print()
        if not sprint_prs:
            print("    (no PRs recorded in task * PRs tables)")
            print()
        else:
            for pr_num, pr_title, td in sprint_prs:
                info = pr_statuses.get(pr_num, {})
                live_title = info.get("title") or pr_title
                state_label = info.get("state", "UNKNOWN")
                url = info.get("url", "")
                task_label = _strip_type_prefix(td.title)
                print(f"🔀  PR #{pr_num}: {ui.header(live_title)}  {ui.CYAN}[{state_label}]{ui.RESET}")
                print(f"    task: {task_label}")
                if url:
                    print(f"    {ui.ycmd(url)}")
                print()

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

def _print_entry(entry, with_pr_state=False):
    """Print one journal entry in the standard fleet-style format.

    with_pr_state queries gh for the PR's live state (open/merged/closed)
    and annotates the PR line — one gh call, so only enabled for single-entry
    views (journal where, bearings), not journal log.
    """
    sl = entry["story_link"]
    sm = _ORG_LINK_RE.match(sl)
    story_title = sm.group(2) if sm else sl
    story_uuid  = sm.group(1).upper() if sm else None

    tl = entry.get("task") or ""
    tm = _ORG_LINK_RE.match(tl)
    task_title = tm.group(2) if tm else (tl or "—")
    task_uuid  = tm.group(1).upper() if tm else None

    print(f"  ● {entry['timestamp']} — {story_title}")
    if story_uuid:
        print(f"    {_ycmd(f'compass show {story_uuid}')}")
    print(f"    Task:   {task_title} — {entry.get('state') or '?'}")
    if task_uuid:
        print(f"            {_ycmd(f'compass show {task_uuid}')}")
    print(f"    Branch: {entry.get('branch') or '—'}")
    pr = entry.get("pr") or "none"
    pm = re.match(r"^#(\d+)$", pr)
    if with_pr_state and pm:
        state = pr_state(pm.group(1))
        chip = _PR_STATE_CHIP.get(state)
        if chip:
            pr = f"{pr} {chip}"
            if state == "MERGED":
                pr += "  (merged — sync main or start the next task)"
    print(f"    PR:     {pr}")

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
    _print_entry(entries[-1], with_pr_state=True)
    return 0

def _journal_log(limit=0, chronological=False):
    """Print journal entries, newest first by default (git log semantics).

    The journal file appends chronologically, so the freshest entry sits
    at the bottom of an ever-growing wall and any head-clipping surfaces
    the OLDEST entries — which reads as stale state. --chronological
    restores oldest-first for reading history as a narrative; --limit
    clips to the N most recent entries in either order.
    """
    if not JOURNAL_FILE.exists() or not JOURNAL_FILE.stat().st_size:
        print("No .journal.org found.")
        return 0
    entries = _journal_entries(JOURNAL_FILE.read_text(encoding="utf-8"))
    if not entries:
        print("No entries in .journal.org.")
        return 0
    total = len(entries)
    if limit > 0:
        entries = entries[-limit:]
    if not chronological:
        entries = list(reversed(entries))
    shown = (f"{len(entries)} of {total} entries, "
             if len(entries) < total else
             f"{total} {'entry' if total == 1 else 'entries'}, ")
    order = "oldest first" if chronological else "newest first"
    print(f"📓 ores.compass — session journal ({shown}{order})\n")
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
    log = sub.add_parser("log", help="Show journal entries, newest first")
    log.add_argument("-n", "--limit", type=int, default=0, metavar="N",
                     help="Show only the N most recent entries (default: all)")
    log.add_argument("--chronological", action="store_true",
                     help="Oldest first, for reading history as a narrative")

    args = ap.parse_args(argv)
    if args.subcmd == "update":
        return _journal_update(args)
    if args.subcmd == "where":
        return _journal_where()
    if args.subcmd == "log":
        if args.limit < 0:
            print("❌ --limit must be a non-negative integer (0 = all).")
            return 1
        return _journal_log(args.limit, args.chronological)

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

_PR_STATE_CACHE = {}

def pr_state(number):
    """State of PR #<number> via gh: 'OPEN', 'MERGED' or 'CLOSED'. None on failure."""
    if number in _PR_STATE_CACHE:
        return _PR_STATE_CACHE[number]
    state = None
    try:
        out = subprocess.run(
            ["gh", "pr", "view", str(number), "--json", "state"],
            capture_output=True, text=True, cwd=str(PROJECT_ROOT), timeout=20)
        if out.returncode == 0:
            state = json.loads(out.stdout).get("state")
    except (OSError, subprocess.SubprocessError, json.JSONDecodeError):
        state = None
    _PR_STATE_CACHE[number] = state
    return state

_PR_STATE_CHIP = {
    "OPEN":   f"{_C_GREEN}[open]{_C_RESET}",
    "MERGED": f"{_C_MAGENTA}[merged]{_C_RESET}",
    "CLOSED": f"{_C_RED}[closed]{_C_RESET}",
}

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

def _task_uuids_from_story(story_file):
    """Return task UUIDs in the order they appear in the story's * Tasks table."""
    try:
        text = story_file.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return []
    m = re.search(r"^\* Tasks\s*$", text, re.MULTILINE)
    if not m:
        return []
    section = text[m.end():]
    nxt = re.search(r"^\* ", section, re.MULTILINE)
    if nxt:
        section = section[:nxt.start()]
    return re.findall(r"\[\[id:([A-F0-9a-f-]+)\]", section)


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


_TERMINAL_STATES = ("DONE", "ABANDONED")

def _audit_pr_states(numbers):
    """Map PR number -> state (OPEN/MERGED/CLOSED) via one gh graphql call.

    Returns {} when gh is unavailable so the audit degrades gracefully."""
    numbers = sorted(set(numbers))
    if not numbers:
        return {}
    try:
        repo = subprocess.run(
            ["gh", "repo", "view", "--json", "nameWithOwner",
             "--jq", ".nameWithOwner"],
            capture_output=True, text=True, cwd=str(PROJECT_ROOT), timeout=20)
        if repo.returncode != 0:
            return {}
        owner, name = repo.stdout.strip().split("/", 1)
        fields = " ".join(
            f'pr{n}: pullRequest(number: {n}) {{ state }}' for n in numbers)
        query = (f'query {{ repository(owner: "{owner}", name: "{name}") '
                 f'{{ {fields} }} }}')
        out = subprocess.run(
            ["gh", "api", "graphql", "-f", f"query={query}"],
            capture_output=True, text=True, cwd=str(PROJECT_ROOT), timeout=30)
        if out.returncode != 0:
            return {}
        data = json.loads(out.stdout).get("data", {}).get("repository")
        if not data:
            return {}
        return {int(k[2:]): v["state"] for k, v in data.items() if v}
    except (OSError, subprocess.SubprocessError, json.JSONDecodeError,
            KeyError, ValueError):
        return {}

def _sprint_table_states(text):
    """Map story uuid (upper) -> state recorded in the sprint page's tables."""
    states = {}
    row_re = re.compile(
        r"^\|\s*\[\[id:([A-Fa-f0-9-]+)\]\[.*?\]\]\s*\|\s*([A-Z]+)\s*\|",
        re.MULTILINE)
    for m in row_re.finditer(text):
        states[m.group(1).upper()] = m.group(2).upper()
    return states

def cmd_sprint_audit(args):
    """Implement compass sprint audit: flag agile state-sync violations."""
    _, current_sprint = current_version_sprint(doc_index.load_all())
    if current_sprint is None:
        print("❌ No current sprint found.", file=sys.stderr)
        return 1
    sprint_dir = Path(PROJECT_ROOT) / _parent_dir(current_sprint.rel_path)
    sprint_file = Path(PROJECT_ROOT) / current_sprint.rel_path

    try:
        sprint_text = sprint_file.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        sprint_text = None

    table_states = _sprint_table_states(sprint_text or "")
    missing_end_date = sprint_text is not None and not re.search(
        r"(?m)^#\+end_date:\s*\S", sprint_text)

    # Gather stories, tasks and every referenced PR number.
    pr_re = re.compile(r"pull/(\d+)")
    now_re = re.compile(r"(?m)^\| Now\s+\|([^|]*)\|")

    def _now_of(text):
        m = now_re.search(text)
        return m.group(1).strip() if m else None

    stories = []
    all_prs = set()
    for sf in sorted(sprint_dir.glob("*/story.org")):
        title, state, uuid = _read_story_state(sf)
        try:
            stext = sf.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            stext = ""
        tasks = []
        for tf in sorted(sf.parent.glob("task_*.org")):
            ttitle, tstate, tuuid, _, _ = _read_task_detail(tf)
            try:
                text = tf.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                text = ""
            prs = sorted({int(n) for n in pr_re.findall(text)})
            all_prs.update(prs)
            tasks.append({"title": ttitle, "state": tstate,
                          "uuid": (tuuid or "").upper(), "prs": prs,
                          "now": _now_of(text)})
        stories.append({"title": _strip_type_prefix(title or ""),
                        "state": state, "uuid": (uuid or "").upper(),
                        "dir": sf.parent.name, "tasks": tasks,
                        "now": _now_of(stext)})

    pr_states = _audit_pr_states(all_prs)
    if all_prs and not pr_states:
        print("⚠️  gh unavailable — skipping merged-PR checks.", file=sys.stderr)

    zombie, closeable, merged_open, mismatch, stale_now = [], [], [], [], []
    for s in stories:
        if (s["state"] in _TERMINAL_STATES and s["now"] is not None
                and s["now"] not in ("Nothing.", "")):
            stale_now.append(("story", s["title"], s["uuid"], s["state"], s["now"]))
        for t in s["tasks"]:
            if (t["state"] in _TERMINAL_STATES and t["now"] is not None
                    and t["now"] not in ("Nothing.", "")):
                stale_now.append(("task", t["title"], t["uuid"], t["state"], t["now"]))
        open_tasks = [t for t in s["tasks"] if t["state"] not in _TERMINAL_STATES]
        if s["state"] in _TERMINAL_STATES and open_tasks:
            zombie.append((s, open_tasks))
        if (s["state"] not in _TERMINAL_STATES and s["tasks"]
                and not open_tasks):
            closeable.append(s)
        for t in open_tasks:
            if (t["prs"] and pr_states
                    and all(pr_states.get(n) == "MERGED" for n in t["prs"])):
                merged_open.append((s, t, t["prs"]))
        table = table_states.get(s["uuid"])
        if table and table != s["state"]:
            mismatch.append((s, table))

    total = (len(zombie) + len(closeable) + len(merged_open) + len(mismatch)
             + len(stale_now) + (1 if missing_end_date else 0))

    if args.format == "json":
        findings = (
            [{"check": "done-story-with-open-tasks", "story": s["title"],
              "story_uuid": s["uuid"],
              "tasks": [{"title": t["title"], "state": t["state"],
                         "uuid": t["uuid"]} for t in ts]}
             for s, ts in zombie]
            + [{"check": "all-tasks-done-story-open", "story": s["title"],
                "story_uuid": s["uuid"], "state": s["state"]}
               for s in closeable]
            + [{"check": "task-open-all-prs-merged", "story": s["title"],
                "story_uuid": s["uuid"], "task": t["title"],
                "task_uuid": t["uuid"], "state": t["state"], "prs": merged}
               for s, t, merged in merged_open]
            + [{"check": "sprint-table-mismatch", "story": s["title"],
                "story_uuid": s["uuid"], "table_state": table,
                "story_state": s["state"]}
               for s, table in mismatch]
            + [{"check": "closed-doc-now-not-nothing", "kind": kind,
                "title": title, "uuid": uuid, "state": state, "now": now}
               for kind, title, uuid, state, now in stale_now]
            + ([{"check": "sprint-missing-end-date",
                 "sprint": current_sprint.title}]
               if missing_end_date else []))
        print(json.dumps({"sprint": current_sprint.title,
                          "findings": findings}, indent=2))
        return 0

    def _section(icon, title):
        print(f"\n{_C_BOLD}{_C_CYAN}{icon}  {title}{_C_RESET}")

    def _hint(uuid, indent):
        if uuid:
            print(f"{' ' * indent}{_ycmd(f'compass show {uuid}')}")

    print(f"🧭 ores.compass — sprint audit: {current_sprint.title}")
    if not total:
        print("\n  ✅ No state-sync violations found.")
        return 0

    if zombie:
        _section("🧟", "Closed stories with unresolved tasks")
        for s, ts in zombie:
            print(f"  • {s['title']}  {_C_RED}{s['state']}{_C_RESET} "
                  f"with {len(ts)} open task(s)")
            _hint(s["uuid"], 4)
            for t in ts:
                print(f"      {_C_YELLOW}{t['state']:<10}{_C_RESET} {t['title']}")
                _hint(t["uuid"], 17)

    if closeable:
        _section("🏁", "Stories ready to close — every task is done")
        for s in closeable:
            print(f"  • {s['title']}  ({s['state']}, "
                  f"{len(s['tasks'])}/{len(s['tasks'])} tasks done)")
            _hint(s["uuid"], 4)

    if merged_open:
        _section("🔀", "Open tasks whose pull requests have all merged")
        for s, t, merged in merged_open:
            prs = ", ".join(f"#{n}" for n in merged)
            print(f"  • {t['title']}  "
                  f"{_C_YELLOW}{t['state']}{_C_RESET}, merged {prs}")
            print(f"    story: {s['title']}")
            _hint(t["uuid"], 4)

    if mismatch:
        _section("⚖️", "Sprint table disagrees with the story file")
        for s, table in mismatch:
            print(f"  • {s['title']}  table says "
                  f"{_C_YELLOW}{table}{_C_RESET}, story file says "
                  f"{_C_YELLOW}{s['state']}{_C_RESET}")
            _hint(s["uuid"], 4)

    if stale_now:
        _section("🧹", "Closed documents whose Now is not 'Nothing.'")
        for kind, title, uuid, state, now in stale_now:
            print(f"  • {title}  ({kind}, {state}) Now: "
                  f"{_C_YELLOW}{now}{_C_RESET}")
            _hint(uuid, 4)

    if missing_end_date:
        _section("📅", "Sprint doc missing #+end_date")
        print(f"  • {current_sprint.title} has no #+end_date keyword.")
        print(f"    Set it to #+start_date + 7 days in {sprint_file.relative_to(Path(PROJECT_ROOT))}")

    print(f"\n  {total} finding(s).")
    return 0


_TAG_RE = re.compile(r"^[a-z][a-z0-9_.-]*$")
_FILETAGS_RE = re.compile(r"(?m)^#\+filetags:\s*:([^:\n]+(?::[^:\n]+)*):\s*$")


def _parse_tags_from_filetags_line(line: str) -> list:
    """Extract individual tag strings from a #+filetags: value."""
    m = re.match(r"#\+filetags:\s*:(.*):\s*$", line.strip())
    if not m:
        return []
    raw = m.group(1)
    return [t for t in raw.split(":") if t.strip()]


def cmd_lint(argv):
    """compass lint — validate filetags across all .org files."""
    ap = argparse.ArgumentParser(
        prog="compass lint",
        description=(
            "Validate #+filetags: values across every .org file in the repo. "
            "Every tag must be lowercase and match [a-z][a-z0-9_-]*. "
            "Prints the offending file and tag; exits non-zero on any violation."
        ),
    )
    ap.add_argument(
        "--path", default=".",
        help="Root directory to scan (default: repo root).",
    )
    ap.add_argument(
        "--exclude", action="append", default=[],
        metavar="DIR",
        help="Exclude a directory prefix (may be repeated). "
             "build/ and venv/ are always excluded.",
    )
    args = ap.parse_args(argv)

    root = Path(PROJECT_ROOT) / args.path
    always_exclude = {"build", "venv", ".git"}
    extra_exclude = set(args.exclude)
    all_exclude = always_exclude | extra_exclude

    violations = []

    for org_file in sorted(root.rglob("*.org")):
        rel = org_file.relative_to(Path(PROJECT_ROOT))
        first_part = rel.parts[0] if rel.parts else ""
        if first_part in all_exclude:
            continue
        try:
            text = org_file.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        for line in text.splitlines():
            if not line.startswith("#+filetags:"):
                continue
            tags = _parse_tags_from_filetags_line(line)
            for tag in tags:
                if not _TAG_RE.match(tag):
                    violations.append((str(rel), tag, line.strip()))
            break

    if not violations:
        print("✅  compass lint: all filetags are valid.")
        return 0

    print(f"❌  compass lint: {len(violations)} filetag violation(s):\n",
          file=sys.stderr)
    for path, tag, raw in violations:
        print(f"  {path}: unknown/malformed tag '{tag}'", file=sys.stderr)
        print(f"    {raw}", file=sys.stderr)
    return 1


def cmd_sprint(argv):
    """compass sprint — sprint-level operations."""
    ap = argparse.ArgumentParser(prog="compass sprint",
                                 description="Sprint-level operations.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    st = sub.add_parser("status", help="All stories in the current sprint grouped by state")
    st.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty")
    st.add_argument("--uuids", action="store_true",
                    help="Show story UUIDs alongside titles for use with other commands")

    au = sub.add_parser("audit",
                        help="Flag state-sync violations: done stories with "
                             "open tasks, closeable stories, open tasks whose "
                             "PRs all merged, sprint-table drift")
    au.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty")

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

    if args.subcmd == "audit":
        return cmd_sprint_audit(args)

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
            print(f"    [{tasks}] {s['title']}")
            if s["uuid"]:
                _u = s["uuid"].upper()
                print(f"           {_ycmd(f'compass show {_u}')}")
        return 0

def _org_link_text(link_str):
    """Extract plain title from an org-roam [[id:...][Title]] link, or return as-is."""
    m = _ORG_LINK_RE.match(link_str or "")
    return m.group(2) if m else link_str

def _worktree_env_value(worktree_path, key):
    """Read a single key from a worktree's .env; returns None if absent."""
    env_file = Path(worktree_path) / ".env"
    if not env_file.is_file():
        return None
    for line in env_file.read_text(encoding="utf-8").splitlines():
        if not line or line.lstrip().startswith("#") or "=" not in line:
            continue
        k, _, v = line.partition("=")
        if k.strip() == key:
            v = v.strip()
            if len(v) >= 2 and v[0] == v[-1] and v[0] in ("'", '"'):
                v = v[1:-1]
            return v
    return None


def cmd_fleet(args):
    worktrees = list_worktrees()
    if not worktrees:
        print("❌ Could not enumerate git worktrees.", file=sys.stderr)
        sys.exit(1)
    here = str(PROJECT_ROOT)
    pr_map = open_prs_by_branch()

    rows = []
    def _org_link_uuid(link_str):
        m = _ORG_LINK_RE.match(link_str or "")
        return m.group(1).upper() if m else None

    for path, branch in worktrees:
        journal = _journal_last_entry(path)
        if journal:
            story_title = _org_link_text(journal.get("story_link"))
            story_uuid  = _org_link_uuid(journal.get("story_link"))
            task_title  = _org_link_text(journal.get("task"))
            task_uuid   = _org_link_uuid(journal.get("task"))
            task_state  = journal.get("state")
            journal_branch = journal.get("branch")
        else:
            task_title = story_title = task_state = journal_branch = None
            story_uuid = task_uuid = None
            if branch:
                task_title, story_title = task_for_branch(path, branch)

        pr = pr_map.get(branch) if branch else None
        rows.append({
            "worktree": Path(path).name,
            "path": path,
            "current": os.path.realpath(path) == os.path.realpath(here),
            "branch": branch,
            "story": story_title,
            "story_uuid": story_uuid,
            "task": task_title,
            "task_uuid": task_uuid,
            "task_state": task_state,
            "journal_branch": journal_branch,
            "journal": bool(journal),
            "pr": ({"number": pr["number"], "state": pr["state"], "url": pr["url"]}
                   if pr else None),
            "staleness": branch_staleness(path),
            "provision_type": _worktree_env_value(path, "ORES_PROVISION_TYPE"),
            "env_name": _worktree_env_value(path, "ORES_ENV_NAME"),
        })

    if args.format == "json":
        print(json.dumps(rows, indent=2))
        return

    print(f"🧭 ores.compass — fleet ({len(rows)} worktrees)\n")

    def _trunc(s, n):
        s = s or ""
        return s if len(s) <= n else s[:n - 1] + "…"

    trows = []
    for r in rows:
        raw_name = r.get("env_name") or ""
        identity = raw_name.replace("-", " ").replace("_", " ")
        stale  = r["staleness"]
        behind = stale.get("behind", 0)
        ahead  = stale.get("ahead", 0)
        sync   = f"↑{ahead}↓{behind}" if (ahead or behind) else "✓"
        sev    = _staleness_severity(stale)
        trows.append({
            "mark":     "→" if r["current"] else " ",
            "worktree": r["worktree"],
            "identity": identity,
            "type":     r.get("provision_type") or "—",
            "branch":   r["branch"] or "(detached)",
            "task":     r["task"] or "—",
            "pr":       f"#{r['pr']['number']}" if r.get("pr") else "—",
            "sync":     sync,
            "sync_sev": sev,
        })

    _COLS = ["worktree", "identity", "type", "branch", "task", "pr", "sync"]
    _HDR  = {"worktree": "WORKTREE", "identity": "IDENTITY", "type": "TYPE",
              "branch": "BRANCH", "task": "TASK", "pr": "PR", "sync": "SYNC"}
    _MAX  = {"worktree": 28, "identity": 18, "type": 6,
              "branch": 38, "task": 38, "pr": 8, "sync": 8}

    widths = {k: len(_HDR[k]) for k in _COLS}
    for tr in trows:
        for k in _COLS:
            widths[k] = max(widths[k], min(len(tr[k]), _MAX[k]))

    sep = "─" * (2 + sum(widths[k] for k in _COLS) + 2 * (len(_COLS) - 1))
    print("  " + "  ".join(_HDR[k].ljust(widths[k]) for k in _COLS))
    print(sep)
    for tr in trows:
        cells = []
        for k in _COLS:
            raw = _trunc(tr[k], _MAX[k])
            if k == "sync":
                col = _C_SEV[tr["sync_sev"]]
                cells.append(col + raw.ljust(widths[k]) + _C_RESET)
            else:
                cells.append(raw.ljust(widths[k]))
        print(f"{tr['mark']} " + "  ".join(cells))

# --- Scaffold pillar: create agile/doc artefacts ---
#
# Generation belongs to ores.codegen, not compass: `add` calls codegen's
# doc_generate *as a library* (no shelling out, no copied generator). For
# the unambiguous cases it fills --parent-dir from the current sprint/version
# via Locate, so `compass add story --title ...` lands in the open sprint.

# Types whose parent compass can resolve from "where we are".
_DEFAULTABLE_PARENT = {"story": "sprint", "sprint": "version"}

# Types whose parent is a fixed folder, independent of "where we are".
_STATIC_PARENT = {
    "skill": "doc/llm/skills",
    "manual": "doc/manual/user_guide",
    "memory": "doc/llm/memory",
    "capture": "doc/agile/product_backlog/inbox",
    "facet": "projects/ores.codegen/library/templates",
    "facet_group": "projects/ores.codegen/library/templates",
}

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

def _capture_body(text):
    """A capture's body: everything from its first top-level heading."""
    m = re.search(r"^\* ", text, re.M)
    return text[m.start():].rstrip() if m else ""


_CAPTURE_PLACEHOLDER_BODIES = {
    "(One paragraph: the idea.)",
    "(Motivation, problem being solved, related context.)",
    "-",
    "",
}


def _is_capture_placeholder(text):
    return text.strip() in _CAPTURE_PLACEHOLDER_BODIES


def _filter_capture_body(body):
    """Strip sections whose content is only template filler.

    When every subsection is a placeholder (unfilled capture), returns
    an empty string so the caller omits the 'Promoted from capture' block.
    """
    if not body:
        return body
    parts = re.split(r"(?=^\* )", body, flags=re.M)
    kept = []
    for part in parts:
        if not part.strip():
            continue
        m = re.match(r"^\* [^\n]+\n?(.*)", part, re.S)
        if m and _is_capture_placeholder(m.group(1)):
            continue
        kept.append(part.rstrip())
    return "\n\n".join(kept)


def _cmd_capture_promote(argv):
    """compass capture promote — turn a capture into a story or task.

    The promoted doc REUSES the capture's UUID, so sprint-deferral
    records and backlog index references resolve to it. The capture
    body is carried over (What section as the goal; everything else
    preserved under 'Promoted from capture'), the capture file is
    removed, and the backlog indexes are regenerated.
    """
    ap = argparse.ArgumentParser(
        prog="compass capture promote",
        description="Promote a capture to a story or task, preserving "
                    "its UUID.")
    ap.add_argument("slug", help="Capture slug (any backlog bucket)")
    ap.add_argument("--to", required=True, choices=["story", "task"],
                    help="Promote to a new story, or a task on an "
                         "existing story")
    ap.add_argument("--story", default="",
                    help="Story UUID/prefix or folder slug (for --to task)")
    ap.add_argument("--title", default="",
                    help="Override the capture's title")
    ap.add_argument("--branch", default="",
                    help="Branch name (default: feature/<slug-kebab>)")
    args = ap.parse_args(argv)

    src = _find_capture(args.slug)
    if src is None:
        print(f"❌ No capture found for slug '{args.slug}'.",
              file=sys.stderr)
        return 1
    text = src.read_text(encoding="utf-8")
    cap_id = _read_org_id(src)
    title = args.title or _read_frontmatter_field(src, "title")
    description = _read_frontmatter_field(src, "description")
    created = _read_frontmatter_field(src, "created")
    if not cap_id:
        print(f"❌ Capture has no :ID:.", file=sys.stderr)
        return 1

    # Goal: the capture's What section when present and not a placeholder,
    # else fall back to the capture description.
    m = re.search(r"^\* What\s*\n(.*?)(?=^\* |\Z)", text, re.M | re.S)
    what_body = m.group(1).strip() if m else ""
    goal = ("" if _is_capture_placeholder(what_body) else what_body) or description

    # Resolve target locations.
    docs = doc_index.load_all()
    _, current_sprint = current_version_sprint(docs)
    if current_sprint is None:
        print("❌ No current sprint found.", file=sys.stderr)
        return 1
    sprint_dir = _parent_dir(current_sprint.rel_path)
    if args.to == "task":
        if not args.story:
            print("❌ --to task requires --story <id-or-slug>.",
                  file=sys.stderr)
            return 1
        story_dir, _story_title = resolve_story(args.story)
        if story_dir is None:
            print(f"❌ Could not resolve story '{args.story}'.",
                  file=sys.stderr)
            return 1
        parent_dir = story_dir
        new_rel = Path(parent_dir) / f"task_{args.slug}.org"
    else:
        parent_dir = sprint_dir
        new_rel = Path(parent_dir) / args.slug / "story.org"

    # Branch off fresh origin/main, as task new does.
    branch = args.branch or "feature/" + args.slug.replace("_", "-")
    subprocess.run(["git", "fetch", "origin", "main"], cwd=PROJECT_ROOT)
    r = subprocess.run(["git", "switch", "-c", branch, "origin/main"],
                       capture_output=True, text=True, cwd=PROJECT_ROOT)
    if r.returncode != 0:
        r = subprocess.run(["git", "switch", branch],
                           capture_output=True, text=True,
                           cwd=PROJECT_ROOT)
        if r.returncode != 0:
            print(f"❌ git switch failed:\n{r.stderr.strip()}",
                  file=sys.stderr)
            return 1
        print(f"✅ switched to existing {branch}")
    else:
        print(f"✅ created and switched to {branch} (off fresh "
              "origin/main)")

    # Scaffold with the capture's own UUID and content.
    doc_generate = _import_generator()
    gen_args = ["--type", args.to, "--slug", args.slug,
                "--parent-dir", str(parent_dir), "--title", title,
                "--description", description, "--id", cap_id,
                "--goal", goal]
    try:
        rc = doc_generate.main(gen_args)
    except SystemExit as exc:
        rc = exc.code
    if rc not in (None, 0):
        if isinstance(rc, str):
            print(rc, file=sys.stderr)
        return 1
    new_path = PROJECT_ROOT / new_rel
    print(f"✅ {args.to} scaffolded with the capture's UUID: {new_rel}")

    # Preserve the rest of the capture body, demoted one level.
    # Strip placeholder sections first so an unfilled capture doesn't
    # append an all-placeholder 'Promoted from capture' block.
    body = _filter_capture_body(_capture_body(text))
    if body:
        demoted = re.sub(r"^(\*+ )", r"*\1", body, flags=re.M)
        with new_path.open("a", encoding="utf-8") as f:
            f.write(f"\n* Promoted from capture\n\n"
                    f"Captured {created or 'earlier'} in the product "
                    f"backlog; promoted preserving the UUID.\n\n"
                    f"{demoted}\n")

    # Wire a task into its story; stories get the sprint hint (the
    # epic choice is judgement).
    if args.to == "task":
        _add_wire_task(["--parent-dir", str(parent_dir),
                        "--slug", args.slug])
    else:
        print(f"ℹ️  Wire the story into the sprint's * Stories table "
              f"(pick the epic):\n    story  {cap_id}\n"
              f"    sprint {current_sprint.id.upper()}  "
              f"({current_sprint.rel_path})")

    # Remove the capture and regenerate the bucket indexes.
    subprocess.run(["git", "rm", "-q", str(src)], cwd=PROJECT_ROOT,
                   capture_output=True, text=True)
    if src.exists():
        src.unlink()
    regen = (PROJECT_ROOT / "projects" / "ores.codegen" / "scripts"
             / "regenerate_backlog_indexes.py")
    subprocess.run([sys.executable, str(regen)], cwd=PROJECT_ROOT)
    print(f"✅ Capture removed; backlog indexes regenerated.")
    print(f"ℹ️  Next: compass task start "
          f"{args.slug if args.to == 'task' else '<first-task-slug>'}")
    return 0


def cmd_capture(argv):
    """Manage product backlog captures.

    Subcommands:
      (default)  --note "..."  [--slug <slug>]        Create a capture in inbox/.
      file <slug> next|deferred|discarded             Move an inbox capture to a bucket.
      promote <slug> --to story|task [--story <id>]   Promote, preserving the UUID.
    """
    if not argv or argv[0] in ("-h", "--help"):
        print(
            "usage:\n"
            "  compass capture --note \"...\" [--slug <slug>] [--commit] [--pr]\n"
            "      Create a capture in doc/agile/product_backlog/inbox/; --commit\n"
            "      regenerates the indexes and commits capture + indexes only;\n"
            "      --pr creates a capture/<slug> branch off origin/main, commits,\n"
            "      pushes, and opens a PR via gh — no task doc required.\n"
            "  compass capture file <slug> next|deferred|discarded\n"
            "      Move an inbox capture to a triaged product backlog bucket.\n"
            "  compass capture promote <slug> --to story|task [--story <id-or-slug>]\n"
            "      Promote a capture to a story or task, preserving its UUID;\n"
            "      carries content over, branches, removes the capture, regenerates indexes.\n"
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
        return _cmd_capture_promote(argv[1:])

    # --- default: create a new capture in inbox ---
    ap = argparse.ArgumentParser(prog="compass capture",
                                 description="Create a capture in the product backlog inbox.")
    ap.add_argument("--note", required=True, help="The note text (used as description and body).")
    ap.add_argument("--slug", default="", help="Snake_case slug (auto-generated from note if omitted).")
    ap.add_argument("--title", default="", help="Title (defaults to first 60 chars of note).")
    ap.add_argument("--tags", default="", help="Comma-separated tags.")
    ap.add_argument("--commit", action="store_true",
                    help="Regenerate the backlog indexes and commit the capture "
                         "plus indexes — and nothing else — with a conventional "
                         "'[agile] Capture: <title>' message.")
    ap.add_argument("--pr", action="store_true",
                    help="Create a capture/<slug> branch off origin/main, commit "
                         "the capture + regenerated indexes on it, push, and open "
                         "a PR via gh pr create. Implies --commit. After the PR is "
                         "opened the original branch is restored.")
    ap.add_argument("--co-author", default="Claude <noreply@anthropic.com>",
                    metavar="IDENT",
                    help="Co-Authored-By identity for --commit "
                         "(default: %(default)s; pass '' to omit the trailer)")
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
        if args.pr:
            print(f"  branch: capture/{slug.replace('_', '-')}")
            print(f"  pr title: [agile] Capture: {title}")
        elif args.commit:
            print(f"  commit: [agile] Capture: {title}")
        return 0

    if args.pr:
        return _capture_pr_flow(slug, title, args, inbox_dir, out_file)

    gen = _import_generator()
    inbox_dir.mkdir(parents=True, exist_ok=True)
    try:
        rc = gen.main(["--type", "capture", "--slug", slug,
                       "--parent-dir", str(inbox_dir.relative_to(PROJECT_ROOT)),
                       "--title", title, "--description", args.note,
                       "--tags", args.tags or "capture"])
    except SystemExit as exc:
        rc = exc.code
    if rc not in (None, 0):
        return rc or 1
    print(f"✅ Product backlog note created: {out_file.relative_to(PROJECT_ROOT)}")
    if args.commit:
        return _capture_commit(out_file, title, args.note, args.co_author)
    print("ℹ️  Use 'compass capture file <slug> next|deferred|discarded' to move to backlog,")
    print("   or 'compass capture promote <slug>' for instructions to make it a story/task.")
    return 0


def _capture_commit(out_file, title, note, co_author):
    """Commit a just-created capture and the regenerated bucket indexes.

    Captures filed mid-task interrupt the flow: each one needs a
    hand-written commit or risks getting tangled into unrelated staged
    work. Stage the capture file plus the bucket indexes explicitly and
    commit with a pathspec, so anything else already staged is left
    untouched.
    """
    regen = (PROJECT_ROOT / "projects" / "ores.codegen" / "scripts"
             / "regenerate_backlog_indexes.py")
    res = subprocess.run([sys.executable, str(regen)], cwd=str(PROJECT_ROOT))
    if res.returncode != 0:
        print("❌ Failed to regenerate backlog indexes — not committing.",
              file=sys.stderr)
        return res.returncode

    backlog = PROJECT_ROOT / "doc" / "agile" / "product_backlog"
    paths = [out_file] + [backlog / f"{b}.org"
                          for b in ("inbox", "next", "deferred")]
    rels = [str(p.relative_to(PROJECT_ROOT)) for p in paths if p.exists()]

    subject = f"[agile] Capture: {title}"
    if len(subject) > 72:
        subject = subject[:71].rstrip() + "…"
    parts = [subject]
    if note.strip():
        parts.append(note.strip())
    if co_author.strip():
        parts.append(f"Co-Authored-By: {co_author.strip()}")
    msg = "\n\n".join(parts) + "\n"

    # add first: the new capture file is untracked, and a commit
    # pathspec alone does not pick those up.
    subprocess.run(["git", "add", "--"] + rels, cwd=str(PROJECT_ROOT))
    staged = subprocess.run(
        ["git", "diff", "--cached", "--quiet", "--"] + rels,
        cwd=str(PROJECT_ROOT)).returncode != 0
    if not staged:
        print("ℹ️  Nothing to commit — capture and indexes unchanged.")
        return 0
    p = subprocess.run(["git", "commit", "-m", msg, "--"] + rels,
                       capture_output=True, text=True, cwd=str(PROJECT_ROOT))
    if p.returncode != 0:
        print(p.stderr.strip() or p.stdout.strip(), file=sys.stderr)
        return p.returncode
    print(f"✅ Capture committed: {subject}")
    return 0

def _capture_pr_flow(slug, title, args, inbox_dir, out_file):
    """Branch, create capture, commit, push, open PR — no task doc required.

    Stashes any tracked changes on the current branch, creates a fresh
    capture/<slug> branch off origin/main, generates the capture file and
    commits it with the regenerated indexes, pushes, and opens a PR via gh.
    The original branch is restored when done.
    """
    # Remember where we are.
    p = subprocess.run(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"],
        capture_output=True, text=True, cwd=str(PROJECT_ROOT))
    orig_branch = p.stdout.strip()

    # Stash any tracked modifications so the branch switch is clean.
    stashed = False
    has_staged   = subprocess.run(["git", "diff", "--cached", "--quiet"],
                                   cwd=str(PROJECT_ROOT)).returncode != 0
    has_unstaged = subprocess.run(["git", "diff", "--quiet"],
                                   cwd=str(PROJECT_ROOT)).returncode != 0
    if has_staged or has_unstaged:
        p = subprocess.run(
            ["git", "stash", "push", "-m", "compass capture --pr: save wip"],
            cwd=str(PROJECT_ROOT))
        stashed = (p.returncode == 0)

    def _restore(rc):
        subprocess.run(["git", "checkout", orig_branch], cwd=str(PROJECT_ROOT))
        if stashed:
            pop = subprocess.run(["git", "stash", "pop"], cwd=str(PROJECT_ROOT))
            if pop.returncode != 0:
                print("⚠️  Stash pop had conflicts — resolve manually.",
                      file=sys.stderr)
        return rc

    branch = "capture/" + slug.replace("_", "-")

    # Fetch and create the capture branch off origin/main.
    subprocess.run(["git", "fetch", "origin", "main"],
                   cwd=str(PROJECT_ROOT), capture_output=True)
    p = subprocess.run(
        ["git", "checkout", "-b", branch, "origin/main"],
        capture_output=True, text=True, cwd=str(PROJECT_ROOT))
    if p.returncode != 0:
        print(f"❌ Could not create branch '{branch}': {p.stderr.strip()}",
              file=sys.stderr)
        return _restore(p.returncode)

    # Create the capture file on this branch.
    gen = _import_generator()
    inbox_dir.mkdir(parents=True, exist_ok=True)
    try:
        rc = gen.main(["--type", "capture", "--slug", slug,
                       "--parent-dir", str(inbox_dir.relative_to(PROJECT_ROOT)),
                       "--title", title, "--description", args.note,
                       "--tags", args.tags or "capture"])
    except SystemExit as exc:
        rc = exc.code
    if rc not in (None, 0):
        return _restore(rc or 1)
    print(f"✅ Product backlog note created: {out_file.relative_to(PROJECT_ROOT)}")

    # Commit capture + regenerated indexes.
    rc = _capture_commit(out_file, title, args.note, args.co_author)
    if rc != 0:
        return _restore(rc)

    # Push the branch.
    p = subprocess.run(
        ["git", "push", "-u", "origin", branch],
        cwd=str(PROJECT_ROOT))
    if p.returncode != 0:
        print("❌ Push failed.", file=sys.stderr)
        return _restore(p.returncode)

    # Open the PR.
    pr_title = f"[agile] Capture: {title}"
    if len(pr_title) > 72:
        pr_title = pr_title[:71].rstrip() + "…"
    pr_body = (
        f"## Summary\n\n{args.note.strip()}\n\n"
        "## Changes\n\n"
        f"- Add capture `{slug}` to the product backlog inbox.\n"
        "- Regenerate inbox/next/deferred backlog indexes.\n\n"
        "🤖 Generated with [Claude Code](https://claude.com/claude-code)"
    )
    p = subprocess.run(
        ["gh", "pr", "create", "--title", pr_title, "--body", pr_body],
        capture_output=True, text=True, cwd=str(PROJECT_ROOT))
    if p.returncode != 0:
        print(p.stderr.strip() or "❌ gh pr create failed.", file=sys.stderr)
        return _restore(p.returncode)

    pr_url = p.stdout.strip().splitlines()[-1] if p.stdout.strip() else "(no URL)"
    print(f"🔗 PR created: {pr_url}")

    return _restore(0)


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
              "  types: story task sprint version recipe knowledge manual component\n"
              "         capture memory investigation product_identity skill\n"
              "         diagram entity_org field_group dataset_overview\n"
              "         facet facet_group technical_space archetype\n"
              "  --parent-dir defaults to the current sprint (story) or\n"
              "  version (sprint), doc/llm/skills (skill),\n"
              "  doc/manual/user_guide (manual), doc/llm/memory (memory),\n"
              "  or doc/agile/product_backlog/inbox (capture); required\n"
              "  otherwise.\n"
              "  diagram: scaffolds a .puml file with the standard licence header.\n"
              "  entity_org: scaffolds projects/ores.<component>/modeling/\n"
              "    ores.<component>.<slug>.org; requires --component, --slug,\n"
              "    --description.\n"
              "  field_group: scaffolds projects/ores.<component>/modeling/\n"
              "    ores.<component>.<slug>_field_group.org; requires\n"
              "    --component, --slug, --description.\n"
              "  memory: supports --statement, --why, --how-to-apply for a\n"
              "    one-shot body (no placeholder fill-in step).\n"
              "  dataset_overview: scaffolds projects/ores.seeder/datasets/<name>/\n"
              "    dataset_overview.org; requires --dataset, --description.\n"
              "  facet: scaffolds projects/ores.codegen/library/templates/\n"
              "    <slug>.org — literate source for one template family,\n"
              "    tangling to its .mustache artefacts.\n"
              "  facet_group: scaffolds projects/ores.codegen/library/templates/\n"
              "    <slug>_group.org — namespace doc indexing a group of facets.\n"
              "  remaining flags are passed through to ores.codegen "
              "(see 'How do I create a new doc?').")
        return 0

    doc_type = argv[0]
    rest = list(argv[1:])

    if doc_type == "diagram":
        return cmd_add_diagram(rest)

    has_parent = any(a == "--parent-dir" or a.startswith("--parent-dir=")
                     for a in rest)
    if not has_parent:
        if doc_type in _STATIC_PARENT:
            default_parent = _STATIC_PARENT[doc_type]
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
    # pystache. Only `add` / `story new` / `task new` require it; the other
    # commands stay dependency-free.
    doc_generate = _import_generator()

    # doc_generate.main returns 0 on success but may sys.exit on error;
    # catch SystemExit so the reminder prints only on success and the
    # generator's own error message/code is preserved.
    try:
        rc = doc_generate.main(["--type", doc_type, *rest])
    except SystemExit as exc:
        rc = exc.code
    if rc in (None, 0):
        wired = _add_wire_task(rest) if doc_type == "task" else False
        if not wired:
            hint = _add_wire_hint(doc_type, rest)
            if hint:
                print(hint, file=sys.stderr)
            elif doc_type != "task":
                print("ℹ️  Remember to wire the new artefact into its "
                      "parent (e.g. the sprint's * Stories table) where "
                      "needed.", file=sys.stderr)
        if doc_type == "skill":
            print("⚠️  Rebuild the skills bundle after filling in the skill body:",
                  file=sys.stderr)
            print(f"   {_ycmd('compass show B5F231A9-5403-482C-8F2A-12D6917C58CD')}",
                  file=sys.stderr)
        if doc_type == "manual":
            print("⚠️  Manual link rule: [[id:UUID]] links to documents outside"
                  " doc/manual/ will break the PDF export.", file=sys.stderr)
            print("   Use HTTP URLs for external references. Internal chapter"
                  " id-links are fine.", file=sys.stderr)
            print("   See: projects/ores.lisp/modeling/manual.org §Link conventions",
                  file=sys.stderr)
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


def _set_frontmatter_field(path, field, value):
    """Set or insert #+field: value in the file's frontmatter.

    Updates the existing line when present; otherwise inserts after
    #+updated: (or #+created: as fallback) so new fields land inside
    the frontmatter block rather than at the end of the file.
    """
    path = Path(path)
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return
    pattern = rf"^#\+{re.escape(field)}:.*$"
    new, count = re.subn(pattern, f"#+{field}: {value}", text, count=1,
                         flags=re.MULTILINE)
    if count == 0:
        for anchor_field in ("updated", "created"):
            anchor = re.search(rf'^#\+{anchor_field}:.*$', text,
                               flags=re.MULTILINE)
            if anchor:
                insert_pos = anchor.end()
                new = (text[:insert_pos] + f"\n#+{field}: {value}"
                       + text[insert_pos:])
                break
        else:
            new = text.rstrip() + f"\n#+{field}: {value}\n"
    if new != text:
        path.write_text(new, encoding="utf-8")


_ORG_ID_RE = re.compile(r"^[ \t]*:ID:\s+(\S+)\s*$", re.MULTILINE)
_ENV_FIELD_RE      = re.compile(r"^#\+environment:[ \t]*(\S+)", re.MULTILINE | re.IGNORECASE)
_BLOCKED_ON_RE     = re.compile(r"^#\+blocked_on:[ \t]*(\S+)", re.MULTILINE | re.IGNORECASE)
_BLOCKED_SINCE_RE  = re.compile(r"^#\+blocked_since:[ \t]*(\S+)", re.MULTILINE | re.IGNORECASE)

def _add_wire_task(rest):
    """Wire a freshly added task into its story's * Tasks table."""
    def _opt(name):
        for i, a in enumerate(rest):
            if a == name and i + 1 < len(rest):
                return rest[i + 1]
            if a.startswith(name + "="):
                return a.split("=", 1)[1]
        return None

    parent_dir, slug = _opt("--parent-dir"), _opt("--slug")
    if not parent_dir or not slug:
        return False
    task_path = Path(PROJECT_ROOT) / parent_dir / f"task_{slug}.org"
    story_path = Path(PROJECT_ROOT) / parent_dir / "story.org"
    task_id = _read_org_id(task_path)
    if not task_id or not story_path.exists():
        return False
    title = _org_doc_title(task_path, "Task")
    description = _read_frontmatter_field(task_path, "description")
    if _wire_task_into_story(story_path, task_id, title, description):
        print(f"🔗 Wired into {story_path.relative_to(PROJECT_ROOT)} "
              f"(* Tasks, BACKLOG).")
    return True


def _add_wire_hint(doc_type, rest):
    """Concrete wiring instruction for a freshly added task or story.

    Reads the new doc's UUID and its parent's UUID so the caller can
    paste a row into the right table without hunting for either id.
    Returns None when the ids cannot be derived (other doc types, or
    missing --slug/--parent-dir); the caller falls back to the generic
    reminder.
    """
    def _opt(name):
        for i, a in enumerate(rest):
            if a == name and i + 1 < len(rest):
                return rest[i + 1]
            if a.startswith(name + "="):
                return a.split("=", 1)[1]
        return None

    parent_dir, slug = _opt("--parent-dir"), _opt("--slug")
    if not parent_dir or not slug:
        return None
    spec = {
        "task": (f"task_{slug}.org", "story.org", "* Tasks", "story"),
        "story": (f"{slug}/story.org", "sprint.org", "* Stories", "sprint"),
    }.get(doc_type)
    if spec is None:
        return None
    new_rel, parent_name, table, parent_kind = spec
    new_path = PROJECT_ROOT / parent_dir / new_rel
    parent_path = PROJECT_ROOT / parent_dir / parent_name
    new_id = _read_org_id(new_path)
    parent_id = _read_org_id(parent_path)
    if not new_id or not parent_id:
        return None
    return (f"ℹ️  Wire the new {doc_type} into the {parent_kind}'s "
            f"{table} table:\n"
            f"    {doc_type:<6} {new_id}\n"
            f"    {parent_kind:<6} {parent_id}  "
            f"({parent_dir}/{parent_name})")


def _read_org_id(path):
    """Return the :ID: value from an org file, or None if absent/unreadable."""
    try:
        text = Path(path).read_text(encoding="utf-8")
        m = _ORG_ID_RE.search(text)
        return m.group(1) if m else None
    except (OSError, UnicodeDecodeError):
        return None


# --- Agile-table wiring: parent tables are compass side-effects ---

def _org_doc_title(path, kind):
    """#+title: value with the 'Task: '/'Story: ' prefix stripped."""
    title = _read_frontmatter_field(Path(path), "title")
    return re.sub(rf"(?i)^{kind}:\s*", "", title)


def _table_bounds(lines, heading):
    """Return (first, last) line indexes of the table under HEADING."""
    try:
        h = next(i for i, l in enumerate(lines)
                 if l.strip().startswith(heading))
    except StopIteration:
        return None, None
    first = None
    for i in range(h + 1, len(lines)):
        stripped = lines[i].strip()
        if stripped.startswith("|"):
            first = i
            break
        if stripped.startswith("* "):  # next heading, no table
            return None, None
    if first is None:
        return None, None
    last = first
    while last + 1 < len(lines) and lines[last + 1].strip().startswith("|"):
        last += 1
    return first, last


def _wire_story_into_sprint(sprint_path, story_id, title, description):
    """Append the story's BACKLOG row to the sprint's last * Stories table.

    Scans for the last column-header row matching '| Story ... | State' rather
    than using _table_bounds — sprint.org has multiple Stories tables under
    themed sub-sections (*** Epic: …) and we always want the last one.
    """
    text = sprint_path.read_text(encoding="utf-8")
    if story_id.upper() in text.upper():
        return False  # already wired
    lines = text.splitlines()
    # Sprint has multiple '| Story | State | …' tables; pick the last one.
    last_header = None
    for i, l in enumerate(lines):
        if l.strip().startswith("| Story") and "| State" in l:
            last_header = i
    if last_header is None:
        return False
    last = last_header
    while last + 1 < len(lines) and lines[last + 1].strip().startswith("|"):
        last += 1
    row = f"| [[id:{story_id}][{title}]] | BACKLOG | | | {description} |"
    cells = [c.strip() for c in lines[last].split("|")]
    if not any(cells):
        lines[last] = row
    else:
        lines.insert(last + 1, row)
    sprint_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _wire_task_into_story(story_path, task_id, title, description):
    """Append the task's BACKLOG row to the story's * Tasks table."""
    text = story_path.read_text(encoding="utf-8")
    if task_id.upper() in text.upper():
        return False  # already wired
    lines = text.splitlines()
    first, last = _table_bounds(lines, "* Tasks")
    if first is None:
        return False
    row = f"| [[id:{task_id}][{title}]] | BACKLOG | | | {description} |"
    # Replace an all-empty placeholder row when present, else append.
    cells = [c.strip() for c in lines[last].split("|")]
    if not any(cells):
        lines[last] = row
    else:
        lines.insert(last + 1, row)
    story_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True


def _update_task_row_in_story(story_path, task_id, state,
                              set_start=False, set_end=False):
    """Set the state/start/end cells of the task's row in * Tasks."""
    try:
        text = story_path.read_text(encoding="utf-8")
    except OSError:
        return False
    lines = text.splitlines()
    today = datetime.date.today().isoformat()
    for i, line in enumerate(lines):
        if f"[[id:{task_id.lower()}]" not in line.lower():
            continue
        cells = line.split("|")
        if len(cells) < 6:
            return False
        cells[2] = f" {state} "
        if set_start and not cells[3].strip():
            cells[3] = f" {today} "
        if set_end:
            cells[4] = f" {today} "
        lines[i] = "|".join(cells)
        story_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
        return True
    return False


def _remove_task_row_from_story(story_path, task_id):
    """Remove and return the task's row from the story's * Tasks table.

    Restores an empty placeholder row when the last data row is removed.
    Returns the raw row text, or None if the row was not found.
    """
    try:
        text = story_path.read_text(encoding="utf-8")
    except OSError:
        return None
    lines = text.splitlines()
    row_idx = None
    for i, line in enumerate(lines):
        if (f"[[id:{task_id.lower()}]" in line.lower()
                and line.strip().startswith("|")):
            row_idx = i
            break
    if row_idx is None:
        return None
    extracted = lines.pop(row_idx)
    # Recalculate table bounds after removal; restore placeholder when empty.
    first, last = _table_bounds(lines, "* Tasks")
    if first is not None:
        data_rows = [l for l in lines[first:last + 1] if "[[id:" in l]
        if not data_rows:
            lines.insert(last + 1, "|   |   |   |   |   |")
    story_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return extracted


def _set_doc_state(path, state):
    """Set the Status table's State row in a story or task doc."""
    text = path.read_text(encoding="utf-8")
    new_text, n = re.subn(r"^(\|\s*State\s*\|)\s*\S+(.*\|)\s*$",
                          rf"\1 {state}\2", text, count=1, flags=re.M)
    if n and new_text != text:
        path.write_text(new_text, encoding="utf-8")
        return True
    return False


def _set_status_field(path, field, value):
    """Set a prose Status row (Now/Next) in a story or task doc."""
    text = path.read_text(encoding="utf-8")
    new_text, n = re.subn(rf"^(\|\s*{field}\s*\|).*\|\s*$",
                          rf"\1 {value} |", text, count=1, flags=re.M)
    if n and new_text != text:
        path.write_text(new_text, encoding="utf-8")
        return True
    return False

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
    if len(cands) > 1:
        # Prefer the story in the current sprint to resolve slug ambiguity
        # across sprints (same approach as _cmd_task_start for tasks).
        _, current_sprint = current_version_sprint(doc_index.load_all())
        if current_sprint:
            sprint_dir = _parent_dir(current_sprint.rel_path)
            sprint_cands = [d for d in cands if d.rel_path.startswith(sprint_dir + "/")]
            if len(sprint_cands) == 1:
                d = sprint_cands[0]
                return _parent_dir(d.rel_path), _strip_type_prefix(d.title)
        # Still ambiguous — report candidates so callers can give a useful error.
        titles = ", ".join(
            f"'{_strip_type_prefix(d.title)}' ({_parent_dir(d.rel_path)})"
            for d in cands)
        print(f"❌ Ambiguous story '{ident}' — {len(cands)} matches: {titles}",
              file=sys.stderr)
    return None, None

def _scaffold_and_branch(sprint_dir, story_dir, story_title, new_story,
                          task_slug, task_title, task_desc, branch, base, dry_run,
                          current_sprint, goal="", acceptance=None):
    """Shared implementation for compass story new and compass task new."""
    if acceptance is None:
        acceptance = []
    task_path = Path(PROJECT_ROOT) / story_dir / f"task_{task_slug}.org"

    if dry_run:
        cmd = "story new" if new_story else "task new"
        print(f"compass {cmd} — plan (dry run):")
        print(f"  mode:    {'new story' if new_story else 'new task on existing story'}")
        print(f"  branch:  {branch}   (base {base})")
        print(f"  story:   {story_dir}/story.org   "
              f"({'new' if new_story else f'existing: {story_title}'})")
        if new_story:
            print(f"  scaffold task: {story_dir}/task_scaffold_{new_story[0]}.org"
                  f"   (#+branch: {branch}, STARTED — the scaffold PR closes it)")
            print(f"  task:    {story_dir}/task_{task_slug}.org   "
                  f"(BACKLOG, no branch — pick up with 'compass task start')")
        else:
            print(f"  task:    {story_dir}/task_{task_slug}.org   (#+branch: {branch})")
        print(f"  sprint:  {current_sprint.title}")
        if goal:
            print(f"  goal:    {goal[:72]}{'…' if len(goal) > 72 else ''}")
        if acceptance:
            for a in acceptance:
                print(f"  accept:  {a}")
        return 0

    gen = _import_generator()

    # 1. fetch + new branch off base.
    # story new: switch immediately so scaffold files land on the story branch.
    # task new on an existing story: create without switching so the caller's
    # worktree stays on its original branch; the user switches later with
    # 'compass task start'.
    subprocess.run(["git", "fetch", "origin"], cwd=str(PROJECT_ROOT), check=False)
    if new_story:
        sw = subprocess.run(["git", "switch", "-c", branch, base],
                            cwd=str(PROJECT_ROOT), capture_output=True, text=True)
        if sw.returncode != 0:
            print(f"❌ git switch -c {branch} {base} failed:\n{sw.stderr.strip()}",
                  file=sys.stderr)
            return 1
        print(f"✅ created and switched to {branch} (off {base})")
    else:
        sw = subprocess.run(["git", "branch", branch, base],
                            cwd=str(PROJECT_ROOT), capture_output=True, text=True)
        if sw.returncode != 0:
            print(f"❌ git branch {branch} {base} failed:\n{sw.stderr.strip()}",
                  file=sys.stderr)
            return 1
        print(f"✅ created branch {branch} (off {base}); worktree unchanged")

    # 2. scaffold story (new mode only) + task(s) via codegen. A new
    # story also gets a scaffold task: the scaffolding work (docs,
    # sprint wiring, the scaffold PR) is real work and rides its own
    # task, so the scaffold PR can never wrongly close the first real
    # task — see the work-lifecycle story.
    goal_args       = ["--goal", goal] if goal else []
    acceptance_args = [arg for a in acceptance for arg in ("--acceptance", a)]

    scaffold_slug = ""
    try:
        if new_story:
            slug, title, desc, tags = new_story
            rc = gen.main(["--type", "story", "--slug", slug, "--parent-dir", sprint_dir,
                           "--title", title, "--description", desc, "--tags", tags]
                          + goal_args + acceptance_args)
            if rc:
                return rc
            scaffold_slug = f"scaffold_{slug}"
            rc = gen.main(["--type", "task", "--slug", scaffold_slug,
                           "--parent-dir", story_dir,
                           "--title", f"Scaffold story: {title}",
                           "--description",
                           "Story scaffolding rides this task: documents, "
                           "sprint wiring, and the scaffold PR. Close it "
                           "before merging that PR."])
            if rc:
                return rc
        rc = gen.main(["--type", "task", "--slug", task_slug, "--parent-dir", story_dir,
                       "--title", task_title, "--description", task_desc]
                      + goal_args + acceptance_args)
        if rc:
            return rc
    except SystemExit as exc:
        if exc.code:
            print(f"❌ scaffolding failed: {exc.code}", file=sys.stderr)
        return exc.code or 0

    # 3. auto-wire: story → sprint * Stories; tasks → story * Tasks
    story_org_path = Path(PROJECT_ROOT) / story_dir / "story.org"
    if new_story:
        slug, title, desc, tags = new_story
        sprint_org = Path(PROJECT_ROOT) / sprint_dir / "sprint.org"
        story_id   = _read_org_id(story_org_path)
        if story_id and sprint_org.exists():
            if _wire_story_into_sprint(sprint_org, story_id, title, desc):
                print(f"🔗 Wired into {sprint_org.relative_to(PROJECT_ROOT)} "
                      f"(* Stories, BACKLOG).")
        scaffold_path = Path(PROJECT_ROOT) / story_dir / f"task_{scaffold_slug}.org"
        scaffold_id   = _read_org_id(scaffold_path)
        if scaffold_id and story_org_path.exists():
            scaffold_title = _org_doc_title(scaffold_path, "Task")
            scaffold_desc  = _read_frontmatter_field(scaffold_path, "description")
            if _wire_task_into_story(story_org_path, scaffold_id, scaffold_title, scaffold_desc):
                print(f"🔗 Wired scaffold task into {story_org_path.relative_to(PROJECT_ROOT)} "
                      f"(* Tasks, BACKLOG).")
    task_id = _read_org_id(task_path)
    if task_id and story_org_path.exists():
        task_title_clean = _org_doc_title(task_path, "Task")
        task_desc_field  = _read_frontmatter_field(task_path, "description")
        if _wire_task_into_story(story_org_path, task_id, task_title_clean, task_desc_field):
            print(f"🔗 Wired task into {story_org_path.relative_to(PROJECT_ROOT)} "
                  f"(* Tasks, BACKLOG).")

    # 4. the branch belongs to the scaffold task on a new story (the
    # real first task is picked up later via 'compass task start',
    # which derives its own branch); on an existing story the new task
    # owns the branch.
    if scaffold_slug:
        _set_frontmatter_branch(
            Path(PROJECT_ROOT) / story_dir / f"task_{scaffold_slug}.org",
            branch)
        rc = _cmd_task_start(scaffold_slug)
        if rc:
            return rc
    else:
        _set_frontmatter_branch(task_path, branch)

    # 5. next steps
    print("\nNext steps:")
    if new_story:
        print(f"  - commit, open the scaffold PR, and close the scaffold task "
              f"before merging: compass task done {scaffold_slug}")
        print(f"  - pick up the first task when work starts: "
              f"compass task start {task_path.stem.removeprefix('task_')}")
    else:
        task_rel = task_path.relative_to(PROJECT_ROOT)
        task_stem = task_path.stem.removeprefix('task_')
        print(f"  - git add {task_rel} && git commit -m 'scaffold: {task_title}'")
        print(f"  - compass task start {task_stem}   # clock on, switch branch, stamp journal")
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
    new_p.add_argument("--goal",        default="",    help="Goal prose for the story (skips placeholder)")
    new_p.add_argument("--acceptance",  action="append", default=[],
                       help="Acceptance bullet (repeatable)")
    new_p.add_argument("--base",        default="origin/main")
    new_p.add_argument("--kind",        default="feature", choices=["feature", "hotfix"])
    new_p.add_argument("--dry-run",     action="store_true")

    st = sub.add_parser("status", help="All tasks in a story grouped by state with branch and PR")
    st.add_argument("story", help="Story UUID/prefix or folder slug")
    st.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty")
    st.add_argument("--uuids", action="store_true",
                    help="Show task UUIDs alongside titles")

    tk = sub.add_parser("tasks", help="Compact one-line-per-task list: state title branch PR")
    tk.add_argument("story", help="Story UUID/prefix or folder slug")
    tk.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty")

    dn = sub.add_parser("done", help="Close a story: flip DONE, update sprint row, stamp journal")
    dn.add_argument("story", help="Story UUID/prefix or folder slug")

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
            branch, args.base, args.dry_run, current_sprint,
            goal=args.goal, acceptance=args.acceptance)

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
            print(f"    {t['title']}")
            if t["uuid"]:
                _u = t["uuid"].upper()
                print(f"           {_ycmd(f'compass show {_u}')}")
            if t["branch"]:
                print(f"           branch: {t['branch']}")
            if t["pr"] and t["pr"] != "none":
                print(f"           PR: {t['pr']}")
        return 0

    if args.subcmd == "tasks":
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

        _state_color = {
            "DONE":    _C_GREEN, "STARTED": _C_CYAN,
            "BLOCKED": _C_RED,   "BACKLOG":  _C_YELLOW,
        }
        for t in tasks:
            col   = _state_color.get(t["state"], "")
            state = f"{col}{t['state']:<8}{_C_RESET}"
            title = t["title"][:50].ljust(50)
            parts = [f"  {state}  {title}"]
            if t["branch"]:
                parts.append(f"branch: {t['branch']}")
            if t["pr"] and t["pr"] != "none":
                parts.append(f"PR: #{t['pr']}" if not t["pr"].startswith("#") else f"PR: {t['pr']}")
            print("  ".join(parts))
        return 0

    if args.subcmd == "done":
        return _cmd_story_done(args.story)


def _cmd_task_start(task_ident, branch_arg=""):
    """compass task start <slug-or-uuid> [--branch <name>] — clock on to an existing task."""
    docs = doc_index.load_all()
    il   = task_ident.lower()

    # Resolve by UUID (exact or prefix), slug, or branch suffix.
    # When a slug is ambiguous across sprints, prefer the current sprint.
    tasks  = [d for d in docs.values() if d.doctype == "task"]
    exact  = [d for d in tasks if d.id and d.id.lower() == il]
    prefix = [d for d in tasks if d.id and d.id.lower().startswith(il)]
    slug   = [d for d in tasks if Path(d.rel_path).stem.lower() == f"task_{il}"
                                or Path(d.rel_path).stem.lower() == il]
    cands  = list({d.rel_path: d for d in (exact or prefix) + slug}.values())

    if not cands:
        print(f"❌ No task matching '{task_ident}'.", file=sys.stderr)
        return 1
    if len(cands) > 1:
        # Prefer tasks in the current sprint to resolve slug ambiguity.
        _, current_sprint = current_version_sprint(docs)
        if current_sprint:
            sprint_dir = _parent_dir(current_sprint.rel_path)
            sprint_cands = [d for d in cands if d.rel_path.startswith(sprint_dir + "/")]
            if len(sprint_cands) == 1:
                cands = sprint_cands
        if len(cands) > 1:
            print(f"❌ Ambiguous — {len(cands)} tasks match '{task_ident}':", file=sys.stderr)
            for c in cands:
                print(f"   {c.id}  {c.rel_path}", file=sys.stderr)
            return 1

    task_doc  = cands[0]
    task_path = Path(PROJECT_ROOT) / task_doc.rel_path

    # Read branch from #+branch: frontmatter. When missing, use --branch
    # if supplied, else derive feature/<slug-kebab> from the task slug —
    # creating a task (docs only) and picking it up are distinct acts,
    # and pick-up must work on any task without manual branch plumbing.
    branch = _read_frontmatter_field(task_path, "branch")
    if not branch:
        if branch_arg:
            branch = branch_arg
        else:
            slug = re.sub(r'^task_', '', task_path.stem)
            branch = f"feature/{slug.replace('_', '-').lower()}"
            print(f"ℹ️  No #+branch: on the task — derived {branch} "
                  f"(override with --branch)")
        text = task_path.read_text(encoding="utf-8")
        new_text, count = re.subn(r'^(#\+branch:)[ \t]*\r?$', f'\\1 {branch}',
                                  text, count=1, flags=re.MULTILINE)
        if count == 0:
            print(f"❌ Could not find empty '#+branch:' line in {task_path.name}",
                  file=sys.stderr)
            return 1
        task_path.write_text(new_text, encoding="utf-8")
        print(f"📝 #+branch: set to {branch}")

    # Find the parent story (one directory up, story.org).
    story_path = task_path.parent / "story.org"
    story_uuid = _read_org_id(story_path) if story_path.exists() else None

    # Switch to the task branch. Skip if already on it (git switch errors in
    # some worktree configurations when the branch is already current).
    current_branch = _git_out("symbolic-ref", "--short", "HEAD", cwd=PROJECT_ROOT)
    if current_branch == branch:
        print(f"✅ already on {branch}")
    else:
        result = subprocess.run(["git", "switch", branch], capture_output=True, text=True,
                                cwd=PROJECT_ROOT)
        if result.returncode != 0:
            # New branch: fetch first so it starts from the latest main.
            subprocess.run(["git", "fetch", "origin", "main"],
                           cwd=PROJECT_ROOT)
            result2 = subprocess.run(
                ["git", "switch", "-c", branch, "origin/main"],
                capture_output=True, text=True, cwd=PROJECT_ROOT)
            if result2.returncode != 0:
                print(f"❌ git switch failed:\n{result2.stderr.strip()}", file=sys.stderr)
                return 1
            print(f"✅ created and switched to {branch} (off fresh origin/main)")
        else:
            print(f"✅ switched to {branch}")

    # Re-resolve task_path after the switch: if the branch diverged before a
    # sprint transition the task file may live at a different path than the one
    # resolved on the caller's branch.  Match by UUID so the lookup is stable.
    if task_doc.id:
        docs_after = doc_index.load_all()
        id_lower = task_doc.id.lower()
        for _d in docs_after.values():
            if _d.id and _d.id.lower() == id_lower:
                task_path = Path(PROJECT_ROOT) / _d.rel_path
                story_path = task_path.parent / "story.org"
                break

    # If the task file still does not exist on this branch (e.g. the branch
    # predates the commit that added the file), restore it from origin/main so
    # the clock-on can proceed without requiring a manual cherry-pick.
    if not task_path.exists():
        rel = task_path.relative_to(PROJECT_ROOT)
        subprocess.run(
            ["git", "checkout", "origin/main", "--", str(rel)],
            cwd=PROJECT_ROOT, capture_output=True)
        if task_path.exists():
            print(f"📥 restored {rel} from origin/main onto {branch}")
        else:
            print(f"❌ {task_path.name} not found on {branch} or origin/main",
                  file=sys.stderr)
            return 1
        # Restore the story too if it's missing.
        if not story_path.exists():
            story_rel = story_path.relative_to(PROJECT_ROOT)
            r2 = subprocess.run(
                ["git", "checkout", "origin/main", "--", str(story_rel)],
                cwd=PROJECT_ROOT, capture_output=True)
            if r2.returncode != 0 or not story_path.exists():
                print(f"⚠️  {story_path.name} not found on origin/main — story row will not be updated",
                      file=sys.stderr)

    # Flip BACKLOG → STARTED in the task file if needed.
    text = task_path.read_text(encoding="utf-8")
    new_text, n = re.subn(r'(\| State\s+\|) BACKLOG', r'\1 STARTED', text, count=1)
    if n:
        task_path.write_text(new_text, encoding="utf-8")
        print(f"📝 state: BACKLOG → STARTED")

    # Mirror into the story: task row STARTED with start date, and the
    # story itself STARTED (reopens a DONE story for follow-up work).
    task_uuid = _read_org_id(task_path)
    if story_path.exists() and task_uuid:
        if _update_task_row_in_story(story_path, task_uuid, "STARTED",
                                     set_start=True):
            print(f"🔗 story * Tasks row → STARTED")
        if _set_doc_state(story_path, "STARTED"):
            print(f"🔗 story state → STARTED")

    # Stamp the working environment on the task (and story on first STARTED).
    env_label = _read_env_map().get("ORES_CHECKOUT_LABEL", "")
    if env_label:
        _set_frontmatter_field(task_path, "environment", env_label)
        if story_path.exists():
            _set_frontmatter_field(story_path, "environment", env_label)

    # Stamp the journal.
    try:
        if story_uuid and task_uuid:
            _journal_update(argparse.Namespace(
                story=story_uuid, task=task_uuid, branch=branch,
                state="STARTED", pr="none"))
    except Exception as e:
        print(f"⚠️  journal update failed: {e}", file=sys.stderr)

    return 0


def _cmd_task_done(task_ident, pr=""):
    """compass task done <slug-or-uuid> [--pr N] — close a task out.

    Task doc: State → DONE, Now/Next → Nothing. Story: task row →
    DONE with end date; when every row is DONE, suggest closing the
    story. Journal stamped with DONE. The * Result prose stays with
    the session.
    """
    docs = doc_index.load_all()
    il = task_ident.lower()
    tasks = [d for d in docs.values() if d.doctype == "task"]
    cands = [d for d in tasks
             if (d.id and (d.id.lower() == il
                           or d.id.lower().startswith(il)))
             or Path(d.rel_path).stem.lower() in (f"task_{il}", il)]
    if len(cands) != 1:
        print(f"❌ {'No' if not cands else 'Ambiguous'} task "
              f"matching '{task_ident}'.", file=sys.stderr)
        return 1
    task_path = Path(PROJECT_ROOT) / cands[0].rel_path
    task_uuid = _read_org_id(task_path)

    _set_doc_state(task_path, "DONE")
    _set_status_field(task_path, "Now", "Nothing.")
    _set_status_field(task_path, "Next", "Nothing.")
    env_label = _read_env_map().get("ORES_CHECKOUT_LABEL", "")
    if env_label:
        _set_frontmatter_field(task_path, "environment", env_label)
    print(f"📝 task state → DONE ({task_path.name})")

    story_path = task_path.parent / "story.org"
    story_uuid = _read_org_id(story_path) if story_path.exists() else None
    if story_uuid and task_uuid:
        if _update_task_row_in_story(story_path, task_uuid, "DONE",
                                     set_end=True):
            print("🔗 story * Tasks row → DONE")
        # When no row remains in another state, suggest closing the story.
        story_lines = story_path.read_text(encoding="utf-8").splitlines()
        first, last = _table_bounds(story_lines, "* Tasks")
        if first is not None:
            rows = story_lines[first:last + 1]
            states = [r.split("|")[2].strip() for r in rows
                      if "[[id:" in r and len(r.split("|")) > 2]
            if states and all(s == "DONE" for s in states):
                print("ℹ️  All story tasks are DONE — consider closing "
                      "the story (State, Now, Next) and its sprint row.")

    pr_value = pr or _read_frontmatter_field(task_path, "pr") or "none"
    branch = _read_frontmatter_field(task_path, "branch") or "(none)"
    try:
        if story_uuid and task_uuid:
            _journal_update(argparse.Namespace(
                story=story_uuid, task=task_uuid, branch=branch,
                state="DONE", pr=str(pr_value)))
    except Exception as e:
        print(f"⚠️  journal update failed: {e}", file=sys.stderr)
    print("ℹ️  Write the task's * Result before committing.")
    return 0


def _read_frontmatter_field(path, field):
    """Return the value of #+<field>: from an org file, or empty string."""
    prefix = f"#+{field}:"
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.lower().startswith(prefix.lower()):
            return line[len(prefix):].strip()
    return ""


def _cmd_story_done(story_ident):
    """compass story done <slug-or-uuid> — close a story.

    Verifies all child tasks are DONE (or ABANDONED), flips the story doc
    State → DONE and stamps Now/Next, updates the sprint's * Stories row,
    and stamps the journal.
    """
    story_dir_rel, _ = resolve_story(story_ident)
    if story_dir_rel is None:
        print(f"❌ Could not resolve story '{story_ident}'.", file=sys.stderr)
        return 1

    story_dir  = Path(PROJECT_ROOT) / story_dir_rel
    story_path = story_dir / "story.org"
    story_uuid = _read_org_id(story_path) if story_path.exists() else None

    # Guard: all tasks must be in a terminal state.
    open_tasks = []
    for tf in sorted(story_dir.glob("task_*.org")):
        _, t_state, *_ = _read_task_detail(tf)
        if t_state not in _TERMINAL_STATES:
            open_tasks.append((tf.name, t_state))
    if open_tasks:
        print("❌ Story has unresolved tasks — close or abandon them first:",
              file=sys.stderr)
        for fname, st in open_tasks:
            print(f"   {st:<10} {fname}", file=sys.stderr)
        return 1

    # Flip story doc → DONE.
    _set_doc_state(story_path, "DONE")
    _set_status_field(story_path, "Now", "Nothing.")
    _set_status_field(story_path, "Next", "Nothing.")
    print(f"📝 story state → DONE ({story_path.name})")

    # Update the sprint's * Stories table row.
    sprint_path = story_dir.parent / "sprint.org"
    if sprint_path.exists() and story_uuid:
        updated = _update_task_row_in_story(sprint_path, story_uuid, "DONE",
                                            set_end=True)
        if updated:
            print("🔗 sprint * Stories row → DONE")
        else:
            print("⚠️  sprint * Stories row not found — update manually",
                  file=sys.stderr)

    # Journal: use the current git branch (stories rarely carry #+branch:).
    branch = (_git_out("symbolic-ref", "--short", "HEAD", cwd=PROJECT_ROOT)
              or "(none)")
    try:
        # task= story_uuid intentionally: there is no task when closing a story.
        _journal_update(argparse.Namespace(
            story=story_uuid, task=story_uuid, branch=branch,
            state="DONE", pr="none"))
    except Exception as e:
        print(f"⚠️  journal update failed: {e}", file=sys.stderr)
    print("ℹ️  Write the story's * Result before committing.")
    return 0


def _cmd_task_move(task_ident, story_ident):
    """compass task move <slug-or-uuid> --story <target> — rehome a task.

    Moves the task file to the target story's folder (UUID intact), removes
    its row from the source story's * Tasks table, adds it to the target's
    table (preserving state/start/end), and rewrites the task's parent links
    (#+filetags, intro org-id link, Parent story row).
    """
    docs = doc_index.load_all()
    il = task_ident.lower()
    tasks = [d for d in docs.values() if d.doctype == "task"]
    cands = [d for d in tasks
             if (d.id and (d.id.lower() == il or d.id.lower().startswith(il)))
             or Path(d.rel_path).stem.lower() in (f"task_{il}", il)]
    if not cands:
        print(f"❌ No task matching '{task_ident}'.", file=sys.stderr)
        return 1
    if len(cands) > 1:
        titles = ", ".join(f"'{d.title}' ({d.rel_path})" for d in cands)
        print(f"❌ Ambiguous — {len(cands)} tasks match '{task_ident}': {titles}",
              file=sys.stderr)
        return 1

    task_doc = cands[0]
    task_path = Path(PROJECT_ROOT) / task_doc.rel_path
    task_uuid = task_doc.id

    old_story_path = task_path.parent / "story.org"
    old_story_uuid = _read_org_id(old_story_path) if old_story_path.exists() else None
    old_story_slug  = task_path.parent.name
    old_sprint_slug = task_path.parent.parent.name
    old_version_slug = task_path.parent.parent.parent.name

    new_story_dir, new_story_title = resolve_story(story_ident)
    if new_story_dir is None:
        return 1  # resolve_story already printed the error

    new_story_path = Path(PROJECT_ROOT) / new_story_dir / "story.org"
    new_story_uuid  = _read_org_id(new_story_path)
    new_story_slug  = Path(new_story_dir).name
    new_sprint_slug  = Path(new_story_dir).parent.name
    new_version_slug = Path(new_story_dir).parent.parent.name

    if (old_story_uuid and new_story_uuid
            and old_story_uuid.upper() == new_story_uuid.upper()):
        print(f"❌ Task is already in story '{new_story_title}'.", file=sys.stderr)
        return 1

    new_task_path = Path(PROJECT_ROOT) / new_story_dir / task_path.name
    if new_task_path.exists():
        print(f"❌ {task_path.name} already exists in {new_story_dir}/.",
              file=sys.stderr)
        return 1

    task_title = _org_doc_title(task_path, "task")
    task_description = _read_frontmatter_field(task_path, "description")

    # 1. Extract the row from the source story (preserves state/start/end).
    extracted_row = None
    if old_story_path.exists():
        extracted_row = _remove_task_row_from_story(old_story_path, task_uuid)
        if extracted_row:
            print(f"🔗 {old_story_slug}/story.org * Tasks row removed")

    # 2. Rewrite the task file: filetags, story UUID/title links.
    text = task_path.read_text(encoding="utf-8")

    def _replace_filetag_slug(text, old_slug, new_slug):
        return re.sub(
            r"(#\+filetags:\s*)(.+)",
            lambda m: m.group(1) + m.group(2).replace(
                f":{old_slug}:", f":{new_slug}:"),
            text, flags=re.IGNORECASE)

    text = _replace_filetag_slug(text, old_story_slug,  new_story_slug)
    if old_sprint_slug != new_sprint_slug:
        text = _replace_filetag_slug(text, old_sprint_slug,  new_sprint_slug)
    if old_version_slug != new_version_slug:
        text = _replace_filetag_slug(text, old_version_slug, new_version_slug)

    # Replace [[id:OLD-STORY-UUID][...]] with [[id:NEW-UUID][New Title]].
    if old_story_uuid and new_story_uuid:
        text = re.sub(
            r"\[\[id:" + re.escape(old_story_uuid) + r"\]\[[^\]]*\]\]",
            f"[[id:{new_story_uuid}][{new_story_title}]]",
            text, flags=re.IGNORECASE)

    task_path.write_text(text, encoding="utf-8")
    print("📝 task file updated (filetags, parent links)")

    # 3. Insert the row into the target story's * Tasks table.
    if extracted_row:
        target_text  = new_story_path.read_text(encoding="utf-8")
        target_lines = target_text.splitlines()
        _, last = _table_bounds(target_lines, "* Tasks")
        if last is not None:
            cells = [c.strip() for c in target_lines[last].split("|")]
            if not any(c for c in cells if c):
                target_lines[last] = extracted_row
            else:
                target_lines.insert(last + 1, extracted_row)
            new_story_path.write_text(
                "\n".join(target_lines) + "\n", encoding="utf-8")
            print(f"🔗 {new_story_slug}/story.org * Tasks row added")
        else:
            print(f"⚠️  Could not find * Tasks table in {new_story_dir}/story.org",
                  file=sys.stderr)
    else:
        _wire_task_into_story(new_story_path, task_uuid, task_title, task_description)
        print(f"🔗 {new_story_slug}/story.org * Tasks row added (BACKLOG, row was absent)")

    # 4. Move the file.
    import shutil
    shutil.move(str(task_path), str(new_task_path))
    print(f"✅ {task_path.name} → {new_story_dir}/")
    return 0


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
    new_p.add_argument("--goal",        default="",  help="Goal prose for the task (skips placeholder)")
    new_p.add_argument("--acceptance",  action="append", default=[],
                       help="Acceptance bullet (repeatable)")
    new_p.add_argument("--base",      default="origin/main")
    new_p.add_argument("--kind",      default="feature", choices=["feature", "hotfix"])
    new_p.add_argument("--branch",    default="",
                       help="Override the auto-derived branch name "
                            "(default: <kind>/<slug-kebab>). Useful when "
                            "several tasks share one feature branch.")
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
    start_p.add_argument("--branch", default="",
                         help="Branch name to write into #+branch: if the task "
                              "has no branch set yet (creates the branch off "
                              "origin/main when it does not already exist)")

    done_p = sub.add_parser(
        "done",
        help="Close a task: DONE in task and story tables, journal stamped")
    done_p.add_argument("task", help="Task slug or UUID/prefix")
    done_p.add_argument("--pr", default="",
                        help="PR number for the journal (default: #+pr:)")

    close_p = sub.add_parser(
        "close",
        help="Alias for 'done': close a task (DONE in task and story tables)")
    close_p.add_argument("task", help="Task slug or UUID/prefix")
    close_p.add_argument("--pr", default="",
                         help="PR number for the journal (default: #+pr:)")

    move_p = sub.add_parser(
        "move",
        help="Relocate a task to a different story, UUID intact")
    move_p.add_argument("task", help="Task slug or UUID/prefix")
    move_p.add_argument("--story", required=True,
                        help="Target story UUID/prefix or folder slug")

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
        branch     = (args.branch.strip()
                      or args.kind + "/" + args.task_slug.replace("_", "-"))
        return _scaffold_and_branch(
            sprint_dir, story_dir, story_title, None,
            args.task_slug, args.title, task_desc,
            branch, args.base, args.dry_run, current_sprint,
            goal=args.goal, acceptance=args.acceptance)

    if args.subcmd == "start":
        return _cmd_task_start(args.task, getattr(args, "branch", ""))

    if args.subcmd in ("done", "close"):
        return _cmd_task_done(args.task, getattr(args, "pr", ""))

    if args.subcmd == "move":
        return _cmd_task_move(args.task, args.story)

def cmd_env(argv):
    """compass env — Provision pillar: environment setup."""
    if argv and argv[0] == "provision":
        import env_create
        return env_create.run_provision(argv[1:], PROJECT_ROOT)
    if argv and argv[0] == "deprovision":
        import env_create
        return env_create.run_deprovision(argv[1:], PROJECT_ROOT)
    if argv and argv[0] == "configure":
        import env_init
        return env_init.run(argv[1:], PROJECT_ROOT)
    if argv and argv[0] == "install-packages":
        import env_packages
        return env_packages.run(argv[1:], PROJECT_ROOT)
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
    if argv and argv[0] == "upgrade":
        import env_upgrade
        return env_upgrade.run(argv[1:], PROJECT_ROOT)
    # No/unknown subcommand: render help (and error on unknown).
    ap = argparse.ArgumentParser(prog="compass env",
                                 description="Provision: checkout environment setup.")
    sub = ap.add_subparsers(dest="subcmd", required=True)
    sub.add_parser("provision", help="Provision a new named worktree (adjective-noun name, "
                                    "e.g. festive-hawking) under ../ores_dev_<name>; "
                                    "optionally run configure with --preset.")
    sub.add_parser("deprovision", help="Remove a named worktree and its directory.")
    sub.add_parser("configure", help="Generate .env + NATS certs + IAM key "
                                     "(reuses existing secrets; --with-diff to show changes)")
    sub.add_parser("install-packages", help="Install system packages (baseline, "
                                            "--with-qt, --with-valgrind, --full-install)")
    sub.add_parser("diff", help="Unified diff of .env.old vs .env")
    sub.add_parser("list", help="List .env vars grouped (secrets masked; --show-secrets to reveal)")
    sub.add_parser("version", help="Show the .env-format version; 'version new <desc>' records a new one")
    sub.add_parser("upgrade", help="Promote a light environment to full (C++/vcpkg): "
                                   "patches .env, initialises vcpkg submodule, runs cmake configure")
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


def cmd_pr(argv):
    """compass pr — PR pillar: pull-request lifecycle verbs via gh."""
    import compass_pr
    return compass_pr.run(argv, PROJECT_ROOT)


def cmd_review(argv):
    """compass review — Review pillar: PR review-round verbs via gh."""
    import compass_review
    return compass_review.run(argv, PROJECT_ROOT)


def cmd_nats(argv):
    """compass nats — NATS pillar: config and certificate management."""
    if argv and argv[0] == "certs":
        import nats_certs
        return nats_certs.run(argv[1:], PROJECT_ROOT)
    if argv and argv[0] == "init":
        import nats_init
        return nats_init.run(argv[1:], PROJECT_ROOT)
    print("Usage: compass nats <subcommand>", file=sys.stderr)
    print("Subcommands: init, certs", file=sys.stderr)
    return 1


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

    lp = sub.add_parser("logging",
                        help="Toggle test logging: 'logging on [level]', "
                             "'logging off', 'logging status'")
    lp.add_argument("action", choices=["on", "off", "status"],
                    help="on: enable test logging; off: disable; "
                         "status: show current .env settings")
    lp.add_argument("level", nargs="?", default="debug",
                    help="Log level for 'on' (trace, debug, info, warn, error; "
                         "default: debug)")

    args = ap.parse_args(argv)

    if args.subcmd == "results":
        return _cmd_test_results(args)
    if args.subcmd == "logging":
        return _cmd_test_logging(args)


def _cmd_test_logging(args):
    """compass test logging on|off|status — toggle test logging in .env.

    Delegates to env_init._logging_only: the three ORES_TEST_LOG_* vars in
    .env are forwarded verbatim to test processes by the root CMakeLists,
    and .env is a CMAKE_CONFIGURE_DEPENDS so the next build re-configures
    automatically.
    """
    env_file = PROJECT_ROOT / ".env"
    if args.action == "status":
        if not env_file.is_file():
            print("❌ No .env found. Run 'compass env configure' first.",
                  file=sys.stderr)
            return 1
        vals = {}
        for line in env_file.read_text(encoding="utf-8").splitlines():
            key, _, val = line.partition("=")
            if key.strip().startswith("ORES_TEST_LOG_"):
                vals[key.strip()] = val.strip()
        if not vals:
            print("Test logging: disabled (no ORES_TEST_LOG_* vars in .env)")
            print("  Enable with: compass test logging on [level]")
        else:
            enabled = vals.get("ORES_TEST_LOG_ENABLED", "false") == "true"
            print(f"Test logging: {'enabled' if enabled else 'disabled'}")
            for k in sorted(vals):
                print(f"  {k}={vals[k]}")
        return 0

    import env_init
    return env_init._logging_only(
        env_file, "enable" if args.action == "on" else "disable", args.level)


def _cmd_test_results(args):
    import glob as _glob
    import re as _re
    import os as _os

    preset = args.preset or _tr_read_preset()
    if not preset:
        print("❌ No preset supplied and ORES_PRESET not set in .env.\n"
              "   Pass --preset <name> or run compass env configure.", file=sys.stderr)
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
          f"Logs: {log_dir or 'none'}\n")

    if log_dir is None:
        print("⚠️  No log directory found. To enable test logging:")
        print("   compass test logging on [trace|debug|info|warn|error]")
        print("   The change is picked up automatically on the next test run")
        print("   (.env is a CMake configure dependency). Then re-run the tests.\n")

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


# --- Command suggestion (Levenshtein) ---

def _levenshtein(a, b):
    """Compute edit distance between two strings."""
    m, n = len(a), len(b)
    dp = list(range(n + 1))
    for i in range(1, m + 1):
        prev, dp[0] = dp[0], i
        for j in range(1, n + 1):
            temp = dp[j]
            dp[j] = prev if a[i - 1] == b[j - 1] else 1 + min(prev, dp[j], dp[j - 1])
            prev = temp
    return dp[n]


def _closest_command(given, known, max_dist=2):
    """Return the closest known command within max_dist edits, or None."""
    given = given.lower()
    best, best_dist = None, max_dist + 1
    for cmd in known:
        d = _levenshtein(given, cmd)
        if d < best_dist:
            best, best_dist = cmd, d
    return best if best_dist <= max_dist else None


# --- Bearings: cold-start orientation ---

def _ycmd(cmd):
    """Render a compass command in yellow."""
    return f"{_C_YELLOW}{cmd}{_C_RESET}"


def _bearings_section(icon, title, cmd=None):
    print(f"\n{_C_BOLD}{_C_CYAN}{icon}  {title}{_C_RESET}")
    if cmd:
        print(f"    {_C_YELLOW}{cmd}{_C_RESET}")


def _claude_refresh_warnings():
    """Warn when generated .claude/ artefacts are missing or older than their org sources.

    .claude/ is never checked in; settings.json tangles from
    doc/llm/claude_code_settings.org and skills deploy from doc/llm/skills.
    After a pull that touches the org sources the generated copies go stale
    silently — surface that at orientation time, when it is cheapest to fix.
    For skills the comparison is conservative: the OLDEST deployed file
    against the NEWEST source file, so any partially-stale deployment warns.
    """
    warnings = []
    settings = PROJECT_ROOT / ".claude/settings.json"
    settings_src = PROJECT_ROOT / "doc/llm/claude_code_settings.org"
    if settings_src.exists():
        if (not settings.exists()
                or settings.stat().st_mtime < settings_src.stat().st_mtime):
            warnings.append((".claude/settings.json",
                             "doc/llm/claude_code_settings.org",
                             "compass build --direct settings"))
    skills = PROJECT_ROOT / ".claude/skills"
    skills_src = PROJECT_ROOT / "doc/llm/skills"
    if skills_src.exists():
        src = [f.stat().st_mtime for f in skills_src.rglob("*") if f.is_file()]
        dep = ([f.stat().st_mtime for f in skills.rglob("*") if f.is_file()]
               if skills.exists() else [])
        if src and (not dep or min(dep) < max(src)):
            warnings.append((".claude/skills/", "doc/llm/skills/",
                             "compass build --direct skills"))
    for target, source, cmd in warnings:
        target_path = PROJECT_ROOT / target.rstrip("/")
        if not target_path.exists():
            print(f"  {_C_YELLOW}⚠  {target} does not exist — "
                  f"run to provision:{_C_RESET}")
        else:
            print(f"  {_C_YELLOW}⚠  {target} is older than {source} — "
                  f"a refresh may be required:{_C_RESET}")
        print(f"     {_ycmd(cmd)}")
    if warnings:
        print()


def cmd_bearings(argv):
    """compass bearings — cold-start orientation for LLMs and new contributors."""
    import types as _types
    ap = argparse.ArgumentParser(
        prog="compass bearings",
        description="Cold-start orientation: project identity, last session, "
                    "key recipes, memories, and where we are.")
    ap.add_argument("-n", "--limit", type=int, default=5, metavar="N",
                    help="Max items shown per section (recipes, memories). "
                         "0 = unlimited (default: 5).")
    ap.add_argument("--no-heading", action="store_true",
                    help="Omit the 'Suggested next action' heading section.")
    args = ap.parse_args(argv)
    if args.limit < 0:
        ap.error("--limit must be >= 0 (use 0 for unlimited)")
    limit = args.limit
    docs = doc_index.load_all()

    env_name = _read_env_map().get("ORES_ENV_NAME") or PROJECT_ROOT.name
    identity = env_name.replace("-", " ").replace("_", " ")
    print("🧭 ores.compass — bearings\n")
    print(f"  Your name is: {identity}\n")

    _claude_refresh_warnings()

    # ── LLM entry point ─────────────────────────────────────────────────────
    _bearings_section("🤖", "If you are an LLM, read this first",
                      "compass show F9AD7932-8E11-433C-A812-B57DBC9BF5D8")
    print("  LLM instructions: rules, architecture links, build, code style,")
    print("  documentation conventions, git/PR conventions, and project memory.")

    # ── What is ORE Studio? ──────────────────────────────────────────────────
    _bearings_section("🏢", "What is ORE Studio?",
                      "compass show 2F71292F-CDB0-4E2E-B50F-4F02E10597C4")
    identity_docs = [d for d in docs.values() if d.doctype == "product_identity"]
    if identity_docs:
        print(f"  {identity_docs[0].description}")
    else:
        print("  ❌ No product_identity document found.")

    # ── Where have we been? ──────────────────────────────────────────────────
    # ── Environment status ──────────────────────────────────────────────────
    _bearings_section("🔌", "Is the environment up?",
                      "compass services status")
    _genesis_dir = PROJECT_ROOT.parent / "ores_dev_prime_origin"
    _is_at_genesis_path = PROJECT_ROOT.resolve() == _genesis_dir.resolve()
    try:
        import compass_db as _cdb
        import compass_services as _csv
        _env = _cdb.load_env(PROJECT_ROOT)
        _preset = _env.get("ORES_PRESET", "(not set)")
        _label = _env.get("ORES_CHECKOUT_LABEL", "?")
        _envver = _env.get("ORES_ENV_VERSION", "?")
        print(f"  Preset   : {_preset}  (label: {_label}, env v{_envver})")
        _info = _cdb.database_info(_env)
        if _info:
            _delta, _drift_label, _col, _warning = _cdb.schema_drift(PROJECT_ROOT, _info)
            _chip = (f" (schema {_info['schema_version']}, built "
                     f"{_age_human(_delta)} behind HEAD)"
                     if _delta is not None
                     else f" (schema {_info['schema_version']})")
            print(f"  Database : {_col}restored {_info['restored_at']}"
                  f"{_chip}{_C_RESET}")
            if _warning:
                print(f"  {_warning}")
        else:
            print(f"  Database : {_C_RED}unreachable{_C_RESET}  "
                  f"({_ycmd('compass db recreate -y -k')})")
        try:
            _ctx = _csv.Ctx(PROJECT_ROOT, _env, None)
            _st = _csv.gather_counts(_ctx)
            _c = _st["counts"]
            _all_up = (_c["running"] and not _c["stopped"]
                       and not _c["missing"])
            _tone = _C_GREEN if _all_up else _C_YELLOW
            _hint = ("" if _c["running"] or _c["starting"]
                     else f"  ({_ycmd('compass services start')})")
            print(f"  Services : {_tone}running={_c['running']} "
                  f"starting={_c['starting']} stopped={_c['stopped']} "
                  f"missing={_c['missing']}{_C_RESET}  "
                  f"(nats: {_st['nats']}){_hint}")
            _clients = _csv.client_status(_ctx)
            if _clients:
                _desc = ", ".join(f"{n} PID {pid}" for n, pid in _clients)
                print(f"  Client   : {_C_GREEN}running{_C_RESET}  ({_desc})")
            else:
                print(f"  Client   : not running  "
                      f"({_ycmd('compass client')})")
        except SystemExit:
            print("  Services : (no preset in .env — compass env configure)")
        # ── Genesis env check (when .env exists) ────────────────────────────
        if not _is_at_genesis_path and not _genesis_dir.is_dir():
            print(f"  {_C_YELLOW}⚠  Genesis env (ores_dev_prime_origin) not found at "
                  f"{_genesis_dir}{_C_RESET}")
            print(f"     On a fresh machine: clone the repo into that path first.")
    except SystemExit:
        # No .env — detect whether this is a fresh clone / genesis env candidate.
        if _is_at_genesis_path:
            print(f"  🌱 This is the prime-origin genesis environment — no .env yet.")
            print(f"  Next: {_ycmd('compass env configure --preset <preset>')}")
        elif not _genesis_dir.is_dir():
            print(f"  {_C_YELLOW}⚠  Fresh clone detected — no .env and no genesis env found.{_C_RESET}")
            print(f"  This checkout should become the prime-origin genesis environment.")
            print(f"  Suggested setup:")
            print(f"    1. Move to the conventional path:")
            print(f"         mv '{PROJECT_ROOT}' '{_genesis_dir}'")
            print(f"    2. cd into it and configure:")
            print(f"         cd '{_genesis_dir}' && {_ycmd('compass env configure --preset <preset>')}")
        else:
            print("  (.env missing — run compass env configure to provision)")

    # ── Common commands ─────────────────────────────────────────────────────
    _bearings_section("🛠", "Commands you will reach for")
    for _what, _cmd in [
        ("Run SQL in the environment", 'compass sql -- -c "<sql>"'),
        ("Start / stop / inspect services",
         "compass services start|stop|status"),
        ("Rebuild the database from scratch", "compass db recreate -y -k"),
        ("Search every doc in the repo", 'compass search "<term>"'),
        ("Clock on to a task (branch + journal)",
         "compass task start <slug>"),
        ("Check the sprint for state drift", "compass sprint audit"),
    ]:
        print(f"  {_what}:")
        print(f"      {_ycmd(_cmd)}")
    print(f"  (every command has --help)")

    _bearings_section("📓", "Where have we been?", "compass journal where")
    _journal_where()

    # ── What can we do next? ─────────────────────────────────────────────────
    _bearings_section("📖", "What can we do next?",
                      "compass list --type recipe --tag bearings")
    recipes = sorted(
        [d for d in docs.values()
         if d.doctype == "recipe" and d.has_tag("bearings")],
        key=lambda d: d.title or "",
    )
    if not recipes:
        print("  ❌ No bearings-tagged recipes found — configuration error.")
        print("     Tag any recipe with :bearings: to include it here.")
    else:
        shown_recipes = recipes[:limit] if limit > 0 else recipes
        for r in shown_recipes:
            print(f"\n  • {r.title}")
            if r.description:
                print(f"    {r.description}")
            print(f"    {_ycmd(f'compass show {r.id.upper()}')}")
        if limit > 0 and len(recipes) > limit:
            print(f"\n  … {len(recipes) - limit} more (compass list --type recipe --tag bearings)")

    # ── Important things to remember ─────────────────────────────────────────
    _bearings_section("🧠", "Important things to remember",
                      "compass list --type memory --tag bearings")
    memories = sorted(
        [d for d in docs.values()
         if d.doctype == "memory" and d.has_tag("bearings")],
        key=lambda d: d.title or "",
    )
    if not memories:
        print("  ❌ No bearings-tagged memories found — configuration error.")
        print("     Tag any memory with :bearings: to include it here.")
    else:
        shown_memories = memories[:limit] if limit > 0 else memories
        for m in shown_memories:
            print(f"\n  • {m.title}")
            if m.description:
                print(f"    {m.description}")
            print(f"    {_ycmd(f'compass show {m.id.upper()}')}")
        if limit > 0 and len(memories) > limit:
            print(f"\n  … {len(memories) - limit} more (compass list --type memory --tag bearings)")

    # ── Where is everyone? ───────────────────────────────────────────────────
    _bearings_section("👥", "Where is everyone?", "compass fleet")
    cmd_fleet(_types.SimpleNamespace(format="pretty"))

    # ── What is going on? ────────────────────────────────────────────────────
    _bearings_section("📍", "What is going on?", "compass where")
    current_version, current_sprint = current_version_sprint(docs)
    if current_version is None or current_sprint is None:
        print("  ❌ No version/sprint documents found.")
    else:
        chip, warning = staleness_lines(branch_staleness(PROJECT_ROOT))
        print(f"  {chip}")
        if warning:
            print(f"  {warning}")
        print()
        print(f"  Version:  {current_version.title}")
        print(f"            compass show {current_version.id.upper()}")
        print(f"  Sprint:   {current_sprint.title}")
        print(f"            compass show {current_sprint.id.upper()}")
        sprint_dir = _parent_dir(current_sprint.rel_path)
        in_flight = [
            d for d in docs.values()
            if d.doctype in ("story", "task")
            and d.rel_path.startswith(sprint_dir + "/")
            and ui.read_state(d.path) == IN_FLIGHT_STATE
        ]
        in_flight.sort(key=lambda d: (d.doctype, d.rel_path))
        print(f"\n  In flight ({IN_FLIGHT_STATE}):")
        if not in_flight:
            print("    (nothing in flight)")
        else:
            for d in in_flight:
                print(f"    {d.doctype:<5}  {_strip_type_prefix(d.title or '')}")
                print(f"           {_ycmd(f'compass show {d.id.upper()}')}")

    # ── Suggested next action ────────────────────────────────────────────────
    if not args.no_heading:
        _bearings_section("🎯", "Suggested next action", "compass heading")
        try:
            cmd_heading(["--count", "1", "--no-banner"])
        except SystemExit as e:
            if e.code:
                print("  (compass heading unavailable)")

    print()
    return 0


# --- Heading: next-work-item suggester ---

def _heading_keyword_boost(text, keywords):
    """Return a multiplier in [1.0, 2.0] based on keyword matches in text."""
    if not keywords:
        return 1.0
    text_lower = text.lower()
    hits = sum(1 for kw in keywords if kw.lower() in text_lower)
    return min(2.0, 1.0 + 0.5 * hits)


def cmd_heading(argv):
    """compass heading — suggest the next logical work item to pick up."""
    ap = argparse.ArgumentParser(
        prog="compass heading",
        description="Recommend the next work item to pick up, ranked by priority. "
                    "Synthesises sprint state, journal activity, fleet, and backlog.")
    ap.add_argument("keywords", nargs="*",
                    help="Bias ranking toward items matching these keywords "
                         "(matched against title, description, and directory name).")
    ap.add_argument("-f", "--format", choices=["pretty", "json"], default="pretty",
                    help="Output format (default: pretty).")
    ap.add_argument("-n", "--count", type=int, default=10,
                    help="Maximum number of suggestions (default: 10).")
    ap.add_argument("--no-banner", action="store_true",
                    help=argparse.SUPPRESS)
    args = ap.parse_args(argv)

    if args.count < 1:
        print("❌  --count must be a positive integer.")
        return 1

    docs = doc_index.load_all()
    _, current_sprint = current_version_sprint(docs)
    if current_sprint is None:
        print("❌  No current sprint found.")
        return 1

    sprint_dir = PROJECT_ROOT / _parent_dir(current_sprint.rel_path)

    # Fleet: collect branches currently active across all worktrees so we can
    # deprioritise items that are already being worked.
    active_branches = set()
    for wt_path, wt_branch in list_worktrees():
        if wt_branch:
            active_branches.add(wt_branch)

    # ── Environment map ───────────────────────────────────────────────────────
    # Identify the current environment and build a visibility map of what other
    # environments are working on, for scoring and the "Other environments" display.
    current_env = _read_env_map().get("ORES_CHECKOUT_LABEL", "")

    # env_work: {env_label: [(story_title, task_title_or_None, story_tokens)]}
    # story_tokens is a set of lowercase words for overlap detection.
    env_work: dict = {}
    if current_env:
        for _sf in sorted(sprint_dir.glob("*/story.org")):
            try:
                _st = _sf.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            _em = _ENV_FIELD_RE.search(_st)
            if not _em:
                continue
            _env = _em.group(1).strip()
            if not _env or _env == current_env:
                continue
            _st_title, _st_state, _st_uuid = _read_story_state(_sf)
            if _st_state == "DONE":
                continue
            # Find the STARTED task in this story (if any); single parse per file.
            _task_title = None
            for _tf in sorted(_sf.parent.glob("task_*.org")):
                _t_title, _ts, *_ = _read_task_detail(_tf)
                if _ts == "STARTED":
                    _task_title = _t_title
                    break
            _story_tokens = set(re.findall(r"\w+", _st_title.lower()))
            env_work.setdefault(_env, []).append((_st_title, _task_title, _story_tokens, _st_uuid))

    # Flat set of all tokens in other environments' active stories (for overlap scoring).
    # Stopwords stripped once here so each per-story intersection is clean.
    _STOPWORDS = {"the", "a", "an", "and", "or", "of", "in", "to", "sprint", "story", "compass"}
    _other_env_tokens: set = set()
    for _entries in env_work.values():
        for _, _, _toks, _ in _entries:
            _other_env_tokens.update(_toks)
    _other_env_tokens -= _STOPWORDS

    suggestions = []  # list of dicts: score, kind, title, rationale, action

    # ── Pass 1: sprint story/task signals ────────────────────────────────────
    for story_file in sorted(sprint_dir.glob("*/story.org")):
        story_title, story_state, story_uuid = _read_story_state(story_file)
        story_dir = story_file.parent

        if story_state == "DONE":
            continue

        # Environment ownership / overlap scoring multiplier.
        # Direct ownership (story stamped with another env) → heavy penalty.
        # Topical overlap (title tokens shared with another env's stories) → soft penalty.
        try:
            _story_text = story_file.read_text(encoding="utf-8")
            _story_env_m = _ENV_FIELD_RE.search(_story_text)
            _story_env = _story_env_m.group(1).strip() if _story_env_m else ""
        except (OSError, UnicodeDecodeError):
            _story_env = ""
        if not current_env:
            _env_multiplier = 1.0
            _env_note = ""
        elif _story_env and _story_env != current_env:
            _env_multiplier = 0.15   # hard block — another env owns this
            _env_note = f" [owned by {_story_env} — low priority]"
        elif _other_env_tokens:
            _story_title_tokens = set(re.findall(r"\w+", story_title.lower()))
            _overlap = (_story_title_tokens & _other_env_tokens) - _STOPWORDS
            _env_multiplier = max(0.5, 1.0 - 0.1 * len(_overlap))
            _env_note = (f" [possible overlap with other env: {', '.join(sorted(_overlap))}]"
                         if _overlap else "")
        else:
            _env_multiplier = 1.0
            _env_note = ""

        tasks = sorted(story_dir.glob("task_*.org"))
        if not tasks:
            continue

        task_states = []
        for tf in tasks:
            t_title, t_state, t_uuid, t_branch, t_pr = _read_task_detail(tf)
            task_states.append((tf, t_title, t_state, t_uuid, t_branch, t_pr))

        # Respect the story's * Tasks table order (UUIDs in table row order).
        # Falls back to filename order when UUIDs are missing or table absent.
        story_uuid_order = _task_uuids_from_story(story_file)
        if story_uuid_order:
            _pos = {uid.upper(): i for i, uid in enumerate(story_uuid_order)}
            task_states.sort(
                key=lambda r: _pos.get((r[3] or "").upper(), len(_pos))
            )

        done_count  = sum(1 for r in task_states if r[2] == "DONE")
        total_count = len(task_states)

        # Ready-to-close: all tasks DONE but story still STARTED/BACKLOG.
        if done_count == total_count and story_state in ("STARTED", "BACKLOG"):
            search_text = f"{story_title} {story_dir.name}"
            boost = _heading_keyword_boost(search_text, args.keywords)
            suggestions.append({
                "score": int(90 * boost * _env_multiplier),
                "kind": "close",
                "title": story_title,
                "rationale": f"All {total_count} tasks DONE — story needs closing{_env_note}",
                "action": f"# Set State → DONE in {story_dir.name}/story.org",
                "uuid": story_uuid,
            })
            continue

        for tf, t_title, t_state, t_uuid, t_branch, t_pr in task_states:
            search_text = f"{story_title} {t_title} {story_dir.name}"

            if t_state == "BLOCKED":
                # BLOCKED tasks always score 100 regardless of keywords — they
                # require immediate attention irrespective of topic focus.
                # Environment penalty still applies (another env's blocked task
                # is still their problem to unblock, not ours).
                suggestions.append({
                    "score": int(100 * _env_multiplier),
                    "kind": "blocked",
                    "title": t_title,
                    "rationale": f"BLOCKED in story \"{story_title}\" — needs unblocking{_env_note}",
                    "action": f"compass show {t_uuid}" if t_uuid else f"# {tf.name}",
                    "uuid": t_uuid,
                })

            elif t_state == "STARTED":
                # Stale STARTED: PR has merged but task not yet closed.
                if t_pr and pr_state(t_pr) == "MERGED":
                    boost = _heading_keyword_boost(search_text, args.keywords)
                    _stem = tf.stem
                    _slug = _stem[len("task_"):] if _stem.startswith("task_") else _stem
                    suggestions.append({
                        "score": min(99, int(95 * boost * _env_multiplier)),
                        "kind": "close",
                        "title": t_title,
                        "rationale": f"PR #{t_pr} merged — task needs closing{_env_note}",
                        "action": f"compass task done {_slug}",
                        "uuid": t_uuid,
                    })
                else:
                    in_fleet = t_branch and t_branch in active_branches
                    boost = _heading_keyword_boost(search_text, args.keywords)
                    base = 50 if in_fleet else 65
                    suggestions.append({
                        "score": int(base * boost * _env_multiplier),
                        "kind": "in-flight",
                        "title": t_title,
                        "rationale": (
                            f"In flight in story \"{story_title}\""
                            + (" (active in fleet)" if in_fleet else " — may need attention")
                            + _env_note
                        ),
                        "action": f"compass show {t_uuid}" if t_uuid else f"# {tf.name}",
                        "uuid": t_uuid,
                    })

            elif t_state == "BACKLOG" and story_state == "STARTED":
                # Next unstarted task in an already-started story.
                boost = _heading_keyword_boost(search_text, args.keywords)
                slug = tf.stem[len("task_"):] if tf.stem.startswith("task_") else tf.stem
                suggestions.append({
                    "score": int(80 * boost * _env_multiplier),
                    "kind": "next-task",
                    "title": t_title,
                    "rationale": f"Next task in active story \"{story_title}\"{_env_note}",
                    "action": f"compass task start {slug}",
                    "uuid": t_uuid,
                })
                break  # only the first BACKLOG task per story matters

    # ── Pass 2: product-backlog signals ─────────────────────────────────────
    for bucket, base_score in [("next", 40), ("inbox", 20)]:
        bucket_dir = PROJECT_ROOT / "doc" / "agile" / "product_backlog" / bucket
        if not bucket_dir.exists():
            continue
        for cap_file in sorted(bucket_dir.rglob("*.org")):
            try:
                text = cap_file.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            tm = _TITLE_RE2.search(text)
            im = _ORG_ID_RE.search(text)
            dm = re.search(r"^#\+description:\s*(.+)$", text, re.MULTILINE | re.IGNORECASE)
            cap_title = _strip_type_prefix(tm.group(1)) if tm else cap_file.stem
            cap_uuid  = im.group(1) if im else None
            cap_desc  = dm.group(1).strip() if dm else ""
            search_text = f"{cap_title} {cap_desc}"
            boost = _heading_keyword_boost(search_text, args.keywords)
            suggestions.append({
                "score": int(base_score * boost),
                "kind": bucket,
                "title": cap_title,
                "rationale": f"Capture in {bucket} backlog",
                "action": f"compass show {cap_uuid}" if cap_uuid else f"# {cap_file.name}",
                "uuid": cap_uuid,
            })

    # ── Sort, deduplicate by uuid, limit ────────────────────────────────────
    seen_uuids = set()
    ranked = []
    for s in sorted(suggestions, key=lambda x: -x["score"]):
        uid = s.get("uuid")
        if uid and uid in seen_uuids:
            continue
        if uid:
            seen_uuids.add(uid)
        ranked.append(s)
        if len(ranked) >= args.count:
            break

    if args.format == "json":
        print(json.dumps(ranked, indent=2))
        return 0

    if not args.no_banner:
        print(ui.header("🧭 ores.compass — heading"))
        print()

    # ── Section 1: Active in other environments ───────────────────────────────
    if env_work:
        print("🌐  Active in other environments")
        print(f"    {ui.YELLOW}Avoid picking up stories owned by another environment, or stories whose work would clash with theirs.{ui.RESET}")
        print()
        for _env, _entries in sorted(env_work.items()):
            for _st_title, _task_title, _, _st_uuid in _entries:
                _task_note = f"  ▸  {_task_title}" if _task_title else ""
                print(f"🌐  story: {ui.header(_st_title)}  {ui.CYAN}[{_env}]{ui.RESET}")
                if _task_note:
                    print(f"    {_task_note.strip()}")
                print(f"    {ui.ycmd('compass show ' + _st_uuid) if _st_uuid else ''}")
                print()
        print()

    # ── Section 2: Sprint suggestions ────────────────────────────────────────
    SPRINT_KINDS = {"blocked", "close", "next-task", "in-flight"}
    BACKLOG_KINDS = {"next", "inbox"}
    KIND_LABEL = {
        "blocked":   "blocked task",
        "close":     "ready to close",
        "next-task": "next task",
        "in-flight": "in flight",
        "next":      "capture",
        "inbox":     "capture",
    }
    KIND_ICON = {
        "blocked":   "🔴",
        "close":     "✅",
        "next-task": "▶️",
        "in-flight": "🔵",
        "next":      "📥",
        "inbox":     "📥",
    }

    sprint_items  = [s for s in ranked if s["kind"] in SPRINT_KINDS]
    backlog_items = [s for s in ranked if s["kind"] in BACKLOG_KINDS]

    if sprint_items:
        print("🎯  Sprint suggestions")
        print()
        for s in sprint_items:
            icon = KIND_ICON.get(s["kind"], "•")
            kind_lbl = KIND_LABEL.get(s["kind"], s["kind"])
            score_lbl = f"{s['score']}%"
            print(f"{icon}  {kind_lbl}: {ui.header(s['title'])}  {ui.CYAN}[{score_lbl}]{ui.RESET}")
            print(f"    {s['rationale']}")
            print(f"    {ui.ycmd(s['action'])}")
            print()
    elif not backlog_items:
        print("  Nothing to suggest — sprint is clean and backlog is empty.")
        return 0

    # ── Section 3: Next backlog ────────────────────────────────────────────
    if backlog_items:
        print("📋  Next backlog")
        print()
        for s in backlog_items:
            icon = KIND_ICON.get(s["kind"], "•")
            kind_lbl = KIND_LABEL.get(s["kind"], s["kind"])
            score_lbl = f"{s['score']}%"
            print(f"{icon}  {kind_lbl}: {ui.header(s['title'])}  {ui.CYAN}[{score_lbl}]{ui.RESET}")
            print(f"    {s['rationale']}")
            print(f"    {ui.ycmd(s['action'])}")
            print()

    return 0


# --- Build pillar ---

# Friendly target aliases: compass-level names → cmake target names.
# Add new entries here as more build products get compass-level names.
BUILD_TARGET_ALIASES = {
    "site": "deploy_site",
    "manual": "deploy_manual",
    "org-roam-db-sync": "org_roam_db_sync",
    # .claude/ is generated, never checked in: settings.json tangles from
    # doc/llm/claude_code_settings.org; skills deploy from doc/llm/skills.
    # Recreate the whole directory with: compass build --direct settings skills
    "settings": "deploy_settings",
    "skills": "deploy_skills",
    "help": "deploy_help",
}

# Emacs scripts for --direct builds: cmake target name → .el file name.
# These targets can run without cmake or vcpkg — useful in light environments.
EMACS_BUILD_SCRIPTS = {
    "deploy_site":             "ores-build-site.el",
    "deploy_manual":           "ores-build-manual.el",
    "deploy_help":             "ores-build-help.el",
    "deploy_skills":           "ores-build-skills.el",
    "deploy_settings":         "ores-build-settings.el",
    "org_roam_db_sync":        "ores-sync-org-roam.el",
    "tangle_shell_scripts":    "ores-build-recipe-scripts.el",
    "tangle_codegen_templates": "ores-build-codegen-templates.el",
    "tangle_clang_format":     "ores-build-clang-format.el",
}

_EMACS_LISP_DIR = Path("projects") / "ores.lisp" / "src"


def _run_emacs_target(target: str, dry_run: bool = False) -> int:
    """Run a single emacs build script directly, bypassing cmake."""
    script = EMACS_BUILD_SCRIPTS.get(target)
    if not script:
        print(f"❌ No direct emacs script for target '{target}'. "
              f"Available: {', '.join(sorted(EMACS_BUILD_SCRIPTS))}",
              file=sys.stderr)
        return 1
    script_path = PROJECT_ROOT / _EMACS_LISP_DIR / script
    if not script_path.is_file():
        print(f"❌ Emacs script not found: {script_path}", file=sys.stderr)
        return 1
    cmd = ["emacs", "-Q", "--script", str(script_path)]
    print(f"🔨 emacs -Q --script {_EMACS_LISP_DIR / script}")
    if dry_run:
        return 0
    return subprocess.run(cmd, cwd=PROJECT_ROOT).returncode


def cmd_site(argv):
    """compass site — Site pillar: build and serve the org-mode site locally."""
    ap = argparse.ArgumentParser(
        prog="compass site",
        description="Site pillar: build and/or serve the org-mode site locally.")
    sub = ap.add_subparsers(dest="subcmd", metavar="SUBCMD")

    sp = sub.add_parser("serve", help="Serve the site locally (optionally rebuilding first)")
    sp.add_argument("--compile", action="store_true",
                    help="Rebuild the site before serving")
    sp.add_argument("--port", type=int, default=0,
                    help="Port to serve on (default: ORES_SITE_PORT from .env, else 51004)")

    args = ap.parse_args(argv)
    if args.subcmd is None:
        ap.print_help()
        return 0
    if args.subcmd != "serve":
        ap.print_help()
        return 1

    env = _read_env_map()
    port = args.port or int(env.get("ORES_SITE_PORT", 51004))
    build_dir = PROJECT_ROOT / "build" / "output" / "site"

    # Stop any process already listening on the port.
    try:
        result = subprocess.run(["fuser", f"{port}/tcp"],
                                capture_output=True, text=True)
        pids = result.stdout.split()
        for pid in pids:
            try:
                print(f"Stopping process on port {port} (PID {pid})")
                subprocess.run(["kill", pid], check=False)
            except Exception:
                pass
        if pids:
            time.sleep(0.5)
    except FileNotFoundError:
        pass  # fuser not available; skip

    if args.compile:
        rc = _run_emacs_target("deploy_site")
        if rc != 0:
            print(f"❌ Site build failed (exit {rc})", file=sys.stderr)
            return rc

    if not build_dir.is_dir():
        print(f"❌ Build directory not found: {build_dir}\n"
              "   Run with --compile, or: compass build --direct site",
              file=sys.stderr)
        return 1

    print(f"🌐 Serving {build_dir} on http://localhost:{port}/OreStudio/")
    handler = functools.partial(http.server.SimpleHTTPRequestHandler,
                                directory=str(build_dir))
    with http.server.HTTPServer(("", port), handler) as httpd:
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            pass
    return 0


def cmd_build(argv):
    """compass build — Build pillar: cmake builds via the prevailing preset.

    The preset comes from ORES_PRESET in .env (override with --preset), so
    every checkout builds with its own configuration without recipes or
    sessions hardcoding preset names. If the preset has never been
    configured, the configure step (cmake --preset <preset>) runs first.

    Pass --direct to call emacs scripts directly, bypassing cmake and vcpkg
    entirely. Only targets listed in EMACS_BUILD_SCRIPTS are supported in
    direct mode; this is the required path for light environments.
    """
    ap = argparse.ArgumentParser(
        prog="compass build",
        description="Build pillar: run cmake builds with the preset from "
                    ".env (ORES_PRESET), or call emacs scripts directly with "
                    "--direct (no cmake or vcpkg required).")
    aliases = ", ".join(f"'{k}' → {v}" for k, v in
                        sorted(BUILD_TARGET_ALIASES.items()))
    ap.add_argument("targets", nargs="*", metavar="TARGET",
                    help="Build target(s): a friendly alias or any raw "
                         f"cmake target. Aliases: {aliases}. "
                         "Default: build everything.")
    ap.add_argument("--preset", default="",
                    help="CMake preset name (default: read ORES_PRESET "
                         "from .env); ignored with --direct")
    ap.add_argument("-j", "--jobs", type=int, default=0,
                    help="Parallel build jobs (default: cmake's default); "
                         "ignored with --direct")
    ap.add_argument("--dry-run", action="store_true",
                    help="Print the command(s) without running them")
    ap.add_argument("--direct", action="store_true",
                    help="Call emacs scripts directly, bypassing cmake and "
                         "vcpkg. Required for light environments. Supported "
                         f"targets: {', '.join(sorted(EMACS_BUILD_SCRIPTS))}")
    args = ap.parse_args(argv)

    targets = [BUILD_TARGET_ALIASES.get(t, t) for t in args.targets]

    if args.direct:
        if not targets:
            print("❌ --direct requires at least one target.", file=sys.stderr)
            return 1
        for target in targets:
            rc = _run_emacs_target(target, dry_run=args.dry_run)
            if rc != 0:
                print(f"❌ Direct build failed for target '{target}' (exit {rc})",
                      file=sys.stderr)
                return rc
        return 0

    preset = args.preset or _tr_read_preset()
    if not preset:
        print("❌ No preset supplied and ORES_PRESET not set in .env.\n"
              "   Pass --preset <name> or run compass env configure.\n"
              "   For light environments without cmake, use --direct.",
              file=sys.stderr)
        return 1

    commands = []
    build_dir = PROJECT_ROOT / "build" / "output" / preset
    if not build_dir.is_dir():
        print(f"ℹ️  {build_dir.relative_to(PROJECT_ROOT)} not found — "
              f"configuring first: cmake --preset {preset}")
        commands.append(["cmake", "--preset", preset])

    build_cmd = ["cmake", "--build", "--preset", preset]
    if targets:
        build_cmd += ["--target", *targets]
    if args.jobs:
        build_cmd += ["-j", str(args.jobs)]
    commands.append(build_cmd)

    for cmd in commands:
        print(f"🔨 {' '.join(cmd)}")
        if args.dry_run:
            continue
        rc = subprocess.run(cmd, cwd=PROJECT_ROOT).returncode
        if rc != 0:
            print(f"❌ Command failed with exit code {rc}: {' '.join(cmd)}",
                  file=sys.stderr)
            return rc
    return 0


def _read_env_map() -> dict:
    """Read .env into a key/value map; empty if the file is missing."""
    env_file = PROJECT_ROOT / ".env"
    result = {}
    if not env_file.is_file():
        return result
    for line in env_file.read_text(encoding="utf-8").splitlines():
        if not line or line.lstrip().startswith("#") or "=" not in line:
            continue
        key, _, val = line.partition("=")
        val = val.strip()
        if len(val) >= 2 and val[0] == val[-1] and val[0] in ("'", '"'):
            val = val[1:-1]
        result[key.strip()] = val
    return result


def cmd_shell(argv):
    """compass shell — Shell pillar: run ores.shell with .env defaults.

    Resolves the binary from the checkout's preset and passes the NATS
    connection (url, subject prefix, TLS material) and login credentials
    from .env as explicit flags, so a bare `compass shell` lands in a
    connected, logged-in REPL. -f FILE feeds a scripted session.
    """
    ap = argparse.ArgumentParser(
        prog="compass shell",
        description="Shell pillar: launch ores.shell with NATS and login "
                    "defaults resolved from .env.",
        epilog="Arguments after -- are forwarded to ores.shell verbatim.")
    ap.add_argument("-l", "--load", "-f", "--file", dest="file", default="",
                    metavar="FILE",
                    help="Scripted session: run FILE via ores.shell's --load "
                         "(skips # comments, expands $VAR/${VAR}, then exits). "
                         "-f/--file are deprecated aliases for -l/--load.")
    ap.add_argument("-u", "--username", default="",
                    help="Login username (default: ORES_SHELL_LOGIN_USERNAME "
                         "from .env)")
    ap.add_argument("-p", "--password", default="",
                    help="Login password (default: ORES_SHELL_LOGIN_PASSWORD "
                         "from .env or the process environment)")
    ap.add_argument("--preset", default="",
                    help="CMake preset locating the binary (default: "
                         "ORES_PRESET from .env)")
    ap.add_argument("--log-enabled", action="store_true",
                    help="Forwarded: generate an ores.shell log file")
    ap.add_argument("--log-level", default="", metavar="LEVEL",
                    help="Forwarded: trace, debug, info, warn or error")
    ap.add_argument("--dry-run", action="store_true",
                    help="Print the command (password masked) without "
                         "running it")
    args, extra = ap.parse_known_args(argv)
    if extra and extra[0] == "--":
        extra = extra[1:]

    env = _read_env_map()

    preset = args.preset or env.get("ORES_PRESET", "")
    if not preset:
        print("❌ No preset supplied and ORES_PRESET not set in .env.\n"
              "   Pass --preset <name> or run compass env configure.",
              file=sys.stderr)
        return 1

    binary_name = "ores.shell.exe" if sys.platform == "win32" else "ores.shell"
    binary = (PROJECT_ROOT / "build" / "output" / preset / "publish" /
              "bin" / binary_name)
    if not binary.is_file():
        print(f"❌ ores.shell not found: {binary}\n"
              f"   Build it first: compass build ores.shell",
              file=sys.stderr)
        return 1

    cmd = [str(binary)]

    flag_for = {
        "ORES_SHELL_NATS_URL": "--nats-url",
        "ORES_SHELL_NATS_SUBJECT_PREFIX": "--nats-subject-prefix",
        "ORES_SHELL_NATS_TLS_CA": "--nats-tls-ca",
        "ORES_SHELL_NATS_TLS_CERT": "--nats-tls-cert",
        "ORES_SHELL_NATS_TLS_KEY": "--nats-tls-key",
    }
    for key, flag in flag_for.items():
        if env.get(key):
            cmd += [flag, env[key]]

    username = args.username or env.get("ORES_SHELL_LOGIN_USERNAME", "")
    password = (args.password or
                os.environ.get("ORES_SHELL_LOGIN_PASSWORD", "") or
                env.get("ORES_SHELL_LOGIN_PASSWORD", ""))
    if username:
        cmd += ["--login-username", username]
    if password:
        cmd += ["--login-password", password]

    if args.log_enabled:
        cmd.append("--log-enabled")
    if args.log_level:
        cmd += ["--log-level", args.log_level]

    # Scripted session: run the file via ores.shell's first-class `--load` flag,
    # which executes it through the script runner (skips `#` comments, expands
    # $VAR/${VAR}) and exits. This replaces piping the file to stdin, which used
    # the bare REPL and did neither.
    if args.file:
        script_file = Path(args.file)
        if not script_file.is_file():
            print(f"❌ Script file not found: {script_file}", file=sys.stderr)
            return 1
        cmd += ["--load", str(script_file.resolve())]

    cmd += extra

    masked = list(cmd)
    for i in range(len(masked) - 1):
        if masked[i] == "--login-password":
            masked[i + 1] = "********"
    print(f"🐚 {' '.join(masked)}", flush=True)

    if args.dry_run:
        return 0

    # Export the resolved .env into the child environment so scripts can expand
    # $VAR/${VAR} references (e.g. the generated provisioning scripts' `connect
    # $ORES_NATS_URL`). Values from .env take precedence over the inherited
    # process environment.
    #
    # Safe because env_init no longer emits ORES_SHELL_DB_* (the shell is a
    # NATS-only client): ores.shell's make_mapper("SHELL") parse_environment now
    # only sees registered nats-* options. CLI flags (passed above) win over env.
    child_env = {**os.environ, **env}

    return subprocess.run(cmd, cwd=PROJECT_ROOT, env=child_env).returncode


# --- Codegen pillar ---

def _codegen_src():
    """Add ores.codegen/src to sys.path so the codegen package is importable."""
    src = str(PROJECT_ROOT / "projects" / "ores.codegen" / "src")
    if src not in sys.path:
        sys.path.insert(0, src)


def cmd_codegen(argv):
    """compass codegen — generate or regenerate code from model files.

    Calls codegen.generate.cmd_generate / cmd_regenerate directly; no
    shell intermediary.  The base_dir is resolved to projects/ores.codegen.
    """
    # Delegate entity sub-commands before argparse so positional args are not
    # consumed by this parser.
    if argv and argv[0] == "entity":
        _codegen_src()
        try:
            import compass_codegen_entity  # noqa: PLC0415
        except ImportError as exc:
            print(f"❌ Cannot load compass_codegen_entity ({exc}).", file=sys.stderr)
            return 1
        base_dir = PROJECT_ROOT / "projects" / "ores.codegen"
        return compass_codegen_entity.run(argv[1:], base_dir, PROJECT_ROOT)

    import argparse as _ap

    ap = _ap.ArgumentParser(
        prog="compass codegen",
        description="ORE Studio code generation (SQL, C++, Qt).",
    )
    ap.add_argument("-v", "--verbose", action="store_true", help="Enable debug logging")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    gen_p = sub.add_parser("generate", aliases=["gen"],
                            help="Generate from a single model file.")
    gen_p.add_argument("--model", required=True, metavar="PATH",
                       help="Path to the model file")
    gen_p.add_argument("--profile", default="sql", metavar="PROFILE",
                       help="Generation profile (sql, all-cpp, domain, …); default: sql")
    gen_p.add_argument("--dry-run", action="store_true",
                       help="Print output paths without writing")

    regen_p = sub.add_parser("regenerate", aliases=["regen"],
                              help="Regenerate all models for a component.")
    regen_scope = regen_p.add_mutually_exclusive_group(required=True)
    regen_scope.add_argument("--component", metavar="NAME",
                             help="Component to regenerate (e.g. refdata-cpp)")
    regen_scope.add_argument("--all", action="store_true",
                             help="Regenerate all components")
    regen_p.add_argument("--profile", default="sql", metavar="PROFILE",
                         help="Generation profile; default: sql")
    regen_p.add_argument("--dry-run", action="store_true",
                         help="Print output paths without writing")

    args = ap.parse_args(argv)

    _codegen_src()
    try:
        from codegen.generate import cmd_generate, cmd_regenerate  # noqa: PLC0415
        from codegen.logging_config import configure  # noqa: PLC0415
    except ImportError as exc:
        print(f"❌ Cannot import ores.codegen ({exc}). "
              f"Is the venv set up? Run: pip install -e projects/ores.codegen",
              file=sys.stderr)
        return 1

    configure(verbose=args.verbose)
    base_dir = PROJECT_ROOT / "projects" / "ores.codegen"

    if args.subcmd in ("generate", "gen"):
        return cmd_generate(args, base_dir)
    return cmd_regenerate(args, base_dir)


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
    if len(sys.argv) >= 2 and sys.argv[1] == "nats":
        sys.exit(cmd_nats(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "db":
        import compass_db
        sys.exit(compass_db.run(sys.argv[2:], PROJECT_ROOT))
    if len(sys.argv) >= 2 and sys.argv[1] == "sql":
        import compass_db
        sys.exit(compass_db.run(["sql"] + sys.argv[2:], PROJECT_ROOT))
    if len(sys.argv) >= 2 and sys.argv[1] == "services":
        import compass_services
        sys.exit(compass_services.run(sys.argv[2:], PROJECT_ROOT))
    if len(sys.argv) >= 2 and sys.argv[1] == "client":
        import compass_services
        sys.exit(compass_services.run_client(sys.argv[2:], PROJECT_ROOT))
    if len(sys.argv) >= 2 and sys.argv[1] == "test":
        sys.exit(cmd_test(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "site":
        sys.exit(cmd_site(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "build":
        sys.exit(cmd_build(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "shell":
        sys.exit(cmd_shell(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "review":
        sys.exit(cmd_review(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "timeline":
        import compass_timeline
        sys.exit(compass_timeline.run(sys.argv[2:], PROJECT_ROOT))
    if len(sys.argv) >= 2 and sys.argv[1] == "pr":
        sys.exit(cmd_pr(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] in ("bearings", "orient"):
        sys.exit(cmd_bearings(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "heading":
        sys.exit(cmd_heading(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "lint":
        sys.exit(cmd_lint(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] == "codegen":
        sys.exit(cmd_codegen(sys.argv[2:]))
    if len(sys.argv) >= 2 and sys.argv[1] in ALL_BUCKETS:
        sys.exit(cmd_backlog(sys.argv[1], sys.argv[2:]))

    # Unknown command — suggest closest match before handing off to argparse.
    if len(sys.argv) >= 2 and not sys.argv[1].startswith("-"):
        _KNOWN_COMMANDS = [
            "index", "search", "find", "debug", "where", "status", "fleet",
            "list", "show", "add", "sprint", "story", "task", "journal",
            "env", "nats", "db", "sql", "services", "client", "test", "build",
            "site", "shell", "review", "pr", "bearings", "orient", "timeline",
            "capture", "lint", "codegen",
            "inbox", "next", "deferred", "discarded", "backlog",
        ]
        cmd_given = sys.argv[1]
        if cmd_given not in _KNOWN_COMMANDS:
            suggestion = _closest_command(cmd_given, _KNOWN_COMMANDS)
            if suggestion:
                print(f"❌  Unknown command: '{cmd_given}'. "
                      f"Did you mean: compass {suggestion}?", file=sys.stderr)
            else:
                print(f"❌  Unknown command: '{cmd_given}'.", file=sys.stderr)
            sys.exit(1)

    _EPILOG = (
        "Pillars:\n"
        "  Orient:    where, fleet, timeline\n"
        "  Search:    search (find), list, show\n"
        "  Scaffold:  story, task, add\n"
        "  Capture:   capture, inbox, next, deferred, discarded, backlog\n"
        "  Journal:   journal\n"
        "  Provision: env, nats, db\n"
        "  Test:      test\n"
        "  Build:     build\n"
        "  Codegen:   codegen generate | codegen regenerate | codegen entity\n"
        "  Site:      site\n"
        "  Operate:   services, client\n"
        "  Shell:     shell\n"
        "  Review:    review\n"
        "  PR:        pr\n"
        "  Bearings:  bearings (alias: orient)\n"
        "  Lint:      lint\n"
        "\n"
        "Entity commands (sub-subcommands span pillars):\n"
        "  sprint:   status | audit (orient)\n"
        "  story:    new (scaffold) | status (orient)\n"
        "  task:     new (scaffold)\n"
        "  env:      provision | deprovision | configure | diff | list | version [new]\n"
        "  db:       recreate | setup | drop | sql | reset-system | reset-tenant (provision)\n"
        "  sql:      alias for db sql — run SQL in the environment\n"
        "  services: start | stop | status | clear-logs (operate)\n"
    )
    parser = argparse.ArgumentParser(
        description="Compass: developer toolkit for ORE Studio — orient, scaffold, capture, and search.",
        epilog=_EPILOG,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    index_parser = subparsers.add_parser("index", help="Index or update notes from org-roam.db")
    index_parser.add_argument("--rebuild", action="store_true", help="Rebuild the entire index from scratch")
    index_parser.add_argument("--org-roam-db-sync", action="store_true",
                              help="Sync .org-roam.db before indexing (same as compass build --direct org-roam-db-sync)")

    search_parser = subparsers.add_parser("search", aliases=["find"], help="Search your notes")
    search_parser.add_argument("query", type=str, help="The search query")
    search_parser.add_argument("-l", "--limit", type=int, default=5, help="Max results (default 5)")
    search_parser.add_argument("-f", "--format", choices=["pretty", "line", "json"], default="pretty",
                              help="Output format: pretty (default), line (UUID path match), or json")
    search_parser.add_argument("-v", "--verbose", action="store_true",
                              help="Verbose pretty output: file path, location, and matched snippet per hit")
    search_parser.add_argument("--history", action="store_true",
                               help="Include past-sprint stories and tasks (excluded by default)")
    search_parser.add_argument("--all-buckets", dest="all_buckets", action="store_true",
                               help="Show all buckets regardless of score (disables relative dropout)")
    search_parser.add_argument("--floor", type=int, default=None, metavar="PCT",
                               help="Absolute score floor %% (built-in default: 25)")
    search_parser.add_argument("--dropout", type=float, default=None, metavar="RATIO",
                               help="Relative dropout ratio — keep results ≥ max_score × RATIO "
                                    "(default built-in: 0.25; 0 disables)")
    search_parser.add_argument("--related", action="store_true",
                               help="Show related docs per hit via the link graph "
                                    "(outgoing ranked by query relevance; "
                                    "incoming ranked by inbound-link count)")
    search_parser.add_argument("--related-limit", dest="related_limit", type=int,
                               default=3, metavar="N",
                               help="Max related docs per direction per hit (default 3); "
                                    "implies --related")
    search_parser.add_argument("--under", action="append", default=[], metavar="PATH",
                              help="Restrict to docs whose path is below PATH (repeatable)")
    search_parser.add_argument("--type", dest="doctype", default="",
                              help="Filter by document type (task, story, capture, recipe, …)")

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
                          help="Create a doc via ores.codegen (low-level scaffold); 'add --help' for usage")
    subparsers.add_parser("sprint",
                          help="Sprint-level operations: 'sprint status' for all stories by state; 'sprint --help'")
    subparsers.add_parser("story",
                          help="Story-level operations: 'story new' to create story+branch+task; 'story --help'")
    subparsers.add_parser("task",
                          help="Task-level operations: 'task new' to add task+branch to existing story; 'task --help'")
    subparsers.add_parser("journal",
                          help="Read/write the per-worktree session journal; 'journal --help' for subcommands")
    subparsers.add_parser("env",
                          help="Provision: 'env provision' creates a worktree; 'env deprovision' removes it; 'env configure' generates .env; 'env --help'")
    subparsers.add_parser("db",
                          help="Provision: database lifecycle — recreate, "
                               "setup, drop, sql, reset-system, reset-tenant")
    subparsers.add_parser("sql",
                          help="Run SQL in the environment (alias for "
                               "'db sql'; args after -- go to psql)")
    subparsers.add_parser("services",
                          help="Operate: service lifecycle — start, stop, "
                               "status, clear-logs")
    subparsers.add_parser("client",
                          help="Operate: launch the Qt client (detached; "
                               "--colour/--instance-name for parallel runs)")
    subparsers.add_parser("test",
                          help="Test: 'test results' shows last run overview; "
                               "'test logging on|off|status' toggles test logging; 'test --help'")
    subparsers.add_parser("site",
                          help="Site: build and serve the org-mode site locally; "
                               "'site serve [--compile] [--port N]'; 'site --help'")
    subparsers.add_parser("build",
                          help="Build: run cmake with the preset from .env (ORES_PRESET); "
                               "'build site' builds the website; 'build --help'")
    subparsers.add_parser("shell",
                          help="Shell: launch ores.shell with NATS and login "
                               "defaults from .env; '-f FILE' runs a scripted "
                               "session; 'shell --help'")
    subparsers.add_parser("review",
                          help="Review: PR review-round verbs via gh — 'review list <pr>', "
                               "'review reply <pr> <id> <msg>', 'review resolve <pr>', "
                               "'review pending [--since 24h]'; 'review --help'")
    subparsers.add_parser("pr",
                          help="PR: pull-request lifecycle verbs via gh — "
                               "'pr checks [--watch]', 'pr create', 'pr merge [--force]', "
                               "'pr record'; 'pr --help'")
    subparsers.add_parser("timeline",
                          help="Timeline: everyone × past — 'timeline generate [--since 20m]', "
                               "'timeline now', 'timeline snapshot [--since 20m]', "
                               "'timeline show [-n N]'; 'timeline --help'")
    subparsers.add_parser("bearings",
                          help="Cold-start orientation: identity, where, last session, recipes, memories")
    subparsers.add_parser("orient",
                          help="Alias for bearings")
    subparsers.add_parser("heading",
                          help="Suggest the next work item: ranked by priority from sprint state, "
                               "fleet, and backlog; optional keywords bias the ranking")
    subparsers.add_parser("lint",
                          help="Validate #+filetags: values across all .org files; "
                               "exits non-zero on malformed tags")
    subparsers.add_parser("inbox",     help="List captures in the product backlog inbox/")
    subparsers.add_parser("next",      help="List captures in the product backlog next/")
    subparsers.add_parser("deferred",  help="List captures in the product backlog deferred/")
    subparsers.add_parser("discarded", help="List captures in the product backlog discarded/")

    args = parser.parse_args()

    # Only the org-roam-backed commands need org-roam.db; the agile/doc-graph
    # commands read the working tree directly. index --org-roam-db-sync
    # creates the db itself, so it validates after the sync (in cmd_index).
    if args.command in ("index", "search", "find", "debug") and \
            not (args.command == "index"
                 and getattr(args, "org_roam_db_sync", False)):
        validate_paths(args.command)

    if args.command == "index":
        cmd_index(args)
    elif args.command in ("search", "find"):
        if getattr(args, 'related_limit', 3) != 3:
            args.related = True
        cmd_search(args)
    elif args.command == "debug":
        cmd_debug(args)
    elif args.command in ("where", "status"):
        cmd_where(args)
    elif args.command == "fleet":
        cmd_fleet(args)

if __name__ == "__main__":
    main()
