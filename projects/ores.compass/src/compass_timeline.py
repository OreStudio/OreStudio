"""compass timeline — the everyone × past quadrant of the temporal grid.

Two behaviours (see the temporal command coherence investigation,
551FC710-7E3C-484B-AB52-7F2C8DAF3B6F):

- generate: mine the *consistent substrate* — the agile documents'
  git history on origin/main plus the GitHub PR record — for events in
  a time window. Never reads per-worktree journals, so any fresh
  checkout of main produces identical output.
- show: render the last N LLM-curated snapshot buckets from the
  sprint timeline/ folders.
"""

import argparse
import json
import re
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

AGILE_SUBDIR = "doc/agile"
DEFAULT_WINDOW = "20m"

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------


def _run(args, cwd):
    p = subprocess.run(args, cwd=cwd, capture_output=True, text=True)
    return p.returncode, p.stdout, p.stderr


def _parse_duration(spec):
    """'20m' / '2h' / '1d' / '45s' -> timedelta, else None."""
    m = re.fullmatch(r"(\d+)([smhd])", spec.strip())
    if not m:
        return None
    n, unit = int(m.group(1)), m.group(2)
    return timedelta(**{{"s": "seconds", "m": "minutes",
                         "h": "hours", "d": "days"}[unit]: n})


def _parse_when(spec):
    """ISO timestamp (date or datetime, naive = local) -> aware datetime."""
    try:
        dt = datetime.fromisoformat(spec)
    except ValueError:
        return None
    if dt.tzinfo is None:
        dt = dt.astimezone()
    return dt


def _resolve_window(args):
    """(from, to) aware datetimes from --since / --from/--to."""
    now = datetime.now(timezone.utc).astimezone()
    if args.frm or args.to:
        if not (args.frm and args.to):
            print("❌ --from and --to must be given together.",
                  file=sys.stderr)
            return None
        start, end = _parse_when(args.frm), _parse_when(args.to)
        if not start or not end or start >= end:
            print("❌ bad --from/--to window.", file=sys.stderr)
            return None
        return start, end
    spec = args.since or DEFAULT_WINDOW
    delta = _parse_duration(spec)
    if delta:
        return now - delta, now
    start = _parse_when(spec)
    if start:
        return start, now
    print(f"❌ cannot parse --since '{spec}' (try 20m, 2h, 1d or ISO).",
          file=sys.stderr)
    return None


# ---------------------------------------------------------------------------
# Document field extraction (from a blob, not the working tree)
# ---------------------------------------------------------------------------

_ID_RE = re.compile(r"^:ID:\s*([0-9A-Fa-f-]+)", re.M)
_KW_RE = re.compile(r"^#\+(\w+):\s*(.*)$", re.M)
_STATE_RE = re.compile(r"^\|\s*State\s*\|\s*([A-Za-z]+)\s*\|", re.M)


def _doc_fields(blob):
    """id, title, type, environment, state from org source (best effort)."""
    fields = {"id": None, "title": "", "type": "",
              "environment": "", "state": ""}
    if not blob:
        return fields
    m = _ID_RE.search(blob)
    if m:
        fields["id"] = m.group(1).upper()
    for k, v in _KW_RE.findall(blob):
        k = k.lower()
        if k in ("title", "type", "environment"):
            fields[k] = v.strip()
    m = _STATE_RE.search(blob)
    if m:
        fields["state"] = m.group(1).upper()
    return fields


def _git_blob(project_root, ref, path):
    rc, out, _ = _run(["git", "show", f"{ref}:{path}"], project_root)
    return out if rc == 0 else None


# ---------------------------------------------------------------------------
# generate — events from the consistent substrate
# ---------------------------------------------------------------------------


def _doc_events(project_root, start, end):
    """Mine origin/main commits in the window for agile doc events."""
    rc, out, err = _run(
        ["git", "log", "origin/main",
         f"--since={start.isoformat()}", f"--until={end.isoformat()}",
         "--pretty=%H|%cI|%s", "--name-status", "--", AGILE_SUBDIR],
        project_root)
    if rc != 0:
        print(err.strip() or "❌ git log failed.", file=sys.stderr)
        return None

    events = []
    sha = when = subject = None
    for line in out.splitlines():
        if not line.strip():
            continue
        if "|" in line and re.match(r"^[0-9a-f]{40}\|", line):
            sha, when, subject = line.split("|", 2)
            continue
        m = re.match(r"^([AMD])\t(.+)$", line)
        if not m or not sha:
            continue
        status, path = m.group(1), m.group(2)
        if not path.endswith(".org"):
            continue

        new = _git_blob(project_root, sha, path)
        fields = _doc_fields(new)
        event = {
            "time": when,
            "sha": sha[:9],
            "subject": subject,
            "path": path,
            "doc_type": fields["type"],
            "title": re.sub(r"^(Story|Task):\s*", "", fields["title"]),
            "id": fields["id"],
            "environment": fields["environment"],
        }
        if status == "A":
            event["action"] = "created"
            event["state"] = fields["state"]
        elif status == "D":
            old_fields = _doc_fields(_git_blob(project_root, f"{sha}^", path))
            event.update(action="deleted", doc_type=old_fields["type"],
                         title=re.sub(r"^(Story|Task):\s*", "",
                                      old_fields["title"]),
                         id=old_fields["id"])
        else:
            old = _doc_fields(_git_blob(project_root, f"{sha}^", path))
            if old["state"] != fields["state"] and fields["state"]:
                event["action"] = "state"
                event["from"] = old["state"]
                event["to"] = fields["state"]
            else:
                event["action"] = "updated"
        events.append(event)
    events.sort(key=lambda e: e["time"])
    return events


def _pr_events(project_root, start, end):
    """Opened and merged PRs in the window, via gh. Best effort."""
    prs = []

    def search(flavour, query, fields):
        rc, out, err = _run(
            ["gh", "pr", "list", "--state", "all", "--limit", "200",
             "--search", query, "--json", fields], project_root)
        if rc != 0:
            print(f"⚠️  gh unavailable for {flavour} PRs "
                  f"({(err or '').strip().splitlines()[:1]}); skipping.",
                  file=sys.stderr)
            return []
        return json.loads(out or "[]")

    fmt = "%Y-%m-%dT%H:%M:%S"
    a = start.astimezone(timezone.utc).strftime(fmt)
    b = end.astimezone(timezone.utc).strftime(fmt)

    def local(ts):
        # gh returns UTC; align with the doc events' local-offset ISO.
        return datetime.fromisoformat(ts.replace("Z", "+00:00")) \
            .astimezone().isoformat()

    for pr in search("created", f"created:{a}Z..{b}Z",
                     "number,title,createdAt,author"):
        prs.append({"time": local(pr["createdAt"]), "action": "pr-opened",
                    "number": pr["number"], "title": pr["title"]})
    for pr in search("merged", f"merged:{a}Z..{b}Z",
                     "number,title,mergedAt,author"):
        prs.append({"time": local(pr["mergedAt"]), "action": "pr-merged",
                    "number": pr["number"], "title": pr["title"]})
    prs.sort(key=lambda e: e["time"])
    return prs


_ACTION_ICON = {"created": "✨", "state": "🔁", "updated": "📝",
                "deleted": "🗑️", "pr-opened": "🔀", "pr-merged": "✅"}

_PLURAL = {"story": "Stories", "task": "Tasks", "capture": "Captures",
           "sprint": "Sprints"}

# A commit touching more than this many docs with plain "updated" events
# is a bulk sweep (mass renames, link rewrites); collapse it to one line
# so it cannot drown the real activity. State changes and creations are
# never collapsed.
BULK_THRESHOLD = 5


def _split_bulk(doc_events):
    """(events, bulk) — pull plain updates of oversized commits aside."""
    updates_per_sha = {}
    for e in doc_events:
        if e["action"] == "updated":
            updates_per_sha.setdefault(e["sha"], []).append(e)
    bulk_shas = {sha for sha, es in updates_per_sha.items()
                 if len(es) > BULK_THRESHOLD}
    kept = [e for e in doc_events
            if not (e["action"] == "updated" and e["sha"] in bulk_shas)]
    # Coalesce repeated plain updates of the same document across
    # commits: one line with a count, stamped with the latest time.
    seen = {}
    coalesced = []
    for e in kept:
        if e["action"] == "updated" and e["id"]:
            if e["id"] in seen:
                prior = seen[e["id"]]
                prior["count"] = prior.get("count", 1) + 1
                prior["time"] = e["time"]
                continue
            seen[e["id"]] = e
        coalesced.append(e)
    kept = coalesced
    bulk = [{"sha": sha, "time": es[0]["time"], "subject": es[0]["subject"],
             "count": len(es)}
            for sha, es in updates_per_sha.items() if sha in bulk_shas]
    bulk.sort(key=lambda b: b["time"])
    return kept, bulk


def _print_pretty(window, doc_events, pr_events):
    start, end = window
    print(f"🧭 ores.compass — timeline: "
          f"{start.strftime('%Y-%m-%d %H:%M')} → "
          f"{end.strftime('%Y-%m-%d %H:%M')}")
    if not doc_events and not pr_events:
        print("\n(no agile activity in the window)")
        return
    doc_events, bulk = _split_bulk(doc_events)

    by_type = {}
    for e in doc_events:
        by_type.setdefault(e["doc_type"] or "other", []).append(e)
    for doc_type in ("story", "task", "capture", "sprint"):
        group = by_type.pop(doc_type, [])
        if not group:
            continue
        print(f"\n{_PLURAL[doc_type]}:")
        for e in group:
            icon = _ACTION_ICON.get(e["action"], "•")
            when = e["time"][11:16]
            count = e.get("count", 1)
            what = {
                "created": "created" + (f" ({e['state']})"
                                        if e.get("state") else ""),
                "state": f"{e.get('from', '?')} → {e.get('to', '?')}",
                "updated": "updated" + (f" ×{count}" if count > 1 else ""),
                "deleted": "deleted",
            }[e["action"]]
            env = f"  [{e['environment']}]" if e["environment"] else ""
            print(f"  {icon} {when}  {e['title']}  — {what}{env}")
            if e["id"]:
                print(f"       compass show {e['id']}")
    for doc_type, group in sorted(by_type.items()):
        print(f"\nOther ({doc_type}):")
        for e in group:
            print(f"  • {e['time'][11:16]}  {e['path']} — {e['action']}")
    if bulk:
        print("\nBulk sweeps (collapsed):")
        for b in bulk:
            print(f"  🧹 {b['time'][11:16]}  {b['count']} docs updated by "
                  f"{b['sha']} — {b['subject']}")
    if pr_events:
        print("\nPull requests:")
        for e in pr_events:
            icon = _ACTION_ICON[e["action"]]
            print(f"  {icon} {e['time'][11:16]}  #{e['number']}  "
                  f"{e['title']}  — {e['action'][3:]}")


def _cmd_generate(args, project_root):
    window = _resolve_window(args)
    if not window:
        return 1
    start, end = window
    # The consistent substrate is origin/main as last fetched; refresh it
    # so "now" windows see the latest merges.
    _run(["git", "fetch", "origin", "main"], project_root)
    doc_events = _doc_events(project_root, start, end)
    if doc_events is None:
        return 1
    pr_events = _pr_events(project_root, start, end)
    if args.format == "json":
        print(json.dumps({
            "from": start.isoformat(), "to": end.isoformat(),
            "documents": doc_events, "prs": pr_events}, indent=1))
    else:
        _print_pretty(window, doc_events, pr_events)
    return 0


# ---------------------------------------------------------------------------
# show — render stored snapshot buckets
# ---------------------------------------------------------------------------


def _cmd_show(args, project_root):
    root = Path(project_root) / "doc/agile/versions"
    snaps = sorted(root.glob("*/sprint_*/timeline/*.org"),
                   key=lambda p: p.name)
    if not snaps:
        print("(no timeline snapshots yet — see the agile timeline story; "
              "they are written by the snapshot skill)")
        return 0
    picked = snaps[-args.count:][::-1]  # newest first
    for i, p in enumerate(picked):
        if i:
            print("\n" + "─" * 72 + "\n")
        print(f"📍 {p.relative_to(project_root)}\n")
        print(p.read_text().rstrip())
    return 0


# ---------------------------------------------------------------------------
# entry point
# ---------------------------------------------------------------------------


def run(argv, project_root):
    """Entry point: compass timeline <subcommand>."""
    ap = argparse.ArgumentParser(
        prog="compass timeline",
        description="Timeline pillar: the everyone × past quadrant — "
                    "events from the consistent substrate (origin/main + "
                    "GitHub), and stored snapshot buckets.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    gp = sub.add_parser(
        "generate",
        help="List agile documents changed in a window (LLM snapshot input)")
    gp.add_argument("--since", default=None,
                    help=f"Window as duration (20m, 2h, 1d) or ISO start "
                         f"(default: {DEFAULT_WINDOW})")
    gp.add_argument("--from", dest="frm", default=None,
                    help="Window start (ISO; needs --to)")
    gp.add_argument("--to", dest="to", default=None,
                    help="Window end (ISO; needs --from)")
    gp.add_argument("-f", "--format", choices=["pretty", "json"],
                    default="pretty", help="Output format (default: pretty)")

    np = sub.add_parser(
        "now", help=f"Alias: generate over the last {DEFAULT_WINDOW}")
    np.add_argument("-f", "--format", choices=["pretty", "json"],
                    default="pretty", help="Output format (default: pretty)")

    sp = sub.add_parser(
        "show", help="Render the last N snapshot buckets from timeline/")
    sp.add_argument("-n", "--count", type=int, default=3,
                    help="How many buckets, newest first (default: 3)")

    args = ap.parse_args(argv)
    if args.subcmd == "now":
        args.since, args.frm, args.to = DEFAULT_WINDOW, None, None
        return _cmd_generate(args, project_root)
    if args.subcmd == "generate":
        return _cmd_generate(args, project_root)
    if args.subcmd == "show":
        return _cmd_show(args, project_root)
    return 1
