#!/usr/bin/env python3
"""compass review — Review pillar: PR review-round verbs wrapping the gh CLI.

Subcommands:
  list <pr>                      List review comments — line-level threads
                                 plus conversation-level comments and
                                 review summaries (--detail for full
                                 bodies; --format json for machines).
  reply <pr> <comment-id> <msg>  Reply to a specific review comment.
  resolve <pr>                   Resolve all unresolved review threads
                                 (--dry-run to preview).
  pending                        List PRs with unresolved review threads
                                 within a look-back window (--since 24h),
                                 to catch review rounds we forgot.

All subcommands accept --owner/--repo; defaults are derived from the
checkout's origin remote.
"""

import argparse
import datetime
import json
import re
import pathlib
import subprocess
import sys

_C_GREEN  = "\033[32m"
_C_YELLOW = "\033[33m"
_C_RED    = "\033[31m"
_C_RESET  = "\033[0m"


def _run_gh(args):
    """Run gh with ARGS; return (rc, stdout, stderr)."""
    try:
        p = subprocess.run(["gh"] + args, capture_output=True, text=True)
    except FileNotFoundError:
        print("❌ gh CLI not found on PATH.", file=sys.stderr)
        return 127, "", ""
    return p.returncode, p.stdout, p.stderr


def _default_owner_repo(project_root):
    """Derive (owner, repo) from the origin remote URL."""
    try:
        p = subprocess.run(
            ["git", "-C", str(project_root), "remote", "get-url", "origin"],
            capture_output=True, text=True)
        url = p.stdout.strip()
    except OSError:
        url = ""
    m = re.search(r"github\.com[:/]([^/]+)/([^/\s]+?)(?:\.git)?$", url)
    if m:
        return m.group(1), m.group(2)
    return "", ""


def _resolve_owner_repo(args, project_root):
    """Fill in args.owner/args.repo from the remote when not supplied."""
    owner, repo = args.owner, args.repo
    if not owner or not repo:
        d_owner, d_repo = _default_owner_repo(project_root)
        owner = owner or d_owner
        repo = repo or d_repo
    if not owner or not repo:
        print("❌ Could not derive --owner/--repo from the origin remote; "
              "pass them explicitly.", file=sys.stderr)
        return None, None
    return owner, repo


def _fetch_comments(owner, repo, pr):
    """Fetch all review comments for PR; return (rc, list-of-dicts)."""
    # --jq '.[]' emits one JSON object per line, which keeps --paginate
    # output parseable (raw pagination concatenates JSON arrays).
    rc, out, err = _run_gh([
        "api", f"repos/{owner}/{repo}/pulls/{pr}/comments",
        "--paginate", "--jq", ".[]"])
    if rc != 0:
        print(err.strip() or "❌ gh api call failed.", file=sys.stderr)
        return rc, []
    comments = [json.loads(line) for line in out.splitlines() if line.strip()]
    return 0, comments


def _fetch_conversation(owner, repo, pr):
    """Fetch conversation-level comments and review summaries for PR.

    Returns (rc, list) where each entry carries kind ('comment' for an
    issue comment, 'review' for a review summary), id, author,
    created_at, state (reviews only), and body. State-only reviews
    with no body (bare approvals) are dropped.
    """
    rc, out, err = _run_gh([
        "api", f"repos/{owner}/{repo}/issues/{pr}/comments",
        "--paginate", "--jq", ".[]"])
    if rc != 0:
        print(err.strip() or "❌ gh api call failed.", file=sys.stderr)
        return rc, []
    entries = []
    for line in out.splitlines():
        if not line.strip():
            continue
        c = json.loads(line)
        entries.append({
            "kind": "comment",
            "id": c["id"],
            "author": (c.get("user") or {}).get("login", "?"),
            "created_at": c.get("created_at") or "",
            "state": None,
            "body": c.get("body") or "",
        })
    rc, out, err = _run_gh([
        "api", f"repos/{owner}/{repo}/pulls/{pr}/reviews",
        "--paginate", "--jq", ".[]"])
    if rc != 0:
        print(err.strip() or "❌ gh api call failed.", file=sys.stderr)
        return rc, []
    for line in out.splitlines():
        if not line.strip():
            continue
        r = json.loads(line)
        if not (r.get("body") or "").strip():
            continue
        entries.append({
            "kind": "review",
            "id": r["id"],
            "author": (r.get("user") or {}).get("login", "?"),
            "created_at": r.get("submitted_at") or "",
            "state": r.get("state"),
            "body": r["body"],
        })
    entries.sort(key=lambda e: e["created_at"])
    return 0, entries


def _preview(body, width=100):
    """First line of BODY squashed to WIDTH chars, image markup stripped."""
    text = re.sub(r"!\[[^]]*\]\([^)]*\)", "", body or "")  # strip images
    text = " ".join(text.split())
    return text[:width] + ("…" if len(text) > width else "")


def _print_body(body, detail):
    """Print BODY indented — full when DETAIL, one-line preview otherwise."""
    if detail:
        for body_line in (body or "").splitlines():
            print(f"    {body_line}")
    else:
        print(f"    {_preview(body)}")


def _cmd_list(args, project_root):
    owner, repo = _resolve_owner_repo(args, project_root)
    if owner is None:
        return 1
    rc, comments = _fetch_comments(owner, repo, args.pr)
    if rc != 0:
        return rc
    rc, conversation = _fetch_conversation(owner, repo, args.pr)
    if rc != 0:
        return rc

    if args.format == "json":
        keep = ("id", "in_reply_to_id", "path", "line", "side",
                "html_url", "body")
        print(json.dumps({
            "line": [{k: c.get(k) for k in keep}
                     | {"author": (c.get("user") or {}).get("login", "")}
                     for c in comments],
            "conversation": conversation,
        }, indent=2))
        return 0

    print(f"🧭 ores.compass — review comments: "
          f"PR #{args.pr} ({owner}/{repo})\n")

    print("Line comments:\n" if comments else "Line comments: (none)\n")
    for c in comments:
        author = (c.get("user") or {}).get("login", "?")
        line = c.get("line") or c.get("original_line") or "?"
        reply_to = c.get("in_reply_to_id")
        marker = f"  ↳ reply to #{reply_to}" if reply_to else ""
        print(f"#{c['id']}  {c.get('path', '?')}:{line}  {author}{marker}")
        _print_body(c.get("body"), args.detail)
        print()

    print("Conversation:\n" if conversation else "Conversation: (none)\n")
    for e in conversation:
        if e["kind"] == "review":
            kind = f"review summary ({e['state']})"
        elif e["author"].endswith("[bot]"):
            kind = "bot comment (issue-level)"
        else:
            kind = "comment"
        when = (e["created_at"] or "")[:10]
        print(f"#{e['id']}  {e['author']}  {when}  [{kind}]")
        _print_body(e["body"], args.detail)
        print()

    top = sum(1 for c in comments if not c.get("in_reply_to_id"))
    print(f"{len(comments)} line comment(s) ({top} top-level), "
          f"{len(conversation)} conversation item(s).")
    return 0


def _is_not_found(err):
    """Return True iff ERR looks like a genuine 404 / not-found response."""
    return bool(re.search(r"404|Not Found|not found", err, re.I))


def _comment_kind(owner, repo, comment_id):
    """Return 'review' if comment_id is a PR review line comment,
    'issue' if it is an issue-level conversation comment, None if not found,
    or raise RuntimeError on an unexpected API error (auth, rate-limit, …).

    Claude's automated reviews are posted as issue comments (not review
    threads), so their IDs are issue comment IDs and cannot be replied to
    via the review-comment reply API.
    """
    rc, _out, err = _run_gh([
        "api", f"repos/{owner}/{repo}/pulls/comments/{comment_id}"])
    if rc == 0:
        return "review"
    if not _is_not_found(err):
        raise RuntimeError(err.strip() or "gh API error (review comment probe)")
    rc, _out, err = _run_gh([
        "api", f"repos/{owner}/{repo}/issues/comments/{comment_id}"])
    if rc == 0:
        return "issue"
    if not _is_not_found(err):
        raise RuntimeError(err.strip() or "gh API error (issue comment probe)")
    return None


def _cmd_reply(args, project_root):
    owner, repo = _resolve_owner_repo(args, project_root)
    if owner is None:
        return 1
    try:
        kind = _comment_kind(owner, repo, args.comment_id)
    except RuntimeError as exc:
        print(f"❌ {exc}", file=sys.stderr)
        return 1
    if kind == "review":
        rc, out, err = _run_gh([
            "api",
            f"repos/{owner}/{repo}/pulls/{args.pr}/comments/"
            f"{args.comment_id}/replies",
            "--method", "POST", "-f", f"body={args.message}"])
        if rc != 0:
            print(err.strip() or "❌ gh api call failed.", file=sys.stderr)
            return rc
        reply = json.loads(out)
        print(f"✅ Reply #{reply['id']} created: {reply.get('html_url', '')}")
    elif kind == "issue":
        # Claude bot posts issue-level comments, not resolvable review threads.
        # Post a new issue comment as the reply.
        rc, out, err = _run_gh([
            "api",
            f"repos/{owner}/{repo}/issues/{args.pr}/comments",
            "--method", "POST", "-f", f"body={args.message}"])
        if rc != 0:
            print(err.strip() or "❌ gh api call failed.", file=sys.stderr)
            return rc
        comment = json.loads(out)
        print(f"✅ Comment #{comment['id']} posted: "
              f"{comment.get('html_url', '')} "
              f"(issue comment — not an inline thread reply)")
    else:
        print(f"❌ Comment #{args.comment_id} not found as a review comment "
              f"or issue comment on PR #{args.pr}.", file=sys.stderr)
        return 1
    return 0


def _fetch_unresolved_threads(owner, repo, pr):
    """Return (rc, [thread]) where thread has id, path, line, preview."""
    query = (
        'query { repository(owner: "%s", name: "%s") {'
        '  pullRequest(number: %d) {'
        '    reviewThreads(first: 100) {'
        '      nodes { id isResolved'
        '        comments(first: 1) {'
        '          nodes { databaseId path line body } } } } } } }'
        % (owner, repo, pr))
    rc, out, err = _run_gh(["api", "graphql", "-f", f"query={query}"])
    if rc != 0:
        print(err.strip() or "❌ gh graphql call failed.", file=sys.stderr)
        return rc, []
    nodes = (json.loads(out)["data"]["repository"]["pullRequest"]
             ["reviewThreads"]["nodes"])
    threads = []
    for n in nodes:
        if n["isResolved"]:
            continue
        first = (n["comments"]["nodes"] or [{}])[0]
        threads.append({
            "id": n["id"],
            "comment_id": first.get("databaseId"),
            "path": first.get("path", "?"),
            "line": first.get("line"),
            "preview": _preview(first.get("body")),
        })
    return 0, threads


def _cmd_resolve(args, project_root):
    owner, repo = _resolve_owner_repo(args, project_root)
    if owner is None:
        return 1
    rc, threads = _fetch_unresolved_threads(owner, repo, args.pr)
    if rc != 0:
        return rc
    if not threads:
        rc2, conversation = _fetch_conversation(owner, repo, args.pr)
        bot_reviews = ([e for e in conversation
                        if e["kind"] == "comment"
                        and e["author"].endswith("[bot]")]
                       if rc2 == 0 else [])
        if bot_reviews:
            print(f"✅ No resolvable review threads on PR #{args.pr}. "
                  f"{len(bot_reviews)} bot comment(s) are issue-level "
                  f"— they have no resolvable thread.")
        else:
            print(f"✅ No unresolved review threads on PR #{args.pr}.")
        return 0

    for t in threads:
        line = f":{t['line']}" if t["line"] else ""
        print(f"{'would resolve' if args.dry_run else 'resolving'} "
              f"{t['id']}  {t['path']}{line}  (comment #{t['comment_id']})")
        print(f"    {t['preview']}")
        if args.dry_run:
            continue
        mutation = ('mutation { resolveReviewThread(input: {threadId: "%s"})'
                    ' { thread { isResolved } } }' % t["id"])
        rc, out, err = _run_gh(["api", "graphql", "-f", f"query={mutation}"])
        if rc != 0:
            print(err.strip() or "❌ resolve mutation failed.",
                  file=sys.stderr)
            return rc
    verb = "would be resolved" if args.dry_run else "resolved"
    print(f"\n{len(threads)} thread(s) {verb}.")
    return 0


def _ycmd(cmd):
    """Render a compass command hint in the standard yellow."""
    return f"{_C_YELLOW}{cmd}{_C_RESET}"


def _pr_agile_links(project_root, numbers):
    """Map PR number → (task_id, story_id) by scanning doc/agile docs.

    A PR is matched to the task doc that records it (a /pull/<n>] link
    in its * PRs table or a #+pr: <n> property); the story comes from
    the task's Parent story row. When the recording doc is itself a
    story, only the story id is returned. PRs with no match are
    simply absent from the result.
    """
    import os
    results = {}
    base = os.path.join(str(project_root), "doc", "agile")
    remaining = set(numbers)
    for root, _dirs, files in os.walk(base):
        if not remaining:
            break
        for fn in files:
            if not fn.endswith(".org") or not remaining:
                continue
            path = os.path.join(root, fn)
            try:
                with open(path, encoding="utf-8", errors="ignore") as f:
                    text = f.read()
            except OSError:
                continue
            for n in list(remaining):
                if (f"/pull/{n}]" not in text
                        and not re.search(rf"^#\+pr: {n}\s*$", text, re.M)):
                    continue
                doc_id = re.search(r":ID:\s+([0-9A-Fa-f-]{36})", text)
                doc_type = re.search(r"^#\+type:\s+(\w+)", text, re.M)
                if not doc_id:
                    continue
                if doc_type and doc_type.group(1) == "story":
                    results[n] = (None, doc_id.group(1))
                else:
                    parent = re.search(
                        r"Parent story\s*\|\s*\[\[id:([0-9A-Fa-f-]{36})\]",
                        text)
                    results[n] = (doc_id.group(1),
                                  parent.group(1) if parent else None)
                remaining.discard(n)
    return results


def _rebasing_branch(worktree_path):
    """Branch a detached worktree is rebasing, or '' when not rebasing.

    A mid-rebase worktree reports detached HEAD, which would make its
    PR look unowned; git keeps the real branch in rebase-merge/
    rebase-apply head-name.
    """
    try:
        gitdir = subprocess.run(
            ["git", "-C", worktree_path, "rev-parse",
             "--absolute-git-dir"],
            capture_output=True, text=True).stdout.strip()
        for state in ("rebase-merge", "rebase-apply"):
            head = pathlib.Path(gitdir) / state / "head-name"
            if head.is_file():
                return head.read_text().strip().removeprefix(
                    "refs/heads/")
    except OSError:
        pass
    return ""


def _worktree_branches(project_root):
    """Map branch name → worktree directory name, across all worktrees."""
    try:
        p = subprocess.run(
            ["git", "-C", str(project_root), "worktree", "list",
             "--porcelain"],
            capture_output=True, text=True)
    except OSError:
        return {}
    branches, full_path, path = {}, None, None
    for line in p.stdout.splitlines():
        if line.startswith("worktree "):
            full_path = line.split(" ", 1)[1]
            path = full_path.rstrip("/").rsplit("/", 1)[-1]
        elif line.startswith("branch refs/heads/") and path:
            branches[line.rsplit("refs/heads/", 1)[1]] = path
        elif line.strip() == "detached" and full_path:
            branch = _rebasing_branch(full_path)
            if branch:
                branches[branch] = path
    return branches


def _parse_since(spec):
    """Parse a look-back spec like 30m, 2h, 24h, 7d into seconds."""
    m = re.fullmatch(r"(\d+)([mhd])", spec.strip())
    if not m:
        return None
    value, unit = int(m.group(1)), m.group(2)
    return value * {"m": 60, "h": 3600, "d": 86400}[unit]


def _cmd_pending(args, project_root):
    owner, repo = _resolve_owner_repo(args, project_root)
    if owner is None:
        return 1
    seconds = _parse_since(args.since)
    if seconds is None:
        print(f"❌ Invalid --since value '{args.since}'; "
              "use e.g. 30m, 2h, 24h, 7d.", file=sys.stderr)
        return 1
    cutoff = (datetime.datetime.now(datetime.timezone.utc)
              - datetime.timedelta(seconds=seconds))

    states = {"open": "[OPEN]", "merged": "[MERGED]",
              "closed": "[CLOSED]",
              "all": "[OPEN, MERGED, CLOSED]"}[args.state]
    query = (
        'query { repository(owner: "%s", name: "%s") {'
        '  pullRequests(states: %s, first: 100,'
        '    orderBy: {field: UPDATED_AT, direction: DESC}) {'
        '    nodes { number title url updatedAt isDraft state headRefName'
        '      reviewThreads(first: 100) { nodes { isResolved } } } } } }'
        % (owner, repo, states))
    rc, out, err = _run_gh(["api", "graphql", "-f", f"query={query}"])
    if rc != 0:
        print(err.strip() or "❌ gh graphql call failed.", file=sys.stderr)
        return rc
    nodes = (json.loads(out)["data"]["repository"]["pullRequests"]["nodes"])

    now = datetime.datetime.now(datetime.timezone.utc)
    worktrees = _worktree_branches(project_root)
    here = str(project_root).rstrip("/").rsplit("/", 1)[-1]
    pending = []
    for n in nodes:
        updated = datetime.datetime.fromisoformat(
            n["updatedAt"].replace("Z", "+00:00"))
        if updated < cutoff:
            break  # DESC by update time: everything after is older
        unresolved = sum(1 for t in n["reviewThreads"]["nodes"]
                         if not t["isResolved"])
        if unresolved == 0:
            continue
        age = now - updated
        pending.append({
            "number": n["number"],
            "title": n["title"],
            "state": n["state"],
            "url": n["url"],
            "branch": n["headRefName"],
            "worktree": worktrees.get(n["headRefName"]),
            "unresolved": unresolved,
            "updated_at": n["updatedAt"],
            "age": _age_human(age.total_seconds()),
        })

    # Triage: unattended reviews on merged/closed PRs are lost feedback
    # (critical); open PRs owned here or unowned need a round (warning);
    # open PRs being worked in another worktree are fine (info).
    for p in pending:
        if p["state"] != "OPEN":
            p["severity"] = "critical"
            p["note"] = f"{p['state'].lower()} with unattended review threads"
        elif p["worktree"] and p["worktree"] != here:
            p["severity"] = "ok"
            p["note"] = f"being worked in {p['worktree']}"
        else:
            p["severity"] = "attention"
            where = ("this checkout" if p["worktree"] == here
                     else f"branch {p['branch']}, no local worktree")
            p["note"] = f"needs a review round ({where})"

    agile = _pr_agile_links(project_root, [p["number"] for p in pending])
    for p in pending:
        task_id, story_id = agile.get(p["number"], (None, None))
        p["task_id"], p["story_id"] = task_id, story_id

    if args.format == "json":
        print(json.dumps(pending, indent=2))
        return 0

    print(f"🧭 ores.compass — PRs with unattended review threads: "
          f"{args.state}, last {args.since} ({owner}/{repo})\n")
    if not pending:
        print(f"✅ No PRs with unattended review threads "
              f"in the last {args.since}.")
        return 0

    icon = {"critical": "🛑", "attention": "⚠️ ", "ok": "✅"}
    order = {"critical": 0, "attention": 1, "ok": 2}
    for p in sorted(pending, key=lambda x: (order[x["severity"]],
                                            -x["number"])):
        print(f"{icon[p['severity']]} #{p['number']}  [{p['state']}]  "
              f"{p['title']}")
        print(f"    {p['unresolved']} unresolved thread(s), "
              f"updated {p['age']} ago — {p['note']}")
        review_cmd = f"compass review list {p['number']}"
        print(f"    Review: {_ycmd(review_cmd)}")
        if p["story_id"]:
            print(f"    Story:  {_ycmd('compass show ' + p['story_id'])}")
        if p["task_id"]:
            print(f"    Task:   {_ycmd('compass show ' + p['task_id'])}")
        print()
    counts = {k: sum(1 for p in pending if p["severity"] == k)
              for k in ("critical", "attention", "ok")}
    print(f"{counts['critical']} critical, {counts['attention']} need a "
          f"round, {counts['ok']} in progress elsewhere.")
    return 0


def _age_human(seconds):
    """Compact age label: 30m, 4h, 2d."""
    s = int(seconds)
    if s < 3600:
        return f"{s // 60}m"
    if s < 86400:
        return f"{s // 3600}h"
    return f"{s // 86400}d"


def run(argv, project_root):
    """Entry point: compass review <subcommand>."""
    ap = argparse.ArgumentParser(
        prog="compass review",
        description="Review pillar: PR review-round verbs wrapping the "
                    "gh CLI.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    lp = sub.add_parser("list", help="List review comments on a PR")
    lp.add_argument("pr", type=int, help="PR number")
    lp.add_argument("--detail", action="store_true",
                    help="Show full comment bodies instead of previews")
    lp.add_argument("-f", "--format", choices=["pretty", "json"],
                    default="pretty", help="Output format (default: pretty)")

    rp = sub.add_parser("reply", help="Reply to a review comment")
    rp.add_argument("pr", type=int, help="PR number")
    rp.add_argument("comment_id", type=int, help="Review comment id")
    rp.add_argument("message", help="Reply body")

    sp = sub.add_parser("resolve",
                        help="Resolve all unresolved review threads on a PR")
    sp.add_argument("pr", type=int, help="PR number")
    sp.add_argument("--dry-run", action="store_true",
                    help="List the threads without resolving them")

    pp = sub.add_parser("pending",
                        help="List PRs with unresolved review threads")
    pp.add_argument("--since", default="24h",
                    help="Look-back window over PR updates, e.g. 30m, 2h, "
                         "24h, 7d (default: 24h)")
    pp.add_argument("--state", choices=["open", "merged", "closed", "all"],
                    default="all",
                    help="PR state to scan (default: all)")
    pp.add_argument("-f", "--format", choices=["pretty", "json"],
                    default="pretty", help="Output format (default: pretty)")

    for p in (lp, rp, sp, pp):
        p.add_argument("--owner", default="",
                       help="Repo owner (default: from origin remote)")
        p.add_argument("--repo", default="",
                       help="Repo name (default: from origin remote)")

    args = ap.parse_args(argv)
    if args.subcmd == "list":
        return _cmd_list(args, project_root)
    if args.subcmd == "reply":
        return _cmd_reply(args, project_root)
    if args.subcmd == "resolve":
        return _cmd_resolve(args, project_root)
    if args.subcmd == "pending":
        return _cmd_pending(args, project_root)
    return 1
