#!/usr/bin/env python3
"""compass pr — PR pillar: pull-request lifecycle verbs wrapping the gh CLI.

Subcommands:
  checks [pr]   CI status for a PR (default: the current branch's PR).
                --watch polls until checks settle; the exit code follows
                gh (0 green, non-zero on failing/pending checks) so
                runbooks can gate on it.
  record [pr]   Record a PR on its task doc: set #+pr: and add the row
                to the * PRs table. The task is found by matching
                #+branch: against the current branch (override with
                --task); idempotent when the PR is already recorded.
  create        Open a PR with the conventions built in: validated
                [component] title, Summary/Changes body, Traceability
                table derived from the branch's task and story docs,
                branch pushed with -u if needed, PR recorded on the
                task and stamped in the journal.
  merge [pr]    Merge a PR with guard rails: refuses while review
                threads are unresolved or CI is not green (--force to
                override, stating what was bypassed; also admin-merges
                past GitHub branch protection), always a merge commit
                (never squash/rebase), deletes the branch, and retries
                GitHub's transient base-branch-modified race.
                Merge never touches task state: close the task first
                with 'compass task done' so the bookkeeping rides the
                PR; a STARTED task only draws a warning.
  sync          Fetch origin/main and rebase the current branch onto
                it. On conflicts: stop where git stops, list the
                conflicted files and the ways out (--abort-on-conflict
                to roll back automatically for unattended use).
                --push force-pushes (with lease) after success.

The review-round verbs (list, reply, resolve, pending) live in the
Review pillar: compass review --help.
"""

import argparse
import json
import re
import subprocess
import sys
import time
from pathlib import Path


def _cmd_checks(args, project_root):
    cmd = ["gh", "pr", "checks"]
    if args.pr:
        cmd.append(str(args.pr))
    if args.watch:
        cmd.append("--watch")
    if args.fail_fast:
        cmd.append("--fail-fast")
    if args.required:
        cmd.append("--required")
    if args.interval is not None:
        cmd += ["--interval", str(args.interval)]
    try:
        # Stream straight through so --watch updates live; gh's exit
        # code (0 green, non-zero otherwise) is the gating signal.
        return subprocess.run(cmd, cwd=str(project_root)).returncode
    except FileNotFoundError:
        print("❌ gh CLI not found on PATH.", file=sys.stderr)
        return 127


def _current_branch(project_root):
    p = subprocess.run(["git", "symbolic-ref", "--short", "HEAD"],
                       capture_output=True, text=True, cwd=str(project_root))
    return p.stdout.strip() if p.returncode == 0 else ""


def _find_task_doc(project_root, branch, task_ref):
    """Find the task doc to record on.

    With TASK_REF, match a task file by UUID or slug. Otherwise match
    #+branch: BRANCH across doc/agile task docs, preferring a STARTED
    task when several share the branch.
    """
    base = Path(project_root) / "doc" / "agile"
    candidates = []
    for path in base.rglob("task_*.org"):
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        if task_ref:
            if (task_ref.upper() in text[:200]
                    or path.stem == f"task_{task_ref}"):
                candidates.append((path, text))
            continue
        if re.search(rf"^#\+branch: {re.escape(branch)}\s*$", text, re.M):
            candidates.append((path, text))
    if len(candidates) > 1:
        started = [c for c in candidates
                   if re.search(r"^\| State\s*\|\s*STARTED", c[1], re.M)]
        if len(started) == 1:
            return started[0]
    return candidates[0] if candidates else (None, None)


def _cmd_record(args, project_root):
    if args.pr is not None and args.pr <= 0:
        print("❌ PR number must be a positive integer.", file=sys.stderr)
        return 1
    # Resolve the PR: explicit number or the current branch's PR.
    sel = [str(args.pr)] if args.pr else []
    p = subprocess.run(
        ["gh", "pr", "view", *sel, "--json", "number,title,url"],
        capture_output=True, text=True, cwd=str(project_root))
    if p.returncode != 0:
        print(p.stderr.strip() or "❌ gh pr view failed.", file=sys.stderr)
        return p.returncode
    pr = json.loads(p.stdout)
    number, title, url = pr["number"], pr["title"], pr["url"]

    branch = _current_branch(project_root)
    path, text = _find_task_doc(project_root, branch, args.task)
    if path is None:
        print(f"❌ No task doc found for branch '{branch}' "
              "(set #+branch: on the task, or pass --task).",
              file=sys.stderr)
        return 1
    rel = path.relative_to(project_root)

    if f"/pull/{number}]" in text:
        print(f"✅ PR #{number} already recorded in {rel}")
        return 0

    text = re.sub(r"^#\+pr:.*$", f"#+pr: {number}", text,
                  count=1, flags=re.M)
    row = f"| [[{url}][#{number}]] | {title} |"
    empty = "| PR | Title |\n|----+-------|\n|    |       |"
    if empty in text:
        text = text.replace(empty, f"| PR | Title |\n|----+-------|\n{row}")
    else:
        text = text.replace("| PR | Title |\n|----+-------|",
                            f"| PR | Title |\n|----+-------|\n{row}")
    path.write_text(text, encoding="utf-8")
    print(f"✅ PR #{number} recorded in {rel}")
    print(f"   #+pr: {number}  +  * PRs row: {title}")
    return 0


_SITE_BASE = "https://orestudio.github.io/OreStudio/"


def _org_title(text, kind):
    """#+title: value with the 'Task: ' / 'Story: ' prefix stripped."""
    m = re.search(r"^#\+title:\s*(.+)$", text, re.M | re.I)
    if not m:
        return ""
    return re.sub(rf"(?i)^{kind}:\s*", "", m.group(1).strip())


def _org_id(text):
    m = re.search(r"(?i):ID:\s+([0-9A-Fa-f-]{36})", text)
    return m.group(1) if m else ""


def _site_url(rel_path):
    return _SITE_BASE + str(rel_path).removesuffix(".org") + ".html"


def _cmd_create(args, project_root):
    branch = _current_branch(project_root)
    if not branch or branch == "main":
        print("❌ Refusing to open a PR from main or a detached HEAD.",
              file=sys.stderr)
        return 1

    # Title convention: [component] Description, ideally <= 72 chars.
    if not re.match(r"^\[[^\]]+\] \S", args.title):
        print("❌ Title must follow the convention: "
              "'[component] Description' (multi-component: [a,b,c]).",
              file=sys.stderr)
        return 1
    if len(args.title) > 72:
        print(f"⚠️  Title is {len(args.title)} chars; convention prefers "
              "<= 72.", file=sys.stderr)

    # Traceability: the branch's task doc and its parent story.
    task_path, task_text = _find_task_doc(project_root, branch, args.task)
    if task_path is None:
        print(f"❌ No task doc found for branch '{branch}' "
              "(set #+branch: on the task, or pass --task).",
              file=sys.stderr)
        return 1
    task_id = _org_id(task_text)
    task_title = _org_title(task_text, "Task")
    task_rel = task_path.relative_to(project_root)
    if not task_id:
        print(f"❌ No :ID: property found in task doc {task_rel}.",
              file=sys.stderr)
        return 1
    story_path = task_path.parent / "story.org"
    try:
        story_text = story_path.read_text(encoding="utf-8")
    except OSError:
        print(f"❌ No story.org next to {task_rel}.", file=sys.stderr)
        return 1
    story_id = _org_id(story_text)
    story_title = _org_title(story_text, "Story")
    story_rel = story_path.relative_to(project_root)
    if not story_id:
        print(f"❌ No :ID: property found in story doc {story_rel}.",
              file=sys.stderr)
        return 1

    summary = args.summary or "(Fill in: what changes, why.)"
    changes = "\n".join(f"- {c}" for c in (args.change or ["(Fill in.)"]))
    body = (
        f"## Summary\n\n{summary}\n\n"
        f"## Changes\n\n{changes}\n\n"
        "## Traceability\n\n"
        "| Artefact | Link | ID |\n"
        "|----------|------|----|\n"
        f"| Story | [{story_title}]({_site_url(story_rel)}) | {story_id} |\n"
        f"| Task | [{task_title}]({_site_url(task_rel)}) | {task_id} |\n\n"
        "🤖 Generated with [Claude Code](https://claude.com/claude-code)")

    # Ensure the branch exists on the remote under its own name — a
    # branch created off origin/main tracks origin/main until first
    # push, so @{u} is not a reliable signal. push -u is idempotent.
    print(f"🔼 Pushing {branch}…")
    p = subprocess.run(["git", "push", "-u", "origin", branch],
                       cwd=str(project_root))
    if p.returncode != 0:
        return p.returncode

    cmd = ["gh", "pr", "create", "--title", args.title, "--body", body]
    if args.draft:
        cmd.append("--draft")
    p = subprocess.run(cmd, capture_output=True, text=True,
                       cwd=str(project_root))
    if p.returncode != 0:
        print(p.stderr.strip() or "❌ gh pr create failed.",
              file=sys.stderr)
        return p.returncode
    lines = p.stdout.strip().splitlines()
    if not lines:
        print("❌ gh pr create succeeded but returned no output.",
              file=sys.stderr)
        return 1
    url = lines[-1]
    print(f"✅ PR created: {url}")
    try:
        number = int(url.rstrip("/").rsplit("/", 1)[-1])
    except ValueError:
        print(f"❌ Could not parse PR number from URL '{url}'.",
              file=sys.stderr)
        return 1

    # Record on the task doc, commit and push the record, and stamp
    # the journal — no manual follow-up steps.
    import types
    rc = _cmd_record(types.SimpleNamespace(pr=number, task=args.task),
                     project_root)
    if rc != 0:
        return rc
    msg = (f"[agile] Record PR #{number} on task\n\n"
           f"Story-ID: {story_id}\n"
           f"Task-ID: {task_id}")
    # add first: a just-scaffolded task doc is untracked, and a commit
    # pathspec alone does not pick those up. Check the staged diff
    # rather than parsing git's commit message, which varies with the
    # state of the rest of the tree ("nothing to commit" vs "no
    # changes added to commit").
    subprocess.run(["git", "add", "--", str(task_rel)],
                   cwd=str(project_root))
    staged = subprocess.run(
        ["git", "diff", "--cached", "--quiet", "--", str(task_rel)],
        cwd=str(project_root)).returncode != 0
    if staged:
        p = subprocess.run(
            ["git", "commit", "-m", msg, "--", str(task_rel)],
            capture_output=True, text=True, cwd=str(project_root))
        if p.returncode != 0:
            print(p.stderr.strip() or p.stdout.strip(), file=sys.stderr)
            return p.returncode
        p = subprocess.run(["git", "push"], cwd=str(project_root))
        if p.returncode != 0:
            return p.returncode
        print(f"✅ PR record committed and pushed ({task_rel}).")
    compass = Path(project_root) / "projects" / "ores.compass" / "compass.sh"
    subprocess.run([str(compass), "journal", "update",
                    "--story", story_id, "--task", task_id,
                    "--branch", branch, "--state", "STARTED",
                    "--pr", str(number)], cwd=str(project_root))
    return 0


def _cmd_merge(args, project_root):
    if args.pr is not None and args.pr <= 0:
        print("❌ PR number must be a positive integer.", file=sys.stderr)
        return 1
    # Resolve the PR number (explicit or the current branch's).
    sel = [str(args.pr)] if args.pr else []
    p = subprocess.run(
        ["gh", "pr", "view", *sel, "--json", "number,title,state"],
        capture_output=True, text=True, cwd=str(project_root))
    if p.returncode != 0:
        print(p.stderr.strip() or "❌ gh pr view failed.", file=sys.stderr)
        return p.returncode
    pr = json.loads(p.stdout)
    number, title, state = pr["number"], pr["title"], pr["state"]
    if state != "OPEN":
        print(f"❌ PR #{number} is {state}, not open.", file=sys.stderr)
        return 1
    print(f"🔀 Merging PR #{number}: {title}", flush=True)

    # Resolve the head branch and task doc early — task_result_empty must
    # be computed before the blocked/force gate so it can join blocked[].
    head_proc = subprocess.run(
        ["gh", "pr", "view", str(number), "--json", "headRefName",
         "--jq", ".headRefName"],
        capture_output=True, text=True, cwd=str(project_root))
    head = head_proc.stdout.strip() if head_proc.returncode == 0 else ""

    # Merge never touches task state. Bookkeeping (task done, Result,
    # story/sprint sync) ends the review round and rides the PR via
    # 'compass task done' — see the work-lifecycle story. A task that
    # is still STARTED here gets a warning, not a close-out: tasks may
    # legitimately span several PRs, and a scaffolding PR must never
    # close the task whose deliverable it does not contain.
    task_id = ""
    task_state = ""
    task_result_empty = False
    if head:
        task_path, task_text = _find_task_doc(project_root, head, "")
        if task_path is not None:
            task_id = _org_id(task_text)
            m = re.search(r"^\| State\s*\|\s*([A-Z]+)", task_text, re.M)
            task_state = m.group(1) if m else ""
            # Guard: * Result section must have substantive prose.
            result_m = re.search(
                r"^\* Result\s*$(.*?)(?=^\* |\Z)",
                task_text, re.M | re.S)
            if result_m:
                result_body = result_m.group(1).strip()
                task_result_empty = not result_body

    # Guard rails: unresolved review threads, CI state, empty Result.
    import compass_review
    owner, repo = compass_review._default_owner_repo(project_root)
    blocked = []
    rc, threads = compass_review._fetch_unresolved_threads(
        owner, repo, number)
    if rc != 0:
        return rc
    if threads:
        blocked.append(f"{len(threads)} unresolved review thread(s) — "
                       f"compass review list {number}")
    ci = subprocess.run(["gh", "pr", "checks", str(number)],
                        capture_output=True, text=True,
                        cwd=str(project_root))
    if ci.returncode != 0:
        blocked.append(f"CI is not green — compass pr checks {number}")
    if task_id and task_result_empty:
        blocked.append(
            f"task * Result section is empty — write the result before "
            f"merging (task done stamps it): compass task done {task_id}"
        )

    if task_id and task_state and task_state != "DONE":
        print(f"⚠️  Task on this branch is {task_state}, not DONE — if "
              f"this PR delivers it, close it first: compass task done "
              f"{task_id}", file=sys.stderr)

    if blocked and not args.force:
        print("❌ Refusing to merge:", file=sys.stderr)
        for b in blocked:
            print(f"   - {b}", file=sys.stderr)
        print("   Use --force to merge anyway.", file=sys.stderr)
        return 1
    if args.force and blocked:
        print("⚠️  --force: merging despite:", flush=True)
        for b in blocked:
            print(f"   - {b}", flush=True)
    elif args.force:
        print("⚠️  --force: admin merge (bypassing branch protection).",
              flush=True)

    # Always a merge commit — never squash, never rebase — matching
    # the repository's history. gh's --admin bypasses the base branch
    # protection whenever --force was given (a human taking
    # responsibility for whatever the guard reported). No gh
    # --delete-branch: it checks out the default
    # branch locally, which fails in a multi-worktree fleet where main
    # lives in another worktree. Delete the remote branch directly
    # instead. GitHub's merge API intermittently rejects with "Base
    # branch was modified" right after a push (a GraphQL race) — retry
    # a few times before giving up.
    cmd = ["gh", "pr", "merge", str(number), "--merge"]
    if args.force:
        cmd.append("--admin")
    for attempt in range(3):
        p = subprocess.run(cmd, capture_output=True, text=True,
                           cwd=str(project_root))
        if p.returncode == 0:
            break
        err = (p.stdout + p.stderr)
        if "Base branch was modified" in err and attempt < 2:
            print("ℹ️  GitHub reports the base branch was modified "
                  "(transient race) — retrying…", flush=True)
            time.sleep(3)
            continue
        print(err.strip(), file=sys.stderr)
        return p.returncode
    deleted = False
    if head and head not in ("main", "master"):
        deleted = subprocess.run(
            ["git", "push", "origin", "--delete", head],
            cwd=str(project_root)).returncode == 0
    if deleted:
        print(f"✅ PR #{number} merged (merge commit); remote branch "
              f"{head} deleted.")
    else:
        print(f"✅ PR #{number} merged (merge commit).")
        print(f"⚠️  Remote branch deletion skipped or failed"
              f"{f' ({head})' if head else ''} — delete it manually if "
              "needed.", file=sys.stderr)

    if task_id and task_state and task_state != "DONE":
        print(f"ℹ️  Task is still {task_state} — when its work is "
              f"complete, close it on the next PR: compass task done "
              f"{task_id}")
    elif task_id and task_result_empty:
        print(f"ℹ️  Task * Result section is still empty — write the "
              f"result prose before closing: compass task done {task_id}")
    elif not task_id:
        print("ℹ️  No task doc matches the merged branch.")
    return 0


def _cmd_sync(args, project_root):
    branch = _current_branch(project_root)
    if not branch:
        print("❌ Detached HEAD — check out a branch first.",
              file=sys.stderr)
        return 1
    if branch == "main":
        print("❌ Already on main — use: git pull --ff-only origin main",
              file=sys.stderr)
        return 1

    print(f"🔄 Syncing {branch} with origin/main…", flush=True)
    p = subprocess.run(["git", "fetch", "origin", "main"],
                       cwd=str(project_root))
    if p.returncode != 0:
        return p.returncode

    p = subprocess.run(["git", "rebase", "origin/main"],
                       capture_output=True, text=True,
                       cwd=str(project_root))
    if p.returncode != 0:
        conflicted = subprocess.run(
            ["git", "diff", "--name-only", "--diff-filter=U"],
            capture_output=True, text=True,
            cwd=str(project_root)).stdout.split()
        if not conflicted:
            # Not a conflict stop (dirty tree, etc.) — surface git's
            # own message untouched.
            print(p.stderr.strip() or p.stdout.strip(), file=sys.stderr)
            return p.returncode
        print(f"❌ Rebase stopped on {len(conflicted)} conflicted "
              "file(s):", file=sys.stderr)
        for f in conflicted:
            print(f"   - {f}", file=sys.stderr)
        if args.abort_on_conflict:
            subprocess.run(["git", "rebase", "--abort"],
                           cwd=str(project_root))
            print("↩️  Rebase aborted; branch restored. Sync needs a "
                  "human.", file=sys.stderr)
            return 1
        print("   Resolve each file, then:  git add <file> && "
              "git rebase --continue", file=sys.stderr)
        print("   Or roll back with:        git rebase --abort",
              file=sys.stderr)
        return 1

    msg = (p.stdout.strip() or p.stderr.strip()).splitlines()
    if msg:
        print(msg[-1])
    ahead = subprocess.run(
        ["git", "rev-list", "--count", "origin/main..HEAD"],
        capture_output=True, text=True,
        cwd=str(project_root)).stdout.strip()
    print(f"✅ {branch}: {ahead} commit(s) ahead of origin/main.")

    if args.push:
        p = subprocess.run(
            ["git", "push", "--force-with-lease", "-u", "origin", branch],
            cwd=str(project_root))
        if p.returncode != 0:
            return p.returncode
        print(f"🔼 Pushed {branch} (force-with-lease).")
    return 0


def run(argv, project_root):
    """Entry point: compass pr <subcommand>."""
    ap = argparse.ArgumentParser(
        prog="compass pr",
        description="PR pillar: pull-request lifecycle verbs wrapping "
                    "the gh CLI.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    cp = sub.add_parser("checks", help="CI status for a PR")
    cp.add_argument("pr", nargs="?", default="",
                    help="PR number (default: the current branch's PR)")
    cp.add_argument("--watch", action="store_true",
                    help="Poll until all checks settle")
    cp.add_argument("--fail-fast", action="store_true",
                    help="With --watch, exit on the first failure")
    cp.add_argument("--required", action="store_true",
                    help="Only show checks marked required")
    cp.add_argument("--interval", type=int, default=None,
                    help="Polling interval in seconds for --watch "
                         "(default: gh's 10)")

    rp = sub.add_parser("record",
                        help="Record a PR on its task doc (#+pr: and the "
                             "* PRs table)")
    rp.add_argument("pr", nargs="?", type=int,
                    help="PR number (default: the current branch's PR)")
    rp.add_argument("--task", default="",
                    help="Task UUID or slug (default: match #+branch: "
                         "against the current branch)")

    cr = sub.add_parser("create",
                        help="Open a PR with the conventions built in")
    cr.add_argument("--title", required=True,
                    help="PR title: '[component] Description'")
    cr.add_argument("--summary", default="",
                    help="Summary paragraph for the body")
    cr.add_argument("--change", action="append", default=[],
                    help="A Changes bullet (repeatable)")
    cr.add_argument("--task", default="",
                    help="Task UUID or slug (default: match #+branch: "
                         "against the current branch)")
    cr.add_argument("--draft", action="store_true",
                    help="Open as a draft PR")

    mp = sub.add_parser("merge",
                        help="Merge a PR (merge commit) with guard rails")
    mp.add_argument("pr", nargs="?", type=int,
                    help="PR number (default: the current branch's PR)")
    mp.add_argument("--force", action="store_true",
                    help="Merge even with unresolved threads, red CI, or "
                         "blocking branch protection (admin merge; states "
                         "what was bypassed)")

    sp = sub.add_parser("sync",
                        help="Fetch origin/main and rebase the current "
                             "branch onto it")
    sp.add_argument("--push", action="store_true",
                    help="Force-push (with lease) after a successful "
                         "rebase")
    sp.add_argument("--abort-on-conflict", action="store_true",
                    help="Roll back automatically when the rebase "
                         "conflicts (for unattended use)")

    args = ap.parse_args(argv)
    if args.subcmd == "checks" and args.interval is not None:
        if args.interval < 0:
            ap.error("argument --interval: must be a non-negative integer")
        if not args.watch:
            ap.error("argument --interval: only allowed with --watch")
    if args.subcmd == "checks":
        return _cmd_checks(args, project_root)
    if args.subcmd == "record":
        return _cmd_record(args, project_root)
    if args.subcmd == "create":
        return _cmd_create(args, project_root)
    if args.subcmd == "merge":
        return _cmd_merge(args, project_root)
    if args.subcmd == "sync":
        return _cmd_sync(args, project_root)
    return 1
