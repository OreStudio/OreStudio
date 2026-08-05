# -*- coding: utf-8 -*-
"""compass branches -- Orient/Operate pillar: git branch hygiene.

Formalises the manual git-branch cleanup workflow into a compass
command: report which local/remote branches are safe to delete (fully
merged into origin/main, not the current branch, not main, not
checked out by another fleet worktree), and prune them.

    compass branches            ->  report (default, read-only)
    compass branches report     ->  same as above, explicit
    compass branches prune      ->  dry-run: show what WOULD be deleted
    compass branches prune -y   ->  actually delete the safe set,
                                     local then remote
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

from ui import GREEN, YELLOW, RED, CYAN, BOLD, RESET

_MAIN_BRANCHES = {"main", "master"}


def _git(*args, cwd, timeout=20):
    """Run a git command; return stripped stdout, or None on failure."""
    try:
        p = subprocess.run(["git"] + list(args), capture_output=True, text=True,
                            cwd=str(cwd), timeout=timeout)
        return p.stdout.strip() if p.returncode == 0 else None
    except (OSError, subprocess.SubprocessError):
        return None


def _lines(out):
    return [l.strip() for l in (out or "").splitlines() if l.strip()]


def fetch_prune(project_root):
    """git fetch --prune origin, so deleted remotes don't show as phantoms."""
    return _git("fetch", "--prune", "origin", cwd=project_root, timeout=60) is not None


def current_branch(project_root):
    return _git("symbolic-ref", "--short", "HEAD", cwd=project_root)


def local_branches(project_root):
    out = _git("for-each-ref", "refs/heads", "--format=%(refname:short)", cwd=project_root)
    return _lines(out)


def remote_branches(project_root):
    out = _git("for-each-ref", "refs/remotes/origin", "--format=%(refname)", cwd=project_root)
    names = []
    for l in _lines(out):
        if l.endswith("/HEAD"):
            continue
        names.append(re.sub(r"^refs/remotes/origin/", "", l))
    return names


def merged_local_branches(project_root):
    out = _git("branch", "--merged", "origin/main", "--format=%(refname:short)", cwd=project_root)
    return set(_lines(out))


def merged_remote_branches(project_root):
    out = _git("branch", "-r", "--merged", "origin/main", "--format=%(refname)", cwd=project_root)
    names = set()
    for l in _lines(out):
        if l.endswith("/HEAD"):
            continue
        names.add(re.sub(r"^refs/remotes/origin/", "", l))
    return names


def fleet_active_branches(project_root):
    """Map branch -> worktree name, for every checked-out worktree (any branch)."""
    try:
        out = subprocess.run(["git", "worktree", "list", "--porcelain"],
                              capture_output=True, text=True, cwd=str(project_root), timeout=15)
    except (OSError, subprocess.SubprocessError):
        return {}
    if out.returncode != 0:
        return {}
    active, path = {}, None
    for line in out.stdout.splitlines():
        if line.startswith("worktree "):
            path = line[len("worktree "):]
        elif line.startswith("branch "):
            branch = re.sub(r"^refs/heads/", "", line[len("branch "):])
            active[branch] = Path(path).name if path else None
    return active


def classify(project_root):
    """Bucket every local/remote branch into safe-to-delete vs. reasons kept.

    Returns a dict:
      delete_local, delete_remote  - names safe to delete
      kept - list of (name, side, reason) for everything else, side in
             {"local", "remote"}, reason one of "current", "main",
             "fleet:<worktree>", "unmerged"
    """
    cur = current_branch(project_root)
    fleet = fleet_active_branches(project_root)
    merged_local = merged_local_branches(project_root)
    merged_remote = merged_remote_branches(project_root)

    delete_local, delete_remote, kept = [], [], []

    for name in local_branches(project_root):
        if name in _MAIN_BRANCHES:
            kept.append((name, "local", "main"))
        elif name == cur:
            kept.append((name, "local", "current"))
        elif name in fleet:
            kept.append((name, "local", f"fleet:{fleet[name]}"))
        elif name in merged_local:
            delete_local.append(name)
        else:
            kept.append((name, "local", "unmerged"))

    for name in remote_branches(project_root):
        if name in _MAIN_BRANCHES:
            kept.append((name, "remote", "main"))
        elif name == cur:
            kept.append((name, "remote", "current"))
        elif name in fleet:
            kept.append((name, "remote", f"fleet:{fleet[name]}"))
        elif name in merged_remote:
            delete_remote.append(name)
        else:
            kept.append((name, "remote", "unmerged"))

    return {"delete_local": delete_local, "delete_remote": delete_remote, "kept": kept}


_REASON_LABEL = {
    "current": "is the current branch",
    "main": "is main",
    "unmerged": "unmerged/active work",
}


def _reason_label(reason):
    if reason.startswith("fleet:"):
        return f"checked out by {reason.split(':', 1)[1]}"
    return _REASON_LABEL.get(reason, reason)


def print_report(result):
    n_del = len(result["delete_local"]) + len(result["delete_remote"])
    print(f"{BOLD}{CYAN}🌿  ores.compass — branches ({n_del} safe to prune){RESET}\n")

    if result["delete_local"] or result["delete_remote"]:
        print(f"{GREEN}Safe to delete (merged into origin/main):{RESET}")
        for name in result["delete_local"]:
            print(f"  local   {name}")
        for name in result["delete_remote"]:
            print(f"  remote  {name}")
        print()
    else:
        print(f"{GREEN}Nothing to prune.{RESET}\n")

    if result["kept"]:
        print(f"{YELLOW}Kept:{RESET}")
        for name, side, reason in sorted(result["kept"], key=lambda r: (r[2], r[1], r[0])):
            print(f"  {side:<6}  {name:<50}  {_reason_label(reason)}")
        print()

    print(f"{_ycmd('compass branches prune')}  to delete the safe set "
          f"(dry-run; add -y to actually delete)")


def _ycmd(cmd):
    return f"{YELLOW}{cmd}{RESET}"


def cmd_report(project_root):
    if not fetch_prune(project_root):
        print(f"{RED}⚠️  git fetch --prune failed; report may be stale.{RESET}", file=sys.stderr)
    result = classify(project_root)
    print_report(result)
    return 0


def cmd_prune(project_root, confirmed):
    if not fetch_prune(project_root):
        print(f"{RED}⚠️  git fetch --prune failed; results may be stale.{RESET}", file=sys.stderr)
    result = classify(project_root)
    to_local = result["delete_local"]
    to_remote = result["delete_remote"]

    if not to_local and not to_remote:
        print(f"{GREEN}✅ Nothing to prune -- no fully-merged, fleet-free branches found.{RESET}")
        return 0

    if not confirmed:
        print(f"{YELLOW}Dry run -- would delete:{RESET}")
        for name in to_local:
            print(f"  local   {name}")
        for name in to_remote:
            print(f"  remote  {name}")
        print(f"\nRe-run with {_ycmd('-y')} to actually delete.")
        return 0

    # `git branch -d`/`git push origin --delete` process each ref
    # independently and return non-zero if ANY one fails, even when the
    # rest succeeded -- so the deleted set is computed from the actual
    # before/after branch list rather than trusted from the return code.
    deleted_local, deleted_remote = [], []
    if to_local:
        p = subprocess.run(["git", "branch", "-d"] + to_local, cwd=str(project_root))
        still_present = set(local_branches(project_root))
        deleted_local = [name for name in to_local if name not in still_present]
        if p.returncode != 0:
            print(f"{RED}⚠️  some local branch deletions failed (may have "
                  f"unmerged commits despite the --merged check).{RESET}", file=sys.stderr)

    if to_remote:
        p = subprocess.run(["git", "push", "origin", "--delete"] + to_remote, cwd=str(project_root))
        still_present = set(remote_branches(project_root))
        deleted_remote = [name for name in to_remote if name not in still_present]
        if p.returncode != 0:
            print(f"{RED}⚠️  some remote branch deletions failed.{RESET}", file=sys.stderr)

    print(f"\n{GREEN}✅ Deleted {len(deleted_local)} local, "
          f"{len(deleted_remote)} remote branch(es).{RESET}")

    kept = classify(project_root)["kept"]
    if kept:
        print(f"\n{YELLOW}Left alone:{RESET}")
        for name, side, reason in sorted(kept, key=lambda r: (r[2], r[1], r[0])):
            print(f"  {side:<6}  {name:<50}  {_reason_label(reason)}")

    return 0


def run(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass branches",
        description="Orient: report and prune git branches fully merged into origin/main.")
    sub = ap.add_subparsers(dest="subcmd")
    sub.add_parser("report", help="List branches safe to prune and everything kept, with reasons "
                                  "(default when no subcommand given)")
    pp = sub.add_parser("prune", help="Delete the safe-to-delete set, local then remote "
                                      "(dry-run unless -y is given)")
    pp.add_argument("-y", "--yes", action="store_true",
                     help="Actually delete; without this, only prints what would be deleted")

    args = ap.parse_args(argv)
    subcmd = args.subcmd or "report"
    if subcmd == "prune":
        return cmd_prune(project_root, args.yes)
    return cmd_report(project_root)
