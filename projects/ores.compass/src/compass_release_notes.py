"""compass release-notes — sprint-closure release notes pipeline
(create, charts, export, commit, draft).

Five verbs, run in order at sprint close:

  1. create  — collect merged-PR data and render release_notes.org for a
               closed sprint (wraps build/scripts/collect_release_pr_data.py
               and generate_release_notes.py). Human/LLM then fills in the
               summary-blurb placeholders left in the file.
  2. charts  — render the sprint's gnuplot health charts (delegates to
               `compass sprint charts`) so the org file's [[proj:...]]
               chart images resolve to real files.
  3. export  — batch-export release_notes.org to GitHub-flavoured Markdown
               (projects/ores.lisp/src/ores-build-release-notes.el), with
               proj: links resolved to raw.githubusercontent.com (images)
               / github.com blob URLs (everything else) so the result
               renders correctly as a GitHub release body.
  4. commit  — stage and commit release_notes.org/.md and the four chart
               PNGs, push the current branch. There is no separate GitHub
               "upload" call for the images: the markdown embeds them via
               raw.githubusercontent.com/.../main/... URLs, so they only
               render once these files are committed and merged to main
               (open/merge that PR the normal way — `compass pr create` /
               `compass pr merge`).
  5. draft   — tag main and open a **draft** GitHub release via `gh
               release create --draft --notes-file`. Title follows house
               convention: '<tag>, "<codename>"' (--codename is required —
               a codename is invented per release, e.g. "Capopolo",
               "Otjinhungwa"). Always draft: a human reviews, attaches
               extras, and publishes it live. Idempotent: safe to re-run
               if the tag or release already exists (e.g. created by
               hand) — reuses/updates rather than erroring, including the
               title, so re-running with the right --codename fixes it in
               place without losing the tag/PR/notes work already done.
"""
import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path


def _sprint_dir(project_root, sprint):
    return Path(project_root) / "doc" / "agile" / "versions" / "v0" / f"sprint_{sprint:02d}"


def _cmd_create(args, project_root):
    sprint_dir = _sprint_dir(project_root, args.sprint)
    if not (sprint_dir / "sprint.org").exists():
        print(f"❌ No sprint.org at {sprint_dir}.", file=sys.stderr)
        return 1

    collect_cmd = [sys.executable,
                   str(Path(project_root) / "build" / "scripts" / "collect_release_pr_data.py")]
    if args.since_tag:
        collect_cmd += ["--since-tag", args.since_tag]
    p = subprocess.run(collect_cmd, cwd=str(project_root))
    if p.returncode != 0:
        return p.returncode

    generate_cmd = [sys.executable,
                    str(Path(project_root) / "build" / "scripts" / "generate_release_notes.py"),
                    "--sprint-dir", str(sprint_dir)]
    if args.screenshot_filename:
        generate_cmd += ["--screenshot-filename", args.screenshot_filename]
    p = subprocess.run(generate_cmd, cwd=str(project_root))
    if p.returncode != 0:
        return p.returncode

    print(f"\n✅ Draft release_notes.org ready at {sprint_dir / 'release_notes.org'}.")
    print("   Fill in the two '(( ... ))' summary-blurb placeholders "
          "before running 'compass release-notes charts' / 'export'.")
    return 0


def _cmd_charts(args, project_root):
    import compass
    ns = argparse.Namespace(sprint=args.sprint, start_date=None,
                            end_date=None, output_dir=None,
                            branch=args.branch)
    return compass.cmd_sprint_charts(ns)


def _cmd_export(args, project_root):
    if not shutil.which("emacs"):
        print("❌ emacs not found on PATH — required for the Markdown export.",
              file=sys.stderr)
        return 1
    sprint_dir = _sprint_dir(project_root, args.sprint)
    org_file = sprint_dir / "release_notes.org"
    if not org_file.exists():
        print(f"❌ {org_file} not found — run 'compass release-notes create' first.",
              file=sys.stderr)
        return 1
    rel_dir = sprint_dir.relative_to(project_root).as_posix()
    script = Path(project_root) / "projects" / "ores.lisp" / "src" / "ores-build-release-notes.el"
    p = subprocess.run(
        ["emacs", "-Q", "--script", str(script), "--", rel_dir],
        cwd=str(project_root))
    if p.returncode != 0:
        return p.returncode
    md_file = sprint_dir / "release_notes.md"
    if not md_file.exists():
        print(f"❌ Export reported success but {md_file} is missing.", file=sys.stderr)
        return 1
    print(f"✅ {md_file}")
    return 0


_CHART_FILES = ("prs_commits.png", "line_churn.png", "pr_cycle.png", "stories_done.png")


def _cmd_commit(args, project_root):
    sprint_dir = _sprint_dir(project_root, args.sprint)
    rel_paths = ["release_notes.org", "release_notes.md", *_CHART_FILES]
    existing = [p for p in rel_paths if (sprint_dir / p).exists()]
    missing = [p for p in rel_paths if p not in existing]
    if missing:
        print(f"⚠️  Missing (skipped): {', '.join(missing)}", file=sys.stderr)
    if not existing:
        print("❌ Nothing to commit — run create/charts/export first.", file=sys.stderr)
        return 1

    rel_sprint_dir = sprint_dir.relative_to(project_root)
    paths = [str(rel_sprint_dir / p) for p in existing]

    # release_notes.org may reference a top-of-page screenshot under
    # assets/images/ (house convention: ore_studio-v<tag>.png) — pick it
    # up too so its raw.githubusercontent.com URL resolves once merged.
    org_text = (sprint_dir / "release_notes.org").read_text(encoding="utf-8") \
        if (sprint_dir / "release_notes.org").exists() else ""
    m = re.search(r"\[\[proj:(assets/images/[^]]+)\]\]", org_text)
    if m and (Path(project_root) / m.group(1)).exists():
        paths.append(m.group(1))

    subprocess.run(["git", "add", "--"] + paths, cwd=str(project_root))

    staged = subprocess.run(
        ["git", "diff", "--cached", "--quiet", "--"] + paths,
        cwd=str(project_root)).returncode != 0
    if not staged:
        print("ℹ️  Nothing changed — release notes/charts already committed.")
        return 0

    msg = f"[agile] Sprint {args.sprint} release notes and charts"
    p = subprocess.run(["git", "commit", "-m", msg, "--"] + paths,
                       cwd=str(project_root), capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr.strip() or p.stdout.strip(), file=sys.stderr)
        return p.returncode

    branch = subprocess.run(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"],
        cwd=str(project_root), capture_output=True, text=True).stdout.strip()
    p = subprocess.run(["git", "push", "-u", "origin", branch], cwd=str(project_root))
    if p.returncode != 0:
        return p.returncode

    print(f"✅ Committed and pushed {len(existing)} file(s) on {branch}.")
    print("   Open/merge the PR the normal way (compass pr create / pr merge) "
          "so the images resolve on main before running 'compass release-notes draft'.")
    return 0


def _cmake_project_version(project_root):
    text = (Path(project_root) / "CMakeLists.txt").read_text(encoding="utf-8")
    m = re.search(r"project\(\w+\s+VERSION\s+([\d.]+)", text)
    return m.group(1) if m else None


def _cmd_draft(args, project_root):
    sprint_dir = _sprint_dir(project_root, args.sprint)
    md_file = sprint_dir / "release_notes.md"
    if not md_file.exists():
        print(f"❌ {md_file} not found — run 'compass release-notes export' first.",
              file=sys.stderr)
        return 1

    tag = args.tag
    if not tag:
        version = _cmake_project_version(project_root)
        if not version:
            print("❌ Could not derive a tag from CMakeLists.txt project() VERSION "
                  "— pass --tag explicitly.", file=sys.stderr)
            return 1
        tag = f"v{version}"

    # Tag may already exist locally, on origin, both, or neither — make
    # each leg idempotent rather than assuming a clean-slate run.
    local_tag = subprocess.run(["git", "tag", "-l", tag], cwd=str(project_root),
                               capture_output=True, text=True).stdout.strip()
    remote_tag = subprocess.run(
        ["git", "ls-remote", "--tags", "origin", tag],
        cwd=str(project_root), capture_output=True, text=True).stdout.strip()

    # The main-branch / up-to-date check only matters when we are about to
    # CREATE a new tag — that is the operation that could point at the
    # wrong commit if run from a stale feature branch. Reusing a tag that
    # already exists (locally and/or on origin) needs no branch check at
    # all: gh release create/edit doesn't care what's checked out locally.
    if not local_tag and not remote_tag and not args.force:
        branch = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=str(project_root), capture_output=True, text=True).stdout.strip()
        if branch != "main":
            print(f"❌ Tag {tag} doesn't exist yet — refusing to create it from "
                  f"'{branch}' (checkout main first, or pass --force if you know "
                  "what you're doing).", file=sys.stderr)
            return 1
        local_head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=str(project_root),
                                    capture_output=True, text=True).stdout.strip()
        subprocess.run(["git", "fetch", "origin", "main"], cwd=str(project_root))
        remote_head = subprocess.run(["git", "rev-parse", "origin/main"], cwd=str(project_root),
                                     capture_output=True, text=True).stdout.strip()
        if local_head != remote_head:
            print("❌ Local main is not up to date with origin/main — "
                  "git pull --ff-only first (or pass --force).", file=sys.stderr)
            return 1

    if local_tag:
        print(f"ℹ️  Local tag {tag} already exists — reusing it.")
    elif remote_tag:
        # Exists on origin but not locally: fetch the real object rather
        # than create a fresh one, which could point at a different commit.
        p = subprocess.run(["git", "fetch", "origin", f"refs/tags/{tag}:refs/tags/{tag}"],
                           cwd=str(project_root))
        if p.returncode != 0:
            return p.returncode
        print(f"✅ Fetched existing tag {tag} from origin.")
    else:
        p = subprocess.run(["git", "tag", "-s", tag, "-m", f"Sprint {args.sprint}"],
                           cwd=str(project_root))
        if p.returncode != 0:
            return p.returncode
        print(f"✅ Tagged {tag}.")

    if not remote_tag:
        p = subprocess.run(["git", "push", "origin", tag], cwd=str(project_root))
        if p.returncode != 0:
            return p.returncode
        print(f"✅ Pushed {tag} to origin.")
    else:
        print(f"ℹ️  {tag} already on origin — reusing it.")

    release_exists = subprocess.run(
        ["gh", "release", "view", tag], cwd=str(project_root),
        capture_output=True, text=True).returncode == 0

    # House convention: "v0.0.20, "Capopolo"" — tag, comma, quoted codename
    # (see `gh release list`). A codename is invented per release, so it
    # can't be derived; require it explicitly rather than silently falling
    # back to something like "Sprint N".
    title = f'{tag}, "{args.codename}"'

    if release_exists:
        cmd = ["gh", "release", "edit", tag,
              "--title", title, "--notes-file", str(md_file)]
    else:
        cmd = ["gh", "release", "create", tag, "--draft",
              "--title", title, "--notes-file", str(md_file)]
    p = subprocess.run(cmd, cwd=str(project_root), capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr.strip() or "❌ gh release create/edit failed.", file=sys.stderr)
        return p.returncode
    print(p.stdout.strip())
    if release_exists:
        print(f"✅ Existing release {tag} updated with the latest notes "
              "(its draft/published state was left untouched).")
    else:
        print("✅ Draft release created — a human reviews, attaches extras, and publishes it live.")
    return 0


def run(argv, project_root):
    """Entry point: compass release-notes <subcommand>."""
    ap = argparse.ArgumentParser(
        prog="compass release-notes",
        description="Sprint-closure release notes pipeline: create, charts, "
                    "export, commit, draft.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    cr = sub.add_parser("create",
                        help="Collect merged-PR data and render release_notes.org")
    cr.add_argument("--sprint", type=int, required=True,
                    help="Sprint number to generate release notes for "
                         "(the just-closed sprint, not the current one)")
    cr.add_argument("--since-tag", default="",
                    help="Tag to collect PRs after (default: latest v*)")
    cr.add_argument("--screenshot-filename", default="",
                    help="Filename under assets/images/ to embed at the top "
                         "(house convention: ore_studio-v<tag>.png)")

    ch = sub.add_parser("charts", help="Render the sprint's gnuplot health charts")
    ch.add_argument("--sprint", type=int, required=True)
    ch.add_argument("--branch", default=None,
                    help="Git ref to read commit/merge activity from "
                         "(default: origin/main). Use the current feature "
                         "branch to include not-yet-merged work, e.g. "
                         "--branch HEAD.")

    ex = sub.add_parser("export",
                        help="Export release_notes.org to release_notes.md "
                             "(GitHub-flavoured Markdown)")
    ex.add_argument("--sprint", type=int, required=True)

    co = sub.add_parser("commit",
                        help="Commit and push release_notes.org/.md and the "
                             "chart PNGs on the current branch")
    co.add_argument("--sprint", type=int, required=True)

    dr = sub.add_parser("draft",
                        help="Tag main and open a draft GitHub release from "
                             "release_notes.md")
    dr.add_argument("--sprint", type=int, required=True)
    dr.add_argument("--codename", required=True,
                    help="Release codename, e.g. 'Otjinhungwa' — the release "
                         "title becomes '<tag>, \"<codename>\"' per house "
                         "convention (see `gh release list`)")
    dr.add_argument("--tag", default="",
                    help="Release tag (default: v<CMakeLists project version>)")
    dr.add_argument("--force", action="store_true",
                    help="Skip the main-branch/up-to-date check when creating "
                         "a brand new tag (has no effect when the tag already "
                         "exists — that path never needs the check)")

    args = ap.parse_args(argv)

    if args.subcmd == "create":
        return _cmd_create(args, project_root)
    if args.subcmd == "charts":
        return _cmd_charts(args, project_root)
    if args.subcmd == "export":
        return _cmd_export(args, project_root)
    if args.subcmd == "commit":
        return _cmd_commit(args, project_root)
    if args.subcmd == "draft":
        return _cmd_draft(args, project_root)
    return 1
