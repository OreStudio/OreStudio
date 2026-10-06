#!/usr/bin/env python3
"""Apply one codegen change to every generated file, and nothing else.

Regenerating a component writes everything its templates say, so a component
whose generated files already drift from their templates would take that
drift along with the change. This applies only the change:

  1. render the whole catalogue with the codegen of BASE (a git revision,
     checked out into a temporary worktree);
  2. render it again with the working tree's codegen;
  3. for every file the two renders disagree on, three-way merge the
     difference into the checked-in file with `git merge-file`, using the
     BASE render as the common ancestor.

A file that matches its BASE render merges to the new render exactly. A file
that already drifts keeps its drift and gains only the change. A conflict is
left with markers and reported, for a person to resolve. A file only the new
render has is written as rendered. A file both renders have but the tree lacks
is a gap that predates the change, and is left for its component's clean-up.

--exclude names a component whose generated files are left untouched, because
they drift too far from their templates to take the change by merge; its own
clean-up regenerates them.

Run from the repository root, with the codegen venv:

    projects/ores.codegen/venv/bin/python3 <this file> --base origin/main
    projects/ores.codegen/venv/bin/python3 <this file> --base origin/main --dry-run
"""
import argparse
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path.cwd()
RENDER = """
import sys
from pathlib import Path
sys.path.insert(0, "projects/ores.codegen/scripts")
sys.path.insert(0, "projects/ores.codegen/src")
import check_component_drift as d
root = Path(sys.argv[1])
d._seed_clang_format(root)
sys.exit(d._render_components(d._catalogue_components(), "ores", root, whole_address=True))
"""


def render(tree: Path, out: Path) -> None:
    python = REPO / "projects/ores.codegen/venv/bin/python3"
    result = subprocess.run([str(python), "-c", RENDER, str(out)], cwd=tree,
                            capture_output=True, text=True)
    if result.returncode != 0:
        sys.stderr.write(result.stdout[-4000:] + result.stderr[-4000:])
        raise SystemExit(f"rendering failed in {tree}")


def rendered_files(root: Path) -> dict:
    return {p.relative_to(root): p for p in root.rglob("*")
            if p.is_file() and p.name != ".clang-format"}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base", required=True)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--exclude", action="append", default=[],
                        help="a component, e.g. ores.synthetic, to leave untouched")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix="ores-merge-") as tmp:
        tmp = Path(tmp)
        worktree = tmp / "base"
        subprocess.run(["git", "worktree", "add", "--detach", str(worktree), args.base],
                       check=True, capture_output=True)
        try:
            # The base tree renders the working tree's models, so the only
            # difference between the two renders is the codegen.
            subprocess.run(["rsync", "-a", "--delete", "--include=*/",
                            "--include=*.org", "--exclude=*",
                            "projects/", str(worktree / "projects") + "/"],
                           check=True)
            subprocess.run(["git", "-C", str(worktree), "checkout", args.base, "--",
                            "projects/ores.codegen"], check=True, capture_output=True)
            old, new = tmp / "old", tmp / "new"
            old.mkdir()
            new.mkdir()
            render(worktree, old)
            render(REPO, new)
        finally:
            subprocess.run(["git", "worktree", "remove", "--force", str(worktree)],
                           capture_output=True)

        old_files, new_files = rendered_files(old), rendered_files(new)
        def excluded(rel):
            text = str(rel)
            return any(text.startswith(f"projects/{c}/") or
                       f"/generated/{c.removeprefix('ores.')}/" in text
                       for c in args.exclude)

        changed = sorted(rel for rel, p in new_files.items()
                         if not excluded(rel) and
                         (rel not in old_files or old_files[rel].read_bytes() != p.read_bytes()))
        clean, conflicted, created = [], [], []
        for rel in changed:
            target = REPO / rel
            if not target.exists():
                if rel in old_files:
                    continue
                created.append(rel)
                if not args.dry_run:
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(new_files[rel].read_bytes())
                continue
            ours = target if not args.dry_run else tmp / "ours"
            if args.dry_run:
                ours.write_bytes(target.read_bytes())
            result = subprocess.run(
                ["git", "merge-file", "-L", "checked-in", "-L", "base render",
                 "-L", "new render", str(ours), str(old_files[rel]), str(new_files[rel])],
                capture_output=True)
            (conflicted if result.returncode != 0 else clean).append(rel)

        print(f"{len(changed)} generated file(s) change: {len(clean)} merged cleanly, "
              f"{len(conflicted)} conflicted, {len(created)} new")
        for rel in conflicted:
            print(f"conflict: {rel}")
        for rel in created:
            print(f"new: {rel}")
        return 1 if conflicted else 0


if __name__ == "__main__":
    sys.exit(main())
