#!/usr/bin/env python3
"""Move every skill this project owns into the compass- namespace.

The prefix separates our skills from every plugin skill the harness also
loads. A rename touches more than the directory: the name field the harness
reads, every id-link label and prose mention across the repository, the
generated catalogue, the runbooks, the memories, CLAUDE.md, and the deployed
copy, which deployment never removes.

Doing that by hand is how a reference gets missed, so it is a script: a
reviewer can re-run it, and the next namespace change is cheap.

Idempotent. A skill already carrying the prefix is left alone, so a second
run is a no-op and reports nothing to do.

Usage: python3 build/scripts/rename_skills_to_compass_namespace.py [--check]
                                                                  [--dry-run]
  --check    exit non-zero if any skill still lacks the prefix (for CI).
  --dry-run  report what would change, write nothing.
"""
import argparse
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SKILLS = ROOT / "doc" / "llm" / "skills"
DEPLOYED = ROOT / ".claude" / "skills"
PREFIX = "compass-"

# Where a skill name can appear as prose or as a link label. The deployed
# tree is rebuilt from source, so it is purged rather than rewritten.
TEXT_SUFFIXES = {".org", ".md", ".py", ".json", ".yml", ".yaml", ".el"}
# Skipped wholesale: version control, vendored trees, and the deployed copy,
# which is rebuilt from source. build/ holds 40G of output alongside the
# scripts that name skills, so it is pruned by subdirectory rather than
# whole.
SKIP_DIRS = {".git", "vcpkg", ".packages", "node_modules", ".claude"}
SKIP_PREFIXES = {
    ("build", "output"), ("build", "docker-stage"), ("build", "test_tmp"),
    ("build", "doxygen"), ("build", "cpack"), ("build", "valgrind"),
    ("build", "quadlet-log"), ("build", "quadlet-run"),
}


def skill_dirs():
    return sorted(d for d in SKILLS.iterdir()
                  if d.is_dir() and (d / "SKILL.org").is_file())


def to_rename():
    return [d for d in skill_dirs() if not d.name.startswith(PREFIX)]


def text_files():
    for path in ROOT.rglob("*"):
        if not path.is_file() or path.suffix not in TEXT_SUFFIXES:
            continue
        rel = path.relative_to(ROOT)
        if rel.parts and rel.parts[0] in SKIP_DIRS:
            continue
        if rel.parts[:2] in SKIP_PREFIXES:
            continue
        yield path


def rewrite_mentions(names, dry_run):
    """Rewrite every mention of an old name. Longest first, so renaming a
    name never eats a longer name that contains it."""
    ordered = sorted(names, key=len, reverse=True)
    pattern = re.compile(
        r"(?<![\w-])(" + "|".join(re.escape(n) for n in ordered) + r")(?![\w-])")
    touched = 0
    for path in text_files():
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        updated = pattern.sub(lambda m: PREFIX + m.group(1), text)
        if updated == text:
            continue
        touched += 1
        if not dry_run:
            path.write_text(updated, encoding="utf-8")
    return touched


def git_mv(src, dst, dry_run):
    if dry_run:
        return
    subprocess.run(["git", "mv", str(src), str(dst)], cwd=ROOT, check=True)


def purge_stale_deployed(dry_run):
    """Deployment writes but never deletes, so a directory whose source is
    gone would keep loading under its old name."""
    if not DEPLOYED.is_dir():
        return []
    live = {d.name for d in skill_dirs()}
    stale = [d for d in DEPLOYED.iterdir() if d.is_dir() and d.name not in live]
    if not dry_run:
        for d in stale:
            subprocess.run(["rm", "-rf", str(d)], check=True)
    return [d.name for d in stale]


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="exit non-zero if any skill still lacks the prefix")
    ap.add_argument("--dry-run", action="store_true",
                    help="report what would change, write nothing")
    args = ap.parse_args()

    pending = to_rename()

    if args.check:
        if pending:
            print(f"❌ {len(pending)} skill(s) outside the {PREFIX} namespace:",
                  file=sys.stderr)
            for d in pending:
                print(f"  {d.name}", file=sys.stderr)
            print("   run: python3 build/scripts/"
                  "rename_skills_to_compass_namespace.py", file=sys.stderr)
            return 1
        print(f"✅ every skill is in the {PREFIX} namespace.")
        return 0

    if not pending:
        print(f"✅ nothing to do: every skill is in the {PREFIX} namespace.")
        stale = purge_stale_deployed(args.dry_run)
        if stale:
            print(f"🧹 purged {len(stale)} stale deployed directory(ies).")
        return 0

    names = [d.name for d in pending]
    print(f"🔤 renaming {len(names)} skill(s) into {PREFIX}")

    touched = rewrite_mentions(names, args.dry_run)
    print(f"   {touched} file(s) mention them")

    for d in pending:
        target = d.parent / (PREFIX + d.name)
        print(f"   {d.name} → {target.name}")
        git_mv(d, target, args.dry_run)

    stale = purge_stale_deployed(args.dry_run)
    if stale:
        print(f"🧹 purged {len(stale)} stale deployed directory(ies).")

    if args.dry_run:
        print("   dry run: nothing written.")
        return 0

    print("\nNext: regenerate the catalogue and the type tables, then rebuild "
          "the bundle.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
