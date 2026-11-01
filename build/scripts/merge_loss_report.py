#!/usr/bin/env python3
"""Which files a suspect merge dropped, and whether they are still missing.

A merge can take an old tree over the newer one: the result compiles, the suites
that matter to the new files pass, and a hundred files disappear into a diff
nobody reads. This reports the three answers a reader needs, for any two trees:

  - paths the baseline had that the result does not have at all;
  - paths whose content differs and that no commit since the suspect commit has
    touched, so the suspect commit is the only thing that could have changed them;
  - paths whose content differs and that a later commit did touch, which still
    need a look because a later commit may have restored only part of what the
    suspect commit removed.

Usage:
  merge_loss_report.py <suspect-commit> [<baseline-ref>] [<result-ref>]
                       [--expected <path> ...]

A path passed with --expected is the suspect commit's own intended work, which is
supposed to differ from the baseline; it moves to its own list rather than
counting as a loss.

With one argument the baseline is the suspect commit's first parent, the result is
origin/main, and the range walked for later commits is the suspect commit to
origin/main. Override them to describe an older incident.

Examples:
  merge_loss_report.py 9e500a223f
  merge_loss_report.py 1398383538 6b28050821 origin/main

Exits 0 when nothing is missing, 1 when paths are gone or stale, so it can be run
as a gate for one incident.
"""
import subprocess
import sys


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True, check=True).stdout


def tree(ref):
    out = {}
    for line in git("ls-tree", "-r", ref).splitlines():
        meta, path = line.split("\t", 1)
        out[path] = meta.split()[2]
    return out


def touched_since(range_spec):
    touched = set()
    for commit in git("log", "--format=%H", range_spec).split():
        for path in git("diff-tree", "--no-commit-id", "--name-only", "-r", commit).splitlines():
            touched.add(path)
    return touched


def main(argv):
    expected = set()
    while "--expected" in argv:
        index = argv.index("--expected")
        expected.add(argv[index + 1])
        del argv[index:index + 2]

    if not argv:
        print(__doc__)
        return 2
    suspect = argv[0]
    baseline = argv[1] if len(argv) > 1 else f"{suspect}^1"
    result = argv[2] if len(argv) > 2 else "origin/main"
    range_spec = f"{suspect}..{result}"

    before = tree(baseline)
    after = tree(result)
    touched = touched_since(range_spec)

    deleted = sorted(path for path in before if path not in after)
    stale = sorted(
        path
        for path in before
        if path in after
        and before[path] != after[path]
        and path not in touched
        and path not in expected
    )
    kept = sorted(
        path
        for path in before
        if path in after and before[path] != after[path] and path in expected
    )
    review = sorted(
        path
        for path in before
        if path in after and before[path] != after[path] and path in touched
    )

    print(f"suspect:  {suspect}")
    print(f"baseline: {baseline} ({len(before)} files)")
    print(f"result:   {result} ({len(after)} files)")
    print()
    print(f"gone and not mentioned since: {len(deleted)}")
    for path in deleted:
        print(f"  {path}")
    print()
    print(f"changed and not mentioned since: {len(stale)}")
    for path in stale:
        print(f"  {path}")
    print()
    print(f"kept from the suspect commit on purpose: {len(kept)}")
    for path in kept:
        print(f"  {path}")
    print()
    print(f"changed and touched later, so read the hunks: {len(review)}")
    for path in review:
        added, removed, _ = git("diff", "--numstat", baseline, result, "--", path).split("\t")
        direction = "lost lines" if added == "0" else "later work"
        print(f"  +{added} -{removed}  {direction}  {path}")

    return 0 if not deleted and not stale else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
