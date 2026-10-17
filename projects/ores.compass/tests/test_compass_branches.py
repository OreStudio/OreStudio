"""
Tests for the compass branches pruner.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_branches.py -v
Builds throwaway repositories in a temp directory; no network and no database.
"""

import subprocess
import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_branches  # noqa: E402

GIT_IDENTITY = [
    "-c", "user.name=Test",
    "-c", "user.email=test@example.test",
    "-c", "commit.gpgsign=false",
]


def git(cwd, *args):
    p = subprocess.run(["git"] + GIT_IDENTITY + list(args), cwd=str(cwd),
                       capture_output=True, text=True)
    assert p.returncode == 0, (args, p.stdout, p.stderr)
    return p.stdout


def commit(cwd, name, message):
    (Path(cwd) / name).write_text(message + "\n")
    git(cwd, "add", name)
    git(cwd, "commit", "-m", message)


@pytest.fixture
def repos(tmp_path):
    """An origin holding two commits, and a clone that has both.

    Returns (origin_bare, clone).
    """
    bare = tmp_path / "origin.git"
    subprocess.run(["git", "init", "--bare", "-b", "main", str(bare)],
                   capture_output=True, text=True, check=True)

    clone = tmp_path / "clone"
    subprocess.run(["git", "clone", str(bare), str(clone)],
                   capture_output=True, text=True, check=True)
    commit(clone, "first.txt", "first")
    first = git(clone, "rev-parse", "HEAD").strip()
    commit(clone, "second.txt", "second")
    git(clone, "push", "origin", "main")
    return bare, clone, first


def merged_branch(cwd, name="merged-into-main"):
    """A branch that main holds and that has no upstream.

    No upstream is the shape that reproduces the defect: `git branch -d`
    consults a branch's upstream when it has one, so only an upstream-less
    branch falls back to HEAD -- and HEAD is what the old code asked.
    """
    git(cwd, "branch", name, "origin/main")
    git(cwd, "branch", "--unset-upstream", name)


def branches(cwd):
    return {
        line.strip().lstrip("* ").strip()
        for line in git(cwd, "branch", "--format=%(refname:short)").splitlines()
        if line.strip()
    }


def test_a_branch_main_holds_is_pruned_even_when_head_is_older(repos):
    """The reported defect: the delete asked HEAD, so a merged branch survived.

    The checked-out branch here is behind main, so it does not contain the
    branch being pruned -- exactly the shape that made `git branch -d` refuse.
    """
    _bare, clone, first = repos
    merged_branch(clone)
    git(clone, "checkout", "-b", "older-work", first)

    assert compass_branches.cmd_prune(clone, confirmed=True) == 0

    assert "merged-into-main" not in branches(clone)
    assert "older-work" in branches(clone)


def test_a_branch_main_lacks_is_never_pruned(repos):
    _bare, clone, _first = repos
    git(clone, "checkout", "-b", "unmerged-work")
    commit(clone, "local-only.txt", "local only")

    compass_branches.cmd_prune(clone, confirmed=True)

    assert "unmerged-work" in branches(clone)


def test_the_checked_out_branch_is_never_pruned(repos):
    _bare, clone, _first = repos
    git(clone, "checkout", "-b", "checked-out")
    merged_branch(clone)

    compass_branches.cmd_prune(clone, confirmed=True)

    assert "checked-out" in branches(clone)
    assert "merged-into-main" not in branches(clone)


def test_a_dry_run_deletes_nothing(repos):
    _bare, clone, _first = repos
    merged_branch(clone)

    assert compass_branches.cmd_prune(clone, confirmed=False) == 0

    assert "merged-into-main" in branches(clone)


def test_the_safe_set_is_decided_against_main_not_head(repos):
    """classify() reports the branch, and the check agrees it is main's."""
    _bare, clone, first = repos
    merged_branch(clone)
    git(clone, "checkout", "-b", "older-work", first)

    assert "merged-into-main" in compass_branches.classify(clone)["delete_local"]
    assert compass_branches.is_merged_into_main(clone, "merged-into-main")


def test_an_unmerged_branch_is_not_main_s(repos):
    _bare, clone, _first = repos
    git(clone, "checkout", "-b", "unmerged-work")
    commit(clone, "local-only.txt", "local only")

    assert not compass_branches.is_merged_into_main(clone, "unmerged-work")
