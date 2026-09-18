"""
The capture commit must stage every backlog index the regenerator writes.

Two files hold the bucket list: regenerate_backlog_indexes.py writes one
index per bucket, and _capture_commit stages them. A bucket in the first
list but not the second is regenerated and left unstaged, so the commit
silently drops it and the next branch switch carries the dirty file along.

Run with:  python -m pytest projects/ores.compass/tests/test_capture_commit_indexes.py -v
No live database required.
"""

import ast
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
COMPASS = ROOT / "projects" / "ores.compass" / "src" / "compass.py"
REGENERATOR = (ROOT / "projects" / "ores.codegen" / "scripts"
               / "regenerate_backlog_indexes.py")


def _string_tuple(node):
    """The tuple of string literals a loop or comprehension iterates, if it is one."""
    if not isinstance(node, ast.Tuple):
        return None
    if not all(isinstance(e, ast.Constant) for e in node.elts):
        return None
    return tuple(e.value for e in node.elts)


def _staged_buckets():
    """Buckets _capture_commit stages: the comprehension inside it."""
    tree = ast.parse(COMPASS.read_text(encoding="utf-8"))
    fn = next(n for n in ast.walk(tree)
              if isinstance(n, ast.FunctionDef) and n.name == "_capture_commit")
    for node in ast.walk(fn):
        for gen in getattr(node, "generators", ()):
            found = _string_tuple(gen.iter)
            if found:
                return found
    raise AssertionError("no bucket tuple found in _capture_commit")


def _written_buckets():
    """Buckets the regenerator writes an index for: its `for bucket in ...` loop."""
    tree = ast.parse(REGENERATOR.read_text(encoding="utf-8"))
    for node in ast.walk(tree):
        if isinstance(node, ast.For):
            found = _string_tuple(node.iter)
            if found and "inbox" in found:
                return found
    raise AssertionError("no bucket tuple found in regenerate_backlog_indexes.py")


def test_capture_commit_stages_every_regenerated_index():
    written = set(_written_buckets())
    staged = set(_staged_buckets())
    missing = sorted(written - staged)
    assert not missing, (
        f"the regenerator writes {sorted(written)} but _capture_commit stages "
        f"{sorted(staged)}; {missing} would be left regenerated but unstaged")
