"""Tests for the create-file reachability check.

The check only has value if it can fail, and its two failure modes are easy to
lose: a file nothing includes, and an include that names no file. The second
was silently skipped until it was fixed, so these tests pin both.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_sql_create_reachability.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_sql_create_reachability as check  # noqa: E402


def _build(tmp_path, entry: str, files: dict[str, str]):
    """A miniature schema: one entry point and a create/ tree."""
    sql_root = tmp_path / "ores.sql"
    create_dir = sql_root / "create" / "trading"
    create_dir.mkdir(parents=True)
    (sql_root / "create.sql").write_text(entry)
    for name, body in files.items():
        (create_dir / name).write_text(body)
    return sql_root, sql_root / "create"


def _bind(monkeypatch, sql_root, create_dir):
    monkeypatch.setattr(check, "SQL_ROOT", sql_root)
    monkeypatch.setattr(check, "CREATE_DIR", create_dir)


def test_the_repository_is_clean():
    """The tree as checked in has neither failure."""
    orphans, missing = check.check()
    assert orphans == []
    assert missing == []


def test_a_wired_tree_passes(monkeypatch, tmp_path):
    sql_root, create_dir = _build(
        tmp_path,
        "\\ir ./create/trading/thing_create.sql\n",
        {"thing_create.sql": "create table thing ();\n"},
    )
    _bind(monkeypatch, sql_root, create_dir)

    orphans, missing = check.check()

    assert orphans == []
    assert missing == []


def test_an_include_that_names_no_file_is_reported(monkeypatch, tmp_path):
    """A file deleted but still named by an aggregator stops the build."""
    sql_root, create_dir = _build(
        tmp_path,
        "\\ir ./create/trading/gone_create.sql\n",
        {},
    )
    _bind(monkeypatch, sql_root, create_dir)

    orphans, missing = check.check()

    assert orphans == []
    assert len(missing) == 1
    including, entry, _target = missing[0]
    assert including.name == "create.sql"
    assert entry == "./create/trading/gone_create.sql"


def test_a_create_file_that_nothing_includes_is_reported(monkeypatch, tmp_path):
    """The failure that hid the link tables: written, generated, never run."""
    sql_root, create_dir = _build(
        tmp_path,
        "\\ir ./create/trading/wired_create.sql\n",
        {
            "wired_create.sql": "create table wired ();\n",
            "orphan_create.sql": "create table orphan ();\n",
        },
    )
    _bind(monkeypatch, sql_root, create_dir)

    orphans, missing = check.check()

    assert missing == []
    assert [path.name for path in orphans] == ["orphan_create.sql"]


def test_a_backslash_ir_resolves_against_the_including_file(monkeypatch, tmp_path):
    """\\ir is relative to the file that writes it, not to the root."""
    sql_root, create_dir = _build(
        tmp_path,
        "\\ir ./create/trading/outer_create.sql\n",
        {},
    )
    nested = create_dir / "trading"
    (nested / "outer_create.sql").write_text("\\ir ./inner_create.sql\n")
    (nested / "inner_create.sql").write_text("create table inner ();\n")

    _bind(monkeypatch, sql_root, create_dir)

    orphans, missing = check.check()

    assert missing == []
    assert orphans == []
