"""Tests for the WIRE_001 component-aggregator wiring rule.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_er_parse_sql_wiring.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from plantuml_er_parse_sql import SQLParser  # noqa: E402


def _write(dir_path: Path, name: str, content: str = "") -> Path:
    """Create a file (and its parent directories) under dir_path."""
    target = dir_path / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(content)
    return target


def _wire_warnings(parser: SQLParser) -> list:
    return [w for w in parser.warnings if w.code == 'WIRE_001']


def test_wired_files_pass_on_both_sides(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"

    # Root wires an aggregator, which wires a leaf and a transitive chain.
    _write(create_dir, "create.sql", "\\ir ./comp/comp_create.sql\n")
    _write(create_dir, "comp/comp_create.sql",
           "\\ir ./entity_create.sql\n\\ir ./sub/sub_entity_create.sql\n")
    _write(create_dir, "comp/entity_create.sql")
    _write(create_dir, "comp/sub/sub_entity_create.sql")

    _write(drop_dir, "drop.sql", "\\ir ./comp/comp_drop.sql\n")
    _write(drop_dir, "comp/comp_drop.sql", "\\ir ./entity_drop.sql\n")
    _write(drop_dir, "comp/entity_drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert _wire_warnings(parser) == []


def test_unwired_create_file_warns(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    unwired = _write(create_dir, "comp/dangling_create.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    warnings = _wire_warnings(parser)
    assert len(warnings) == 1
    assert unwired.name in warnings[0].message
    assert str(unwired) in warnings[0].file


def test_unwired_drop_file_warns(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")
    _write(drop_dir, "comp/dangling_drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert len(_wire_warnings(parser)) == 1


def test_create_side_rls_files_are_rls_003_domain(tmp_path):
    """Unwired *_rls_policies_create.sql files must not double-report."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(create_dir, "comp/comp_rls_policies_create.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert _wire_warnings(parser) == []


def test_drop_side_rls_files_stay_in_scope(tmp_path):
    """No drop-side RLS reachability rule exists, so WIRE_001 must cover it."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")
    _write(drop_dir, "comp/comp_rls_policies_drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert len(_wire_warnings(parser)) == 1


def test_service_bundles_wired_from_bootstrap_flows_are_exempt(tmp_path):
    """iam service bundles are \\ir'd from setup_schema.sql / setup_user.sql /
    recreate_database.sql, outside the create.sql chain this rule can see."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(create_dir, "iam/service_users_create.sql")
    _write(create_dir, "iam/iam_service_db_grants_create.sql")
    _write(create_dir, "iam/regular_create.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    warnings = _wire_warnings(parser)
    assert len(warnings) == 1
    assert 'regular_create.sql' in warnings[0].message


def test_ignore_file_suppresses_wire_001(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    dangling = _write(create_dir, "comp/dangling_create.sql")
    _write(drop_dir, "drop.sql")
    ignore_file = _write(tmp_path, "ignore.txt", "WIRE_001 dangling_create.sql\n")

    parser = SQLParser(warn=True, ignore_file=ignore_file)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert _wire_warnings(parser) == []
    assert dangling.exists()


def test_root_aggregators_are_not_in_scope(tmp_path):
    """create.sql / drop.sql end without the *_create.sql / *_drop.sql suffix,
    so the roots themselves can never warn."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert _wire_warnings(parser) == []
