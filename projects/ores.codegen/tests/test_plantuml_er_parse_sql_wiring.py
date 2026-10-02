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
    """Every .sql file is in scope, so the roots are excluded by identity: a
    root is what reachability starts from and can never be reached."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert _wire_warnings(parser) == []


def test_unwired_file_with_any_suffix_warns(tmp_path):
    """A leftover named outside the *_create.sql convention, such as an old
    *_notify_trigger.sql copy, is the file most likely to be dead."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql", "\\ir ./comp/comp_create.sql\n")
    _write(create_dir, "comp/comp_create.sql")
    _write(create_dir, "comp/entity_notify_trigger.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    warnings = _wire_warnings(parser)
    assert len(warnings) == 1
    assert "entity_notify_trigger.sql" in warnings[0].message


def test_unwired_populate_file_warns(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    populate_dir = tmp_path / "populate"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")
    _write(populate_dir, "populate.sql", "\\ir ./comp/wired_populate.sql\n")
    _write(populate_dir, "comp/wired_populate.sql")
    _write(populate_dir, "comp/dangling_populate.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir, populate_dir)
    warnings = _wire_warnings(parser)
    assert len(warnings) == 1
    assert "dangling_populate.sql" in warnings[0].message


def test_a_bootstrap_script_beside_the_trees_counts_as_a_root(tmp_path):
    """setup_schema.sql includes the foundation seeds directly, not through
    populate.sql, and a recreate runs it."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    populate_dir = tmp_path / "populate"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")
    _write(populate_dir, "populate.sql")
    _write(populate_dir, "foundation/foundation_populate.sql")
    _write(tmp_path, "setup_schema.sql",
           "\\ir ./populate/foundation/foundation_populate.sql\n")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir, populate_dir)
    assert _wire_warnings(parser) == []


def test_populate_tree_is_skipped_when_not_given(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql")
    _write(drop_dir, "drop.sql")
    _write(tmp_path, "populate/comp/dangling_populate.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    assert _wire_warnings(parser) == []


def test_a_file_reached_only_through_another_tree_is_not_wired(tmp_path):
    """A drop file that create.sql includes runs during a create, not a drop,
    so it is not wired into the drop tree."""
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql", "\\ir ../drop/comp/misplaced_drop.sql\n")
    _write(drop_dir, "drop.sql")
    _write(drop_dir, "comp/misplaced_drop.sql")
    _write(tmp_path, "setup_schema.sql", "\\ir ./create/create.sql\n")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    warnings = _wire_warnings(parser)
    assert len(warnings) == 1
    assert "misplaced_drop.sql" in warnings[0].message


def test_a_commented_out_include_wires_nothing(tmp_path):
    create_dir = tmp_path / "create"
    drop_dir = tmp_path / "drop"
    _write(create_dir, "create.sql",
           "\\ir ./comp/wired_create.sql\n-- \\ir ./comp/retired_create.sql\n")
    _write(create_dir, "comp/wired_create.sql")
    _write(create_dir, "comp/retired_create.sql")
    _write(drop_dir, "drop.sql")

    parser = SQLParser(warn=True)
    parser.validate_component_wiring(create_dir, drop_dir)
    warnings = _wire_warnings(parser)
    assert len(warnings) == 1
    assert "retired_create.sql" in warnings[0].message
