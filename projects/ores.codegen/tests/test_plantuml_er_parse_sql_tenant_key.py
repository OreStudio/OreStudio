"""Tests for TENANT_001/TENANT_002, and for the surrogate key that needs no tenant.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_er_parse_sql_tenant_key.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from plantuml_er_parse_sql import SQLParser  # noqa: E402


def _parse(tmp_path: Path, table_sql: str) -> SQLParser:
    """Parse one create file and return the parser, warnings and all."""
    create_dir = tmp_path / "create"
    target = create_dir / "iam" / "iam_widget_create.sql"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(table_sql)
    parser = SQLParser(warn=True)
    parser.parse_create_dir(create_dir)
    return parser


def _codes(parser: SQLParser) -> list:
    return [warning.code for warning in parser.warnings]


def test_one_surrogate_uuid_needs_no_tenant_in_the_key(tmp_path):
    """A generated uuid is unique across every tenant, so the tenant adds nothing."""
    parser = _parse(tmp_path, """\
create table if not exists "ores_iam_widget_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "name" text not null,
    primary key (id)
);
""")

    assert "TENANT_002" not in _codes(parser)


def test_a_natural_key_still_needs_the_tenant(tmp_path):
    """Two tenants can hold the same natural key, so the tenant belongs in it."""
    parser = _parse(tmp_path, """\
create table if not exists "ores_iam_widget_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "name" text not null,
    primary key (name)
);
""")

    assert "TENANT_002" in _codes(parser)


def test_a_composite_key_still_needs_the_tenant(tmp_path):
    """A uuid beside another column is not a surrogate key on its own."""
    parser = _parse(tmp_path, """\
create table if not exists "ores_iam_widget_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "name" text not null,
    primary key (id, name)
);
""")

    assert "TENANT_002" in _codes(parser)


def test_a_table_with_no_tenant_is_refused_once(tmp_path):
    """A table with no tenant column is one warning, not two."""
    parser = _parse(tmp_path, """\
create table if not exists "ores_iam_widget_tbl" (
    "id" uuid not null,
    "name" text not null,
    primary key (id)
);
""")

    codes = _codes(parser)
    assert "TENANT_001" in codes
    assert "TENANT_002" not in codes
