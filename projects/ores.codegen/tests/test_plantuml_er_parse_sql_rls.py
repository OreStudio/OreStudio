"""Tests for the RLS_001/RLS_002/RLS_003 row-level-security coverage rules.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_er_parse_sql_rls.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from plantuml_er_parse_sql import SQLParser  # noqa: E402


def _write(dir_path: Path, name: str, content: str = "") -> Path:
    """Create a file (and its parent directories) under dir_path."""
    target = dir_path / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(content)
    return target


def _rls_warnings(parser: SQLParser, code: str) -> list:
    return [w for w in parser.warnings if w.code == code]


def _parse_and_validate(create_dir: Path) -> SQLParser:
    """Parse a create dir and run the RLS validator, as plantuml main() does."""
    parser = SQLParser(warn=True)
    parser.parse_create_dir(create_dir)
    parser.validate_rls_policies(create_dir)
    return parser


# A tenant- and party-scoped table with a compound primary key. The name
# follows the <product>_<component>_<entity>_tbl convention, so the NAMING
# checks stay silent, and tenant_id is part of the primary key.
CREATE_TABLE = """\
create table if not exists "ores_marketdata_widget_tbl" (
    tenant_id uuid not null,
    id uuid not null,
    party_id uuid,
    name text not null,
    primary key (tenant_id, id)
);
"""

# The marketdata inline-emission pattern: the RLS directives live in the
# table's own *_create.sql file, after the create statement.
INLINE_RLS = """\
alter table ores_marketdata_widget_tbl enable row level security;

create policy widget_tbl_tenant_isolation_policy
on ores_marketdata_widget_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy widget_tbl_party_isolation_policy
on ores_marketdata_widget_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
"""


def test_inline_rls_satisfies_all_checks_without_legacy_files(tmp_path):
    create_dir = tmp_path / "create"
    _write(create_dir, "marketdata/marketdata_widget_create.sql",
           CREATE_TABLE + INLINE_RLS)

    parser = _parse_and_validate(create_dir)

    assert _rls_warnings(parser, 'RLS_001') == []
    assert _rls_warnings(parser, 'RLS_002') == []
    assert _rls_warnings(parser, 'RLS_003') == []


def test_missing_directives_warn_without_legacy_policy_files(tmp_path):
    """Regression: RLS_001/RLS_002 used to go dark entirely once no
    *_rls_policies_create.sql file remained anywhere, because the validator
    early-returned off the legacy files' existence. Components on inline
    emission (marketdata) keep the directives in each table's own
    *_create.sql file, so the checks must run regardless."""
    create_dir = tmp_path / "create"
    _write(create_dir, "marketdata/marketdata_widget_create.sql", CREATE_TABLE)

    parser = _parse_and_validate(create_dir)

    warnings_001 = _rls_warnings(parser, 'RLS_001')
    warnings_002 = _rls_warnings(parser, 'RLS_002')
    assert len(warnings_001) == 1
    assert 'ores_marketdata_widget_tbl' in warnings_001[0].message
    assert len(warnings_002) == 1
    assert 'ores_marketdata_widget_tbl' in warnings_002[0].message


def test_enable_without_restrictive_policy_warns_rls_002_only(tmp_path):
    create_dir = tmp_path / "create"
    content = CREATE_TABLE + (
        "alter table ores_marketdata_widget_tbl enable row level security;\n"
    )
    _write(create_dir, "marketdata/marketdata_widget_create.sql", content)

    parser = _parse_and_validate(create_dir)

    assert _rls_warnings(parser, 'RLS_001') == []
    assert len(_rls_warnings(parser, 'RLS_002')) == 1


def test_legacy_policy_file_reachable_from_rls_create_is_silent(tmp_path):
    create_dir = tmp_path / "create"
    _write(create_dir, "marketdata/marketdata_widget_create.sql", CREATE_TABLE)
    _write(create_dir, "marketdata/marketdata_widget_rls_policies_create.sql",
           INLINE_RLS)
    _write(create_dir, "rls/rls_create.sql",
           "\\ir ../marketdata/marketdata_widget_rls_policies_create.sql\n")

    parser = _parse_and_validate(create_dir)

    assert _rls_warnings(parser, 'RLS_001') == []
    assert _rls_warnings(parser, 'RLS_002') == []
    assert _rls_warnings(parser, 'RLS_003') == []


def test_unreachable_legacy_policy_file_warns_rls_003(tmp_path):
    create_dir = tmp_path / "create"
    legacy = _write(create_dir,
                    "marketdata/marketdata_widget_rls_policies_create.sql",
                    INLINE_RLS)
    # No rls/rls_create.sql exists: nothing wires the legacy file into the
    # schema flow. Its directives still satisfy RLS_001/RLS_002 for the
    # table; only the reachability check fires.
    _write(create_dir, "marketdata/marketdata_widget_create.sql", CREATE_TABLE)

    parser = _parse_and_validate(create_dir)

    warnings_003 = _rls_warnings(parser, 'RLS_003')
    assert len(warnings_003) == 1
    assert legacy.name in warnings_003[0].message
    assert _rls_warnings(parser, 'RLS_001') == []
    assert _rls_warnings(parser, 'RLS_002') == []
