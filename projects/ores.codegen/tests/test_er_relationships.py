"""Tests for the relationships the ER diagram draws.

The parser reads the create scripts; the only places a foreign key's
target is stated there are the comment above a trigger check and a
database constraint. A column that merely looks like a key states
nothing, so it draws no edge.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_er_relationships.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from plantuml_er_parse_sql import SQLParser  # noqa: E402

CREATE_DIR = REPO_ROOT / "projects/ores.sql/create"

PARENT = """\
create table if not exists "ores_testcomp_parents_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to)
);
"""

CHILD = """\
create table if not exists "ores_testcomp_children_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "parent_id" uuid not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to)
);
"""

INSERT_FN = """\
create or replace function ores_testcomp_children_insert_fn() returns trigger as $$
begin
{check}
    return NEW;
end;
$$ language plpgsql;
"""

TRIGGER_CHECK = """\
    -- Validate parent_id ({kind}soft FK to ores_testcomp_parents_tbl)
    if not exists (
        select 1 from ores_testcomp_parents_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.parent_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid parent_id: %.', NEW.parent_id
            using errcode = '23503';
    end if;
"""


def _parse(tmp_path, sql):
    (tmp_path / "ores.testcomp.probe_create.sql").write_text(
        PARENT + CHILD + sql, encoding="utf-8")
    parser = SQLParser(warn=False)
    parser.parse_create_dir(tmp_path)
    parser.apply_column_markings()
    parser.detect_relationships()
    return parser


def _edges(parser):
    return {(r.from_table, r.to_table): r for r in parser.relationships}


def test_a_trigger_check_comment_becomes_an_edge(tmp_path):
    parser = _parse(tmp_path, INSERT_FN.format(check=TRIGGER_CHECK.format(kind="")))

    edges = _edges(parser)
    assert ("ores_testcomp_parents_tbl",
            "ores_testcomp_children_tbl") in edges
    edge = edges[("ores_testcomp_parents_tbl", "ores_testcomp_children_tbl")]
    assert edge.cardinality == "||--o{"
    assert edge.label


def test_an_optional_trigger_check_comment_becomes_an_edge(tmp_path):
    parser = _parse(
        tmp_path, INSERT_FN.format(check=TRIGGER_CHECK.format(kind="optional ")))

    assert ("ores_testcomp_parents_tbl",
            "ores_testcomp_children_tbl") in _edges(parser)


def test_a_database_constraint_becomes_an_edge(tmp_path):
    parser = _parse(tmp_path, "")

    # The same two tables, with the key declared as a constraint instead of
    # a trigger check: a hand-written script states it this way.
    (tmp_path / "ores.testcomp.probe_create.sql").write_text(
        PARENT + CHILD + """\
alter table "ores_testcomp_children_tbl"
    add constraint ores_testcomp_children_parent_fk
    foreign key ("parent_id") references "ores_testcomp_parents_tbl" ("id");
""", encoding="utf-8")
    parser = SQLParser(warn=False)
    parser.parse_create_dir(tmp_path)
    parser.apply_column_markings()
    parser.detect_relationships()

    assert ("ores_testcomp_parents_tbl",
            "ores_testcomp_children_tbl") in _edges(parser)


def test_a_key_no_script_states_draws_no_edge(tmp_path):
    # parent_id is a key by every naming convention and states no target,
    # so the diagram marks the column and draws nothing from it.
    parser = _parse(tmp_path, "")

    assert _edges(parser) == {}


def test_the_schema_yields_the_keys_its_scripts_state():
    parser = SQLParser(warn=False)
    parser.parse_create_dir(CREATE_DIR)
    parser.apply_column_markings()
    parser.detect_relationships()

    edges = _edges(parser)
    assert len(edges) > 300

    # A generated instrument checks the trade it belongs to.
    assert ("ores_trading_trades_tbl",
            "ores_trading_fra_instruments_tbl") in edges

    # Every edge names two tables the schema holds: a target that no create
    # script declares would be a key into nothing.
    tables = set(parser.tables)
    for from_table, to_table in edges:
        assert from_table in tables, from_table
        assert to_table in tables, to_table

    # No table references itself; a hierarchy is not a loop on the diagram.
    assert all(f != t for f, t in edges)
