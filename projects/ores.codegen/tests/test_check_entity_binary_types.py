# -*- coding: utf-8 -*-
"""Tests of the check that a row binds a column in its column's own format."""

import importlib.util
import sys
import textwrap
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "check_entity_binary_types.py"
spec = importlib.util.spec_from_file_location("check_entity_binary_types", SCRIPT)
check = importlib.util.module_from_spec(spec)
sys.modules["check_entity_binary_types"] = check
spec.loader.exec_module(check)

CREATE = textwrap.dedent("""\
    create table "ores_probe_things_tbl" (
        "id" uuid,
        "lag" integer null,
        "label" character varying(50) null,
        "amount" numeric(38, 12),
        "started_at" timestamp with time zone null,
        "sizes" integer[] null,
        "kind" product_type_t null
    );
""")

HEADER = textwrap.dedent("""\
    /**
     * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
     */
    struct probe_entity {
        constexpr static const char* schema = "public";
        constexpr static const char* tablename = "ores_probe_things_tbl";

        boost::uuids::uuid id;
        std::optional<int> lag;
        std::optional<std::string> label;
        std::optional<ores::utility::decimal::decimal> amount;
        std::optional<std::chrono::system_clock::time_point> started_at;
        std::optional<std::vector<std::int32_t>> sizes;
        std::optional<std::string> kind;

        static constexpr auto columns() {
            using e = probe_entity;
            return query_builder::columns<e>() | query_builder::key("id", &e::id) |
                   query_builder::bind("lag", &e::lag) |
                   query_builder::bind_text("label", &e::label) |
                   query_builder::bind("amount", &e::amount) |
                   query_builder::bind("started_at", &e::started_at) |
                   query_builder::bind("sizes", &e::sizes) |
                   query_builder::bind("kind", &e::kind);
        }
    };
""")


def tree(tmp_path, header=HEADER, sql=CREATE):
    sql_dir = tmp_path / "projects" / "ores.sql" / "create" / "probe"
    sql_dir.mkdir(parents=True)
    (sql_dir / "probe_things_create.sql").write_text(sql, encoding="utf-8")
    entity_dir = tmp_path / "projects" / "ores.probe" / "core" / "include" / (
        "ores.probe.core") / "repository"
    entity_dir.mkdir(parents=True)
    (entity_dir / "probe_entity.hpp").write_text(header, encoding="utf-8")
    return tmp_path


def test_a_row_whose_members_are_its_columns_types_passes(tmp_path):
    result = check.compare(tree(tmp_path))
    assert result.mismatches == []
    assert result.unclassified == []
    assert result.no_table == []
    # The text-bound label is skipped; the custom type the row keeps as text is
    # counted apart; the unconstrained and sized columns are compared.
    assert result.compared == 5
    assert result.skipped == 1
    assert result.unchecked == 1


def test_a_wider_integer_than_its_column_is_reported(tmp_path):
    header = HEADER.replace("std::optional<int> lag;",
                            "std::optional<std::int64_t> lag;")
    result = check.compare(tree(tmp_path, header=header))
    assert len(result.mismatches) == 1
    message = result.mismatches[0]
    assert "'lag'" in message
    assert "std::optional<std::int64_t>" in message
    assert "integer" in message
    assert result.failed


def test_a_custom_column_type_the_row_keeps_as_text_is_counted_apart(tmp_path):
    result = check.compare(tree(tmp_path))
    assert result.unchecked == 1
    assert result.unclassified == []


def test_a_custom_column_type_the_row_binds_as_a_number_is_reported(tmp_path):
    header = HEADER.replace("std::optional<std::string> kind;", "std::optional<int> kind;")
    result = check.compare(tree(tmp_path, header=header))
    assert len(result.unclassified) == 1
    assert "'kind'" in result.unclassified[0]


def test_a_member_of_another_type_class_than_its_column_is_reported(tmp_path):
    # A time point on a date column: the codec writes a timestamptz.
    header = HEADER.replace(
        "std::optional<std::chrono::system_clock::time_point> started_at;",
        "std::optional<std::chrono::year_month_day> started_at;")
    result = check.compare(tree(tmp_path, header=header))
    assert len(result.mismatches) == 1
    assert "'started_at'" in result.mismatches[0]


def test_a_column_the_table_does_not_create_is_reported(tmp_path):
    header = HEADER.replace('bind("amount"', 'bind("total_amount"')
    result = check.compare(tree(tmp_path, header=header))
    assert len(result.mismatches) == 1
    assert "total_amount" in result.mismatches[0]


def test_a_member_the_struct_does_not_declare_is_reported(tmp_path):
    header = HEADER.replace("std::optional<int> lag;", "")
    result = check.compare(tree(tmp_path, header=header))
    assert len(result.mismatches) == 1
    assert "not declared" in result.mismatches[0]


def test_a_table_no_create_script_creates_is_reported(tmp_path):
    result = check.compare(tree(tmp_path, sql=""))
    assert len(result.no_table) == 1
    assert "ores_probe_things_tbl" in result.no_table[0]


def test_a_view_is_left_to_the_suite_that_reads_it(tmp_path):
    sql = CREATE + 'create or replace view "ores_probe_things_vw" as select 1 as "id";\n'
    header = HEADER.replace("ores_probe_things_tbl", "ores_probe_things_vw")
    result = check.compare(tree(tmp_path, header=header, sql=sql))
    assert result.no_table == []
    assert result.mismatches == []


def test_an_optional_member_is_read_by_the_type_it_holds():
    assert check.wire_of_cpp("std::optional<int>") == ("integer", 32)
    assert check.wire_of_cpp("std::optional<std::int64_t>") == ("integer", 64)
    assert check.wire_of_cpp("boost::uuids::uuid") == ("uuid", None)
    assert check.wire_of_cpp("ores::utility::domain::mystery") is None


def test_a_tree_with_nothing_to_compare_is_not_read_as_a_pass(tmp_path, monkeypatch):
    empty = tmp_path / "projects"
    empty.mkdir()
    monkeypatch.setattr(sys, "argv", ["check", "--root", str(tmp_path)])
    assert check.compare(tmp_path).compared == 0
    assert check.main() == 1
