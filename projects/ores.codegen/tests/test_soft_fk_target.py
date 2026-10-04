"""Tests for the trigger check a foreign key renders against its target.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_soft_fk_target.py

A foreign key that is not enforced by the database becomes a check in the
insert trigger. A temporal target holds closed versions beside its current
row, so the check reads the current row only. An immutable target keeps no
versions and has no validity column, so the check reads its row as it is.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"
TRADING = REPO_ROOT / "projects/ores.trading/modeling"


def _create_sql(tmp_path, model):
    output = "out.sql"
    generate_from_model(
        str(model), DATA_DIR, TEMPLATES_DIR, tmp_path,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output=output)
    return (tmp_path / output).read_text(encoding="utf-8")


def _check(sql, column):
    match = re.search(
        rf"-- Validate {column} \((?:optional )?soft FK to (\S+)\)(.*?)\) then",
        sql, re.S)
    assert match, f"no trigger check for {column}"
    return match.group(1), match.group(2)


def test_a_check_against_an_immutable_target_reads_its_row_as_it_is(tmp_path):
    sql = _create_sql(tmp_path, TRADING / "ores.trading.fra_instrument.org")

    table, body = _check(sql, "trade_id")

    assert table == "ores_trading_trade_anchors_tbl"
    assert "valid_to" not in body


def test_a_check_against_a_temporal_target_reads_its_current_row(tmp_path):
    sql = _create_sql(tmp_path, TRADING / "ores.trading.trade_booking.org")

    table, body = _check(sql, "book_id")

    assert table == "ores_refdata_books_tbl"
    assert "valid_to = ores_utility_infinity_timestamp_fn()" in body
