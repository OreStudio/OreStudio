"""Tests for the common-date views generated from the trade-type catalogue.

Run with:
    projects/ores.codegen/venv/bin/python -m pytest projects/ores.codegen/tests/test_common_dates.py

A family maps its own date columns onto the catalogue's common names by
declaring ``:common_date:`` on them. The view is generated from those
declarations, so these tests hold the mapping to the columns the models
declare.
"""
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _entity_columns,
    load_org_trade_type_catalogue_model,
)

TRADING = REPO_ROOT / "projects/ores.trading/modeling"
CATALOGUE = TRADING / "ores.trading.trade_type_catalogue.org"
CREATE = REPO_ROOT / "projects/ores.sql/create/trading/trading_common_dates_vw_create.sql"
DROP = REPO_ROOT / "projects/ores.sql/drop/trading/trading_common_dates_vw_drop.sql"


def _dates():
    return load_org_trade_type_catalogue_model(CATALOGUE)[
        "trade_type_catalogue"]["common_dates"]


def _rates_view():
    views = [v for v in _dates()["views"] if v["entity"] == "rate_instrument"]
    assert len(views) == 1
    return views[0]


def test_the_view_has_one_column_per_common_name_in_catalogue_order():
    names = [entry["name"] for entry in _dates()["names"]]
    assert names == ["trade_date", "start_date", "expiry_date", "maturity_date"]
    assert [c["name"] for c in _rates_view()["columns"]] == names


def test_every_mapped_column_is_one_the_model_declares():
    by_name = {}
    for org in TRADING.glob("*.org"):
        text = org.read_text(encoding="utf-8")
        match = re.search(r"^#\+entity_singular:\s*(\S+)", text, re.M)
        if match:
            by_name[match.group(1)] = org
    mapped = _rates_view()["mapped"]
    assert mapped
    for entry in mapped:
        entity, column = entry["entity_column"].split(".")
        columns = _entity_columns(by_name[entity])
        assert column in columns
        assert columns[column]["common_date"] == entry["name"]


def test_a_swaption_states_its_expiry_and_the_swap_states_the_rest():
    mapped = {m["name"]: m["entity_column"] for m in _rates_view()["mapped"]}
    assert mapped == {
        "start_date": "rate_instrument.start_date",
        "maturity_date": "rate_instrument.maturity_date",
        "expiry_date": "swaption_instrument.expiry_date",
    }


def test_the_trade_date_comes_from_the_booking():
    columns = {c["name"]: c["expr"] for c in _rates_view()["columns"]}
    booking = [j for j in _rates_view()["joins"]
               if j["table"] == "ores_trading_trade_bookings_tbl"]
    assert len(booking) == 1
    assert columns["trade_date"] == f"{booking[0]['alias']}.trade_date"


def test_the_committed_view_names_the_generated_columns():
    sql = CREATE.read_text(encoding="utf-8")
    view = _rates_view()
    assert f"create or replace view {view['view']}" in sql
    for column in view["columns"]:
        assert f"{column['expr']} as {column['name']}" in sql
    assert f"drop view if exists {view['view']};" in DROP.read_text(
        encoding="utf-8")


def test_a_leg_table_is_not_keyed_by_the_trade_alone():
    from codegen.org_loader import _keyed_by_trade_id_alone
    assert _keyed_by_trade_id_alone(_entity_columns(
        TRADING / "ores.trading.swaption_instrument.org"))
    assert not _keyed_by_trade_id_alone(_entity_columns(
        TRADING / "ores.trading.swap_leg.org"))


def _entity(singular, plural, table, columns, cascade=()):
    cols = "".join(
        f"** {name}\n:PROPERTIES:\n:type: date\n"
        + (":primary_key: true\n" if key else "")
        + (f":common_date: {mapped}\n" if mapped else "")
        + ":END:\n\nA column.\n\n"
        for name, key, mapped in columns)
    casc = "".join(
        f"** {t}\n:PROPERTIES:\n:table: {t}\n:column: trade_id\n:END:\n\n"
        for t in cascade)
    return (
        f":PROPERTIES:\n:ID: 00000000-0000-0000-0000-{abs(hash(singular)) % 10**12:012d}\n"
        f":END:\n#+title: ores.testcomp.{singular}\n#+type: ores.codegen.entity\n"
        f"#+component: testcomp\n#+entity_singular: {singular}\n"
        f"#+entity_plural: {plural}\n\nA row.\n\n* Columns\n\n{cols}"
        + (f"* Delete cascade\n\n{casc}" if cascade else "")
        + f"* SQL\n** Flags\n:PROPERTIES:\n:tablename: {table}\n:END:\n")


def _tree(tmp_path, header_cols, fact_cols, vocabulary=None, shared="| "):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True)
    (modeling / "ores.testcomp.header.org").write_text(
        _entity("header", "headers", "ores_testcomp_headers_tbl", header_cols,
                cascade=["ores_testcomp_facts_tbl"]), encoding="utf-8")
    (modeling / "ores.testcomp.fact.org").write_text(
        _entity("fact", "facts", "ores_testcomp_facts_tbl", fact_cols),
        encoding="utf-8")
    vocabulary = vocabulary or ["start_date", "expiry_date"]
    rows = "\n".join(f"| {n} | A date. | | |" for n in vocabulary)
    (modeling / "ores.testcomp.trade_type_catalogue.org").write_text(
        "#+title: ores.testcomp.trade_type_catalogue\n"
        "#+type: ores.codegen.trade_type_catalogue\n#+component: testcomp\n\n"
        "* Trade types\n\n"
        "| code | description | product_type | has_options | has_extension | instrument |\n"
        "|------+-------------+--------------+-------------+---------------+------------|\n"
        "| Swap | x | swap | false | false | header |\n\n"
        "* Common dates\n\n| name | description | shared_entity | shared_column |\n"
        "|------+-------------+---------------+---------------|\n"
        f"{rows}\n", encoding="utf-8")
    return modeling / "ores.testcomp.trade_type_catalogue.org"


KEY = ("trade_id", True, None)


def test_a_family_that_declares_its_dates_gets_a_view(tmp_path):
    path = _tree(tmp_path,
                 [KEY, ("begins", False, "start_date")],
                 [KEY, ("lapses", False, "expiry_date")])

    dates = load_org_trade_type_catalogue_model(path)[
        "trade_type_catalogue"]["common_dates"]

    assert [v["view"] for v in dates["views"]] == [
        "ores_testcomp_headers_common_dates_vw"]
    columns = {c["name"]: c["expr"] for c in dates["views"][0]["columns"]}
    assert columns == {"start_date": "h.begins", "expiry_date": "j1.lapses"}


@pytest.mark.parametrize("header,fact,vocabulary,message", [
    ([KEY, ("a", False, "birth_date")], [KEY], None, "does not list"),
    ([KEY, ("a", False, "start_date")], [KEY, ("b", False, "start_date")],
     None, "stated by both"),
    ([KEY], [KEY, ("trade_no", True, None), ("b", False, "start_date")],
     None, "not keyed by the trade alone"),
    ([KEY], [KEY], ["start_date", "start_date"], "duplicate common date"),
])
def test_the_loader_refuses_a_mapping_it_cannot_honour(
        tmp_path, header, fact, vocabulary, message):
    path = _tree(tmp_path, header, fact, vocabulary)

    with pytest.raises(ValueError, match=message):
        load_org_trade_type_catalogue_model(path)


def test_the_loader_refuses_a_shared_date_the_entity_does_not_declare(tmp_path):
    path = _tree(tmp_path, [KEY], [KEY])
    text = path.read_text(encoding="utf-8")
    path.write_text(text.replace(
        "| start_date | A date. | | |",
        "| start_date | A date. | fact | missing_col |"), encoding="utf-8")

    with pytest.raises(ValueError, match="does not declare"):
        load_org_trade_type_catalogue_model(path)
