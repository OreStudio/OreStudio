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
