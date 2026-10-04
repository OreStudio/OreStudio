"""Tests for the trade-type catalogue and the checks routed from it.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_trade_type_catalogue.py

The catalogue names, for each trade type, the instrument entity whose table
holds it. A model that declares ``:routed_by:`` and ``:routed_column:`` gets a
check listing exactly the codes routed to it, so no two tables accept the
same code.
"""
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import load_org_trade_type_catalogue_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"
TRADING = REPO_ROOT / "projects/ores.trading/modeling"
CATALOGUE = TRADING / "ores.trading.trade_type_catalogue.org"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E1
:END:
#+title: ores.testcomp.widget
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: widget
#+entity_plural: widgets

A row.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

The row.

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_widgets_tbl
:END:
"""


def _catalogue(rows: str) -> str:
    return f"""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E2
:END:
#+title: ores.testcomp.trade_type_catalogue
#+type: ores.codegen.trade_type_catalogue
#+component: testcomp

* Trade types

| code | description | product_type | has_options | has_extension | instrument |
|------+-------------+--------------+-------------+---------------+------------|
{rows}
"""


def _write_catalogue(tmp_path, rows):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True)
    (modeling / "ores.testcomp.widget.org").write_text(ENTITY, encoding="utf-8")
    path = modeling / "ores.testcomp.trade_type_catalogue.org"
    path.write_text(_catalogue(rows), encoding="utf-8")
    return path


def test_the_catalogue_groups_codes_by_the_table_that_holds_them(tmp_path):
    path = _write_catalogue(tmp_path, "\n".join([
        "| Swap | Interest Rate Swap | swap | false | false | widget |",
        "| FxForward | FX Forward | fx | false | false | widget |",
        "| Failed | A placeholder | swap | false | false | |",
    ]))

    catalogue = load_org_trade_type_catalogue_model(path)["trade_type_catalogue"]

    assert [row["code"] for row in catalogue["rows"]] == ["Swap", "FxForward", "Failed"]
    assert catalogue["instruments"] == [
        {"name": "widget", "codes": ["Swap", "FxForward"], "comma": ""}]
    assert [route["code"] for route in catalogue["routes"]] == ["Swap", "FxForward"]


@pytest.mark.parametrize("row,message", [
    ("| Swap | x | rates | false | false | widget |", "product type"),
    ("| Swap | x | swap | yes | false | widget |", "has_options"),
    ("| Swap | x | swap | false | false | gadget |", "no entity model declares"),
])
def test_the_catalogue_refuses_a_row_it_cannot_honour(tmp_path, row, message):
    path = _write_catalogue(tmp_path, row)

    with pytest.raises(ValueError, match=message):
        load_org_trade_type_catalogue_model(path)


def test_the_catalogue_refuses_a_repeated_code(tmp_path):
    path = _write_catalogue(tmp_path, "\n".join([
        "| Swap | x | swap | false | false | widget |",
        "| Swap | y | swap | false | false | widget |",
    ]))

    with pytest.raises(ValueError, match="duplicate"):
        load_org_trade_type_catalogue_model(path)


def _create_sql(tmp_path, model):
    output = "out.sql"
    generate_from_model(
        str(model), DATA_DIR, TEMPLATES_DIR, tmp_path,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output=output)
    return (tmp_path / output).read_text(encoding="utf-8")


def test_a_routed_table_checks_exactly_the_codes_routed_to_it(tmp_path):
    sql = _create_sql(tmp_path, TRADING / "ores.trading.vanilla_swap_instrument.org")

    assert "\"trade_type_code\" in ('Swap', 'CrossCurrencySwap', 'FlexiSwap')" in sql


def test_no_two_routed_tables_accept_the_same_code():
    catalogue = load_org_trade_type_catalogue_model(CATALOGUE)["trade_type_catalogue"]
    routed = [code for entry in catalogue["instruments"] for code in entry["codes"]]

    assert len(routed) == len(set(routed))


def test_every_routed_model_names_the_catalogue():
    routed = {
        entry["name"]
        for entry in load_org_trade_type_catalogue_model(CATALOGUE)[
            "trade_type_catalogue"]["instruments"]}
    for model in TRADING.glob("*.org"):
        text = model.read_text(encoding="utf-8")
        singular = re.search(r"^#\+entity_singular:\s*(\S+)", text, re.M)
        if not singular or singular.group(1) not in routed:
            continue
        if "trade_type_code" not in text:
            continue
        assert ":routed_by:     ores.trading.trade_type_catalogue" in text, model.name
