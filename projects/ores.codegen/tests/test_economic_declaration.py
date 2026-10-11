"""Tests for the model's declaration of which fields are economic.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_economic_declaration.py

A column states ``:economic: true`` or ``:economic: false``. The domain header
carries the answer as an ``economic_fields_of`` overload the economic digest
reads, so no field is economic or not by its name. A struct declares all of its
members or none of them, and the trade-type catalogue's digest section lists
only entities that declare.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    economic_declaration,
    load_org_field_group_model,
    load_org_trade_type_catalogue_model,
)

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F1
:END:
#+title: ores.testcomp.widget
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: widget
#+entity_plural: widgets
#+brief: A widget.

A widget.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:no_subcomponent: true
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
{id_economic}:END:

The row.

** amount
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
{amount_economic}:END:

An amount.

** reference_id
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
{reference_economic}:END:

A reference.

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_widgets_tbl
:END:
"""


def _widget(tmp_path, id_economic="", amount_economic="", reference_economic=""):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True, exist_ok=True)
    path = modeling / "ores.testcomp.widget.org"
    path.write_text(
        ENTITY.format(
            id_economic=id_economic,
            amount_economic=amount_economic,
            reference_economic=reference_economic),
        encoding="utf-8")
    return path


def _header(tmp_path, model):
    output = "widget.hpp"
    generate_from_model(
        str(model), DATA_DIR, TEMPLATES_DIR, tmp_path,
        is_processing_batch=True,
        target_template="cpp_domain_type_class.hpp.mustache",
        target_output=output)
    return (tmp_path / output).read_text(encoding="utf-8")


def test_a_declared_entity_lists_its_economic_members(tmp_path):
    model = _widget(
        tmp_path,
        id_economic=":economic: false\n",
        amount_economic=":economic: true\n",
        reference_economic=":economic: false\n")

    header = _header(tmp_path, model)

    assert 'inline constexpr std::string_view widget_economic_fields[] = {\n    "amount",\n};' \
        in header
    assert "economic_fields_of(std::type_identity<widget>)" in header
    assert "#include <span>" in header


def test_the_names_do_not_decide(tmp_path):
    model = _widget(
        tmp_path,
        id_economic=":economic: false\n",
        amount_economic=":economic: false\n",
        reference_economic=":economic: true\n")

    header = _header(tmp_path, model)

    assert '"reference_id"' in header
    assert '"amount"' not in header


def test_an_entity_with_no_economic_member_declares_an_empty_list(tmp_path):
    model = _widget(
        tmp_path,
        id_economic=":economic: false\n",
        amount_economic=":economic: false\n",
        reference_economic=":economic: false\n")

    header = _header(tmp_path, model)

    assert "economic_fields_of(std::type_identity<widget>)" in header
    assert "widget_economic_fields" not in header
    assert "return {};" in header


def test_an_entity_that_declares_nothing_has_no_overload(tmp_path):
    model = _widget(tmp_path)

    header = _header(tmp_path, model)

    assert "economic_fields_of" not in header


def test_a_half_declared_entity_is_refused(tmp_path):
    model = _widget(tmp_path, amount_economic=":economic: true\n")

    with pytest.raises(ValueError, match="reference_id"):
        _header(tmp_path, model)


def test_a_value_that_is_not_true_or_false_is_refused():
    members = [{"name": "amount", "economic": "maybe"}]

    with pytest.raises(ValueError, match="true or false"):
        economic_declaration("widget.org", members, [])


def test_a_column_that_reaches_no_member_must_not_declare():
    carried = [{"name": "trade_id", "economic": False}]

    with pytest.raises(ValueError, match="trade_id"):
        economic_declaration("widget.org", [], carried)


def test_declaration_keeps_the_members_in_model_order():
    members = [
        {"name": "b", "economic": True},
        {"name": "a", "economic": False},
        {"name": "c", "economic": True},
    ]

    assert economic_declaration("widget.org", members, []) == (True, ["b", "c"])


def test_a_field_group_declares_the_same_way(tmp_path):
    path = tmp_path / "ores.testcomp.pair_field_group.org"
    path.write_text("""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F2
:END:
#+title: ores.testcomp.pair_field_group
#+type: field_group
#+component: testcomp
#+entity_singular: pair
#+namespace: ores::testcomp::domain

A pair.

* Fields

** first
:PROPERTIES:
:cpp_type: std::string
:economic: true
:END:

The first.

** second
:PROPERTIES:
:cpp_type: std::string
:economic: false
:END:

The second.
""", encoding="utf-8")

    group = load_org_field_group_model(path)["field_group"]

    assert group["has_economic_declaration"] is True
    assert group["economic_fields"] == ["first"]


CATALOGUE_ENTITY = ENTITY.format(
    id_economic=":economic: false\n",
    amount_economic=":economic: true\n",
    reference_economic=":economic: false\n")


def _catalogue_in(tmp_path, entity_text, digest_rows, shared_rows=""):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True, exist_ok=True)
    (modeling / "ores.testcomp.widget.org").write_text(entity_text, encoding="utf-8")
    path = modeling / "ores.testcomp.trade_type_catalogue.org"
    path.write_text(f"""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F3
:END:
#+title: ores.testcomp.trade_type_catalogue
#+type: ores.codegen.trade_type_catalogue
#+component: testcomp

* Trade types

| code | description | product_type | has_options | has_extension | instrument |
|------+-------------+--------------+-------------+---------------+------------|
| Swap | Swap        | swap         | false       | false         | widget     |

* Digest

** Instrument components

| instrument | component |
|------------+-----------|
{digest_rows}

** Shared components

| component | reader |
|-----------+--------|
{shared_rows}
""", encoding="utf-8")
    return path


def test_the_catalogue_lists_the_rows_each_instrument_digest_folds(tmp_path):
    path = _catalogue_in(tmp_path, CATALOGUE_ENTITY, "| widget | widget |")

    catalogue = load_org_trade_type_catalogue_model(path)["trade_type_catalogue"]

    assert catalogue["digest"] == [
        {"instrument": "widget", "components": [{"entity": "widget"}]}]


def test_the_catalogue_lists_the_shared_rows_with_their_readers(tmp_path):
    path = _catalogue_in(
        tmp_path, CATALOGUE_ENTITY, "| widget | widget |",
        shared_rows="| widget | read_widgets_by_trade_ids |")

    catalogue = load_org_trade_type_catalogue_model(path)["trade_type_catalogue"]

    assert catalogue["digest_shared"] == [
        {"entity": "widget", "reader": "read_widgets_by_trade_ids"}]


def test_the_catalogue_refuses_a_reader_that_is_not_a_function_name(tmp_path):
    path = _catalogue_in(
        tmp_path, CATALOGUE_ENTITY, "| widget | widget |",
        shared_rows="| widget | read widgets |")

    with pytest.raises(ValueError, match="not a function name"):
        load_org_trade_type_catalogue_model(path)


def test_the_catalogue_refuses_a_routed_instrument_the_digest_omits(tmp_path):
    path = _catalogue_in(tmp_path, CATALOGUE_ENTITY, "| other | widget |")

    with pytest.raises(ValueError, match="no routed trade type|omits"):
        load_org_trade_type_catalogue_model(path)


def test_the_catalogue_refuses_a_component_that_declares_nothing(tmp_path):
    undeclared = ENTITY.format(id_economic="", amount_economic="", reference_economic="")
    path = _catalogue_in(tmp_path, undeclared, "| widget | widget |")

    with pytest.raises(ValueError, match="declares no ':economic:'"):
        load_org_trade_type_catalogue_model(path)
