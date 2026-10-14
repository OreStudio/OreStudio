"""Tests for the composite family model kind (``#+type: ores.codegen.family``).

Run::

    python3 -m pytest projects/ores.codegen/tests/test_family_model.py

A family is the protocol of a composite entity: one central row plus its parts.
It derives its records and its four message pairs -- get, get_many, put and
put_many -- from the central entity's own derived key record and from the part
entities its ``* Parts`` table names, and it renders both protocol twins from
that one derivation. These cases pin the derived shape, the load-time refusal
of a part that is not a full composite member, and the fact that the family
addresses its own subjects rather than the central entity's.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen import core  # noqa: E402
from codegen.core import load_model, resolve_output_path  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402
from codegen.manifest import _CODEGEN_ORG_TYPES  # noqa: E402
from codegen.org_loader import entity_protocol_messages  # noqa: E402

BASE_DIR = REPO_ROOT / "projects/ores.codegen"
CENTRAL = "convention"
PARTS = (("pay_receive", "tenor_basis_swap_pay_receive"),
         ("long_short", "tenor_basis_swap_long_short"))


def _central_model() -> str:
    return """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000001
:END:
#+title: ores.refdata.convention
#+type: ores.codegen.entity
#+component: refdata
#+entity_singular: convention
#+entity_plural: conventions
#+entity_title: Convention
#+brief: The central convention row.

* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: refdata
:subcomponent: api
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

Key.
"""


def _part_model(name: str, entity_plural: str, *, parent: str = CENTRAL,
                bump: str = "true", list_by: str = "true",
                list_by_uuid: str = "true", list_by_as_of: str = "true",
                foreign_keys: bool = True) -> str:
    fk = ""
    if foreign_keys:
        fk = f"""\
* Foreign keys

** {parent}_id
:PROPERTIES:
:table:               ores_refdata_{parent}s_tbl
:parent_entity:       {parent}
:nullable:            false
:error_message:       Invalid {parent}_id: %.
:list_by:             {list_by}
:list_by_uuid:        {list_by_uuid}
:bump_parent_version: {bump}
:list_by_as_of:       {list_by_as_of}
:END:
"""
    return f"""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000{abs(hash(name)) % 90 + 10:02d}
:END:
#+title: ores.refdata.{name}
#+type: ores.codegen.entity
#+component: refdata
#+entity_singular: {name}
#+entity_plural: {entity_plural}
#+entity_title: {name.replace('_', ' ').title()}
#+brief: A family part.

* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: refdata
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

Key.

** {parent}_id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:END:

The central row this part belongs to.

{fk}"""


def _family_model() -> str:
    rows = "\n".join(f"| {shim} | {entity} |" for shim, entity in PARTS)
    return f"""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000004
:END:
#+title: ores.refdata.tenor_basis_swap_convention
#+description: A composite family model.
#+type: ores.codegen.family
#+component: refdata
#+subcomponent: api
#+entity_singular: tenor_basis_swap_convention
#+entity_plural: tenor_basis_swap_conventions
#+entity_title: Tenor Basis Swap Convention
#+brief: A family.
+#+filetags: :model:family:refdata:

Prose.

* Flags
:PROPERTIES:
:central_entity: {CENTRAL}
:END:

* Parts

| part        | entity                       |
|-------------+------------------------------|
{rows}
"""


def _modeling_dir(tmp_path: Path, *, part_overrides=None,
                  central: str | None = None) -> Path:
    """Write a central entity, two parts and the family into a modeling dir.

    ``part_overrides`` maps a part entity name to the part model text to write
    instead of the default, so one part can be made defective.
    """
    overrides = part_overrides or {}
    (tmp_path / f"ores.refdata.{CENTRAL}.org").write_text(
        central if central is not None else _central_model(), encoding="utf-8")
    for _, entity in PARTS:
        text = overrides.get(entity)
        if text is None:
            text = _part_model(entity, entity + "s")
        (tmp_path / f"ores.refdata.{entity}.org").write_text(text, encoding="utf-8")
    family_path = tmp_path / "ores.refdata.tenor_basis_swap_convention.org"
    family_path.write_text(_family_model(), encoding="utf-8")
    return family_path


def _messages(family: dict) -> dict:
    return {message["name"]: message for message in family["messages"]}


def _fields(message: dict) -> dict:
    return {field["name"]: field["cpp_type"] for field in message["fields"]}


def test_family_loads_and_derives_the_records_and_the_eight_messages(tmp_path):
    family = load_model(_modeling_dir(tmp_path))["family"]

    assert [message["name"] for message in family["messages"]] == [
        "tenor_basis_swap_convention_key",
        "tenor_basis_swap_convention_item",
        "tenor_basis_swap_convention_write",
        "get_tenor_basis_swap_convention_request",
        "get_tenor_basis_swap_convention_response",
        "get_many_tenor_basis_swap_conventions_request",
        "get_many_tenor_basis_swap_conventions_response",
        "put_tenor_basis_swap_convention_request",
        "put_tenor_basis_swap_convention_response",
        "put_many_tenor_basis_swap_conventions_request",
        "put_many_tenor_basis_swap_conventions_response",
    ]
    # Four request/response pairs, so eight messages; only the requests
    # address a subject, exactly as the entity derivation states them.
    addressable = [m for m in family["messages"] if m.get("subject")]
    assert len(family["messages"]) == 11
    assert len(addressable) == 4

    messages = _messages(family)
    # The key is copied from the central entity's own derived key record.
    assert _fields(messages["tenor_basis_swap_convention_key"]) == {
        "id": "boost::uuids::uuid"}
    # The item is the family at one instant, in field order.
    assert list(_fields(messages["tenor_basis_swap_convention_item"]).items()) == [
        ("key", "tenor_basis_swap_convention_key"),
        ("convention", "std::optional<ores::refdata::domain::convention>"),
        ("pay_receive",
         "std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive>"),
        ("long_short",
         "std::vector<ores::refdata::domain::tenor_basis_swap_long_short>"),
    ]
    # The write is the central change plus one array of part changes per part.
    assert list(_fields(messages["tenor_basis_swap_convention_write"]).items()) == [
        ("change", "convention_change"),
        ("pay_receive", "std::vector<tenor_basis_swap_pay_receive_change>"),
        ("long_short", "std::vector<tenor_basis_swap_long_short_change>"),
    ]

    assert _fields(messages["get_tenor_basis_swap_convention_request"]) == {
        "key": "tenor_basis_swap_convention_key"}
    assert _fields(messages["get_tenor_basis_swap_convention_response"]) == {
        "result": "ores::utility::domain::result",
        "item": "std::optional<tenor_basis_swap_convention_item>"}
    assert _fields(messages["get_many_tenor_basis_swap_conventions_request"]) == {
        "keys": "std::vector<tenor_basis_swap_convention_key>"}
    assert _fields(messages["get_many_tenor_basis_swap_conventions_response"]) == {
        "result": "ores::utility::domain::result",
        "items": "std::vector<tenor_basis_swap_convention_item>"}
    assert _fields(messages["put_tenor_basis_swap_convention_request"]) == {
        "item": "tenor_basis_swap_convention_write",
        "intent": "ores::utility::domain::change_intent"}
    assert _fields(messages["put_tenor_basis_swap_convention_response"]) == {
        "result": "ores::utility::domain::result",
        "item": "std::optional<tenor_basis_swap_convention_item>"}
    assert _fields(messages["put_many_tenor_basis_swap_conventions_request"]) == {
        "items": "std::vector<tenor_basis_swap_convention_write>",
        "intent": "ores::utility::domain::change_intent"}
    assert _fields(messages["put_many_tenor_basis_swap_conventions_response"]) == {
        "result": "ores::utility::domain::result",
        "items": "std::vector<tenor_basis_swap_convention_item>"}


def test_each_message_pair_names_its_response_and_the_relation_is_typed(tmp_path):
    messages = _messages(load_model(_modeling_dir(tmp_path))["family"])
    for request, response in (
        ("get_tenor_basis_swap_convention_request",
         "get_tenor_basis_swap_convention_response"),
        ("get_many_tenor_basis_swap_conventions_request",
         "get_many_tenor_basis_swap_conventions_response"),
        ("put_tenor_basis_swap_convention_request",
         "put_tenor_basis_swap_convention_response"),
        ("put_many_tenor_basis_swap_conventions_request",
         "put_many_tenor_basis_swap_conventions_response"),
    ):
        assert messages[request]["response_type"] == response
        assert messages[request]["requires_session"] == "true"


def test_the_family_key_is_the_central_entitys_own_key(tmp_path):
    """The family addresses its central row by the key the central entity's
    generic operations address it by, so a key declared as text stays text."""
    central = _central_model().replace(
        """:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true""",
        """:type:        text
:cpp_type:    std::string
:primary_key: true""")
    family = load_model(_modeling_dir(tmp_path, central=central))["family"]
    messages = _messages(family)
    assert _fields(messages["tenor_basis_swap_convention_key"]) == {
        "id": "std::string"}


@pytest.mark.parametrize("flag", [
    "bump_parent_version", "list_by", "list_by_uuid", "list_by_as_of"])
def test_a_part_missing_a_composite_flag_fails_the_load(tmp_path, flag):
    kwarg = {"bump_parent_version": "bump", "list_by": "list_by",
             "list_by_uuid": "list_by_uuid", "list_by_as_of": "list_by_as_of"}[flag]
    broken = _part_model("tenor_basis_swap_pay_receive",
                         "tenor_basis_swap_pay_receives", **{kwarg: "false"})
    path = _modeling_dir(tmp_path, part_overrides={
        "tenor_basis_swap_pay_receive": broken})
    with pytest.raises(ValueError) as exc:
        load_model(path)
    message = str(exc.value)
    assert "tenor_basis_swap_convention" in message
    assert "pay_receive" in message
    assert "tenor_basis_swap_pay_receive" in message
    assert f":{flag}:" in message


def test_a_part_with_no_foreign_key_to_the_central_entity_fails_the_load(tmp_path):
    broken = _part_model("tenor_basis_swap_pay_receive",
                         "tenor_basis_swap_pay_receives", foreign_keys=False)
    path = _modeling_dir(tmp_path, part_overrides={
        "tenor_basis_swap_pay_receive": broken})
    with pytest.raises(ValueError) as exc:
        load_model(path)
    message = str(exc.value)
    assert "pay_receive" in message
    assert "tenor_basis_swap_pay_receive" in message
    assert ":parent_entity:" in message
    assert CENTRAL in message


def test_a_part_whose_foreign_key_points_at_another_entity_fails_the_load(tmp_path):
    broken = _part_model("tenor_basis_swap_pay_receive",
                         "tenor_basis_swap_pay_receives", parent="other")
    path = _modeling_dir(tmp_path, part_overrides={
        "tenor_basis_swap_pay_receive": broken})
    with pytest.raises(ValueError) as exc:
        load_model(path)
    assert "pay_receive" in str(exc.value)
    assert ":parent_entity:" in str(exc.value)


def test_a_family_without_a_central_entity_fails_the_load(tmp_path):
    path = _modeling_dir(tmp_path)
    text = path.read_text(encoding="utf-8").replace(
        ":central_entity: convention\n", "")
    path.write_text(text, encoding="utf-8")
    with pytest.raises(ValueError) as exc:
        load_model(path)
    assert ":central_entity:" in str(exc.value)


def test_the_family_subjects_do_not_collide_with_the_central_entitys(tmp_path):
    family = load_model(_modeling_dir(tmp_path))["family"]
    family_subjects = {m["subject"] for m in family["messages"]
                       if m.get("subject")}
    assert family_subjects == {
        "refdata.v1.tenor_basis_swap_conventions.get",
        "refdata.v1.tenor_basis_swap_conventions.get_many",
        "refdata.v1.tenor_basis_swap_conventions.put",
        "refdata.v1.tenor_basis_swap_conventions.put_many",
    }

    central = load_model(tmp_path / f"ores.refdata.{CENTRAL}.org")["domain_entity"]
    central_subjects = {m["subject"] for m in entity_protocol_messages(central)
                        if m.get("subject")}
    assert central_subjects
    assert family_subjects.isdisjoint(central_subjects)


def test_both_protocol_twins_render_the_same_family(tmp_path):
    path = _modeling_dir(tmp_path)
    out = tmp_path / "out"
    out.mkdir()
    core.generate_from_model(
        str(path), BASE_DIR / "library/data", BASE_DIR / "library/templates",
        out, target_template="cpp_protocol.hpp.mustache", target_output="family.hpp")
    core.generate_from_model(
        str(path), BASE_DIR / "library/data", BASE_DIR / "library/templates",
        out, target_template="ts_protocol.ts.mustache", target_output="family.ts")
    header = (out / "family.hpp").read_text(encoding="utf-8")
    twin = (out / "family.ts").read_text(encoding="utf-8")

    for subject in ("refdata.v1.tenor_basis_swap_conventions.get",
                    "refdata.v1.tenor_basis_swap_conventions.get_many",
                    "refdata.v1.tenor_basis_swap_conventions.put",
                    "refdata.v1.tenor_basis_swap_conventions.put_many"):
        assert subject in header
        assert subject in twin

    assert "struct tenor_basis_swap_convention_item {" in header
    assert ("std::optional<ores::refdata::domain::convention> convention;"
            in header)
    assert ("std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive> "
            "pay_receive;" in header)
    assert "struct tenor_basis_swap_convention_write {" in header
    assert "convention_change change;" in header
    assert ('#include "ores.refdata.api/messaging/convention_protocol.hpp"'
            in header)

    assert "export interface TenorBasisSwapConventionItem {" in twin
    assert "convention: Convention | null;" in twin
    assert "pay_receive: TenorBasisSwapPayReceive[];" in twin
    assert "export interface TenorBasisSwapConventionWrite {" in twin
    assert "change: ConventionChange;" in twin
    assert ("import type { ConventionChange } from './convention_protocol.js';"
            in twin)
    assert ("import type { Convention } from '../domain/convention.js';"
            in twin)


def test_resolve_targets_reaches_both_protocol_facets(tmp_path):
    """A family that names neither facet would silently generate nothing."""
    path = _modeling_dir(tmp_path)
    units, model_type, _ = resolve_targets(path, BASE_DIR)
    assert model_type == "family"
    assert sorted(unit["template"] for unit in units) == [
        "cpp_protocol.hpp.mustache", "ts_protocol.ts.mustache"]
    outputs = {unit["template"]: unit["output"] for unit in units}
    # The family writes its own protocol file, never the central entity's.
    assert outputs["cpp_protocol.hpp.mustache"].endswith(
        "/messaging/tenor_basis_swap_convention_protocol.hpp")
    assert outputs["ts_protocol.ts.mustache"].endswith(
        "/refdata/protocol/tenor_basis_swap_convention_protocol.ts")


def test_the_family_type_is_registered_everywhere_a_model_type_is(tmp_path):
    assert core._ORG_TYPE_TO_MODEL_TYPE["ores.codegen.family"] == "family"
    assert "ores.codegen.family" in _CODEGEN_ORG_TYPES
    assert core.get_model_type(
        "ores.refdata.tenor_basis_swap_convention.org",
        _modeling_dir(tmp_path)) == "family"


def test_resolve_output_path_names_the_family_file(tmp_path):
    family = load_model(_modeling_dir(tmp_path))
    resolved = resolve_output_path(
        "projects/{component_dir}/include/ores.{component_include}/messaging/"
        "{entity}_protocol.hpp", family, "family")
    assert resolved == ("projects/ores.refdata/api/include/ores.refdata.api/"
                        "messaging/tenor_basis_swap_convention_protocol.hpp")
