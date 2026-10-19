# -*- coding: utf-8 -*-
"""Tests for the convention family registry and the templates that render it.

The registry is derived from the refdata entity models that declare
``#+convention_family: <key>``. Scope comes from the model's ``party_id``
column and the write mode from its scope or from ``#+convention_write: by_id``.
The derivation and the rendering are tested apart: the derivation decides what
the families are, and the templates of the ``ores.cpp.convention-registry`` and
``ores.cpp.convention-family`` facets decide how they are written.
"""

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.convention_family_registry import (  # noqa: E402
    discover_entries,
    parse_header,
)
from codegen.core import (  # noqa: E402
    convention_family_context,
    convention_unit_context,
    render_template,
)

TEMPLATES = REPO_ROOT / "projects/ores.codegen/library/templates"


def _model(
    entity: str, key: str, *, party_id: bool = True, write: str | None = None
) -> str:
    party = ""
    if party_id:
        party = """
** party_id
:PROPERTIES:
:type: uuid
:cpp_type: boost::uuids::uuid
:END:

The owning party.
"""
    declaration = f"#+convention_family: {key}\n"
    if write:
        declaration += f"#+convention_write: {write}\n"
    return f"""\
#+title: ores.refdata.{entity}
#+type: ores.codegen.entity
#+component: refdata
#+entity_singular: {entity}
#+entity_plural: {entity}s
#+entity_title: {entity}
{declaration}#+brief: A convention family.

* Columns

** id
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Key.
{party}"""


def _tree(tmp_path: Path, models: dict[str, tuple]) -> Path:
    """Write each entity model into a scratch project root."""
    modeling = tmp_path / "projects/ores.refdata/modeling"
    modeling.mkdir(parents=True, exist_ok=True)
    for entity, declaration in models.items():
        key, party_id = declaration[0], declaration[1]
        write = declaration[2] if len(declaration) > 2 else None
        (modeling / f"ores.refdata.{entity}.org").write_text(
            _model(entity, key, party_id=party_id, write=write), encoding="utf-8"
        )
    return tmp_path


def _render(template_name: str, project_root: Path) -> str:
    """Render one registry template the way the generator does."""
    return render_template(
        str(TEMPLATES / template_name),
        {
            "cpp_license": "",
            "convention_registry": convention_family_context(project_root),
        },
    )


TWO = {
    "ois_convention": ("ois", True),
    "ibor_index_convention": ("ibor_index", False),
}

THREE = {
    "convention": ("convention", True, "by_id"),
    "ois_convention": ("ois", True),
    "ibor_index_convention": ("ibor_index", False),
}


def test_scope_and_write_mode_follow_from_the_model(tmp_path):
    entries = discover_entries(_tree(tmp_path, TWO))

    assert [
        (entry.key, entry.entity, entry.party_scoped, entry.write_mode)
        for entry in entries
    ] == [
        ("ibor_index", "ibor_index_convention", False, "world"),
        ("ois", "ois_convention", True, "party"),
    ]
    assert entries[1].domain_type == "ores::refdata::domain::ois_convention"
    assert entries[1].subject_stem == "refdata.v1.ois_conventions"


def test_a_declared_write_mode_overrides_the_scope_default(tmp_path):
    entries = discover_entries(_tree(tmp_path, THREE))

    by_key = {entry.key: entry for entry in entries}
    assert by_key["convention"].party_scoped is True
    assert by_key["convention"].write_mode == "by_id"
    assert by_key["ois"].write_mode == "party"


def test_world_is_derived_and_cannot_be_declared(tmp_path):
    try:
        discover_entries(
            _tree(tmp_path, {"ois_convention": ("ois", True, "world")})
        )
    except ValueError as error:
        assert "world follows from the absence of a party_id column" in str(error)
    else:
        raise AssertionError("a declared world write mode must be rejected")


def test_an_unknown_write_mode_is_rejected(tmp_path):
    try:
        discover_entries(
            _tree(tmp_path, {"ois_convention": ("ois", True, "upsert")})
        )
    except ValueError as error:
        assert "not one of party, by_id" in str(error)
    else:
        raise AssertionError("an unknown write mode must be rejected")


def test_the_context_lists_each_family_with_its_write_mode_flags(tmp_path):
    context = convention_family_context(_tree(tmp_path, THREE))

    assert [family["key"] for family in context["families"]] == [
        "convention", "ibor_index", "ois"]
    assert [
        (family["is_party"], family["is_by_id"], family["is_world"])
        for family in context["families"]
    ] == [(False, True, False), (False, False, True), (True, False, False)]


def test_a_unit_context_is_the_one_family_the_entity_is(tmp_path):
    root = _tree(tmp_path, THREE)

    unit = convention_unit_context(root, "ibor_index_convention")

    assert unit["key"] == "ibor_index"
    assert unit["is_world"] is True
    try:
        convention_unit_context(root, "no_such_convention")
    except ValueError as error:
        assert "not a convention family entity" in str(error)
    else:
        raise AssertionError("an entity that is not a family has no unit")


def test_the_header_states_every_family_the_models_declare(tmp_path):
    root = _tree(tmp_path, THREE)

    text = _render("cpp_convention_family_registry.hpp.mustache", root)

    stated = parse_header(text)
    assert [entry.key for entry in stated] == ["convention", "ibor_index", "ois"]
    assert [entry.write_mode for entry in stated] == ["by_id", "world", "party"]
    assert stated[1].subject_stem == "refdata.v1.ibor_index_conventions"
    assert stated[2].domain_type == "ores::refdata::domain::ois_convention"


def test_the_header_ties_each_key_to_its_row(tmp_path):
    text = _render("cpp_convention_family_registry.hpp.mustache", _tree(tmp_path, THREE))

    assert "enum class convention_family_key : std::size_t {" in text
    for key in ("convention", "ibor_index", "ois"):
        assert re.search(
            r"static_assert\(convention_families\[static_cast<std::size_t>\("
            rf"convention_family_key::{key}\)\]\.key\s*==\s*\"{key}\"\);",
            text,
        )


def test_the_units_header_declares_one_pair_per_family(tmp_path):
    text = _render("cpp_convention_family_units.hpp.mustache", _tree(tmp_path, THREE))

    applies = re.findall(r"void apply_([a-z_]+)\(", text)
    reads = re.findall(r"read_([a-z_]+)_payloads\(", text)
    assert applies == reads == ["convention", "ibor_index", "ois"]
    # The declarations take and return strings alone, so the header names no
    # family type.
    assert "ores::refdata::domain" not in text


def test_the_dispatch_has_one_row_per_family_and_names_no_family_type(tmp_path):
    text = _render("cpp_convention_family_dispatch.cpp.mustache", _tree(tmp_path, THREE))

    rows = re.findall(
        r'\{"([a-z_]+)", &convention_family::apply_([a-z_]+), '
        r"&convention_family::read_([a-z_]+)_payloads\}",
        text,
    )
    assert rows == [(key, key, key) for key in ("convention", "ibor_index", "ois")]
    assert "static_assert(std::size(units) == domain::convention_family_count" in text
    assert "rfl::" not in text
    assert "ores::refdata::domain::" not in text.replace(
        "domain::convention_families", "").replace("domain::convention_family_count", "")
    assert "std::logic_error" in text


def _render_unit(root: Path, entity: str) -> str:
    return render_template(
        str(TEMPLATES / "cpp_convention_family_unit.cpp.mustache"),
        {"cpp_license": "", "convention_unit": convention_unit_context(root, entity)},
    )


def test_a_unit_writes_its_family_in_the_mode_the_registry_states(tmp_path):
    root = _tree(tmp_path, THREE)

    party = _render_unit(root, "ois_convention")
    by_id = _render_unit(root, "convention")
    world = _render_unit(root, "ibor_index_convention")

    assert "replace_party_rows(ctx," in party
    assert "replace_rows_by_id" not in party
    assert "replace_rows_by_id(ctx," in by_id
    assert "replace_party_rows" not in by_id
    assert "add_missing_world_rows(ctx," in world
    assert "result.world_kept" in world
    # Only the world mode reads the result, so the others mark it unused.
    assert "[[maybe_unused]] conventions_apply_result& result" in party
    assert "[[maybe_unused]]" not in world


def test_a_unit_reflects_its_own_type_and_includes_its_own_repository(tmp_path):
    text = _render_unit(_tree(tmp_path, THREE), "ois_convention")

    assert text.count("rfl::json::read<ores::refdata::domain::ois_convention>") == 1
    assert text.count("ores::refdata::domain::") == text.count(
        "ores::refdata::domain::ois_convention")
    includes = [line for line in text.splitlines() if "repository/" in line]
    assert includes == [
        '#include "ores.refdata.core/repository/ois_convention_repository.hpp"'
    ]
    assert "void apply_ois(" in text
    assert "std::vector<std::string> read_ois_payloads(" in text
