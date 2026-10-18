# -*- coding: utf-8 -*-
"""Tests for the convention family registry and the templates that render it.

The registry is derived from the refdata entity models that declare
``#+convention_family: <key>``. Scope comes from the model's ``party_id``
column and the write mode from its scope or from ``#+convention_write: by_id``.
The derivation and the rendering are tested apart: the derivation decides what
the families are, and the three templates of the
``ores.cpp.convention-registry`` facet decide how they are written.
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
from codegen.core import convention_family_context, render_template  # noqa: E402

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


def test_the_context_groups_each_family_by_its_write_mode(tmp_path):
    context = convention_family_context(_tree(tmp_path, THREE))

    assert [family["key"] for family in context["families"]] == [
        "convention", "ibor_index", "ois"]
    assert [family["key"] for family in context["party"]] == ["ois"]
    assert [family["key"] for family in context["by_id"]] == ["convention"]
    assert [family["key"] for family in context["world"]] == ["ibor_index"]
    assert context["repositories"] == [
        {"repository": "convention_repository"},
        {"repository": "ibor_index_convention_repository"},
        {"repository": "ois_convention_repository"},
    ]


def test_the_header_states_every_family_the_models_declare(tmp_path):
    root = _tree(tmp_path, THREE)

    text = _render("cpp_convention_family_registry.hpp.mustache", root)

    stated = parse_header(text)
    assert [entry.key for entry in stated] == ["convention", "ibor_index", "ois"]
    assert [entry.write_mode for entry in stated] == ["by_id", "world", "party"]
    assert stated[1].subject_stem == "refdata.v1.ibor_index_conventions"
    assert stated[2].domain_type == "ores::refdata::domain::ois_convention"


def test_the_apply_fragment_calls_one_helper_per_family_in_its_mode(tmp_path):
    root = _tree(tmp_path, THREE)

    text = _render("cpp_convention_family_apply.inc.mustache", root)

    assert text.count("replace_party_rows(ctx,") == 2
    assert text.count("replace_rows_by_id(ctx,") == 2
    assert text.count("add_missing_world_rows(ctx,") == 2
    assert text.count(".read_latest(ctx);") == 3
    assert text.count("_repository()") == 12
    # Two dispatchers branch on the family, the apply and the payload read.
    assert text.count('if (family == "ois")') == 2
    assert text.count("return false;") == 1

    # Each payload branch reads the repository of the family it names, so a
    # rename that pairs a key with the wrong table fails here rather than in a
    # test that only counts branches.
    reader = text.split("read_family_payloads(")[1]
    branches = re.findall(
        r'if \(family == "([a-z_]+)"\) \{\s*'
        r'std::vector<std::string> payloads;\s*'
        r'for \(const auto& row : ([a-z_]+)_repository\(\)\.read_latest\(ctx\)\)\s*'
        r'payloads\.push_back\(rfl::json::write\(row\)\);\s*'
        r'return payloads;\s*\}',
        reader,
    )
    assert branches == [
        ("convention", "convention"),
        ("ibor_index", "ibor_index_convention"),
        ("ois", "ois_convention"),
    ]
    assert 'throw std::logic_error' in reader


def test_the_includes_name_each_repository_once_in_order(tmp_path):
    root = _tree(tmp_path, THREE)

    text = _render("cpp_convention_family_apply_includes.inc.mustache", root)

    includes = [
        line for line in text.splitlines() if line.startswith("#include")
    ]
    assert includes == [
        '#include "ores.refdata.core/repository/convention_repository.hpp"',
        '#include "ores.refdata.core/repository/ibor_index_convention_repository.hpp"',
        '#include "ores.refdata.core/repository/ois_convention_repository.hpp"',
    ]
