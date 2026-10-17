# -*- coding: utf-8 -*-
"""Tests for the generated convention family artefacts and their drift check.

The registry header and the apply fragment are derived from the refdata entity
models that declare ``#+convention_family: <key>``. Scope comes from the
model's ``party_id`` column and the write mode from its scope or from
``#+convention_write: by_id``. The check fails on a model the registry does not
list, on an entry whose model is gone, and on either artefact edited by hand.
"""

import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.convention_family_registry import (  # noqa: E402
    FRAGMENT_RELATIVE_PATH,
    HEADER_RELATIVE_PATH,
    INCLUDES_RELATIVE_PATH,
    discover_entries,
    parse_header,
    render_fragment,
    render_header,
    render_includes,
)

SCRIPT = (
    REPO_ROOT
    / "projects/ores.codegen/scripts/check_convention_family_registry.py"
)
spec = importlib.util.spec_from_file_location(
    "check_convention_family_registry", SCRIPT
)
check = importlib.util.module_from_spec(spec)
sys.modules["check_convention_family_registry"] = check
spec.loader.exec_module(check)


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


def _write_artefacts(root: Path, entries) -> None:
    header = root / HEADER_RELATIVE_PATH
    header.parent.mkdir(parents=True, exist_ok=True)
    header.write_text(render_header(entries), encoding="utf-8")
    fragment = root / FRAGMENT_RELATIVE_PATH
    fragment.parent.mkdir(parents=True, exist_ok=True)
    fragment.write_text(render_fragment(entries), encoding="utf-8")
    includes = root / INCLUDES_RELATIVE_PATH
    includes.parent.mkdir(parents=True, exist_ok=True)
    includes.write_text(render_includes(entries), encoding="utf-8")


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


def test_a_fresh_render_passes_the_check(tmp_path):
    root = _tree(tmp_path, THREE)
    _write_artefacts(root, discover_entries(root))

    assert check.check(root) == []


def test_a_declared_entity_missing_from_the_registry_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    entries = discover_entries(root)
    _write_artefacts(root, [entries[1]])

    problems = check.check(root)

    assert any(
        "ibor_index_convention" in problem and "missing from the registry" in problem
        for problem in problems
    )


def test_a_registry_entry_whose_entity_is_gone_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_artefacts(root, discover_entries(root))
    (root / "projects/ores.refdata/modeling/ores.refdata.ibor_index_convention.org").unlink()

    problems = check.check(root)

    assert any(
        "registry entry ibor_index_convention" in problem
        and "no longer declares" in problem
        for problem in problems
    )


def test_a_scope_change_without_regeneration_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_artefacts(root, discover_entries(root))
    modeling = root / "projects/ores.refdata/modeling"
    (modeling / "ores.refdata.ois_convention.org").write_text(
        _model("ois_convention", "ois", party_id=False), encoding="utf-8"
    )

    problems = check.check(root)

    assert any(
        "ois_convention" in problem and "party_scoped" in problem
        for problem in problems
    )


def test_a_write_mode_change_without_regeneration_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_artefacts(root, discover_entries(root))
    modeling = root / "projects/ores.refdata/modeling"
    (modeling / "ores.refdata.ois_convention.org").write_text(
        _model("ois_convention", "ois", party_id=True, write="by_id"),
        encoding="utf-8",
    )

    problems = check.check(root)

    assert any(
        "ois_convention" in problem and "write_mode" in problem
        for problem in problems
    )


def test_a_missing_fragment_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_artefacts(root, discover_entries(root))
    (root / FRAGMENT_RELATIVE_PATH).unlink()

    problems = check.check(root)

    assert problems == [f"{FRAGMENT_RELATIVE_PATH} is missing"]


def test_a_hand_edited_fragment_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_artefacts(root, discover_entries(root))
    fragment = root / FRAGMENT_RELATIVE_PATH
    fragment.write_text(
        fragment.read_text(encoding="utf-8") + "// edited by hand\n",
        encoding="utf-8",
    )

    problems = check.check(root)

    assert any(
        str(FRAGMENT_RELATIVE_PATH) in problem and "differs" in problem
        for problem in problems
    )


def test_the_fragment_names_one_helper_call_per_family(tmp_path):
    root = _tree(tmp_path, THREE)
    text = render_fragment(discover_entries(root))

    assert "replace_party_rows(ctx,\n                       ois_convention_repository()," in text
    assert "replace_rows_by_id(ctx,\n                       convention_repository()," in text
    assert "add_missing_world_rows(ctx,\n                           ibor_index_convention_repository()," in text
    assert text.count("replace_party_rows(ctx,") == 2
    assert text.count("replace_rows_by_id(ctx,") == 2
    assert text.count("add_missing_world_rows(ctx,") == 2
    assert text.count(".read_latest(ctx);") == 3
    assert text.count("_repository()") == 9


def test_the_fragment_emits_one_staged_branch_per_family(tmp_path):
    root = _tree(tmp_path, THREE)
    text = render_fragment(discover_entries(root))

    assert text.count("bool apply_staged_family(") == 1
    assert text.count('if (family == "convention")') == 1
    assert text.count('if (family == "ibor_index")') == 1
    assert text.count('if (family == "ois")') == 1
    assert text.count("rfl::json::read<") == 3
    assert text.count("return false;") == 1
    assert (
        "        add_missing_world_rows(ctx,\n"
        "                               ibor_index_convention_repository(),\n"
        "                               std::move(rows),\n"
        "                               result.world_kept);" in text
    )


def test_the_header_states_every_field_the_entry_carries(tmp_path):
    root = _tree(tmp_path, THREE)
    text = render_header(discover_entries(root))

    stated = parse_header(text)

    assert len(stated) == 3
    assert [entry.write_mode for entry in stated] == ["by_id", "world", "party"]
    assert stated[1].subject_stem == "refdata.v1.ibor_index_conventions"
