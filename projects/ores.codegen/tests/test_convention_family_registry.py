# -*- coding: utf-8 -*-
"""Tests for the generated convention family registry and its drift check.

The registry is derived from the refdata entity models that declare
``#+convention_family: <key>``; scope comes from the model's ``party_id``
column. The check fails on a model the registry does not list and on an entry
whose model is gone.
"""

import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.convention_family_registry import (  # noqa: E402
    HEADER_RELATIVE_PATH,
    discover_entries,
    parse_header,
    render_header,
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


def _model(entity: str, key: str, *, party_id: bool = True) -> str:
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
    return f"""\
#+title: ores.refdata.{entity}
#+type: ores.codegen.entity
#+component: refdata
#+entity_singular: {entity}
#+entity_plural: {entity}s
#+entity_title: {entity}
#+convention_family: {key}
#+brief: A convention family.

* Columns

** id
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Key.
{party}"""


def _tree(tmp_path: Path, models: dict[str, tuple[str, bool]]) -> Path:
    """Write each entity model into a scratch project root."""
    modeling = tmp_path / "projects/ores.refdata/modeling"
    modeling.mkdir(parents=True, exist_ok=True)
    for entity, (key, party_id) in models.items():
        (modeling / f"ores.refdata.{entity}.org").write_text(
            _model(entity, key, party_id=party_id), encoding="utf-8"
        )
    return tmp_path


def _write_header(root: Path, entries) -> None:
    header = root / HEADER_RELATIVE_PATH
    header.parent.mkdir(parents=True, exist_ok=True)
    header.write_text(render_header(entries), encoding="utf-8")


TWO = {
    "ois_convention": ("ois", True),
    "ibor_index_convention": ("ibor_index", False),
}


def test_scope_comes_from_the_party_id_column(tmp_path):
    entries = discover_entries(_tree(tmp_path, TWO))

    assert [(entry.key, entry.entity, entry.party_scoped) for entry in entries] == [
        ("ibor_index", "ibor_index_convention", False),
        ("ois", "ois_convention", True),
    ]
    assert entries[1].domain_type == "ores::refdata::domain::ois_convention"
    assert entries[1].subject_stem == "refdata.v1.ois_conventions"


def test_a_fresh_render_passes_the_check(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_header(root, discover_entries(root))

    assert check.check(root) == []


def test_a_declared_entity_missing_from_the_registry_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    entries = discover_entries(root)
    _write_header(root, [entries[1]])

    problems = check.check(root)

    assert any(
        "ibor_index_convention" in problem and "missing from the registry" in problem
        for problem in problems
    )


def test_a_registry_entry_whose_entity_is_gone_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_header(root, discover_entries(root))
    (root / "projects/ores.refdata/modeling/ores.refdata.ibor_index_convention.org").unlink()

    problems = check.check(root)

    assert any(
        "registry entry ibor_index_convention" in problem
        and "no longer declares" in problem
        for problem in problems
    )


def test_a_scope_change_without_regeneration_fails(tmp_path):
    root = _tree(tmp_path, TWO)
    _write_header(root, discover_entries(root))
    modeling = root / "projects/ores.refdata/modeling"
    (modeling / "ores.refdata.ois_convention.org").write_text(
        _model("ois_convention", "ois", party_id=False), encoding="utf-8"
    )

    problems = check.check(root)

    assert any(
        "ois_convention" in problem and "party_scoped" in problem
        for problem in problems
    )


def test_the_header_states_every_field_the_entry_carries(tmp_path):
    root = _tree(tmp_path, TWO)
    text = render_header(discover_entries(root))

    stated = parse_header(text)

    assert len(stated) == 2
    assert stated[0].subject_stem == "refdata.v1.ibor_index_conventions"
