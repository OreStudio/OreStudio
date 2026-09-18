"""Tests for the populate reference check.

The check only has value if it can fail. A populate script that looks up a
name no script defines aborts the database recreate, so these tests pin both
halves: the guard discovery must not come back empty, and removing a
definition must be reported.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_populate_references.py
"""
import shutil
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_populate_references as check  # noqa: E402

METHODOLOGY = "OreStudio Code Generation Methodology"


def parse_create():
    text = "\n".join(
        check.blank_comments(p.read_text())
        for p in sorted(check.CREATE_DIR.rglob("*.sql"))
    )
    return check.parse_functions(text)


def test_every_populate_reference_resolves():
    assert check.check() == []


def test_guard_discovery_is_not_vacuous():
    """A parser that finds no guards would pass every tree."""
    functions = parse_create()
    guards = {
        name: check.parse_guards(body) for name, (_, body) in functions.items()
    }
    assert guards["ores_dq_datasets_upsert_fn"] == [
        ("ores_dq_methodologies_tbl", [("name", "p_methodology_name")])
    ], "the single-column methodology lookup was not discovered"
    assert guards["ores_dq_tags_upsert_fn"] == [
        (
            "ores_dq_datasets_tbl",
            [
                ("name", "p_dataset_name"),
                ("subject_area_name", "p_subject_area_name"),
                ("domain_name", "p_domain_name"),
            ],
        )
    ], "the composite-key dataset lookup was not discovered"


def test_missing_definition_is_reported(tmp_path):
    populate = tmp_path / "populate"
    shutil.copytree(check.POPULATE_DIR, populate)
    definition = populate / "dq" / "dq_methodology_populate.sql"
    definition.write_text(definition.read_text().replace(f"'{METHODOLOGY}'", "NULL"))

    violations = check.check(populate_dir=populate)

    assert violations, "a tree with no methodology definition was reported clean"
    assert any(METHODOLOGY in message for _, _, message in violations)
    assert any(
        path.name == "acme_dataset_populate.sql" for path, _, _ in violations
    ), "the acme datasets that reference the name were not reported"
