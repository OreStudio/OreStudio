"""Regression tests for the artefact-table consumer census.

``scripts/survey_table_consumers.py`` decides whether an artefact table is
live, and step 2 of the ores.dq clean-up deletes on that verdict. Two
relations were missed before the current rule, and both would have
understated liveness:

- a publish function reaches the table with ``join``, not ``from``, so
  ``ores_dq_lei_bic_artefact_tbl`` looked write-only;
- the artefact-type registry writes the table name short
  (``dq_currencies_artefact_tbl``), so no registration was found at all.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_survey_table_consumers.py
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
LEVER = REPO_ROOT / "projects" / "ores.codegen" / "scripts" / "survey_table_consumers.py"

spec = importlib.util.spec_from_file_location("survey_table_consumers", LEVER)
census = importlib.util.module_from_spec(spec)
sys.modules["survey_table_consumers"] = census
spec.loader.exec_module(census)

SQL = Path("projects/ores.sql/create/dq/x.sql")
DOC = Path("doc/x.org")
REGISTRY = Path("projects/ores.sql/populate/dq/dq_artefact_types_populate.sql")
CPP = Path("projects/ores.dq/core/src/x.cpp")


def test_a_full_name_gains_its_short_alias():
    assert census.aliases_for("ores_dq_currencies_artefact_tbl") == [
        "ores_dq_currencies_artefact_tbl",
        "dq_currencies_artefact_tbl",
    ]


def test_a_name_without_the_prefix_is_left_alone():
    assert census.aliases_for("dq_tags_artefact_tbl") == ["dq_tags_artefact_tbl"]


def test_create_table_is_a_definition():
    text = 'create table if not exists "ores_dq_x_tbl" ("code" text);'
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "defines"


def test_drop_table_is_a_drop():
    text = 'drop table if exists "ores_dq_x_tbl";'
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "drops"


def test_insert_into_is_a_write():
    text = 'insert into ores_dq_x_tbl (dataset_id) values (1);'
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "writes"


def test_update_is_a_write():
    text = 'update "ores_dq_x_tbl" set code = 1;'
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "writes"


def test_select_from_is_a_read():
    text = 'select * from ores_dq_x_tbl where dataset_id = 1;'
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "reads"


def test_join_is_a_read():
    """A publish function reaches the artefact table with JOIN."""
    text = 'select 1 from ores_refdata_bics_tbl b join ores_dq_x_tbl a on a.lei = b.lei;'
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "reads"


def test_the_registry_is_a_registration_not_a_write():
    text = "('dq_x_artefact_tbl', 'refdata_x_tbl', 'refdata.v1.x.publish-from-dq', 41,"
    assert census.relation(text, "dq_x_artefact_tbl", REGISTRY) == "registers"


def test_cpp_and_documents_are_recognised():
    assert census.relation("#include <x.hpp>", "ores_dq_x_tbl", CPP) == "cpp"
    assert census.relation("prose about ores_dq_x_tbl", "ores_dq_x_tbl", DOC) == "doc"


def test_a_bare_mention_is_not_proof_of_use():
    text = "-- ores_dq_x_tbl is mentioned here and nowhere else"
    assert census.relation(text, "ores_dq_x_tbl", SQL) == "mentions"


def test_defining_a_table_does_not_count_as_consuming_it():
    """Every table defines itself. Counting that as a consumer would make
    every table look live."""
    roles = {"defines": ["projects/ores.sql/create/dq/x.sql"], "drops": []}
    trimmed = census.own_file_mentions(roles, "ores_dq_x_tbl", "projects/ores.sql/create/dq/x.sql")

    assert trimmed == {}


def test_a_table_with_only_a_definition_is_a_deletion_candidate():
    assert census.verdict({}) == "DELETE CANDIDATE"
    assert census.verdict({"mentions": ["doc/x.org"]}) == "DELETE CANDIDATE"


def test_a_registration_alone_keeps_a_table():
    assert census.verdict({"registers": ["populate/dq/dq_artefact_types_populate.sql"]}).startswith("keep")
