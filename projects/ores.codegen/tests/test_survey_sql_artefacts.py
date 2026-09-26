"""Regression tests for the SQL artefact census lever.

``scripts/survey_sql_artefacts.py`` pairs an artefact table with the base
table it stages. That pairing is the whole value of the survey and it is
easy to get wrong: two earlier rules (prefix match, then suffix match)
both produced false findings before the entity-name rule replaced them.
The tests below lock the rule, the column parse that feeds it, and the
bucket each pairing lands in.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_survey_sql_artefacts.py
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
LEVER = REPO_ROOT / "projects" / "ores.codegen" / "scripts" / "survey_sql_artefacts.py"

spec = importlib.util.spec_from_file_location("survey_sql_artefacts", LEVER)
census = importlib.util.module_from_spec(spec)
sys.modules["survey_sql_artefacts"] = census
spec.loader.exec_module(census)


def table(columns, generated=False):
    return {"columns": columns, "generated": generated, "file": Path("x.sql")}


def products(**by_product):
    """A product map in the shape ``parse_all_products`` returns."""
    return {name: {t: table(cols) for t, cols in tables.items()} for name, tables in by_product.items()}


def test_entity_of_strips_the_product_and_component_prefix():
    assert census.entity_of("ores_dq_currencies_artefact_tbl") == "currencies"
    assert census.entity_of("ores_dq_currency_pair_conventions_artefact_tbl") == "currency_pair_conventions"


def test_a_base_is_paired_by_entity_name_across_products():
    prods = products(
        dq={"ores_dq_currencies_artefact_tbl": ["dataset_id"]},
        refdata={"ores_refdata_currencies_tbl": ["iso_code"]},
    )
    hits = census.find_bases("currencies", prods, "dq", "ores_dq_currencies_artefact_tbl")

    assert [h[1] for h in hits] == ["ores_refdata_currencies_tbl"]
    assert hits[0][0] == "refdata"


def test_a_shorter_entity_name_does_not_match_a_longer_table():
    """A suffix rule paired ``tags`` with ``image_tags``. The entity-name
    rule deconstructs the whole table, so the longer name cannot match."""
    prods = products(assets={"ores_assets_image_tags_tbl": ["image_id"]})
    hits = census.find_bases("tags", prods, "dq", "ores_dq_tags_artefact_tbl")

    assert hits == []


def test_the_staging_table_never_pairs_with_itself():
    prods = products(dq={"ores_dq_badge_definitions_artefact_tbl": ["code"]})
    hits = census.find_bases(
        "badge_definitions", prods, "dq", "ores_dq_badge_definitions_artefact_tbl"
    )

    assert hits == []


def test_a_local_base_is_reported_before_a_foreign_one():
    prods = products(
        dq={"ores_dq_thing_tbl": ["code"]},
        refdata={"ores_refdata_thing_tbl": ["code"]},
    )
    hits = census.find_bases("thing", prods, "dq", "ores_dq_thing_artefact_tbl")

    assert [h[0] for h in hits] == ["dq", "refdata"]


def test_columns_skip_constraints_and_survive_nested_parentheses():
    body = """
    "code" text not null,
    "name" text null,
    primary key (tenant_id, code),
    check ("valid_from" < "valid_to"),
    exclude using gist (tenant_id WITH =, tstzrange(a, b) WITH &&)
    """

    assert census.column_names(body) == ["code", "name"]


def test_generated_wins_over_every_other_bucket():
    art = table(["dataset_id"], generated=True)
    bucket, _ = census.classify(art, [], "dq")

    assert bucket == "generated"


def test_a_header_plus_base_columns_minus_audit_is_derivable():
    art = table(["dataset_id", "tenant_id", "version", "code", "name"])
    base = table(["code", "tenant_id", "version", "name", "modified_by", "valid_from"])

    bucket, _ = census.classify(art, [("dq", "ores_dq_x_tbl", base)], "dq")

    assert bucket == "derivable"


def test_a_base_in_another_product_is_foreign():
    art = table(["dataset_id", "tenant_id", "version", "code"])
    base = table(["code", "tenant_id", "version"])

    bucket, detail = census.classify(art, [("refdata", "ores_refdata_x_tbl", base)], "dq")

    assert bucket == "foreign"
    assert "ores_refdata_x_tbl" in detail


def test_a_column_the_base_does_not_carry_makes_the_pair_divergent():
    art = table(["dataset_id", "tenant_id", "version", "code", "holiday_calendar"])
    base = table(["code", "tenant_id", "version"])

    bucket, detail = census.classify(art, [("refdata", "ores_refdata_x_tbl", base)], "dq")

    assert bucket == "divergent"
    assert "holiday_calendar" in detail


def test_a_missing_column_makes_the_pair_divergent():
    art = table(["dataset_id", "tenant_id", "version", "code"])
    base = table(["code", "tenant_id", "version", "coding_scheme_code"])

    bucket, detail = census.classify(art, [("refdata", "ores_refdata_x_tbl", base)], "dq")

    assert bucket == "divergent"
    assert "coding_scheme_code" in detail


def test_no_base_at_all_is_an_orphan():
    bucket, detail = census.classify(table(["dataset_id", "code"]), [], "dq")

    assert bucket == "orphan"
    assert "no table of the matching entity name" in detail
