"""Tests for the asset-class catalogue: the one declaration of both lists.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_asset_class_catalogue.py

The catalogue is the single source for the refdata product taxonomy, the
oresmd market-data namespace and the mapping between them. The mapping
must state the authority-to-class relationship the classifier reads, and
nothing else notices when it does not.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    load_org_asset_class_catalogue_model,
)

CATALOGUE = (
    REPO_ROOT / "projects/ores.refdata/modeling"
    / "ores.refdata.asset_class_catalogue.org"
)


def _catalogue():
    return load_org_asset_class_catalogue_model(CATALOGUE)["asset_class_catalogue"]


def test_the_taxonomy_holds_the_seven_product_classes_in_display_order():
    taxonomy = _catalogue()["taxonomy"]
    assert [entry["code"] for entry in taxonomy] == [
        "fx", "interest_rates", "credit", "equity", "commodity", "inflation",
        "bond"]
    assert [entry["display_order"] for entry in taxonomy] == list(range(1, 8))
    # Every class carries the prose the seed SQL used to hold on its own.
    assert all(len(entry["description"]) > 200 for entry in taxonomy)
    assert taxonomy[0]["name"] == "FX"


def test_the_namespace_holds_the_twelve_authorities():
    namespace = _catalogue()["namespace"]
    assert [entry["authority"] for entry in namespace] == [
        "ir", "credit", "equity", "commodity", "fx", "inflation",
        "correlation", "security", "shape_profile", "rating", "power",
        "generic"]


def test_the_mapping_states_the_authority_to_class_relationship():
    mapping = _catalogue()["authority_to_class"]
    # Two concepts under two spellings.
    assert mapping["ir"] == "interest_rates"
    assert mapping["security"] == "bond"
    # The namespace-only authorities: no class of their own, or a class
    # reached through the taxonomy rather than the spelling.
    assert mapping["power"] == "commodity"
    assert mapping["shape_profile"] == "commodity"
    assert mapping["rating"] == "credit"
    assert mapping["correlation"] == ""
    assert mapping["generic"] == ""


def test_every_taxonomy_class_the_mapping_names_is_a_taxonomy_row():
    catalogue = _catalogue()
    codes = {entry["code"] for entry in catalogue["taxonomy"]}
    named = {
        code for code in catalogue["authority_to_class"].values() if code
    }
    assert named <= codes








CATALOGUE_TEMPLATE = """\
:PROPERTIES:
:ID: 11111111-1111-1111-1111-111111111111
:END:
#+title: ores.refdata.asset_class_catalogue
#+type: ores.codegen.asset_class_catalogue
#+component: refdata

* Taxonomy

{taxonomy}

* Namespace

{namespace}

* Code domain

** asset_class
:PROPERTIES:
:name:          Asset Class
:display_order: 30
:END:

Top-level product classification codes ({{codes}}), shown on instrument_code and asset_class_code.
"""

TAXONOMY_FX = (
    "** fx\n:PROPERTIES:\n:name:          FX\n:display_order: 1\n:END:\n"
    "\nForeign exchange.\n"
)


def _temp_catalogue(tmp_path, taxonomy, namespace):
    path = tmp_path / "ores.refdata.asset_class_catalogue.org"
    path.write_text(
        CATALOGUE_TEMPLATE.format(taxonomy=taxonomy, namespace=namespace),
        encoding="utf-8")
    return path


def test_a_mapping_to_an_unknown_taxonomy_code_is_refused(tmp_path):
    path = _temp_catalogue(
        tmp_path, TAXONOMY_FX,
        "** ir\n:PROPERTIES:\n:refdata_code: interest_rate\n:END:\n\nRates.\n")
    with pytest.raises(ValueError, match=r"not a taxonomy code"):
        load_org_asset_class_catalogue_model(path)


def test_a_missing_section_is_refused(tmp_path):
    path = tmp_path / "ores.refdata.asset_class_catalogue.org"
    path.write_text(
        ":PROPERTIES:\n:ID: 11111111-1111-1111-1111-111111111111\n:END:\n"
        "#+title: ores.refdata.asset_class_catalogue\n"
        "#+type: ores.codegen.asset_class_catalogue\n#+component: refdata\n\n"
        "* Namespace\n\n** ir\n:PROPERTIES:\n:refdata_code:\n:END:\n\nRates.\n",
        encoding="utf-8")
    with pytest.raises(ValueError, match=r"no \* Taxonomy"):
        load_org_asset_class_catalogue_model(path)


def test_a_duplicate_taxonomy_code_is_refused(tmp_path):
    path = _temp_catalogue(
        tmp_path, TAXONOMY_FX + TAXONOMY_FX.replace("display_order: 1", "display_order: 2"),
        "** fx\n:PROPERTIES:\n:refdata_code: fx\n:END:\n\nFX.\n")
    with pytest.raises(ValueError, match=r"duplicate taxonomy code"):
        load_org_asset_class_catalogue_model(path)




def test_the_sql_prose_folds_to_one_line_and_doubles_its_quotes(tmp_path):
    path = _temp_catalogue(
        tmp_path,
        "** fx\n:PROPERTIES:\n:name:          FX\n:display_order: 1\n:END:\n"
        "\nThe index's pattern is\nwrapped over two lines.\n",
        "")
    taxonomy = load_org_asset_class_catalogue_model(path)[
        "asset_class_catalogue"]["taxonomy"]
    assert taxonomy[0]["description_sql"] == (
        "The index''s pattern is wrapped over two lines.")


def test_the_real_catalogue_carries_the_dq_rows_the_badges_need(tmp_path):
    catalogue = _catalogue()
    by_code = {entry["code"]: entry for entry in catalogue["taxonomy"]}

    assert by_code["fx"]["badge_code"] == "asset_class_fx"
    assert by_code["fx"]["badge_name"] == "FX"
    assert by_code["fx"]["badge_description"] == "Foreign exchange asset class."
    assert by_code["fx"]["badge_order"] == 50
    # The class whose display name differs from its class name.
    assert by_code["commodity"]["badge_name"] == "Commodity AC"
    assert by_code["bond"]["badge_description"] == "Bond asset class."
    assert by_code["bond"]["badge_order"] == 56

    domain = catalogue["code_domain"]
    assert domain["code"] == "asset_class"
    assert domain["display_order"] == 30
    # ``{codes}`` is replaced by the taxonomy's own codes, in display order.
    assert domain["description_sql"] == (
        "Top-level product classification codes (fx, interest_rates, credit, "
        "equity, commodity, inflation, bond), shown on instrument_code and "
        "asset_class_code.")


def test_a_catalogue_with_two_code_domains_is_refused(tmp_path):
    path = tmp_path / "ores.refdata.asset_class_catalogue.org"
    path.write_text(
        CATALOGUE_TEMPLATE.format(taxonomy=TAXONOMY_FX, namespace="")
        + "\n** other\n:PROPERTIES:\n:name:          Other\n"
        ":display_order: 31\n:END:\n\nAnother domain.\n",
        encoding="utf-8")
    with pytest.raises(ValueError, match=r"exactly one"):
        load_org_asset_class_catalogue_model(path)
