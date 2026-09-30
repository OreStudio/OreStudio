"""Tests for the asset-class catalogue: the one declaration of both lists.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_asset_class_catalogue.py

The catalogue is the single source for the refdata product taxonomy, the
oresmd market-data namespace and the mapping between them. Two properties
matter and both are silent when they break: the mapping must state the
authority-to-class relationship the classifier reads, and an oresmd spec
that names an authority the namespace does not hold must fail the codegen
run rather than generate a namespace nothing else knows.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import codegen.org_loader as org_loader  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    asset_class_catalogue_authorities,
    load_org_asset_class_catalogue_model,
    load_org_oresmd_quote_type_model,
)

CATALOGUE = (
    REPO_ROOT / "projects/ores.refdata/modeling"
    / "ores.refdata.asset_class_catalogue.org"
)
ORESMD_MANIFEST = (
    REPO_ROOT / "projects/ores.marketdata/modeling/oresmd/model.org"
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
    assert asset_class_catalogue_authorities() == frozenset(
        entry["authority"] for entry in namespace)


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


def test_the_real_oresmd_manifest_passes_the_authority_check():
    model = load_org_oresmd_quote_type_model(ORESMD_MANIFEST)
    assert len(model["oresmd_quote_types"]) == len(
        asset_class_catalogue_authorities())


def test_a_spec_with_an_unknown_authority_is_refused(monkeypatch):
    monkeypatch.setattr(
        org_loader, "asset_class_catalogue_authorities",
        lambda: frozenset({"fx"}))
    with pytest.raises(ValueError, match=r"does not hold"):
        load_org_oresmd_quote_type_model(ORESMD_MANIFEST)


def test_a_declared_authority_with_no_spec_is_refused(monkeypatch):
    monkeypatch.setattr(
        org_loader, "asset_class_catalogue_authorities",
        lambda: asset_class_catalogue_authorities() | frozenset({"ghost"}))
    with pytest.raises(ValueError, match=r"ghost"):
        load_org_oresmd_quote_type_model(ORESMD_MANIFEST)
