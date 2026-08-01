"""Tests for :profile: binding resolution in the org-mode entity loader.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_profile_binding.py

Covers ORE Studio Variability Model's "Profiles" mechanism: an entity's
:profile: property resolves against the named profile's own
variability_<slug>.org Assignments table (the sole source of truth, no
hardcoded second copy), supplying feature defaults an explicit per-entity
value always overrides.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    org_document_to_model,
    parse_org,
    _load_profile_assignments,
)

MINIMAL_HEADER = """
:PROPERTIES:
:ID: TEST0000-0000-0000-0000-000000000000
:END:
#+entity_plural: things

"""


def _model_with_flags(flags_body: str) -> dict:
    text = MINIMAL_HEADER + "* Flags\n:PROPERTIES:\n" + flags_body + ":END:\n"
    return org_document_to_model(parse_org(text))["domain_entity"]


def test_load_profile_assignments_reads_real_catalogue_file():
    # simple-lookup is a real, checked-in profile; this is not a fixture.
    assignments = dict(_load_profile_assignments("simple_lookup"))
    assert assignments["has_tenant_id"] is True
    assert assignments["has_workspace_id"] is False


def test_unknown_profile_raises():
    import pytest

    with pytest.raises(ValueError, match="unknown profile"):
        dict(_load_profile_assignments("does-not-exist"))


def test_profile_supplies_root_level_defaults():
    de = _model_with_flags(
        ":schema: public\n:product: ores\n:component: refdata\n"
        ":profile: simple-lookup\n"
    )
    assert de["has_tenant_id"] is True
    assert de["has_workspace_id"] is False


def test_profile_supplies_qt_namespace_defaults():
    de = _model_with_flags(
        ":schema: public\n:product: ores\n:component: refdata\n"
        ":profile: simple-lookup\n"
    )
    assert de["qt"]["has_pagination"] is True
    assert de["qt"]["has_change_reason_cache"] is True
    assert de["qt"]["has_uuid_primary_key"] is False


def test_explicit_value_overrides_profile_default():
    de = _model_with_flags(
        ":schema: public\n:product: ores\n:component: refdata\n"
        ":profile: simple-lookup\n:has_workspace_id: true\n"
    )
    assert de["has_workspace_id"] is True


def test_no_profile_leaves_model_unaffected():
    de = _model_with_flags(
        ":schema: public\n:product: ores\n:component: refdata\n"
    )
    assert "profile" not in de
    assert "has_tenant_id" not in de


def test_profile_qt_default_feeds_derived_qt_flag():
    # has_toolbar is derived (org_loader._parse_qt_drawer) from
    # has_version_navigation, among others. fully-featured-lookup supplies
    # has_version_navigation=true purely via the profile, with no explicit
    # :has_version_navigation: in the drawer -- the derivation must see the
    # profile-supplied value, not run before the profile is applied and see
    # it absent (regression: this previously desynced generated output --
    # e.g. a version-nav QToolBar silently dropped -- for any entity that
    # relied on a profile default instead of an explicit drawer property).
    text = (
        MINIMAL_HEADER
        + "* Flags\n:PROPERTIES:\n:schema: public\n:product: ores\n"
        + ":component: refdata\n:profile: fully-featured-lookup\n:END:\n"
        + "* C++\n** Qt\n:PROPERTIES:\n:domain_class: refdata::domain::thing\n:END:\n"
    )
    de = org_document_to_model(parse_org(text))["domain_entity"]
    assert de["qt"]["has_version_navigation"] is True
    assert de["qt"]["has_toolbar"] is True
