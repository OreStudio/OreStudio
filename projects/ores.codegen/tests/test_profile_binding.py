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

import codegen.org_loader as org_loader  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    load_org_junction_model,
    org_document_to_model,
    parse_org,
    _load_profile_assignments,
    _parse_profile_list,
    read_physical_space_overrides,
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


def _junction_header(profile: str | None = None, extra_frontmatter: str = "") -> str:
    # :profile: binds in the * Flags section -- the single canonical binding
    # point for every metatype (see read_physical_space_overrides); the
    # file-level :PROPERTIES: drawer is rejected.
    flags = f"* Flags\n:PROPERTIES:\n:profile: {profile}\n:END:\n\n" if profile else ""
    return f"""
:PROPERTIES:
:ID: TEST0001-0000-0000-0000-000000000000
:END:
#+title: ores.refdata.thing_other_thing
#+type: ores.codegen.junction
#+component: refdata
#+name: thing_other_things
#+name_singular: thing_other_thing
#+name_title: Thing Other Thing
{extra_frontmatter}{flags}"""


def test_junction_profile_supplies_root_level_default(tmp_path):
    # tenant-scoped-junction's explicit has_tenant_id=true default must
    # resolve here, unlike domain_entity's has_tenant_id=true-by-default --
    # junction defaults has_tenant_id to false absent a profile/explicit value.
    p = tmp_path / "thing.org"
    p.write_text(_junction_header("tenant-scoped-junction"), encoding="utf-8")
    j = load_org_junction_model(p)["junction"]
    assert j["has_tenant_id"] is True


def test_junction_explicit_value_overrides_profile_default(tmp_path):
    p = tmp_path / "thing.org"
    p.write_text(
        _junction_header(
            "tenant-scoped-junction",
            extra_frontmatter="#+has_tenant_id: false\n",
        ),
        encoding="utf-8",
    )
    j = load_org_junction_model(p)["junction"]
    assert j["has_tenant_id"] is False


def test_junction_profile_qt_default_feeds_derived_qt_flag(tmp_path):
    # Same has_toolbar ordering regression as the domain_entity case above,
    # but exercised through the junction loader path (calendar_date is the
    # one junction with its own Qt drawer).
    text = (
        _junction_header("fully-featured-lookup")
        + "* C++\n** Qt\n:PROPERTIES:\n:domain_class: refdata::domain::thing\n:END:\n"
    )
    p = tmp_path / "thing.org"
    p.write_text(text, encoding="utf-8")
    j = load_org_junction_model(p)["junction"]
    assert j["qt"]["has_version_navigation"] is True
    assert j["qt"]["has_toolbar"] is True


# --------------------------------------------------------------------------
# Multiple profiles bound together (":profile: a, b"), and the Physical
# space table mechanism (entity-level and profile-carried).


def _file_header(profile: str, extra_properties: str = "") -> str:
    # :profile: binds in the * Flags section -- the single canonical binding
    # point (see read_physical_space_overrides); the file-level
    # :PROPERTIES: drawer is rejected.
    return (
        ":PROPERTIES:\n:ID: TEST0000-0000-0000-0000-000000000000\n:END:\n"
        "#+entity_plural: things\n\n"
        "* Flags\n:PROPERTIES:\n"
        f":profile: {profile}\n{extra_properties}:END:\n"
    )


def test_parse_profile_list_splits_comma_separated():
    assert _parse_profile_list("simple-lookup, artefact-staging-only") == [
        "simple-lookup", "artefact-staging-only",
    ]
    assert _parse_profile_list("simple-lookup") == ["simple-lookup"]
    assert _parse_profile_list(None) == []
    assert _parse_profile_list("") == []
    assert _parse_profile_list(["already", "a", "list"]) == ["already", "a", "list"]


def test_multiple_profiles_compose_orthogonal_traits():
    # simple-lookup (feature shape) + artefact-staging-only (enablement-only,
    # zero feature Assignments) must not conflict -- genuinely orthogonal.
    text = _file_header(
        "simple-lookup, artefact-staging-only",
        ":schema: public\n:product: ores\n:component: dq\n",
    )
    de = org_document_to_model(parse_org(text))["domain_entity"]
    assert de["has_tenant_id"] is True  # from simple-lookup
    assert de["qt"]["has_pagination"] is True  # from simple-lookup


def test_conflicting_profiles_raise():
    import pytest

    # simple-lookup fixes has_workspace_id=false; workspace-scoped-lookup
    # fixes it true -- a genuine disagreement between two real profiles.
    text = _file_header(
        "simple-lookup, workspace-scoped-lookup",
        ":schema: public\n:product: ores\n:component: dq\n",
    )
    with pytest.raises(ValueError, match="conflicting profiles"):
        org_document_to_model(parse_org(text))


def test_entity_physical_space_table_disables_an_address():
    text = (
        MINIMAL_HEADER
        + "* Physical space\n\n"
        + "| Address | Enabled |\n|---------+---------|\n"
        + "| ores.sql.schema.domain_entity_create | false |\n"
    )
    doc = parse_org(text)
    overrides = read_physical_space_overrides(doc)
    assert overrides == {"ores.sql.schema.domain_entity_create.enabled": False}


def test_profile_physical_space_table_is_inherited():
    # artefact-staging-only is a real, checked-in profile with a Physical
    # space table; an entity binding to it (alongside a shape profile)
    # inherits that table's disables with no table of its own.
    text = _file_header("simple-lookup, artefact-staging-only")
    doc = parse_org(text)
    overrides = read_physical_space_overrides(doc)
    assert overrides["ores.sql.schema.domain_entity_create.enabled"] is False
    assert overrides["ores.sql.schema.notify_trigger.enabled"] is False


def test_entity_physical_space_table_wins_over_profile():
    # The entity's own table overrides its profile's default -- same
    # "explicit beats profile" rule every other profile-carried value follows.
    text = (
        _file_header("artefact-staging-only")
        + "* Physical space\n\n"
        + "| Address | Enabled |\n|---------+---------|\n"
        + "| ores.sql.schema.domain_entity_create | true |\n"
    )
    doc = parse_org(text)
    overrides = read_physical_space_overrides(doc)
    assert overrides["ores.sql.schema.domain_entity_create.enabled"] is True
    # Untouched by the entity's own table, still inherited from the profile.
    assert overrides["ores.sql.schema.notify_trigger.enabled"] is False


def test_file_level_profile_rejected_in_all_readers(tmp_path):
    # The single canonical binding point is * Flags; a file-level
    # :profile: must fail loudly on every read path, never be silently
    # accepted.
    import pytest

    entity = parse_org(
        ":PROPERTIES:\n:ID: TEST0000-0000-0000-0000-000000000000\n"
        ":profile: simple-lookup\n:END:\n#+entity_plural: things\n\n"
    )
    with pytest.raises(ValueError, match="file-level"):
        org_document_to_model(entity)
    with pytest.raises(ValueError, match="file-level"):
        read_physical_space_overrides(entity)

    p = tmp_path / "j.org"
    p.write_text(
        ":PROPERTIES:\n:ID: TEST0001-0000-0000-0000-000000000000\n"
        ":profile: tenant-scoped-junction\n:END:\n"
        "#+type: ores.codegen.junction\n",
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="file-level"):
        load_org_junction_model(p)
