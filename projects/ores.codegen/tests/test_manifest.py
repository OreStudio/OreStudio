"""Tests for codegen.manifest: entity_name_from_path and discover_models.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_manifest.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.manifest import Component, discover_models, entity_name_from_path  # noqa: E402


class TestEntityNameFromPath:
    """Exact entity-name extraction — no type-suffix stripping."""

    def test_entity_model_without_suffix(self):
        name = entity_name_from_path(
            Path("ores.analytics.pricing_engine_type.org"))
        assert name == "pricing_engine_type"

    def test_lookup_entity_keeps_suffix(self):
        name = entity_name_from_path(
            Path("ores.analytics.pricing_engine_type_lookup_entity.org"))
        assert name == "pricing_engine_type_lookup_entity"

    def test_junction_keeps_suffix(self):
        name = entity_name_from_path(
            Path("ores.refdata.currency_currency_group_junction.org"))
        assert name == "currency_currency_group_junction"

    def test_entity_and_lookup_entity_are_distinct(self):
        entity_name = entity_name_from_path(
            Path("ores.analytics.pricing_engine_type.org"))
        lookup_name = entity_name_from_path(
            Path("ores.analytics.pricing_engine_type_lookup_entity.org"))
        assert entity_name != lookup_name

    def test_component_prefix_stripped(self):
        name = entity_name_from_path(
            Path("ores.refdata.country.org"))
        assert name == "country"

    def test_single_segment_name(self):
        name = entity_name_from_path(
            Path("currency_domain_entity.json"))
        # No dot → no prefix stripping; the stem is returned as-is.
        assert name == "currency_domain_entity"


def _write_org(path, org_type):
    path.write_text(
        f"#+title: {path.stem}\n#+type: {org_type}\n", encoding="utf-8"
    )


def _fixture_component(tmp_path):
    modeling_dir = tmp_path / "modeling"
    modeling_dir.mkdir()
    _write_org(modeling_dir / "ores.fixture.widget.org", "ores.codegen.entity")
    _write_org(modeling_dir / "ores.fixture.widget_owner_junction.org", "ores.codegen.junction")
    _write_org(modeling_dir / "ores.fixture.module.org", "ores.codegen.module")
    return Component(
        name="fixture",
        models_dir="",
        modeling_dir=str(modeling_dir.relative_to(tmp_path)),
        exclude_org_types=("junction",),
    )


def test_bulk_discovery_excludes_configured_org_types(tmp_path):
    comp = _fixture_component(tmp_path)
    models = discover_models(comp, tmp_path)

    names = {p.name for p in models}
    assert "ores.fixture.widget.org" in names
    assert "ores.fixture.widget_owner_junction.org" not in names
    # module was never in _CODEGEN_ORG_TYPES to begin with.
    assert "ores.fixture.module.org" not in names


def test_single_entity_discovery_ignores_exclusions(tmp_path):
    comp = _fixture_component(tmp_path)
    models = discover_models(comp, tmp_path, apply_exclusions=False)

    names = {p.name for p in models}
    assert "ores.fixture.widget.org" in names
    assert "ores.fixture.widget_owner_junction.org" in names
    assert "ores.fixture.module.org" not in names
