"""Tests for codegen.manifest.discover_models: org-type exclusion scoping.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_manifest.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.manifest import Component, discover_models  # noqa: E402


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
