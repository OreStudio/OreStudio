"""Tests for codegen.sh regenerate's --entity filter.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_regenerate_entity_filter.py

Isolates cmd_regenerate's --entity filtering logic from real generation by
monkeypatching _generate_single (recording which model paths it's called
with) and get_component (a synthetic fixture component/discover_models,
per test_manifest.py's pattern) -- this test is only about which files
--entity selects, not about template resolution or SQL/C++ output.
"""
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen import generate  # noqa: E402
from codegen.manifest import Component  # noqa: E402


def _write_org(path, org_type):
    path.write_text(
        f"#+title: {path.stem}\n#+type: {org_type}\n", encoding="utf-8"
    )


@pytest.fixture
def fixture_component(tmp_path, monkeypatch):
    modeling_dir = tmp_path / "modeling"
    modeling_dir.mkdir()
    _write_org(modeling_dir / "ores.fixture.rounding_type.org", "ores.codegen.entity")
    _write_org(modeling_dir / "ores.fixture.monetary_nature.org", "ores.codegen.entity")
    _write_org(modeling_dir / "ores.fixture.currency_market_tier.org", "ores.codegen.entity")
    comp = Component(name="fixture", models_dir="", modeling_dir=str(modeling_dir.relative_to(tmp_path)))
    monkeypatch.setattr("codegen.manifest.get_component", lambda name: comp)
    return tmp_path


def _args(**overrides):
    base = dict(component="fixture", all=False, address="ores.cpp.qt",
                dry_run=True, entity=None)
    base.update(overrides)
    return SimpleNamespace(**base)


def _run(fixture_component, monkeypatch, **arg_overrides):
    calls = []

    def fake_generate_single(model_path, dry_run, base_dir, address=None, component_mode=False):
        calls.append(model_path.stem.split(".")[-1])
        return 0

    monkeypatch.setattr(generate, "_generate_single", fake_generate_single)
    # cmd_regenerate derives project_root as base_dir.parent.parent, so nest
    # base_dir two levels under fixture_component (tmp_path) to make that
    # resolve back to it -- Component.modeling_dir is relative to tmp_path.
    base_dir = fixture_component / "x" / "ores.codegen"
    rc = generate.cmd_regenerate(_args(**arg_overrides), base_dir)
    return rc, calls


def test_no_entity_filter_generates_everything(fixture_component, monkeypatch):
    rc, calls = _run(fixture_component, monkeypatch)
    assert rc == 0
    assert set(calls) == {"rounding_type", "monetary_nature", "currency_market_tier"}


def test_entity_filter_restricts_to_named_entities(fixture_component, monkeypatch):
    rc, calls = _run(fixture_component, monkeypatch, entity="rounding_type,monetary_nature")
    assert rc == 0
    assert set(calls) == {"rounding_type", "monetary_nature"}


def test_entity_filter_single_name(fixture_component, monkeypatch):
    rc, calls = _run(fixture_component, monkeypatch, entity="rounding_type")
    assert rc == 0
    assert calls == ["rounding_type"]


def test_unknown_entity_name_is_an_error(fixture_component, monkeypatch):
    rc, calls = _run(fixture_component, monkeypatch, entity="does_not_exist")
    assert rc == 1
    assert calls == []


def test_mixed_known_and_unknown_still_generates_known_but_errors(fixture_component, monkeypatch):
    rc, calls = _run(fixture_component, monkeypatch, entity="rounding_type,does_not_exist")
    assert rc == 1
    assert calls == ["rounding_type"]


def test_entity_with_all_is_rejected(fixture_component, monkeypatch):
    rc, calls = _run(fixture_component, monkeypatch, all=True, entity="rounding_type")
    assert rc == 1
    assert calls == []
