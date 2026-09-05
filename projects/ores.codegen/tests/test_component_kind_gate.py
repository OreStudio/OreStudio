"""Tests for the composite-kind hard gate on sub-component container orgs.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_component_kind_gate.py

A component org whose root hosts sub-component projects (directories with
their own modeling/component_overview.org) is a pure container: it must
declare #+component_kind: composite, the only kind no archetype serves.
The org loader defaults a missing key to "flat", which would select the
flat bootstrap archetypes and write a whole phantom scaffold into the
container directory (the ores.compute incident). resolve_targets raises
instead of emitting it.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

ORG = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000ab
:END:
#+title: ores.{name}
#+type: ores.codegen.component
#+name: {name}
#+full_name: ores.{name}
#+brief: {name} fixture
{kind_line}
* Summary

Fixture body.
"""


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _make_tree(tmp_path: Path, kind_line: str, host_sub: bool) -> Path:
    """A projects/ tree with a minimal codegen base and a component org."""
    base = tmp_path / "projects" / "ores.codegen"
    _write(base / "library/templates/ores.org",
           "#+title: ores\n#+type: technical_space\n")
    _write(base / "library/templates/ores.cpp.org",
           "#+title: ores.cpp\n#+type: technical_space\n")
    comp_root = tmp_path / "projects" / "testcomp"
    _write(comp_root / "modeling/component_overview.org",
           ORG.format(name="testcomp", kind_line=kind_line))
    if host_sub:
        _write(comp_root / "api/modeling/component_overview.org",
               ORG.format(name="testcomp.api", kind_line=""))
    return base


def test_container_without_kind_raises(tmp_path):
    base = _make_tree(tmp_path, kind_line="", host_sub=True)
    model = tmp_path / "projects" / "testcomp" / "modeling/component_overview.org"
    with pytest.raises(ValueError, match="component_kind: composite"):
        resolve_targets(model, base)


def test_container_declaring_flat_kind_raises(tmp_path):
    base = _make_tree(tmp_path, kind_line="#+component_kind: flat", host_sub=True)
    model = tmp_path / "projects" / "testcomp" / "modeling/component_overview.org"
    with pytest.raises(ValueError, match="component_kind: composite"):
        resolve_targets(model, base)


def test_container_declaring_composite_passes(tmp_path):
    base = _make_tree(tmp_path, kind_line="#+component_kind: composite", host_sub=True)
    model = tmp_path / "projects" / "testcomp" / "modeling/component_overview.org"
    units, model_type, _ = resolve_targets(model, base)
    assert model_type == "component"
    assert units == []


def test_flat_component_without_kind_passes(tmp_path):
    base = _make_tree(tmp_path, kind_line="", host_sub=False)
    model = tmp_path / "projects" / "testcomp" / "modeling/component_overview.org"
    units, model_type, _ = resolve_targets(model, base)
    assert model_type == "component"
    assert units == []
