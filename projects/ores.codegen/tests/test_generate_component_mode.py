"""Tests for _generate_single's model-type/address incompatibility handling.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_generate_component_mode.py

Covers the ERROR-vs-skip split for --component (auto-discovery) mode: a
model whose type no facet under the requested address ever generates
(e.g. a junction model against a domain_entity-only facet) must be a
silent DEBUG skip in --component mode and a hard ERROR for an explicit
single-entity invocation. A model whose type IS supported by the address
but has nothing enabled (a disabled facet, no override) is unaffected by
this change — it stays a WARNING either way.
"""
import logging
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import _generate_single  # noqa: E402
from codegen.physical_space import address_supports_model_type, load_graph  # noqa: E402


def _write(dirpath, name, body):
    (dirpath / name).write_text(body, encoding="utf-8")


def _make_templates(templates_dir):
    """A two-facet graph: ores.cpp.domain admits junction+domain_entity but
    is disabled by default; ores.cpp.qt admits domain_entity only."""
    templates_dir.mkdir(parents=True)
    _write(templates_dir, "ores.org", "#+title: ores\n#+type: technical_space\n")
    _write(templates_dir, "ores.cpp.org",
           "#+title: ores.cpp\n#+type: technical_space\n")
    _write(templates_dir, "ores.cpp.domain.org",
           "#+title: ores.cpp.domain\n#+type: facet\n#+facet_group: ores.cpp\n"
           "#+model_types: domain_entity junction\n#+default: disabled\n")
    _write(templates_dir, "ores.cpp.domain.class_header.org",
           "#+title: ores.cpp.domain.class_header\n#+type: archetype\n"
           "#+facet: ores.cpp.domain\n#+output: {name}.hpp\n"
           "* Template\n#+begin_src mustache :tangle d.mustache\nx\n#+end_src\n")
    _write(templates_dir, "ores.cpp.qt.org",
           "#+title: ores.cpp.qt\n#+type: facet\n#+facet_group: ores.cpp\n"
           "#+model_types: domain_entity\n")
    _write(templates_dir, "ores.cpp.qt.controller_header.org",
           "#+title: ores.cpp.qt.controller_header\n#+type: archetype\n"
           "#+facet: ores.cpp.qt\n#+output: {name}_controller.hpp\n"
           "* Template\n#+begin_src mustache :tangle q.mustache\nx\n#+end_src\n")


def _make_junction_model(dirpath):
    """A minimal junction model — load_org_junction_model() never raises,
    so this exercises the empty-units branch without any validation noise."""
    path = dirpath / "foo_bar_junction.org"
    _write(dirpath, path.name,
           "#+title: ores.refdata.foo_bar_junction\n"
           "* Left\n:PROPERTIES:\n:column: foo_id\n:type: uuid\n:END:\n"
           "* Right\n:PROPERTIES:\n:column: bar_id\n:type: uuid\n:END:\n")
    return path


def test_address_supports_model_type_true_when_a_facet_admits_it(tmp_path):
    _make_templates(tmp_path / "library" / "templates")
    graph = load_graph(tmp_path / "library" / "templates")
    assert address_supports_model_type("ores.cpp.domain", "junction", graph) is True


def test_address_supports_model_type_false_when_no_facet_admits_it(tmp_path):
    _make_templates(tmp_path / "library" / "templates")
    graph = load_graph(tmp_path / "library" / "templates")
    assert address_supports_model_type("ores.cpp.qt", "junction", graph) is False


def test_component_mode_skips_incompatible_type_silently(tmp_path, caplog):
    _make_templates(tmp_path / "library" / "templates")
    model_path = _make_junction_model(tmp_path)
    with caplog.at_level(logging.DEBUG, logger="codegen.generate"):
        rc = _generate_single(model_path, True, tmp_path,
                              address="ores.cpp.qt", component_mode=True)
    assert rc == 0
    assert not any(r.levelno >= logging.WARNING for r in caplog.records)
    assert any(r.levelno == logging.DEBUG for r in caplog.records)


def test_explicit_single_entity_errors_on_incompatible_type(tmp_path, caplog):
    _make_templates(tmp_path / "library" / "templates")
    model_path = _make_junction_model(tmp_path)
    with caplog.at_level(logging.DEBUG, logger="codegen.generate"):
        rc = _generate_single(model_path, True, tmp_path,
                              address="ores.cpp.qt", component_mode=False)
    assert rc == 1
    assert any(r.levelno == logging.ERROR for r in caplog.records)


def test_compatible_but_disabled_type_still_warns_in_component_mode(tmp_path, caplog):
    """ores.cpp.domain admits junction but is #+default: disabled and this
    model carries no override — an ordinary empty intersection, unrelated
    to type incompatibility, so it must stay a WARNING even in --component
    mode (unaffected by this change)."""
    _make_templates(tmp_path / "library" / "templates")
    model_path = _make_junction_model(tmp_path)
    with caplog.at_level(logging.DEBUG, logger="codegen.generate"):
        rc = _generate_single(model_path, True, tmp_path,
                              address="ores.cpp.domain", component_mode=True)
    assert rc == 0
    assert any(r.levelno == logging.WARNING for r in caplog.records)


def test_compatible_but_disabled_type_warns_in_explicit_mode_too(tmp_path, caplog):
    _make_templates(tmp_path / "library" / "templates")
    model_path = _make_junction_model(tmp_path)
    with caplog.at_level(logging.DEBUG, logger="codegen.generate"):
        rc = _generate_single(model_path, True, tmp_path,
                              address="ores.cpp.domain", component_mode=False)
    assert rc == 0
    assert any(r.levelno == logging.WARNING for r in caplog.records)
