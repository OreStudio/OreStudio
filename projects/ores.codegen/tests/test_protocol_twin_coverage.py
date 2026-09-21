"""Tests for check_protocol_twin_coverage.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_twin_coverage.py

The check has two rules: every protocol header has a TypeScript twin, and
no model opts the TypeScript protocol facet out. The first rule covers the
components the shared list in component_registry.py names, so a to-do
component is not checked however much TypeScript it has committed. These
tests drive the check against a throw-away repository tree, never the real
one: a header with no twin fails, a complete twin set passes, an opt-out
key fails with its file and line, and the component scoping is shown both
ways.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_protocol_twin_coverage as cptc  # noqa: E402
from codegen.manifest import Component  # noqa: E402

COMPONENT = "widget"

TS_OUTPUT = ("projects/ores.web/packages/wire-protocol/src/generated/"
             "{component}/protocol/{entity}_protocol.ts")
HEADER = ("projects/ores.widget/api/include/ores.widget.api/messaging/"
          "{entity}_protocol.hpp")
MODEL_DIR = "projects/ores.widget/modeling"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000f1
:END:
#+title: ores.widget.widget
#+type: ores.codegen.entity
#+component: widget
#+entity_singular: widget
#+entity_plural: widgets

* Flags
"""

# The key on line 3, so the reported line number is fixed.
OPT_OUT_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000f1
:ores.ts.protocol.enabled: nil
:END:
#+title: ores.widget.widget
#+type: ores.codegen.entity
#+component: widget
"""


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _make_repo(tmp_path: Path) -> Path:
    """A minimal repository: the ores.ts.protocol library and one component.

    The component's header and twin trees are left empty for the caller to
    populate, so each test states exactly which output is committed.
    """
    templates = tmp_path / "projects/ores.codegen/library/templates"
    _write(templates / "ores.ts.org",
           "#+title: ores.ts\n#+type: technical_space\n")
    _write(templates / "ores.ts.protocol.org",
           "#+title: ores.ts.protocol\n#+type: facet\n"
           "#+facet_group: ores.ts\n"
           "#+model_types: domain_entity operation\n")
    _write(templates / "ores.ts.protocol.protocol_types.org",
           "#+title: ores.ts.protocol.protocol_types\n#+type: archetype\n"
           "#+facet: ores.ts.protocol\n"
           f"#+output: {TS_OUTPUT}\n"
           "* Template\n"
           "#+begin_src mustache :tangle ts_protocol.ts.mustache\n"
           "#+end_src\n")
    _write(tmp_path / MODEL_DIR / "ores.widget.widget.org", MODEL)
    return tmp_path


def _point_module_at(monkeypatch, repo: Path) -> None:
    codegen_dir = repo / "projects/ores.codegen"
    monkeypatch.setattr(cptc, "REPO_ROOT", repo)
    monkeypatch.setattr(cptc, "CODEGEN_DIR", codegen_dir)
    monkeypatch.setattr(cptc, "TEMPLATES_DIR", codegen_dir / "library/templates")
    component = Component(name=COMPONENT, modeling_dir=MODEL_DIR)
    monkeypatch.setattr(cptc, "all_components", lambda: [COMPONENT])
    monkeypatch.setattr(cptc, "get_component", lambda name: component)
    # The fixtures are one to-do component, so the shared list has to name it
    # or rule 1 skips it and every failure case below passes vacuously.
    monkeypatch.setattr(cptc, "COMPONENTS_UNDER_TEST", (COMPONENT,))


def test_missing_twin_is_reported_and_fails(tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _write(repo / HEADER.format(entity="widget"), "// generated header\n")
    # The committed directory holds a twin for another entity, so the
    # component is in scope while widget's own twin is absent.
    _write(repo / TS_OUTPUT.format(component="widget", entity="gadget"),
           "export {};\n")
    _point_module_at(monkeypatch, repo)

    rc = cptc.main()
    captured = capsys.readouterr()

    assert rc == 1
    assert "widget_protocol.hpp" in captured.err
    assert "widget_protocol.ts" in captured.err
    assert "incomplete" in captured.err


def test_present_twin_set_passes(tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _write(repo / HEADER.format(entity="widget"), "// generated header\n")
    _write(repo / TS_OUTPUT.format(component="widget", entity="widget"),
           "export interface GetWidgetsRequest {}\n")
    _point_module_at(monkeypatch, repo)

    rc = cptc.main()
    captured = capsys.readouterr()

    assert rc == 0
    assert "intact" in captured.out
    assert captured.err == ""


def test_opt_out_key_is_reported_and_fails(tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _write(repo / HEADER.format(entity="widget"), "// generated header\n")
    _write(repo / TS_OUTPUT.format(component="widget", entity="widget"),
           "export {};\n")
    _write(repo / MODEL_DIR / "ores.widget.widget.org", OPT_OUT_MODEL)
    _point_module_at(monkeypatch, repo)

    rc = cptc.main()
    captured = capsys.readouterr()

    assert rc == 1
    assert "ores.widget.widget.org:3" in captured.err
    assert ":ores.ts.protocol.enabled: nil" in captured.err
    assert "widget_protocol" not in captured.err


def test_component_without_committed_ts_needs_no_twins(tmp_path, monkeypatch,
                                                       capsys):
    repo = _make_repo(tmp_path)
    _write(repo / HEADER.format(entity="widget"), "// generated header\n")
    _point_module_at(monkeypatch, repo)

    rc = cptc.main()
    captured = capsys.readouterr()

    assert rc == 0
    assert "intact" in captured.out


def test_the_shared_list_decides_which_components_are_checked(
        tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _write(repo / HEADER.format(entity="widget"), "// generated header\n")
    _write(repo / TS_OUTPUT.format(component="widget", entity="gadget"),
           "export {};\n")
    _point_module_at(monkeypatch, repo)

    # Committed TypeScript output does not put a component in scope: the
    # shared list does, and this one names it.
    assert cptc.main() == 1
    capsys.readouterr()

    # A component the list does not name is to-do, so its absent twins are
    # not the gate's business.
    monkeypatch.setattr(cptc, "COMPONENTS_UNDER_TEST", ())
    rc = cptc.main()
    captured = capsys.readouterr()

    assert rc == 0
    assert "widget_protocol.hpp" not in captured.err
