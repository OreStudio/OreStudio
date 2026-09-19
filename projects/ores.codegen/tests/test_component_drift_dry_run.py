"""Tests for check_component_drift.py --dry-run.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_component_drift_dry_run.py

The dry-run mode renders each component's models into a throw-away
temporary root and reports what the in-place check would change or create.
It must never write into the repository. These tests drive it against a
throw-away repository tree and assert three properties: a committed file
that differs from its model is reported as would-change, a model with no
committed output is reported as would-create, and the repository tree is
byte-identical afterwards. A clean tree reports no drift and exits zero.
"""
import hashlib
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_component_drift as ccd  # noqa: E402
from codegen.manifest import Component  # noqa: E402


COMPONENT = "testcomp"
ADDRESS = "ores.cpp.domain"
OUTPUT = "projects/ores.testcomp/api/{entity}.hpp"
GENERATED_BODY = "// generated widget header\n"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000{uid}
:END:
#+title: ores.testcomp.{entity}
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: {entity}
#+entity_plural: {entity}s
#+entity_title: {entity}

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:
"""


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _make_repo(tmp_path: Path) -> Path:
    """A minimal repository: a one-facet template library and a component.

    The component holds a ``widget`` model (whose committed output the caller
    controls) and a ``gadget`` model with no committed output.
    """
    templates = tmp_path / "projects/ores.codegen/library/templates"
    _write(templates / "ores.org", "#+title: ores\n#+type: technical_space\n")
    _write(templates / "ores.cpp.org",
           "#+title: ores.cpp\n#+type: technical_space\n")
    _write(templates / "ores.cpp.domain.org",
           "#+title: ores.cpp.domain\n#+type: facet\n"
           "#+facet_group: ores.cpp\n#+model_types: domain_entity\n")
    _write(templates / "ores.cpp.domain.header.org",
           "#+title: ores.cpp.domain.header\n#+type: archetype\n"
           "#+facet: ores.cpp.domain\n"
           f"#+output: {OUTPUT}\n"
           "* Template\n"
           "#+begin_src mustache :tangle widget_header.hpp.mustache\n"
           "#+end_src\n")
    _write(templates / "widget_header.hpp.mustache", GENERATED_BODY)

    modeling = tmp_path / "projects/testcomp/modeling"
    _write(modeling / "ores.testcomp.widget.org",
           MODEL.format(entity="widget", uid="E1"))
    _write(modeling / "ores.testcomp.gadget.org",
           MODEL.format(entity="gadget", uid="E2"))
    return tmp_path


def _point_module_at(monkeypatch, repo_root: Path) -> None:
    codegen_dir = repo_root / "projects/ores.codegen"
    monkeypatch.setattr(ccd, "REPO_ROOT", repo_root)
    monkeypatch.setattr(ccd, "CODEGEN_DIR", codegen_dir)
    monkeypatch.setattr(ccd, "TEMPLATES_DIR", codegen_dir / "library/templates")
    monkeypatch.setattr(
        ccd, "get_component",
        lambda name: Component(name=name,
                               modeling_dir="projects/testcomp/modeling"))


def _tree_hashes(root: Path) -> dict:
    return {
        str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def test_dry_run_reports_would_change_and_would_create(tmp_path, monkeypatch,
                                                       capsys):
    repo = _make_repo(tmp_path)
    # widget has committed output that does not match its model; gadget has
    # no committed output at all.
    committed = repo / OUTPUT.format(entity="widget")
    _write(committed, "// stale widget header\n")
    _point_module_at(monkeypatch, repo)

    rc = ccd._dry_run([COMPONENT], ADDRESS, False)
    out = capsys.readouterr().out

    assert rc == 1
    assert "would change: projects/ores.testcomp/api/widget.hpp" in out
    assert "would create: projects/ores.testcomp/api/gadget.hpp" in out
    assert "1 file(s) would change, 1 file(s) would be created" in out


def test_dry_run_writes_nothing_into_the_tree(tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    committed = repo / OUTPUT.format(entity="widget")
    _write(committed, "// stale widget header\n")
    _point_module_at(monkeypatch, repo)

    before = _tree_hashes(repo)
    rc = ccd._dry_run([COMPONENT], ADDRESS, False)
    capsys.readouterr()
    after = _tree_hashes(repo)

    assert rc == 1
    assert after == before
    # The generated paths were never materialised: gadget stays absent and
    # widget keeps the committed bytes.
    assert not (repo / OUTPUT.format(entity="gadget")).exists()
    assert committed.read_text(encoding="utf-8") == "// stale widget header\n"


def test_dry_run_reports_no_drift_when_tree_matches(tmp_path, monkeypatch,
                                                    capsys):
    repo = _make_repo(tmp_path)
    # Commit exactly what the model generates, and remove the gadget model so
    # there is nothing to create.
    (repo / "projects/testcomp/modeling/ores.testcomp.gadget.org").unlink()
    _write(repo / OUTPUT.format(entity="widget"), GENERATED_BODY)
    _point_module_at(monkeypatch, repo)

    before = _tree_hashes(repo)
    rc = ccd._dry_run([COMPONENT], ADDRESS, False)
    out = capsys.readouterr().out
    after = _tree_hashes(repo)

    assert rc == 0
    assert "No drift" in out
    assert after == before
