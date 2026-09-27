"""Tests for check_component_drift.py --sweep.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_component_drift_sweep.py

The sweep exists because the registry is a list of components that are already
clean: a component waiting its turn has no gate, and neither does output an
archetype routes into another component's tree. It renders every catalogue
component and compares, then fails only on the files the branch touched,
because a component's pre-existing drift belongs to that component's own
clean-up.

These tests drive the sweep against a throw-away repository with the render
stubbed, so they assert the rule rather than the generator: a touched file that
would change fails, drift the branch did not touch does not, a second spelling
of a component is not rendered twice, and nothing is written into the tree.
"""
import hashlib
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_component_drift as ccd  # noqa: E402
from codegen.manifest import Component  # noqa: E402

TOUCHED_GENERATED = "projects/ores.shell/trading/x_commands.hpp"
UNTOUCHED_GENERATED = "projects/ores.refdata/core/y_handler.hpp"


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _make_repo(tmp_path: Path) -> Path:
    """A repository whose two generated files differ from what renders."""
    _write(tmp_path / TOUCHED_GENERATED, "// committed, older\n")
    _write(tmp_path / UNTOUCHED_GENERATED, "// committed, older\n")
    return tmp_path


def _stub_render(monkeypatch, rendered: dict) -> list:
    """Record the render call and write ``rendered`` into the temporary root."""
    calls = []

    def fake_render(components, address, tmp_root, whole_address=False):
        calls.append((list(components), address, whole_address))
        for rel, body in rendered.items():
            _write(tmp_root / rel, body)
        return 0

    monkeypatch.setattr(ccd, "_render_components", fake_render)
    return calls


def _point_at(monkeypatch, repo: Path, touched: set, components=("testcomp",)):
    monkeypatch.setattr(ccd, "REPO_ROOT", repo)
    monkeypatch.setattr(ccd, "_catalogue_components", lambda: list(components))
    monkeypatch.setattr(ccd, "_touched_files", lambda base: set(touched))
    # A seed copy is not needed: clang-format is never invoked in these tests.
    monkeypatch.setattr(ccd, "_seed_clang_format", lambda root: None)


def _tree_hashes(root: Path) -> dict:
    return {
        str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def test_sweep_fails_on_a_touched_file_that_would_change(tmp_path, monkeypatch,
                                                         capsys):
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched={TOUCHED_GENERATED})
    _stub_render(monkeypatch, {TOUCHED_GENERATED: "// rendered, newer\n"})

    rc = ccd._sweep("deadbeefdeadbeef", False)
    captured = capsys.readouterr()

    assert rc == 1
    assert f"would change: {TOUCHED_GENERATED}" in captured.out
    assert "1 file(s) this branch touched would change" in captured.err


def test_sweep_passes_when_only_untouched_files_would_change(
        tmp_path, monkeypatch, capsys):
    # This is the point of the sweep: refdata's remaining drift belongs to
    # refdata's own clean-up and must not fail somebody else's branch.
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched={TOUCHED_GENERATED})
    _stub_render(monkeypatch, {
        TOUCHED_GENERATED: "// committed, older\n",
        UNTOUCHED_GENERATED: "// rendered, newer\n",
    })

    rc = ccd._sweep("deadbeefdeadbeef", False)
    captured = capsys.readouterr()

    assert rc == 0
    assert "Sweep clean" in captured.out
    assert "1 file(s) would change elsewhere" in captured.out


def test_sweep_fails_on_a_created_file_the_branch_added(tmp_path, monkeypatch,
                                                        capsys):
    repo = _make_repo(tmp_path)
    added = "projects/ores.dq/core/z_table.hpp"
    _point_at(monkeypatch, repo, touched={added})
    _stub_render(monkeypatch, {added: "// rendered\n"})

    rc = ccd._sweep("deadbeefdeadbeef", False)
    out = capsys.readouterr().out

    assert rc == 1
    assert f"would change: {added}" in out


def test_sweep_writes_nothing_into_the_tree(tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched={TOUCHED_GENERATED})
    _stub_render(monkeypatch, {TOUCHED_GENERATED: "// rendered, newer\n"})

    before = _tree_hashes(repo)
    rc = ccd._sweep("deadbeefdeadbeef", False)
    capsys.readouterr()
    after = _tree_hashes(repo)

    assert rc == 1
    assert after == before
    assert (repo / TOUCHED_GENERATED).read_text(encoding="utf-8") == \
        "// committed, older\n"


def test_sweep_renders_every_component_at_the_whole_address(
        tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched=set(), components=("a", "b", "c"))
    calls = _stub_render(monkeypatch, {})

    rc = ccd._sweep("deadbeefdeadbeef", False)
    capsys.readouterr()

    assert rc == 0
    # whole_address=True is what reaches the recipe facets, which need a
    # facet from another technical space.
    assert calls == [(["a", "b", "c"], "ores", True)]


def test_catalogue_components_drops_a_second_spelling_of_one_component(
        monkeypatch):
    # trade and trading-cpp share one modeling directory, as dq/dq-cpp and
    # iam/iam-cpp do; rendering both would render the same models twice.
    monkeypatch.setattr(ccd, "all_components",
                        lambda: ["trade", "trading-cpp", "refdata"])
    monkeypatch.setattr(
        ccd, "get_component",
        lambda name: Component(
            name=name,
            modeling_dir=("projects/ores.trading/modeling"
                          if name in ("trade", "trading-cpp")
                          else "projects/ores.refdata/modeling")))

    assert ccd._catalogue_components() == ["refdata", "trade"]


def test_catalogue_components_skips_a_component_without_models(monkeypatch):
    monkeypatch.setattr(ccd, "all_components", lambda: ["with", "without"])
    monkeypatch.setattr(
        ccd, "get_component",
        lambda name: Component(name=name,
                               modeling_dir=("projects/ores.x/modeling"
                                             if name == "with" else None)))

    assert ccd._catalogue_components() == ["with"]


def test_resolve_base_prefers_the_explicit_ref():
    assert ccd._resolve_base("origin/main") == "origin/main"


def test_resolve_base_returns_none_when_git_cannot_answer(monkeypatch):
    # An unresolvable base is a refusal, not a guess: without it every
    # component's clean-up debt would read as a failure.
    class Failed:
        returncode = 1
        stdout = ""
        stderr = "not a git repository"

    monkeypatch.setattr(ccd.subprocess, "run", lambda *a, **k: Failed())

    assert ccd._resolve_base(None) is None


def test_sweep_fails_on_a_file_the_branch_deleted(tmp_path, monkeypatch,
                                                  capsys):
    # The render produces it and the tree does not have it, so it reads as a
    # would-create; the branch touched it by deleting it, so it fails. A
    # generated file no render produces is out of scope, and the doc says so.
    repo = _make_repo(tmp_path)
    deleted = "projects/ores.shell/trading/deleted_commands.hpp"
    _point_at(monkeypatch, repo, touched={deleted})
    _stub_render(monkeypatch,
                 {deleted: "// rendered, and absent from the tree\n"})

    rc = ccd._sweep("deadbeefdeadbeef", False)
    captured = capsys.readouterr()

    assert rc == 1
    assert f"would change: {deleted}" in captured.out


def test_sweep_passes_when_an_untouched_file_would_be_created(
        tmp_path, monkeypatch, capsys):
    # A file the render would create that this branch never touched is some
    # other component's debt, and must not fail this branch.
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched={"projects/ores.refdata/modeling/x.org"})
    _stub_render(monkeypatch, {"projects/ores.dq/core/new_table.hpp": "// new\n"})

    rc = ccd._sweep("deadbeefdeadbeef", False)
    captured = capsys.readouterr()

    assert rc == 0
    assert "Sweep clean" in captured.out


def test_sweep_returns_a_render_failure(tmp_path, monkeypatch, capsys):
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched=set())
    monkeypatch.setattr(ccd, "_render_components",
                        lambda components, address, tmp_root,
                        whole_address=False: 3)

    assert ccd._sweep("deadbeefdeadbeef", False) == 3


def test_sweep_fails_when_the_catalogue_yields_nothing(tmp_path, monkeypatch,
                                                       capsys):
    repo = _make_repo(tmp_path)
    _point_at(monkeypatch, repo, touched=set(), components=())
    _stub_render(monkeypatch, {})

    rc = ccd._sweep("deadbeefdeadbeef", False)
    err = capsys.readouterr().err

    assert rc == 1
    assert "no catalogue component" in err
