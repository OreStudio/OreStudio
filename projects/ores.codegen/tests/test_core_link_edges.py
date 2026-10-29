"""Tests for the core link edge gate.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_core_link_edges.py

The gate reads the CMake tree rather than the build, so its parser and its
ratchet are the things to pin: an edge a core may not have is found and named,
and an edge the baseline accepts stops the gate until the baseline drops it.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_core_link_edges as gate  # noqa: E402

LICENCE = "# licence\n"


def _component(root: Path, name: str, links: str) -> Path:
    """A component whose core links what @links states."""
    directory = root / "projects" / name / "core" / "src"
    directory.mkdir(parents=True)
    file = directory / "CMakeLists.txt"
    file.write_text(
        LICENCE
        + f'set(name "ores.{name}.core")\n'
        + "set(lib_target_name ${name}.lib)\n"
        + "target_link_libraries(${lib_target_name}\n"
        + "    PRIVATE\n"
        + f"        {links})\n"
    )
    return file


def _point_at(monkeypatch, root: Path, baseline: Path) -> None:
    monkeypatch.setattr(gate, "PROJECTS", root / "projects")
    monkeypatch.setattr(gate, "BASELINE", baseline)


def test_a_core_that_links_another_components_core_is_found(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "ores.beta.core.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    edges, problems = gate.scan()

    assert problems == []
    assert edges == [gate.ForbiddenEdge("ores.alpha.core.lib", "ores.beta.core.lib", "beta", "core")]


def test_a_core_that_links_another_components_service_is_found(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "ores.beta.service.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    edges, _ = gate.scan()

    assert edges == [
        gate.ForbiddenEdge("ores.alpha.core.lib", "ores.beta.service.lib", "beta", "service")
    ]


def test_a_core_may_link_an_api_contract_and_its_own_libraries(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "ores.beta.api.lib\n        ores.alpha.service.lib\n        ores.utility.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    assert gate.scan() == ([], [])


def test_the_shared_service_library_is_not_a_component(tmp_path, monkeypatch):
    """``ores.service.lib`` is the layer every service uses, not a component."""
    root = tmp_path
    _component(root, "alpha", "ores.service.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    assert gate.scan() == ([], [])


def test_a_comment_that_names_a_link_is_not_an_edge(tmp_path, monkeypatch):
    root = tmp_path
    file = _component(root, "alpha", "ores.beta.api.lib")
    file.write_text(file.read_text().replace(
        "        ores.beta.api.lib)",
        "        # ores.beta.core.lib\n        ores.beta.api.lib)"))
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    assert gate.scan() == ([], [])


def test_the_gate_fails_on_an_edge_the_baseline_does_not_accept(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "ores.beta.core.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    assert gate.main([]) == 1


def test_the_gate_passes_and_keeps_an_accepted_edge(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "ores.beta.core.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")
    assert gate.main(["--write-baseline", "--accept-new"]) == 0

    assert gate.main([]) == 0


def test_the_gate_fails_on_a_baseline_entry_that_no_longer_links(tmp_path, monkeypatch):
    """The baseline is a ratchet: an edge that is gone has to leave the list."""
    root = tmp_path
    _component(root, "alpha", "ores.beta.api.lib")
    baseline = tmp_path / "baseline.json"
    baseline.write_text(
        '{"edges": [{"core": "ores.alpha.core.lib", "links": "ores.beta.core.lib"}]}\n'
    )
    _point_at(monkeypatch, root, baseline)

    assert gate.main([]) == 1

def test_the_gate_names_the_component_the_dependency_belongs_to(tmp_path, monkeypatch, capsys):
    root = tmp_path
    _component(root, "alpha", "ores.beta.core.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")
    gate.main([])

    message = capsys.readouterr().err
    assert "ores.alpha.core.lib: links ores.beta.core.lib" in message
    assert "which is beta's core library" in message


def test_the_gate_lists_every_edge_with_the_owner_of_the_dependency(tmp_path, monkeypatch, capsys):
    root = tmp_path
    _component(root, "alpha", "ores.beta.core.lib")
    _component(root, "beta", "ores.beta.api.lib")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")
    gate.main(["--list"])

    assert "ores.alpha.core.lib links ores.beta.core.lib (beta core)" in capsys.readouterr().out


def test_the_gate_fails_on_a_link_it_cannot_resolve(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "${something.core.lib}")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    _, problems = gate.scan()

    assert problems and "cannot resolve" in problems[0]


def test_the_baseline_does_not_grow_unless_the_edge_is_accepted(tmp_path, monkeypatch):
    root = tmp_path
    _component(root, "alpha", "ores.beta.core.lib")
    _component(root, "beta", "ores.beta.api.lib")
    baseline = tmp_path / "baseline.json"
    baseline.write_text('{"edges": []}\n')
    _point_at(monkeypatch, root, baseline)

    assert gate.main(["--write-baseline"]) == 1
    assert gate.main([]) == 1
    assert gate.main(["--write-baseline", "--accept-new"]) == 0
    assert gate.main([]) == 0


def test_a_core_file_that_states_no_name_is_a_problem(tmp_path, monkeypatch):
    root = tmp_path
    directory = root / "projects" / "alpha" / "core" / "src"
    directory.mkdir(parents=True)
    (directory / "CMakeLists.txt").write_text(LICENCE + "target_link_libraries(x ores.beta.core.lib)\n")
    _point_at(monkeypatch, root, tmp_path / "baseline.json")

    _, problems = gate.scan()

    assert problems and "states no name" in problems[0]
