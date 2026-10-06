"""Tests for build/scripts/check_boost_dependencies.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_boost_dependencies.py

Boost arrives as per-library vcpkg ports, so the check compares the headers the
tree includes with the transitive closure of vcpkg.json. Most ports are named
after the directory their headers live in; Boost.ContainerHash's deprecated
boost/functional/hash* headers are the exception, and the check must not ask
for boost-functional because of them.
"""
import importlib.util
import json
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_boost_dependencies",
    REPO_ROOT / "build" / "scripts" / "check_boost_dependencies.py")
check_boost_dependencies = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(check_boost_dependencies)


def tree(tmp_path, declared, ports, includes):
    """A repo root with a vcpkg.json, port manifests, and one source file."""
    (tmp_path / "vcpkg.json").write_text(
        json.dumps({"dependencies": declared}), encoding="utf-8")
    for name, dependencies in ports.items():
        port_dir = tmp_path / "vcpkg" / "ports" / name
        port_dir.mkdir(parents=True)
        (port_dir / "vcpkg.json").write_text(
            json.dumps({"dependencies": list(dependencies)}), encoding="utf-8")
    source = tmp_path / "projects" / "x" / "src" / "a.cpp"
    source.parent.mkdir(parents=True)
    source.write_text("".join(f"#include <boost/{h}>\n" for h in includes),
                      encoding="utf-8")
    return tmp_path


def run(root, monkeypatch):
    monkeypatch.setattr(
        "sys.argv", ["check_boost_dependencies.py", "--root", str(root)])
    return check_boost_dependencies.main()


def test_container_hash_covers_the_deprecated_functional_hash_header(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, ["boost-container-hash"], {"boost-container-hash": []},
                ["functional/hash.hpp"])
    assert run(root, monkeypatch) == 0


def test_the_hash_header_is_not_satisfied_by_boost_functional(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, ["boost-functional"], {"boost-functional": []},
                ["functional/hash.hpp"])
    assert run(root, monkeypatch) == 1
    assert "boost-container-hash" in capsys.readouterr().err


def test_boost_functional_still_owns_its_other_headers(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, ["boost-functional"], {"boost-functional": []},
                ["functional/factory.hpp"])
    assert run(root, monkeypatch) == 0


def test_a_header_with_no_declared_port_is_reported(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, ["boost-container-hash"], {"boost-container-hash": []},
                ["multiprecision/cpp_dec_float.hpp"])
    assert run(root, monkeypatch) == 1
    assert "boost-multiprecision" in capsys.readouterr().err


def test_a_port_reached_transitively_is_accepted(tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, ["boost-multiprecision"],
                {"boost-multiprecision": ["boost-math"], "boost-math": []},
                ["math/special_functions.hpp"])
    assert run(root, monkeypatch) == 0
