"""Tests for locating a composite part in the component puml generator.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_component_puml_project_paths.py

The generator read ``projects/<name>/include/<name>``, so a composite
part was invisible to it: ``ores.ore.core`` lives at
``projects/ores.ore/core``, and ``--project ores.ore.core`` answered
"no include/ directory found" while ``--all`` returned only the simple
components. Every composite's part diagrams therefore rotted, because
nothing could refresh them. These tests pin the name-to-directory
mapping and the discovery walk that depend on it.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "build/scripts/generate_component_puml.py"


def _load_generator():
    spec = importlib.util.spec_from_file_location("generate_component_puml", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


generator = _load_generator()


def test_simple_component_resolves_to_its_directory():
    assert generator._project_dir("ores.platform") == REPO_ROOT / "projects/ores.platform"


def test_composite_part_resolves_below_its_component():
    assert generator._project_dir("ores.ore.core") == REPO_ROOT / "projects/ores.ore/core"


def test_part_include_dir_is_found():
    include_dir = generator._find_include_dir("ores.ore.core")
    assert include_dir is not None
    assert include_dir.name == "ores.ore.core"


def test_discovery_includes_parts_and_simple_components():
    projects = generator.find_all_projects()
    assert "ores.platform" in projects
    assert "ores.ore.core" in projects
    assert "ores.ore.api" in projects


def test_unknown_name_has_no_directory():
    assert generator._project_dir("ores.does_not_exist.core") is None
