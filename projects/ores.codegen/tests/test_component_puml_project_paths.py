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


def _auto_section():
    """A bare generated section, sentinel included, as the generator emits it."""
    return generator.generate_puml("ores.example", {})


def test_merge_preserves_the_manual_tail(tmp_path):
    target = tmp_path / "ores.example.puml"
    target.write_text(
        _auto_section() + "\nnote right of \"a::b::c\"\n  hand written\nend note\n",
        encoding="utf-8")

    merged = generator.merge_with_existing(_auto_section(), target)

    assert merged is not None
    assert "hand written" in merged
    assert merged.count(generator.SENTINEL) == 1


def test_merge_refuses_a_file_without_a_sentinel(tmp_path):
    """An unmanaged diagram must not be replaced by a bare skeleton.

    Eighteen .puml files under projects/ ship no sentinel, among them the
    hand-authored meta-model and architecture diagrams. Replacing one
    discards its body, and no gate notices because the file still parses
    and still renders.
    """
    target = tmp_path / "ores.example.puml"
    hand_written = "@startuml\nclass only_mine\n@enduml\n"
    target.write_text(hand_written, encoding="utf-8")

    assert generator.merge_with_existing(_auto_section(), target) is None


def test_a_hand_authored_diagram_in_the_tree_is_refused():
    """Pin the real case, not just the synthetic one.

    ores.history.core.puml is hand-authored, carries no sentinel, and is
    discovered by find_all_projects because the part has an include/
    directory. Running the generator over every project used to replace
    its body with a bare type skeleton, dropping two relationship lines
    and a note.
    """
    out_path = REPO_ROOT / "projects/ores.history/core/modeling/ores.history.core.puml"
    assert out_path.exists()
    assert generator.SENTINEL not in out_path.read_text(encoding="utf-8")

    before = out_path.read_text(encoding="utf-8")
    assert generator.process_project("ores.history.core", dry_run=False) is False
    assert out_path.read_text(encoding="utf-8") == before
