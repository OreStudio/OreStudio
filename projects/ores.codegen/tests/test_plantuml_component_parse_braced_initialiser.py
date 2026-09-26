"""Tests for brace-initialised data members in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_braced_initialiser.py

A data member may carry a brace-or-equal initialiser::

    boost::uuids::uuid id{};
    std::array<int, 3> bounds{1, 2, 3};

The field pattern accepted an ``= value`` initialiser but not a braced one,
so the match ran to the end of ``id{}`` and failed. The member vanished from
the component diagram while its neighbours survived, which reads as a class
that does not declare the field.

The fix adds a braced-initialiser branch to the field pattern.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
GENERATOR = REPO_ROOT / "build/scripts/generate_component_puml.py"


def _load_generator():
    """Imports build/scripts/generate_component_puml.py as a module.

    The script is not importable by name: it lives outside any package and a
    dataclass declares a ClassVar annotated with a name the script itself
    defines, so the module has to be registered before it executes.
    """
    spec = importlib.util.spec_from_file_location("generate_component_puml", GENERATOR)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


gen = _load_generator()


BRACED_HEADER = """\
namespace ores::sample {

struct row {
    boost::uuids::uuid id{};
    std::string name;
    std::optional<boost::uuids::uuid> parent_id;
    std::array<int, 3> bounds{1, 2, 3};
};

}
"""


def _parse(tmp_path: Path, body: str):
    header = tmp_path / "sample.hpp"
    header.write_text(body, encoding="utf-8")
    return gen.parse_header(header)


def _members(tmp_path: Path):
    data = _parse(tmp_path, BRACED_HEADER)
    row = [t for t in data.get(("ores", "sample"), []) if t.name == "row"]
    assert len(row) == 1
    return row[0].members


def test_empty_braced_initialiser_member_is_read(tmp_path):
    members = _members(tmp_path)

    assert "id" in [m.name for m in members]


def test_braced_initialiser_does_not_swallow_its_neighbours(tmp_path):
    members = _members(tmp_path)

    assert [m.name for m in members] == ["id", "name", "parent_id", "bounds"]


def test_braced_initialiser_with_values_is_read(tmp_path):
    members = _members(tmp_path)

    bounds = [m for m in members if m.name == "bounds"]
    assert len(bounds) == 1
    assert bounds[0].type_str == "array<int, 3>"
