"""Tests for nested type declarations in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_nested_type.py

A repository class declares its outcome enum inside its own body::

    class feed_binding_repository {
    public:
        enum class remove_status { removed, conflicting, missing, unsupported };
        ...
    };

The body opens and closes on one line, so the parser's brace bookkeeping saw a
balanced line and the field pattern read the declaration as a data member whose
type is the keyword: a box gained ``+remove_status : enum class``. The pass
draws types at namespace scope only, so the nested declaration is left to the
manual section rather than read as a member.
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


NESTED_TYPE_HEADER = """\
namespace ores::sample {

enum class exit_code : int {
    ok = 0,
    general_error = 1
};

class store final {
public:
    enum class remove_status { removed, conflicting, missing, unsupported };
    int value = 0;
    std::string name;

    remove_status remove(const std::string& id);

private:
    struct cache_entry { int hits = 0; };
    int hits_ = 0;
};

}
"""


def _parse(tmp_path: Path, body: str):
    header = tmp_path / "sample.hpp"
    header.write_text(body, encoding="utf-8")
    return gen.parse_header(header)


def _types_at(data, namespace):
    return [t for t in data.get(tuple(namespace), [])]


def test_nested_enum_is_not_read_as_a_member(tmp_path):
    data = _parse(tmp_path, NESTED_TYPE_HEADER)

    store = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "store"][0]
    assert [m.name for m in store.members] == ["value", "name", "hits_"]


def test_nested_struct_is_not_read_as_a_member(tmp_path):
    data = _parse(tmp_path, NESTED_TYPE_HEADER)

    store = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "store"][0]
    assert "cache_entry" not in [m.name for m in store.members]


def test_members_after_the_nested_enum_are_still_read(tmp_path):
    data = _parse(tmp_path, NESTED_TYPE_HEADER)

    store = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "store"][0]
    value = [m for m in store.members if m.name == "value"]
    assert len(value) == 1
    assert value[0].type_str == "int"


def test_namespace_scope_enum_keeps_its_values(tmp_path):
    data = _parse(tmp_path, NESTED_TYPE_HEADER)

    exit_code = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "exit_code"]
    assert len(exit_code) == 1
    assert [m.name for m in exit_code[0].members] == ["ok", "general_error"]
