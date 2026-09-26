"""Tests for operator functions in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_operator_function.py

An operator function carries an operator token in its name::

    application& operator=(const application&) = delete;
    bool operator==(const application& other) const;

The field-versus-method test reads a line as a field when its ``=`` precedes
its first parenthesis.  That is the shape of ``operator=(``, so the deleted
assignment operator was emitted as a data member named ``operator``: every
component that declares one carried the phantom member in its committed
diagram.  A return type precedes the operator name, so recognising an operator
anywhere in the line keeps the declaration out of the field parser.

The fix treats any line naming an operator as a function.
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


OPERATOR_HEADER = """\
namespace ores::sample {

class application {
public:
    application();
    ~application();

    application(const application&) = delete;
    application& operator=(const application&) = delete;
    application(application&&) = delete;
    application& operator=(application&&) = delete;

    bool operator==(const application& other) const;
    bool operator!=(const application& other) const;
    bool operator<(const application& other) const;
    application& operator[](std::size_t index);
    int operator()(int value) const;
    void* operator new(std::size_t size);
    void operator delete(void* pointer);

    std::string name;
    int retries = 3;
    bool enabled = true;
};

}
"""


def _members(tmp_path: Path):
    header = tmp_path / "sample.hpp"
    header.write_text(OPERATOR_HEADER, encoding="utf-8")
    data = gen.parse_header(header)
    application = [
        t for t in data.get(("ores", "sample"), []) if t.name == "application"
    ]
    assert len(application) == 1
    return application[0].members


def test_operator_functions_are_not_read_as_members(tmp_path):
    members = _members(tmp_path)

    assert [m.name for m in members] == ["name", "retries", "enabled"]


def test_no_phantom_operator_member(tmp_path):
    members = _members(tmp_path)

    assert [m for m in members if m.name == "operator"] == []


def test_fields_keep_their_types(tmp_path):
    members = {m.name: m.type_str for m in _members(tmp_path)}

    assert members == {"name": "string", "retries": "int", "enabled": "bool"}
