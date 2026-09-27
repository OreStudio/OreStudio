"""Tests for parameters whose type carries parentheses of its own.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_nested_paren_param.py

A parameter's type may state a signature of its own:

    void add_extension(const std::string& name,
                       std::function<void(cli::Menu&)> extend);

The method pattern excluded parentheses from the parameter list, so a type
holding one ended the match and the whole declaration was dropped: the method
never reached the diagram. ores.shell's shell_root_menu lost add_extension that
way, and the omission was read as the class's whole API until a manual pass put
it back.

The pattern now admits one level of paired parentheses inside the parameter
list, which is what a function type needs.
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


HEADER = """\
namespace ores::sample {

class registry {
public:
    void insert(const std::string& name);
    void add_extension(const std::string& name,
                       std::function<void(cli::Menu&)> extend);
    void on_event(std::function<void(int, int)> handler);
    void attach(void (*callback)(int, int));
    void plain(int a, void (*cb)(int));
};

}
"""


def _type(tmp_path: Path):
    header = tmp_path / "sample.hpp"
    header.write_text(HEADER, encoding="utf-8")
    data = gen.parse_header(header)
    found = [t for t in data.get(("ores", "sample"), []) if t.name == "registry"]
    assert len(found) == 1
    return found[0]


def test_a_function_typed_parameter_keeps_its_method(tmp_path):
    methods = [(m.name, m.params) for m in _type(tmp_path).methods]

    assert ("add_extension", "const string& name, function<void(cli::Menu&)> extend") in methods


def test_a_function_type_inside_one_parameter_is_not_split(tmp_path):
    by_name = {m.name: m.params for m in _type(tmp_path).methods}

    # The comma inside the function type belongs to that type, not to the
    # parameter list, so the method keeps two parameters and not three.
    assert by_name["on_event"] == "function<void(int, int)> handler"


def test_a_function_pointer_parameter_is_parsed(tmp_path):
    by_name = {m.name: m.params for m in _type(tmp_path).methods}

    assert by_name["attach"] == "void (*callback)(int, int)"
    assert by_name["plain"] == "int a, void (*cb)(int)"


def test_the_rendered_output_carries_the_method(tmp_path):
    text = gen.generate_puml("ores.sample", {("ores", "sample"): [_type(tmp_path)]})

    assert "add_extension" in text
    assert "on_event" in text
