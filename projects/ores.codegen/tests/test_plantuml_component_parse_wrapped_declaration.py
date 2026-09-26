"""Tests for wrapped declarations in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_wrapped_declaration.py

A class may declare a function with its parameter list wrapped across two
lines::

    static std::string to_local_display_string(
        const std::chrono::system_clock::time_point& tp,
        const std::string& format = k_timestamp_format);

The parser skips the declaration itself, because it begins with ``static``
and holds a ``(``. The continuation line holds neither, and it reads as a
field named ``format`` of type ``const std::string&``, so the component
diagram showed a data member the class does not have.

The fix requires a candidate field line to carry balanced parentheses of
its own, which a continuation line never does.
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


WRAPPED_HEADER = """\
namespace ores::sample {

class datetime {
public:
    static std::string to_local_display_string(
        const std::chrono::system_clock::time_point& tp,
        const std::string& format = k_timestamp_format);

private:
    numeric_style style_;
    std::size_t count_ = 0;
};

}
"""


def _parse(tmp_path: Path, body: str):
    header = tmp_path / "sample.hpp"
    header.write_text(body, encoding="utf-8")
    return gen.parse_header(header)


def _members(tmp_path: Path):
    data = _parse(tmp_path, WRAPPED_HEADER)
    datetime = [t for t in data.get(("ores", "sample"), []) if t.name == "datetime"]
    assert len(datetime) == 1
    return datetime[0].members


def test_wrapped_parameter_is_not_a_member(tmp_path):
    members = _members(tmp_path)

    assert "format" not in [m.name for m in members]


def test_real_members_survive_the_guard(tmp_path):
    members = _members(tmp_path)

    assert [(m.name, m.type_str, m.visibility) for m in members] == [
        ("style_", "numeric_style", "-"),
        ("count_", "size_t", "-"),
    ]
