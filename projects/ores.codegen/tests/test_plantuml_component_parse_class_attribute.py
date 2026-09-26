"""Tests for attributed class declarations in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_class_attribute.py

A class may carry an attribute between the ``class`` keyword and the export
macro, or after the macro::

    class [[nodiscard]] ORES_NATS_EXPORT subscription {

The parser accepts a run of attributes and ALL_CAPS export macros between
the ``class`` or ``struct`` keyword and the name, in either order. A
declaration outside that shape does not match, and the class is then absent
from the diagram with no warning: ``ores.nats`` showed a client that
returned a type it never declared.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
GENERATOR = REPO_ROOT / "build/scripts/generate_component_puml.py"


def _load_generator():
    """Imports build/scripts/generate_component_puml.py as a module."""
    spec = importlib.util.spec_from_file_location("generate_component_puml", GENERATOR)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


gen = _load_generator()


ATTRIBUTED_HEADER = """\
namespace ores::sample {

class [[nodiscard]] ORES_SAMPLE_EXPORT subscription {
public:
    std::string subject() const;

private:
    std::unique_ptr<impl> i_;
};

class ORES_SAMPLE_EXPORT [[nodiscard]] buffered {
private:
    std::size_t capacity_;
};

class ORES_SAMPLE_EXPORT plain {
private:
    int count_;
};

struct [[nodiscard]] wrapped {
    int field_;
};

}
"""


def _parse(tmp_path: Path, body: str):
    header = tmp_path / "sample.hpp"
    header.write_text(body, encoding="utf-8")
    return gen.parse_header(header)


def _classes(tmp_path: Path):
    data = _parse(tmp_path, ATTRIBUTED_HEADER)
    return {t.name: t for t in data.get(("ores", "sample"), [])}


def test_attribute_before_macro_keeps_the_class(tmp_path):
    assert "subscription" in _classes(tmp_path)


def test_attribute_after_macro_keeps_the_class(tmp_path):
    assert "buffered" in _classes(tmp_path)


def test_attributed_struct_keeps_the_struct(tmp_path):
    assert "wrapped" in _classes(tmp_path)


def test_the_class_without_an_attribute_still_parses(tmp_path):
    assert "plain" in _classes(tmp_path)


def test_the_attributed_class_keeps_its_members(tmp_path):
    members = _classes(tmp_path)["subscription"].members

    assert [(m.name, m.type_str, m.visibility) for m in members] == [
        ("i_", "unique_ptr<impl>", "-"),
    ]
