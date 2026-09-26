"""Tests for the one-line-enum brace handling in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_one_line_enum.py

A C++ header may declare an enum with its whole body on one line::

    enum class time_unit { second, minute, hour, day, week, month, quarter, year };

The component-diagram parser used to treat that declaration as an open type
and push a brace level for it, even though the only brace on the line closes
the declaration itself. Left one level too deep, it then consumed every
following type as if it were a member of the one-line enum, and emitted the
enum with an empty body. Refreshing such a component's diagram therefore
deleted correct content. Seven components declare a one-line enum class, so
the defect reached all of them.

The fix is in the parser, not in the headers: a declaration whose body opens
and closes on the same line is emitted directly and never pushed as an open
type.
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


ONE_LINE_ENUM_HEADER = """\
namespace ores::sample {

enum class numeric_style {
    always,
    automatic
};

enum class time_unit { second, minute, hour, day, week, month, quarter, year };

class relative_time_formatter final {
public:
    explicit relative_time_formatter(numeric_style style);

private:
    numeric_style style_;
};

}
"""


def _parse(tmp_path: Path, body: str):
    header = tmp_path / "sample.hpp"
    header.write_text(body, encoding="utf-8")
    return gen.parse_header(header)


def _types_at(data, namespace):
    return [t for t in data.get(tuple(namespace), [])]


def test_one_line_enum_keeps_its_members(tmp_path):
    data = _parse(tmp_path, ONE_LINE_ENUM_HEADER)

    time_unit = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "time_unit"]
    assert len(time_unit) == 1
    assert time_unit[0].kind == "enum class"
    assert [m.name for m in time_unit[0].members] == [
        "second",
        "minute",
        "hour",
        "day",
        "week",
        "month",
        "quarter",
        "year",
    ]


def test_type_after_one_line_enum_is_not_swallowed(tmp_path):
    data = _parse(tmp_path, ONE_LINE_ENUM_HEADER)

    names = [t.name for t in _types_at(data, ["ores", "sample"])]
    assert names == ["numeric_style", "time_unit", "relative_time_formatter"]


def test_class_after_one_line_enum_keeps_its_members(tmp_path):
    data = _parse(tmp_path, ONE_LINE_ENUM_HEADER)

    formatter = [t for t in _types_at(data, ["ores", "sample"])
                 if t.name == "relative_time_formatter"]
    assert len(formatter) == 1
    assert formatter[0].kind == "class"
    assert [m.name for m in formatter[0].members] == ["style_"]
    assert [m.visibility for m in formatter[0].members] == ["-"]


def test_multiline_enum_still_parses(tmp_path):
    """The fix must not disturb the multi-line form the parser already read."""
    body = """\
namespace ores::sample {

enum class time_unit {
    second,
    minute
};

class after final {
};

}
"""
    data = _parse(tmp_path, body)

    names = [t.name for t in _types_at(data, ["ores", "sample"])]
    assert names == ["time_unit", "after"]
    time_unit = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "time_unit"][0]
    assert [m.name for m in time_unit.members] == ["second", "minute"]


def test_one_line_plain_enum_keeps_its_members(tmp_path):
    body = """\
namespace ores::sample {

enum colour { red, green, blue };

class after final {
};

}
"""
    data = _parse(tmp_path, body)

    names = [t.name for t in _types_at(data, ["ores", "sample"])]
    assert names == ["colour", "after"]
    colour = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "colour"][0]
    assert colour.kind == "enum"
    assert [m.name for m in colour.members] == ["red", "green", "blue"]


def test_one_line_type_does_not_leave_the_parser_nested(tmp_path):
    """A later namespace must still be visible after a one-line declaration.

    This is the brace-sync symptom directly: with the old parser the depth
    stayed one too high, so the closing brace of a following namespace was
    consumed as if it closed a member block.
    """
    body = """\
namespace ores::sample {

enum class time_unit { second, minute };

}

namespace ores::other {

class later final {
};

}
"""
    data = _parse(tmp_path, body)

    assert [t.name for t in _types_at(data, ["ores", "sample"])] == ["time_unit"]
    assert [t.name for t in _types_at(data, ["ores", "other"])] == ["later"]
