"""Tests for enum-value comment handling in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_enum_trailing_comment.py

A C++ header may document an enumerator on the same line, in either Doxygen
trailing style::

    enum class exit_code : int {
        ok = 0,                     ///< Clean shutdown
        auth_error = 6,             /**< Authentication failure */
    };

The component-diagram parser matched an enumeration value against the whole
stripped line, so a trailing comment left the pattern unmatched and the value
was dropped. The emitted enum then had an empty body and a refresh deleted the
values the checked-in diagram held.

The fix is in the parser, not in the headers: a trailing comment is removed
before the value pattern runs. A whole-line comment still ends the scan of that
line rather than being read as a value.
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


TRAILING_COMMENT_HEADER = """\
namespace ores::sample {

enum class exit_code : int {
    ok = 0,                     ///< Clean shutdown
    general_error = 1,          ///< Unclassified runtime error
    auth_error = 6,             /**< Authentication failure */
    plain,
    // only_a_comment,
    last = 7
};

class after final {
public:
    int value = 0;
};

}
"""


def _parse(tmp_path: Path, body: str):
    header = tmp_path / "sample.hpp"
    header.write_text(body, encoding="utf-8")
    return gen.parse_header(header)


def _types_at(data, namespace):
    return [t for t in data.get(tuple(namespace), [])]


def test_enum_values_with_trailing_comments_are_read(tmp_path):
    data = _parse(tmp_path, TRAILING_COMMENT_HEADER)

    exit_code = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "exit_code"]
    assert len(exit_code) == 1
    assert [m.name for m in exit_code[0].members] == [
        "ok",
        "general_error",
        "auth_error",
        "plain",
        "last",
    ]


def test_whole_line_comment_is_not_read_as_a_value(tmp_path):
    data = _parse(tmp_path, TRAILING_COMMENT_HEADER)

    exit_code = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "exit_code"][0]
    assert "only_a_comment" not in [m.name for m in exit_code.members]


def test_class_after_commented_enum_keeps_its_members(tmp_path):
    data = _parse(tmp_path, TRAILING_COMMENT_HEADER)

    after = [t for t in _types_at(data, ["ores", "sample"]) if t.name == "after"]
    assert len(after) == 1
    assert after[0].kind == "class"
    assert [m.name for m in after[0].members] == ["value"]
