"""Tests for member functions in generate_component_puml.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_component_parse_member_functions.py

The capture read data members only, so a class whose content is member
functions arrived as an empty box. PlantUML cannot be made to attach
hand-written members to a class inside a nested namespace -- it draws a second,
empty namespace instead -- so the fix belongs here, in the parser.

A member function is a declaration whose name is the last identifier before its
parameter list. The declaration may be split over several lines, a specifier
may open the line, and the body may open on the same line, in which case the
parser has to account for that brace itself because consuming the line skips
its brace bookkeeping.
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


METHOD_HEADER = """\
namespace ores::sample {

class service {
public:
    service();
    ~service();

    void start(int port);
    std::string name_of(int id) const;
    static int count();
    template <typename T>
    T convert(const T& value);
    bool ready() {
        return ready_;
    }
    void removed() = delete;
    void defaulted() = default;

    static std::vector<ores::nats::service::subscription>
    register_handlers(ores::nats::service::client& nats,
                      const std::string& base_url);

    int run(int limit) {
        if (limit > 0) {
            return limit;
        }
        for (int i = 0; i < limit; ++i) {
            total_ += i;
        }
        return 0;
    }

private:
    std::string label;
    int retries = 3;
    int total_ = 0;
};

}
"""


def _type(tmp_path: Path):
    header = tmp_path / "sample.hpp"
    header.write_text(METHOD_HEADER, encoding="utf-8")
    data = gen.parse_header(header)
    found = [t for t in data.get(("ores", "sample"), []) if t.name == "service"]
    assert len(found) == 1
    return found[0]


def test_methods_are_parsed_with_their_signatures(tmp_path):
    methods = [(m.name, m.type_str, m.params) for m in _type(tmp_path).methods]

    assert methods == [
        ("service", "", ""),
        ("~service", "", ""),
        ("start", "void", "int port"),
        ("name_of", "string", "int id"),
        ("count", "int", ""),
        ("convert", "T", "const T& value"),
        ("ready", "bool", ""),
        ("register_handlers", "vector<ores::nats::service::subscription>",
         "ores::nats::service::client& nats, const string& base_url"),
        ("run", "int", "int limit"),
    ]


def test_deleted_and_defaulted_members_are_not_methods(tmp_path):
    names = [m.name for m in _type(tmp_path).methods]

    assert "removed" not in names
    assert "defaulted" not in names


def test_statements_in_a_body_are_not_methods(tmp_path):
    names = [m.name for m in _type(tmp_path).methods]

    for statement in ("if", "for", "while", "switch", "return"):
        assert statement not in names


def test_a_body_opening_on_the_declaration_line_does_not_leak_its_locals(tmp_path):
    members = [(m.name, m.type_str) for m in _type(tmp_path).members]

    assert members == [("label", "string"), ("retries", "int"), ("total_", "int")]


def test_rendered_output_carries_the_signatures(tmp_path):
    service = _type(tmp_path)
    text = gen.generate_puml("ores.sample", {("ores", "sample"): [service]})

    assert "    +start(int port) : void" in text
    assert "    +ready() : bool" in text
    assert "    +service()" in text
    assert "    -label : string" in text
    assert "removed" not in text
