"""Tests for build/scripts/check_unity_collisions.py and check_unique_type_names.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_unity_checks.py

The two scripts guard the unity build: one refuses a file-local name two
sources of a target define, the other a type two headers define under one
qualified name. Each test names the defect it would let through.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]


def load(name):
    spec = importlib.util.spec_from_file_location(name, REPO_ROOT / "build" / "scripts" / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


collisions = load("check_unity_collisions")
type_names = load("check_unique_type_names")


def test_a_type_in_a_nested_namespace_written_over_several_lines_is_qualified_in_full():
    text = """
namespace ores {
namespace iam {
namespace messaging {

struct list_role_permissions_request {
    int offset;
};

}
}
}
"""
    assert type_names.defined_types(text) == {"ores::iam::messaging::list_role_permissions_request"}


def test_a_type_in_a_sibling_namespace_is_qualified_by_its_own_namespace():
    text = """
namespace ores::iam::domain { struct account {}; }
namespace ores::iam::messaging { struct account {}; }
"""
    assert type_names.defined_types(text) == {
        "ores::iam::domain::account", "ores::iam::messaging::account"}


def test_a_type_in_a_class_a_function_or_an_anonymous_namespace_is_not_a_namespace_type():
    text = """
namespace ores::x {
namespace { struct hidden {}; }
class ORES_X_API outer final : public base<int> {
    struct inner {};
};
inline void f() { struct local {}; }
}
"""
    assert type_names.defined_types(text) == {"ores::x::outer"}


def test_a_brace_in_a_comment_a_string_or_a_directive_does_not_move_the_scope():
    text = """
namespace ores::x {
// a stray { in a comment
#define OPEN {
inline const char* s = "}";
struct kept {};
}
"""
    assert type_names.defined_types(text) == {"ores::x::kept"}


def test_a_forward_declaration_is_not_a_definition():
    assert type_names.defined_types("namespace ores::x { struct later; }") == set()


def test_two_public_headers_or_a_test_header_and_a_public_header_clash():
    public_twice = {"a::t": {type_names.PUBLIC: {"x/include/a.hpp", "y/include/b.hpp"}}}
    test_and_public = {"a::t": {type_names.PUBLIC: {"x/include/a.hpp"},
                                "projects/ores.x/core/tests": {"projects/ores.x/core/tests/t.hpp"}}}
    assert set(type_names.clashes(public_twice)) == {"a::t"}
    assert set(type_names.clashes(test_and_public)) == {"a::t"}


def test_test_headers_clash_only_within_one_test_target():
    apart = {"a::t": {"p/x/tests": {"p/x/tests/t.hpp"}, "p/y/tests": {"p/y/tests/t.hpp"}}}
    together = {"a::t": {"p/x/tests": {"p/x/tests/t.hpp", "p/x/tests/u.hpp"}}}
    assert type_names.clashes(apart) == {}
    assert set(type_names.clashes(together)) == {"a::t"}


def test_a_header_is_public_under_include_and_otherwise_belongs_to_its_test_target():
    root = type_names.ROOT
    assert type_names.binary_of(root / "projects/ores.x/core/include/ores.x.core/a.hpp") == "public"
    assert type_names.binary_of(root / "projects/ores.x/core/tests/fixtures/a.hpp") == \
        "projects/ores.x/core/tests"


def test_an_anonymous_namespace_helper_and_a_namespace_scope_constant_are_internal():
    text = """
namespace ores::x {
namespace {
int helper() { return 1; }
}
const int limit = 3;
inline constexpr int shared = 4;
int exported() { return 2; }
}
"""
    assert collisions.internal_names(text) == {("ores::x", "helper"), ("ores::x", "limit")}


def test_two_files_of_one_target_in_different_directories_collide(tmp_path, monkeypatch):
    target = tmp_path / "projects/ores.x/core/src"
    for sub in ("a", "b"):
        (target / sub).mkdir(parents=True)
        (target / sub / "f.cpp").write_text("namespace {\nint helper() { return 1; }\n}\n")
    (target / "CMakeLists.txt").write_text("")
    monkeypatch.setattr(collisions, "ROOT", tmp_path)
    collisions.target_dir.cache_clear()
    try:
        found = collisions.collisions()
    finally:
        collisions.target_dir.cache_clear()
    assert found == {("projects/ores.x/core/src", "", "helper"):
                     ["projects/ores.x/core/src/a/f.cpp", "projects/ores.x/core/src/b/f.cpp"]}


def test_a_new_file_that_sorts_first_is_excluded_before_a_committed_one():
    group = {("t", "", "helper"): ["projects/a_new.cpp", "projects/b_old.cpp"]}
    assert collisions.excluded(group, committed={"projects/b_old.cpp"}) == {"projects/a_new.cpp"}
    assert collisions.excluded(group) == {"projects/b_old.cpp"}


def test_a_log_path_relative_to_the_build_directory_is_blamed():
    source = "projects/ores.analytics/quant/src/service/processes/libor_market_model_process.cpp"
    log = (f"../../../{source}:1:1: error: redefinition of 'helper'\n"
           "../../../projects/ores.none/missing.cpp:2:1: error: redefinition of 'helper'\n")
    assert collisions.blamed(log) == ({source}, set())


def test_a_const_member_function_defined_out_of_line_is_not_file_local():
    text = """
namespace ores::x {
const std::vector<double>& process::factors() const { return factors_; }
}
"""
    assert collisions.internal_names(text) == set()


def test_a_parameter_on_its_own_line_is_not_a_namespace_scope_constant():
    text = """
namespace {
std::string entry(const std::string& kind,
const std::string& id,
int position) { return kind + id; }
}
"""
    assert collisions.internal_names(text) == {("", "entry")}
