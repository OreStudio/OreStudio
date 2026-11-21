"""Tests for build/scripts/check_unity_collisions.py.

Run::

    python3 -m pytest build/scripts/tests/test_unity_checks.py

The script guards the unity build: it refuses a file-local name two sources
of a target define. Each test names the defect it would let through.
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


def test_an_unbalanced_parenthesis_on_a_directive_does_not_hide_later_names():
    text = """
#define OPEN_CALL(x) f((x)
namespace {
int helper() { return 1; }
}
"""
    assert collisions.internal_names(text) == {("", "helper")}
