"""Tests for check_test_case_reachability.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_test_case_reachability.py

The check fails when a ``TEST_CASE`` sits inside a conditional compilation
block, because a false conditional deletes the case: the file compiles, the
link succeeds, and the suite reports green with a case count nothing compares
against the sources. Most of these tests drive the check against a throw-away
tree. The last one runs it against the real tree, which is what stops the check
from passing every tree by finding nothing.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_test_case_reachability as check  # noqa: E402

SOURCE_DIR = "projects/widget/tests"
SOURCE = "projects/widget/tests/widget_tests.cpp"


def _write(root: Path, name: str, body: str) -> Path:
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")
    return path


def _point_at(monkeypatch, root: Path) -> None:
    monkeypatch.setattr(check, "REPO_ROOT", root)
    monkeypatch.setattr(check, "PROJECTS_DIR", root / "projects")


def test_case_inside_a_conditional_fails(tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
TEST_CASE("reads the socket", tags) {
    CHECK(true);
}
#endif

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "widget_tests.cpp:4" in captured.err
    assert "reads the socket" in captured.err
    assert "opened at line 3" in captured.err
    assert "#if defined(HAS_SOCKETS)" in captured.err
    assert "always runs" not in captured.err
    assert "1 of 2 declared case(s)" in captured.err


def test_case_unconditional_with_a_conditional_helper_passes(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
int helper() { return 1; }
#endif

TEST_CASE("always runs", tags) {
#if defined(HAS_SOCKETS)
    CHECK(helper() == 1);
#else
    SKIP("no sockets here");
#endif
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "1 declared case(s) in 1 test source(s)" in captured.out


def test_conditional_keywords_in_comments_do_not_open_a_region(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

// #if defined(HAS_SOCKETS)
/* #ifdef HAS_SOCKETS */
TEST_CASE("always runs", tags) {
    // TEST_CASE("commented out", tags) {}
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    # The commented-out case is not a case: the count is one, not two.
    assert "1 declared case(s)" in captured.out


def test_a_case_named_inside_a_raw_string_is_not_a_case(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

const char* sample = R"(
#if defined(HAS_SOCKETS)
TEST_CASE("inside a raw string", tags) {}
#endif
)";

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "1 declared case(s)" in captured.out


def test_a_case_after_else_is_still_inside_the_region(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
int helper() { return 1; }
#elif defined(HAS_PIPES)
TEST_CASE("uses pipes", tags) {
    CHECK(true);
}
#else
TEST_CASE("uses nothing", tags) {
    CHECK(true);
}
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "2 of 2 declared case(s)" in captured.err
    assert "uses pipes" in captured.err
    assert "uses nothing" in captured.err
    assert "opened at line 3" in captured.err


def test_the_innermost_open_region_is_the_one_reported(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(OUTER)
#if defined(INNER)
TEST_CASE("nested", tags) {
    CHECK(true);
}
#endif
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "opened at line 4" in captured.err
    assert "#if defined(INNER)" in captured.err


def test_a_case_after_the_closing_endif_passes(tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
int helper() { return 1; }
#endif

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0


def test_a_tree_with_no_test_sources_fails(tmp_path, monkeypatch, capsys):
    _write(tmp_path, "projects/widget/src/widget.cpp", "int main() {}\n")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "no test sources matched" in captured.err


def test_a_test_source_that_declares_no_case_fails(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, "#include <catch2/catch_test_macros.hpp>\n")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "no test case declared in 1 test source(s)" in captured.err


def test_vendored_trees_are_not_scanned(tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    # A conditional case under node_modules must not be reported: the gate
    # looks at our sources, and a vendored tree is not ours to fix.
    _write(tmp_path, "projects/widget/node_modules/lib/vendor_tests.cpp", """\
#if defined(VENDOR)
TEST_CASE("vendor", tags) {
    CHECK(true);
}
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert "vendor_tests.cpp" not in captured.err


def test_the_real_tree_has_cases_and_declares_none_conditionally(capsys):
    """The invariant the gate exists for, checked against the tree itself."""
    assert check.main() == 0
    captured = capsys.readouterr()
    assert "none inside a conditional compilation block" in captured.out
    declared = int(re.search(r"(\d+) declared", captured.out).group(1))
    sources = int(re.search(r"in (\d+) test source", captured.out).group(1))
    assert declared > 0
    assert sources > 0
