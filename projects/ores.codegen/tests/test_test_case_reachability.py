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
    assert "no test sources found" in captured.err


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


def test_a_line_comment_continued_by_a_backslash_hides_the_next_line(
        tmp_path, monkeypatch, capsys):
    # The backslash splices the lines, so the compiler reads the #if as comment
    # text and the case is unconditional. The scan must agree.
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

// the directive below is quoted, not compiled \\
#if defined(HAS_SOCKETS)
TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "1 declared case(s)" in captured.out


def test_a_directive_written_with_a_space_after_the_hash_is_a_directive(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#  if defined(HAS_SOCKETS)
TEST_CASE("conditional", tags) {
    CHECK(true);
}
#  endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "conditional" in captured.err
    assert "opened at line 3" in captured.err


def test_a_directive_split_by_a_backslash_newline_is_a_directive(
        tmp_path, monkeypatch, capsys):
    # Translation phase 2 removes the splice, so the compiler reads "#if 0".
    # The scan has to agree, or the case vanishes from under it.
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#\\
if 0
TEST_CASE("never compiled", tags) {
    CHECK(true);
}
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "never compiled" in captured.err


def test_a_directive_split_as_end_if_does_not_close_a_region(
        tmp_path, monkeypatch, capsys):
    # "#end\\" + newline + "if" splices to "#endif", so the case below it is
    # unconditional and the gate must not report it.
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
int helper() { return 1; }
#end\\
if

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert "always runs" not in captured.err


def test_a_case_declared_through_a_local_macro_is_a_case(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#define MY_CASE(name) TEST_CASE(name, tags)

#if defined(HAS_SOCKETS)
MY_CASE("hidden behind a macro")
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "hidden behind a macro" in captured.err
    assert "1 of 1 declared case(s)" in captured.err


def test_a_test_source_with_another_cpp_suffix_is_scanned(
        tmp_path, monkeypatch, capsys):
    _write(tmp_path, "projects/widget/tests/widget_tests.cc", """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
TEST_CASE("in a .cc file", tags) {
    CHECK(true);
}
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "widget_tests.cc" in captured.err
    assert "in a .cc file" in captured.err


def test_a_fragment_under_tests_is_scanned(tmp_path, monkeypatch, capsys):
    # A case can live in a header the test source includes, so a guard there
    # hides a case just as well.
    _write(tmp_path, "projects/widget/tests/helpers.hpp", """\
#if defined(HAS_SOCKETS)
TEST_CASE("in a header", tags) {
    CHECK(true);
}
#endif
""")
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "helpers.hpp" in captured.err
    assert "in a header" in captured.err


def test_a_guard_left_open_is_a_finding(tmp_path, monkeypatch, capsys):
    # The guard applies to whatever includes this file, so the case it deletes
    # is in the includer, where nothing looks conditional.
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
int helper() { return 1; }

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "do not balance" in captured.err
    assert "opens a conditional that never closes" in captured.err


def test_an_unmatched_endif_is_a_finding(tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#endif

TEST_CASE("always runs", tags) {
    CHECK(true);
}
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "closes a conditional that nothing opened" in captured.err


def test_a_checkout_under_a_directory_named_build_is_still_scanned(
        tmp_path, monkeypatch, capsys):
    # The skip list names the parts below projects/, not the parts of the
    # absolute path, so where the checkout lives cannot blind the gate.
    root = tmp_path / "build" / "checkout"
    _write(root, SOURCE, """\
#if defined(HAS_SOCKETS)
TEST_CASE("conditional", tags) {
    CHECK(true);
}
#endif
""")
    _point_at(monkeypatch, root)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "conditional" in captured.err


def test_a_case_declared_through_an_object_like_alias_is_a_case(
        tmp_path, monkeypatch, capsys):
    # The replacement names the macro without repeating its argument list,
    # which is the same alias as the function-like form.
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#define MY_CASE TEST_CASE

#if defined(HAS_SOCKETS)
MY_CASE("hidden behind an object-like macro", tags)
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "hidden behind an object-like macro" in captured.err
    assert "1 of 1 declared case(s)" in captured.err


def test_two_cases_on_one_line_are_two_findings(tmp_path, monkeypatch, capsys):
    _write(tmp_path, SOURCE, """\
#include <catch2/catch_test_macros.hpp>

#if defined(HAS_SOCKETS)
TEST_CASE("first", tags) {} TEST_CASE("second", tags) {}
#endif
""")
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "2 of 2 declared case(s)" in captured.err
    # Both cases are counted, and the one line they share is reported once.
    assert captured.err.count("widget_tests.cpp:4:") == 1


def test_a_splice_inside_a_raw_string_is_reported_not_guessed(
        tmp_path, monkeypatch, capsys):
    # A compiler keeps the backslash-newline inside the raw string and the
    # literal ends at the last line, so the #if below is string content. The
    # scan removes the splice, which moves where the literal ends, and can no
    # longer tell string content from code. It must say so rather than guess.
    _write(tmp_path, SOURCE, r'''#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}

const char* sample = R"(xx)\
" ;
)";
''')
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "whose raw strings this check cannot read" in captured.err
    assert "cannot be settled from the file alone" in captured.err
    # The file is reported once, for the reason the reading failed, and its
    # cases are left out of the census rather than counted from a bad reading.
    assert "inside a conditional compilation block" not in captured.err


def test_a_raw_string_without_a_splice_is_string_content(
        tmp_path, monkeypatch, capsys):
    # The same directives, inside a raw string, with no splice: the compiler
    # reads them as string content and so must the scan.
    _write(tmp_path, SOURCE, r'''#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}

const char* sample = R"(
#if 0
TEST_CASE("not code", tags) {}
#endif
)";
''')
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "not code" not in captured.err
    assert "1 declared case(s)" in captured.out


def test_a_raw_string_opener_split_by_a_splice_is_reported(
        tmp_path, monkeypatch, capsys):
    # R\ + newline + "( splices to R"(, so a compiler sees a raw string while
    # the source on its own shows a caret, a backslash and a quote. Reading the
    # source alone therefore finds no literal at all, and the guarded case
    # below it would be read as code and then lost. The two readings disagree,
    # and the file is reported.
    _write(tmp_path, SOURCE, r'''#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}

const char* sample = R\
"(xx)\
" ;
)";
''')
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 1
    captured = capsys.readouterr()
    assert "whose raw strings this check cannot read" in captured.err


def test_a_splice_that_moves_no_raw_string_is_left_alone(
        tmp_path, monkeypatch, capsys):
    # The splice is inside the literal's content and moves neither end, so
    # both readings agree and there is nothing to report.
    _write(tmp_path, SOURCE, r'''#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}

const char* sample = R"(first\
second)";
''')
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "1 declared case(s)" in captured.out


def test_a_splice_immediately_after_a_raw_string_does_not_move_it(
        tmp_path, monkeypatch, capsys):
    # The splice sits after the closing quote, so the literal is the same in
    # both readings and there is nothing to report. Carrying the spliced span
    # back by the offset of the character after it, rather than of its own last
    # character, would overshoot by the deleted splice and reject a valid file.
    _write(tmp_path, SOURCE, r'''#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}

const char* sample = R"(ab)"\
"cd";
''')
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "1 declared case(s)" in captured.out


def test_a_splice_between_two_raw_strings_moves_neither(
        tmp_path, monkeypatch, capsys):
    # Both literals keep their extent in both readings, so the two readings
    # agree. Neither is reported, and the splice between them is not a reason.
    _write(tmp_path, SOURCE, r'''#include <catch2/catch_test_macros.hpp>

TEST_CASE("always runs", tags) {
    CHECK(true);
}

const char* first = R"(ab)"\
R"(cd)";
''')
    _point_at(monkeypatch, tmp_path)

    assert check.main() == 0
    captured = capsys.readouterr()
    assert captured.err == ""
    assert "1 declared case(s)" in captured.out


def test_the_real_tree_has_cases_and_declares_none_conditionally(capsys):
    """The invariant the gate exists for, checked against the tree itself."""
    assert check.main() == 0
    captured = capsys.readouterr()
    assert "none inside a conditional compilation block" in captured.out
    declared = int(re.search(r"(\d+) declared", captured.out).group(1))
    sources = int(re.search(r"in (\d+) test source", captured.out).group(1))
    assert declared > 0
    assert sources > 0
