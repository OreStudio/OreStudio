"""Tests for the backtick-link guard in validate_docs.py.

The site export aborts on the first unresolved link and stops, so one C++
attribute written inside backticks takes the whole build down: backticks are
not org verbatim markup, and org reads [[nodiscard]] as a link. That reached
main three times in one day, through three different clean passes, while every
other documentation check passed.

The guard is the only thing that looks for the construct before a merge, so
both edges are pinned here: what it flags, and what it must leave alone.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
VALIDATOR = REPO_ROOT / "projects/ores.codegen/validate_docs.py"


def _load():
    spec = importlib.util.spec_from_file_location("validate_docs", VALIDATOR)
    module = importlib.util.module_from_spec(spec)
    sys.modules["validate_docs"] = module
    spec.loader.exec_module(module)
    return module


def _check(tmp_path, body):
    doc_dir = tmp_path / "doc"
    doc_dir.mkdir()
    (doc_dir / "page.org").write_text(body, encoding="utf-8")
    return _load().check_backtick_links(doc_dir)


def test_flags_an_attribute_written_in_backticks(tmp_path):
    body = "| G05 | Pass | dropped every class whose macro is (`class [[nodiscard]] ORES_EXPORT s`) |\n"

    violations = _check(tmp_path, body)

    assert len(violations) == 1
    code, owner, detail = violations[0]
    assert code == "BACKTICK_LINK"
    assert owner == "doc/page.org"
    assert "page.org:1" in detail
    assert "nodiscard" in detail


def test_ignores_an_attribute_in_org_verbatim(tmp_path):
    assert _check(tmp_path, "and =[[nodiscard]]= is the standard attribute\n") == []


def test_ignores_an_attribute_in_a_source_block(tmp_path):
    body = "#+begin_src cpp\nclass [[nodiscard]] thing;\n#+end_src\n"

    assert _check(tmp_path, body) == []


def test_ignores_an_attribute_in_an_example_block(tmp_path):
    body = "#+begin_example\n[[no_unique_address]]\n#+end_example\n"

    assert _check(tmp_path, body) == []


def test_ignores_brackets_that_are_themselves_in_verbatim(tmp_path):
    body = "is known as `=[[nodiscard]]=` in the standard\n"

    assert _check(tmp_path, body) == []


def test_ignores_a_resolvable_link_in_backticks(tmp_path):
    body = "write `[[id:1234-5678][Some Page]]` to link it\n"

    assert _check(tmp_path, body) == []


def test_ignores_a_bare_link_outside_backticks(tmp_path):
    """Only the backtick case is this guard's job; the site build owns the rest."""
    assert _check(tmp_path, "see [[nodiscard]] for the attribute\n") == []


def test_reports_every_offending_line(tmp_path):
    body = "a `[[nodiscard]]` b\nclean line\nc `[[maybe_unused]]` d\n"

    violations = _check(tmp_path, body)

    assert [detail.split(":")[1] for _code, _owner, detail in violations] == ["1", "3"]
