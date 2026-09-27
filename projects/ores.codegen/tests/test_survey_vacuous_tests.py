"""Tests for the vacuous-assertion classifier.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_survey_vacuous_tests.py

The classifier decides whether a TEST_CASE carries at least one assertion
that could distinguish a subject that worked from one that returned
nothing. Its two hard cases are the ones worth pinning: a `bool found` the
test computed inside a loop is a real assertion even though it is a bare
identifier, and a comparison on a container's size is not one however the
right-hand side is spelled.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import survey_vacuous_tests as survey  # noqa: E402


def test_a_non_throw_alone_is_vacuous():
    verdict, _ = survey.classify("CHECK_NOTHROW(repo.write(ctx, row));")
    assert verdict == "VACUOUS"


def test_emptiness_and_length_alone_are_vacuous():
    assert survey.classify("CHECK(!rows.empty());")[0] == "VACUOUS"
    assert survey.classify("CHECK(rows.size() >= written.size());")[0] == "VACUOUS"
    assert survey.classify("CHECK(rows.size() == 3);")[0] == "VACUOUS"


def test_a_size_comparison_is_weak_even_when_the_other_side_is_a_call():
    # The right-hand side being a method call does not make a count a value.
    assert survey.classify("CHECK(rows.size() >= written_rows.size());")[0] == "VACUOUS"


def test_a_field_comparison_is_strong():
    assert survey.classify("CHECK(rows[0].name == row.name);")[0] == "OK"


def test_a_computed_flag_is_strong():
    body = (
        "bool found = false;\n"
        "for (const auto& r : rows) {\n"
        "    if (r.code == row.code) {\n"
        "        found = true;\n"
        "        CHECK(r.name == row.name);\n"
        "    }\n"
        "}\n"
        "CHECK(found);\n"
    )
    assert survey.classify(body)[0] == "OK"


def test_an_unassigned_identifier_is_weak():
    # A flag the test never computed tells us nothing about the subject.
    assert survey.classify("CHECK(ok);")[0] == "VACUOUS"


def test_one_strong_assertion_rescues_a_case():
    body = "CHECK(rows.size() == 1);\nCHECK(rows[0].code == row.code);\n"
    assert survey.classify(body)[0] == "OK"


def test_no_assertion_is_reported_separately():
    assert survey.classify("repo.write(ctx, row);")[0] == "NO_ASSERTION"
