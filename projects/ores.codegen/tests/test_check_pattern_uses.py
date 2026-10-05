"""Tests for build/scripts/check_pattern_uses.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_pattern_uses.py

A service architecture pattern names its uses, and each use names the pattern.
The check reports a link that goes one way only, and a Patterns used entry that
is not a pattern.
"""
import importlib.util
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_pattern_uses", REPO_ROOT / "build" / "scripts" / "check_pattern_uses.py")
check_pattern_uses = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(check_pattern_uses)

PATTERN = "11111111-1111-1111-1111-111111111111"
USE = "22222222-2222-2222-2222-222222222222"
OTHER = "33333333-3333-3333-3333-333333333333"


def page(did, kind, body):
    return f":PROPERTIES:\n:ID: {did}\n:END:\n#+title: t\n#+type: {kind}\n\n{body}\n"


def link(did, text="x"):
    return f"[[id:{did}][{text}]]"


def tree(tmp_path, pages):
    root = tmp_path / "doc"
    for rel, text in pages.items():
        p = tmp_path / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")
    return check_pattern_uses.load([root], tmp_path)


def pattern_page(uses):
    return page(PATTERN, "service_architecture_pattern",
                "* In ORE Studio\n\n" + "\n".join(f"- {link(u)}" for u in uses)
                + "\n\n* Sources\n")


def test_links_both_ways_pass(tmp_path):
    docs = tree(tmp_path, {
        "doc/p.org": pattern_page([USE]),
        "doc/u.org": page(USE, "knowledge",
                          f"* Patterns used\n\n- {link(PATTERN)}\n\n* See also\n"),
    })
    assert check_pattern_uses.check(docs) == []


def test_a_use_that_does_not_link_back_is_reported(tmp_path):
    docs = tree(tmp_path, {
        "doc/p.org": pattern_page([USE]),
        "doc/u.org": page(USE, "knowledge", "* Detail\n\nno link here\n"),
    })
    problems = check_pattern_uses.check(docs)
    assert len(problems) == 1
    assert "doc/u.org is a use listed by doc/p.org" in problems[0]


def test_a_link_in_the_body_counts_as_the_back_link(tmp_path):
    docs = tree(tmp_path, {
        "doc/p.org": pattern_page([USE]),
        "doc/u.org": page(USE, "knowledge", f"* Detail\n\nUses {link(PATTERN)}.\n"),
    })
    assert check_pattern_uses.check(docs) == []


def test_a_pattern_used_but_not_listing_the_page_is_reported(tmp_path):
    docs = tree(tmp_path, {
        "doc/p.org": pattern_page([]),
        "doc/u.org": page(USE, "knowledge", f"* Patterns used\n\n- {link(PATTERN)}\n"),
    })
    problems = check_pattern_uses.check(docs)
    assert problems == ["doc/u.org uses doc/p.org, which does not list it under In ORE Studio"]


def test_patterns_used_naming_a_page_that_is_not_a_pattern_is_reported(tmp_path):
    docs = tree(tmp_path, {
        "doc/o.org": page(OTHER, "knowledge", "* Detail\n"),
        "doc/u.org": page(USE, "knowledge", f"* Patterns used\n\n- {link(OTHER)}\n"),
    })
    problems = check_pattern_uses.check(docs)
    assert problems == ["doc/u.org lists doc/o.org under Patterns used, "
                        "which is not a service architecture pattern"]


@pytest.mark.parametrize("folder", ["doc/plans", "doc/knowledge/external", "doc/agile"])
def test_plans_external_pages_and_agile_records_are_exempt(tmp_path, folder):
    docs = tree(tmp_path, {
        "doc/p.org": pattern_page([USE]),
        f"{folder}/u.org": page(USE, "knowledge", "* Detail\n"),
    })
    assert check_pattern_uses.check(docs) == []


def test_a_lower_case_link_counts(tmp_path):
    docs = tree(tmp_path, {
        "doc/p.org": pattern_page([USE]),
        "doc/u.org": page(USE, "knowledge", f"* Detail\n\n{link(PATTERN.lower())}\n"),
    })
    assert check_pattern_uses.check(docs) == []
