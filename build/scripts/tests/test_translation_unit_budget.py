"""The translation unit budget check counts a file's families only when it reflects."""

import importlib.util
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_translation_unit_budget", ROOT / "build/scripts/check_translation_unit_budget.py")
budget = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(budget)

TYPES = ["zero_convention", "ois_convention", "convention"]


def test_a_file_counts_each_family_type_it_names_once():
    text = "domain::zero_convention a; domain::zero_convention b; domain::ois_convention c;"

    assert budget.families_named(text, TYPES) == {"zero_convention", "ois_convention"}


def test_a_longer_name_does_not_count_as_the_shorter_family():
    assert budget.families_named("domain::convention_family_entry x;", TYPES) == set()
    assert budget.families_named("domain::convention x;", TYPES) == {"convention"}


def test_only_a_file_that_reflects_is_counted():
    assert budget.reflects("auto r = rfl::json::read<T>(s);")
    assert budget.reflects('#include <rfl/json.hpp>')
    assert not budget.reflects("domain::zero_convention z;")


def test_a_unit_that_includes_the_reflecting_header_reflects():
    unit = '#include "conventions_family_pack.hpp"\nrefdata::domain::zero_convention z;'

    assert budget.reflects(unit)
    assert not budget.reflects("refdata::domain::zero_convention z;")


def test_a_file_over_its_budget_fails_and_one_at_it_passes():
    counts = {"a.cpp": 2, "b.cpp": 1}

    failures = budget.judge(counts, known={})

    assert len(failures) == 1
    assert failures[0].startswith("a.cpp reflects 2 convention families")


def test_a_known_file_holds_its_ceiling_and_cannot_grow():
    known = {"a.cpp": (3, "one element")}

    assert budget.judge({"a.cpp": 3}, known=known) == []
    assert budget.judge({"a.cpp": 4}, known=known)[0].startswith("a.cpp reflects 4")


def test_a_known_file_that_improved_must_lower_its_ceiling():
    failures = budget.judge({"a.cpp": 1}, known={"a.cpp": (3, "one element")})

    assert len(failures) == 1
    assert "below its recorded ceiling of 3" in failures[0]


def test_the_tree_agrees_with_the_budget():
    assert budget.judge(budget.measure()) == []
