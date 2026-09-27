"""Tests for the registry's accepted exceptions.

A listed component may carry checklist items it does not pass, so long as each
is recorded with its reason and the person who accepted it. These tests are what
keeps the record honest: they check the registry against the standard's own item
list, so an exception cannot name an item that does not exist, state no reason,
name nobody, or be recorded against a component the gates do not even check.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_component_registry.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

from check_registry_exceptions import checklist_items, STANDARD  # noqa: E402
from component_registry import (  # noqa: E402
    ACCEPTED_EXCEPTIONS,
    COMPONENTS_UNDER_TEST,
    accepted_exceptions,
)


def test_the_standard_defines_checklist_items():
    """The item list is derived, so a standard that parses to nothing is a bug."""
    items = checklist_items(STANDARD)
    assert len(items) > 40
    assert {"B01", "M01", "P01", "G01", "W01", "S01", "H01", "V01"} <= items


def test_every_exception_names_an_item_the_standard_defines():
    items = checklist_items(STANDARD)
    for component, exceptions in ACCEPTED_EXCEPTIONS.items():
        for entry in exceptions:
            assert entry.item in items, f"{component} names {entry.item}"


def test_every_exception_states_a_reason_and_who_accepted_it():
    for component, exceptions in ACCEPTED_EXCEPTIONS.items():
        for entry in exceptions:
            assert entry.reason.strip(), f"{component} {entry.item} has no reason"
            assert entry.accepted_by.strip(), f"{component} {entry.item} names nobody"
            assert len(entry.accepted_on) == 10, f"{component} {entry.item} has no date"


def test_no_component_records_the_same_item_twice():
    for component, exceptions in ACCEPTED_EXCEPTIONS.items():
        items = [entry.item for entry in exceptions]
        assert len(items) == len(set(items)), component


def test_an_exception_is_only_recorded_against_a_listed_component():
    """An exception for a component the gates do not check means nothing."""
    for component in ACCEPTED_EXCEPTIONS:
        assert component in COMPONENTS_UNDER_TEST, component


def test_a_component_with_no_exception_reports_none():
    assert accepted_exceptions("iam") == ()


def test_variability_records_the_one_item_it_does_not_pass():
    """H01 was retired when the diagram capture learned to draw methods."""
    assert {e.item for e in accepted_exceptions("variability-cpp")} == {"V08"}
