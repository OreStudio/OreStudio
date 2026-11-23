"""Tests for external/ore/tools/script_input_codes.py.

Run::

    python3 -m pytest build/scripts/tests/test_script_input_codes.py

The reader turns a library comment's prose into the codes a form offers, so a
label it misreads is booked under the wrong name.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "script_input_codes", REPO_ROOT / "external" / "ore" / "tools" / "script_input_codes.py")
codes = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = codes
SPEC.loader.exec_module(codes)


def test_articles_and_a_trailing_position_are_dropped():
    assert codes.read("1 for a long, -1 for a short position") == [[1, "long"], [-1, "short"]]


def test_words_every_label_ends_with_are_dropped():
    assert codes.read("1 for American KO style, -1 for European KO style") == [
        [1, "American"], [-1, "European"]]


def test_a_bracketed_note_is_dropped():
    assert codes.read("0 = atHit, 1 = atExpiry (only for out)") == [[0, "atHit"], [1, "atExpiry"]]


def test_equals_signs_with_or_without_spaces_are_read():
    assert codes.read("barrier type 3=DownOut, 4 = UpOut") == [[3, "DownOut"], [4, "UpOut"]]


def test_a_trailing_or_before_the_next_code_is_dropped():
    assert codes.read("the type of barrier event; 1 for DownIn or 2 for UpIn") == [
        [1, "DownIn"], [2, "UpIn"]]


def test_fewer_than_two_codes_is_not_coded():
    assert codes.read("the strike, 1 for at the money") == []
    assert codes.read("option strike") == []
    assert codes.read(None) == []


def test_the_first_statement_of_a_code_wins_and_a_relabel_is_a_conflict():
    line = "1 = KoAlways, 2 = KoBeforeKi, 3 = KoAfterKi, should be 1 for pure KO"
    assert codes.read(line) == [[1, "KoAlways"], [2, "KoBeforeKi"], [3, "KoAfterKi"]]
    assert codes.conflicts(line) == [1]


def test_a_correction_replaces_the_reading():
    fixes = {"S/BarrierType": {"codes": [[1, "DownIn"]], "reason": "misspelt"}}
    assert codes.codes("S", "BarrierType", "Number", "1 = DonwIn, 2 = UpIn", fixes) == [[1, "DownIn"]]


def test_an_input_that_is_not_a_number_is_not_coded():
    assert codes.codes("S", "Dates", "Event", "1 for start, 2 for end", {}) == []
