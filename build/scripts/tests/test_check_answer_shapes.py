"""The answer-shape check compares shapes, and never values.

Run::

    python3 -m pytest build/scripts/tests/test_check_answer_shapes.py

The check stands behind a gate, so the parts that decide what a difference is
have to be right on their own: what a shape is, how the elements of an array
become one, which facts about the data are not facts about the answer, and what
counts as an excused difference. The requests themselves need two servers and
are not tested here.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "build/scripts"))

import check_answer_shapes as check  # noqa: E402


def test_a_shape_states_member_names_nesting_and_types_and_no_value():
    assert check.shape_of({"total": 3, "rows": [{"id": "a", "legs": 2}], "kinds": []}) == {
        "object": {
            "kinds": {"array": None},
            "rows": {"array": {"object": {"id": ["string"], "legs": ["number"]}}},
            "total": ["number"],
        }
    }


def test_a_value_that_changes_between_rows_changes_nothing():
    # A list of pages read at one instant need not agree on a value, and a shape
    # is about what the members are, so two rows with different totals are one
    # shape.
    first = check.shape_of({"rows": [{"total": 1}, {"total": 900}]})
    second = check.shape_of({"rows": [{"total": 7}]})
    assert first == second


def test_a_null_in_one_row_of_a_page_keeps_the_type_the_other_rows_state():
    # A nullable member is not a different member: the type the other rows state
    # is the answer's.
    mixed = check.shape_of({"latest": [None, {"id": "a", "completedAt": "now"},
                                      {"id": "b", "completedAt": None}]})
    stringy = check.shape_of({"latest": [{"id": "c", "completedAt": "later"}]})
    assert check.differences(mixed, stringy) == []


def test_a_member_every_row_holds_as_null_is_a_null_not_a_gap():
    shape = check.shape_of({"rows": [{"last": None}, {"last": None}]})
    assert check.differences(shape, check.shape_of({"rows": [{"last": None}]})) == []
    assert check.differences(shape, check.shape_of({"rows": [{"last": "now"}]}))


def test_a_record_keyed_by_identifier_is_one_member_and_no_value():
    # The public rates coverage is keyed by series id, and an id is a value.
    shape = check.shape_of({"series": {"00aca86e-05b1-4f69-9cf0-67671a6ef5c0":
                                       {"first": "a", "last": "b", "count": 3},
                                       "03386798-bbfd-4200-b563-c98b266eadf6":
                                       {"first": "c", "last": "d", "count": 4}}})
    assert shape == {"object": {"series": {"object": {"{}": {"object": {
        "count": ["number"], "first": ["string"], "last": ["string"]}}}}}}
    other = check.shape_of({"series": {"ffffffff-ffff-4fff-8fff-ffffffffffff":
                                       {"first": "x", "last": "y", "count": 9}}})
    assert check.differences(shape, other) == []


def test_an_array_no_row_filled_states_nothing_below_it():
    assert check.differences(check.shape_of({"kinds": []}),
                             check.shape_of({"kinds": ["Package"]})) == []


def test_a_missing_member_is_the_difference_the_check_exists_for():
    # The bug this check was written for: an array where the BFF answered an object.
    found = check.differences(check.shape_of({"runs": [{"id": "a"}]}),
                              check.shape_of([{"id": "a"}]))
    assert found == [
        {"path": "(root)",
         "expected": {"object": {"runs": {"array": {"object": {"id": ["string"]}}}}},
         "actual": {"array": {"object": {"id": ["string"]}}}}
    ]


def test_a_member_the_gateway_leaves_out_is_named_by_its_path():
    found = check.differences(check.shape_of({"total": 1, "rows": []}),
                              check.shape_of({"total": 1}))
    assert [(one["path"], one["expected"], one["actual"]) for one in found] == [
        ("rows", {"array": None}, None)
    ]


def test_a_type_that_moved_is_a_difference():
    found = check.differences(check.shape_of({"total": 1}), check.shape_of({"total": "1"}))
    assert [(one["path"], one["expected"], one["actual"]) for one in found] == [
        ("total", ["number"], ["string"])
    ]


def test_an_exception_excuses_only_the_member_it_names():
    exceptions = [{"address": "GET /api/site/deployment", "path": "environment.tls.ca"}]
    excused = {"path": "environment.tls.ca"}
    elsewhere = {"path": "environment.tls.cert"}
    another_route = {"path": "environment.tls.ca"}
    assert check.excused_by(exceptions, "GET /api/site/deployment", excused)
    assert not check.excused_by(exceptions, "GET /api/site/deployment", elsewhere)
    assert not check.excused_by(exceptions, "GET /api/other", another_route)


def test_an_exception_with_no_path_excuses_the_whole_answer():
    exceptions = [{"address": "GET /api/operations/bus", "why": "the permission model"}]
    assert check.excused_by(exceptions, "GET /api/operations/bus", {"path": "code"})


def test_a_pair_is_read_off_the_inventory_with_both_spellings_of_a_parameter():
    rows = [
        {"kind": "resource", "area": "access", "method": "get",
         "path": "/api/accounts/:accountId/access", "server": "bff"},
        {"kind": "resource", "area": "access", "method": "get",
         "path": "/api/accounts/{accountId}/access", "server": "ores.http"},
        {"kind": "resource", "area": "access", "method": "get",
         "path": "/api/accounts", "server": "bff"},
        {"kind": "resource", "area": "access", "method": "get",
         "path": "/api/accounts", "server": "ores.http"},
    ]
    pairs = check.pairs_of(rows)
    assert pairs == [
        {"method": "get", "address": "/api/accounts", "bff": "/api/accounts"},
        {"method": "get", "address": "/api/accounts/{}/access",
         "bff": "/api/accounts/:accountId/access"},
    ]


def test_a_pair_with_a_path_parameter_or_a_write_is_skipped_with_its_reason():
    parameterised = {"method": "get", "address": "/api/accounts/{}/access", "bff": ""}
    writing = {"method": "post", "address": "/api/accounts", "bff": "/api/accounts"}
    readable = {"method": "get", "address": "/api/accounts", "bff": "/api/accounts"}
    assert "path parameter" in check.skip_reason(parameterised)
    assert "writes" in check.skip_reason(writing)
    assert check.skip_reason(readable) is None
