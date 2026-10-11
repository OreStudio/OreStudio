"""Tests for scripts/declare_economic_fields.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_declare_economic_fields.py

The lever applies the digest's old name rule once and writes the answer into
the model. Each case below is a place the rule is easy to get wrong.
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "declare_economic_fields",
    REPO_ROOT / "scripts" / "declare_economic_fields.py")
lever = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(lever)

MODEL = """\
* Columns

** notional
:PROPERTIES:
:type:     numeric(38, 12)
:cpp_type: ores::utility::decimal::decimal
:END:

The notional.

** security_id
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

The ISIN.

** issue_id
:PROPERTIES:
:type:     uuid
:cpp_type: boost::uuids::uuid
:END:

The issue key.

** trade_id
:PROPERTIES:
:group:    identity
:type:     uuid
:cpp_type: boost::uuids::uuid
:END:

Carried by the identity group.

** shadow
:PROPERTIES:
:sql_only: true
:type:     text
:END:

A column with no C++ member.

* Foreign keys

** notional
:PROPERTIES:
:table: other
:END:
"""


def _declared(text):
    out = {}
    name = None
    for line in text.splitlines():
        if line.startswith("** "):
            name = line[3:].strip()
        elif line.startswith(":economic:"):
            out.setdefault(name, []).append(line.split()[1])
    return out


def test_a_name_is_economic_unless_the_old_rule_excluded_it():
    assert lever.is_economic("notional")
    assert lever.is_economic("security_id")
    assert not lever.is_economic("issue_id")
    assert not lever.is_economic("modified_by")
    assert not lever.is_economic("description")
    assert not lever.is_economic("id")


def test_it_declares_each_column_that_reaches_the_struct():
    new, added = lever.declare(MODEL)

    assert added == 3
    assert _declared(new) == {
        "notional": ["true"], "security_id": ["true"], "issue_id": ["false"]}


def test_it_leaves_an_identity_group_column_and_a_sql_only_column_alone():
    new, _ = lever.declare(MODEL)

    declared = _declared(new)
    assert "trade_id" not in declared
    assert "shadow" not in declared


def test_it_does_not_touch_a_heading_outside_the_columns():
    new, _ = lever.declare(MODEL)

    assert new.split("* Foreign keys")[1] == MODEL.split("* Foreign keys")[1]


def test_running_it_twice_changes_nothing():
    once, _ = lever.declare(MODEL)
    twice, added = lever.declare(once)

    assert added == 0
    assert twice == once


def test_the_new_line_lines_up_with_the_drawer():
    new, _ = lever.declare(MODEL)

    line = next(row for row in new.splitlines() if row.startswith(":economic:"))
    type_line = next(row for row in new.splitlines() if row.startswith(":type:"))
    assert line.index("true") == type_line.index("numeric")
