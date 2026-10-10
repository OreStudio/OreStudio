"""Tests for scripts/convert_money_column_to_decimal.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_convert_money_column.py

The lever rewrites three places per column, and the build fails if one is
missed. It has also carried two bugs that no test caught -- a rewrite ending in
`\\s*$` that welds the `:END:` drawer closed, and a generator-block search that
ran to the next generator in the file and so rewrote the *neighbour's* block
whenever the named column had none. Both now have a test.
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "convert_money_column_to_decimal",
    REPO_ROOT / "scripts" / "convert_money_column_to_decimal.py")
lever = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(lever)

NEIGHBOUR = """\
** amount
:PROPERTIES:
:type:          numeric(38, 12)
:cpp_type:      ores::utility::decimal::decimal
:END:

An amount.

** rate
:PROPERTIES:
:type:          numeric(38, 12)
:cpp_type:      double
:END:

A rate.

#+begin_src cpp :name generator
0.05
#+end_src
"""

MONEY = """\
** spread
:PROPERTIES:
:type:          double precision
:cpp_type:      double
:default_value: 0.0
:END:

A spread.

#+begin_src cpp :name generator
0.05
#+end_src
"""

CONTINUOUS = """\
** recovery_rate
:PROPERTIES:
:type:          numeric(38, 12)
:cpp_type:      double
:END:

A recovery rate.

#+begin_src cpp :name generator
ores::utility::decimal::decimal::from_string("0.4").value()
#+end_src
"""


def test_money_column_becomes_exact(tmp_path):
    text, changes = lever.convert(MONEY, "spread", 12)
    assert "type" in changes and "cpp_type" in changes and \
        "generator" in changes
    assert ":type:          numeric(38, 12)" in text
    assert ":cpp_type:      ores::utility::decimal::decimal" in text
    assert 'from_string("0.05").value()' in text
    assert ":default_value: ores::utility::decimal::decimal{}" in text


def test_continuous_column_becomes_float(tmp_path):
    text, changes = lever.convert(CONTINUOUS, "recovery_rate", 12,
                                  to_float=True)
    assert "type" in changes and "generator" in changes
    assert ":type:          double precision" in text
    assert ":cpp_type:      double" in text
    assert "from_string" not in text
    assert " 0.4" in text


def test_a_column_with_no_generator_does_not_touch_its_neighbour(tmp_path):
    """The bug the mirror direction exposed: the search ran to the next
    generator block in the whole file, so a column with none rewrote the
    following column's literal."""
    text, changes = lever.convert(NEIGHBOUR, "amount", 12, to_float=True)
    assert not any(c.startswith("generator") for c in changes)
    assert "0.05" in text
    assert "from_string" not in text


def test_a_money_column_with_no_generator_leaves_the_neighbour_bare(tmp_path):
    text, changes = lever.convert(NEIGHBOUR, "amount", 12)
    assert not any(c.startswith("generator") for c in changes)
    assert text.count("from_string") == 0
    assert "\n0.05\n" in text


def test_the_neighbour_becomes_exact_when_it_is_the_target(tmp_path):
    """The same file, naming the column that does own the generator."""
    text, changes = lever.convert(NEIGHBOUR, "rate", 12)
    assert "generator" in changes
    assert 'from_string("0.05").value()' in text


def test_a_welded_drawer_is_refused(tmp_path):
    path = tmp_path / "m.org"
    welded = "** x\n:PROPERTIES:\n:type: numeric(38, 12):END:\n"
    try:
        lever.check_drawers(welded, path, "x")
    except SystemExit as e:
        assert "welded drawer" in str(e)
    else:
        raise AssertionError("a welded drawer must be refused")


def test_an_unknown_column_is_refused():
    try:
        lever.convert(MONEY, "not_a_column", 12)
    except SystemExit as e:
        assert "no column heading" in str(e)
    else:
        raise AssertionError("an unknown column must be refused")


def test_the_baselines_are_readable():
    for name in ("money_decimal_columns.baseline",
                 "continuous_decimal_columns.baseline"):
        path = REPO_ROOT / "build" / "scripts" / name
        assert path.is_file(), name
        assert path.read_text(encoding="utf-8").strip(), name
