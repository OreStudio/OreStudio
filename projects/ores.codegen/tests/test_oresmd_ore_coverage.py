"""Tests for the oresmd-to-ORE coverage check.

The check has value only if it can fail in both directions: a new ORE
series type with no oresmd representation must be reported, and a recorded
exception that no longer applies must be reported too. These tests pin
both halves, plus the two facts that keep the record honest -- every
exception carries a reason, and the sets the check compares are the ones
it claims to compare.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_oresmd_ore_coverage.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_oresmd_ore_coverage as check  # noqa: E402


def test_every_ore_series_type_is_represented_or_recorded():
    assert check.main_with_args(corpus=False) == 0


def test_ore_type_discovery_is_not_vacuous():
    """A parser that read no types would pass every tree."""
    declared = check.ore_types()
    assert len(declared) > 35, f"shape table yielded only {len(declared)} types"
    assert "IR_SWAP" in declared
    assert "BOND" in declared
    assert "GENERIC-MD" in declared, "the hyphenated wrapper type was missed"


def test_oresmd_discovery_is_not_vacuous():
    """The quote-type side must be read from the models, not assumed."""
    modelled = check.oresmd_ore_types()
    assert len(modelled) > 25, f"oresmd models yielded only {len(modelled)} types"
    assert "IR_SWAP" in set(modelled.values())
    assert "FX" in set(modelled.values())


def test_every_exception_carries_a_reason():
    for table_name in ("UNREPRESENTED", "SHAPE_MISMATCH",
                       "VOL_DECLARED_UNWIRED"):
        table = getattr(check, table_name)
        assert table, f"{table_name} is empty"
        for key, reason in table.items():
            assert reason and reason.strip(), f"{table_name}[{key}] has no reason"
            assert reason != "TODO", f"{table_name}[{key}] is a placeholder"


def test_an_unrecorded_gap_is_reported(monkeypatch):
    """The whole point: a type with no representation and no record fails."""
    gap = next(iter(check.UNREPRESENTED))
    monkeypatch.delitem(check.UNREPRESENTED, gap)
    problems = check.coverage_problems()
    assert any(gap in p and "no entry in UNREPRESENTED" in p
               for p in problems), problems


def test_a_newly_modelled_type_makes_its_exception_stale(monkeypatch):
    """Removing entries is the point of the list, so a fixed gap must fail."""
    gap = next(iter(check.UNREPRESENTED))
    monkeypatch.setitem(check.VOL_REPRESENTED, gap, "ir")
    problems = check.coverage_problems()
    assert any(gap in p and "stale" in p for p in problems), problems


def test_an_unwired_vol_asset_class_is_reported(monkeypatch):
    """A vol field with neither a projection nor a record is a silent gap."""
    asset_class = next(iter(check.VOL_DECLARED_UNWIRED))
    monkeypatch.delitem(check.VOL_DECLARED_UNWIRED, asset_class)
    problems = check.coverage_problems()
    assert any(asset_class in p and "no inverse projection" in p
               for p in problems), problems


def test_every_modelled_quote_type_is_exercised_both_ways():
    """A type tested one way only is how a projection its inverse cannot read
    survives a green suite."""
    coverage = check.quote_type_test_coverage()
    assert coverage, "no quote types were discovered"
    untested = [
        key for key, (_, projections, round_trips) in coverage.items()
        if not projections or not round_trips
    ]
    assert set(untested) <= set(check.TEST_COVERAGE_EXEMPT), (
        f"quote types with one-sided test coverage and no recorded reason: "
        f"{sorted(set(untested) - set(check.TEST_COVERAGE_EXEMPT))}"
    )


def test_a_one_sided_quote_type_is_reported(monkeypatch):
    """The check must fail when a round trip is dropped."""
    monkeypatch.setattr(
        check, "quote_type_test_coverage",
        lambda: {"ir.ir_swap": ("IR_SWAP", 3, 0)},
    )
    problems = check.coverage_problems()
    assert any("ir.ir_swap" in p and "no round-trip test" in p
               for p in problems), problems
