"""Tests for the value a generated shell script sends for a decimal.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_decimal_sentinel.py

A required ``ores::utility::decimal::decimal`` field once fell through the
sentinel table to ``__none__``. The shell's decimal parser rejects that
token, so 33 ores.trading write recipes aborted at their own client and
checked nothing. The V04 replay found it. These tests pin the mapped
value: a digit the parser accepts and the strict lower bounds the models
state (such as a notional ``> 0``) also accept, and the absent token for an
optional decimal.
"""
import sys
from decimal import Decimal
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import _sentinel_for_field  # noqa: E402

DECIMAL = "ores::utility::decimal::decimal"


def test_required_decimal_sends_a_positive_number():
    token = _sentinel_for_field("notional", DECIMAL)

    assert token != "__none__"
    assert Decimal(token) > 0


def test_optional_decimal_sends_the_absent_token():
    assert _sentinel_for_field("notional", f"std::optional<{DECIMAL}>") == "-"
