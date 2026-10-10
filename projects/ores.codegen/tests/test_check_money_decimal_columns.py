"""Tests for build/scripts/check_money_decimal_columns.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_money_decimal_columns.py

The check enforces both halves of the numeric rule: a money quantity is exact
decimal in the domain and the column, and a continuous quantity is binary
float. The second half was added because the first was gated while the mirror
error went unnoticed -- ten columns were exact at rest, four of them with a
`double` over a `numeric`, which is the shape that is exact at rest and lossy
in the domain.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_money_decimal_columns",
    REPO_ROOT / "build" / "scripts" / "check_money_decimal_columns.py")
check = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(check)


def tree(tmp_path, name, body):
    """A repo root holding one model file."""
    path = tmp_path / "projects" / "ores.trading" / "modeling" / f"{name}.org"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")
    return tmp_path


def column(name, sql_type, cpp_type, extra=""):
    return (f"** {name}\n:PROPERTIES:\n:type:          {sql_type}\n"
            f":cpp_type:      {cpp_type}\n:END:\n{extra}\n")


def names(offenders):
    return [o[2] for o in offenders]


def test_money_as_float_is_reported(tmp_path):
    root = tree(tmp_path, "ores.trading.t", column("strike", "double precision",
                                                    "double"))
    assert names(check.money_offenders(root)) == ["strike"]


def test_money_exact_in_sql_but_lossy_in_domain_is_reported(tmp_path):
    root = tree(tmp_path, "ores.trading.t",
                column("notional", "numeric(38, 12)", "double"))
    assert names(check.money_offenders(root)) == ["notional"]


def test_money_exact_in_both_places_is_clean(tmp_path):
    root = tree(tmp_path, "ores.trading.t",
                column("spread", "numeric(38, 12)",
                       "std::optional<ores::utility::decimal::decimal>"))
    assert names(check.money_offenders(root)) == []


def test_continuous_typed_exact_is_reported(tmp_path):
    root = tree(tmp_path, "ores.trading.t",
                column("recovery_rate", "numeric(38, 12)", "double"))
    assert names(check.continuous_offenders(root)) == ["recovery_rate"]


def test_continuous_typed_float_is_clean(tmp_path):
    root = tree(tmp_path, "ores.trading.t",
                column("weight", "double precision",
                       "std::optional<double>"))
    assert names(check.continuous_offenders(root)) == []


def test_a_money_token_loses_to_a_continuous_token(tmp_path):
    """`recovery_rate` carries `rate`; the continuous token must win."""
    root = tree(tmp_path, "ores.trading.t",
                column("recovery_rate", "numeric(38, 12)", "double"))
    assert names(check.money_offenders(root)) == []


def test_an_excluded_column_is_not_reported(tmp_path):
    """A variance strike is volatility, not money."""
    root = tree(tmp_path, "ores.trading.fx_variance_swap_instrument",
                column("strike", "double precision", "double"))
    assert names(check.money_offenders(root)) == []


def test_an_embedded_money_member_is_reported(tmp_path):
    """A field group declares no table, but its members become one's columns."""
    body = "** amount\n:PROPERTIES:\n:cpp_type:      double\n:END:\n"
    root = tree(tmp_path, "ores.trading.t_field_group", body)
    offenders = list(check.money_offenders(root))
    assert [o[2] for o in offenders] == ["amount"]
    assert offenders[0][3] == "(embedded)"


def test_a_junction_column_is_read(tmp_path):
    """A junction declares its columns with :column:, not `** name`."""
    body = ("* Left\n:PROPERTIES:\n:column: party_id\n:END:\n"
            "* Right\n:PROPERTIES:\n:column: spread\n"
            ":type:          double precision\n:cpp_type:      double\n:END:\n")
    root = tree(tmp_path, "ores.trading.t_junction", body)
    assert names(check.money_offenders(root)) == ["spread"]


def test_main_fails_when_a_column_is_new(tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, "ores.trading.t",
                column("strike", "double precision", "double"))
    monkeypatch.setattr(sys, "argv",
                        ["check", "--root", str(root), "--min-columns", "1"])
    assert check.main() == 1
    assert "NEW money columns not exact" in capsys.readouterr().out


def test_main_fails_when_it_reads_too_few_columns(tmp_path, monkeypatch,
                                                  capsys):
    root = tree(tmp_path, "ores.trading.t",
                column("spread", "numeric(38, 12)",
                       "ores::utility::decimal::decimal"))
    monkeypatch.setattr(sys, "argv",
                        ["check", "--root", str(root), "--min-columns", "1000"])
    assert check.main() == 1
    assert "pass without looking at anything" in capsys.readouterr().out


def test_main_passes_a_clean_tree(tmp_path, monkeypatch, capsys):
    body = (column("spread", "numeric(38, 12)",
                   "ores::utility::decimal::decimal")
            + column("weight", "double precision", "double"))
    root = tree(tmp_path, "ores.trading.t", body)
    monkeypatch.setattr(sys, "argv",
                        ["check", "--root", str(root), "--min-columns", "1"])
    assert check.main() == 0
    assert "NEW " not in capsys.readouterr().out
