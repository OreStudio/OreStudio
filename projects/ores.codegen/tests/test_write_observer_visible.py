"""A model whose entity is observed must have its repository see the observer.

A component observes an entity's writes by specialising entity_write_observer,
and the repository only observes when it includes that specialisation. Without
the include, whether it sees it depends on the unity batch it lands in, which
failed silently once for 32 trading repositories. Codegen refuses such a model
and names it; every observed trading model includes its observer today.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_write_observer_visible.py
"""
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import codegen.core as core  # noqa: E402

TRADING = REPO_ROOT / "projects/ores.trading"
OBSERVATION = "ores.trading.core/repository/trade_write_observation.hpp"


def test_the_observer_header_names_the_observed_entities():
    observers = core._write_observers(str(TRADING))
    assert observers.get("trade_identifier") == OBSERVATION
    assert observers.get("bond_instrument") == OBSERVATION


def test_a_model_without_the_include_is_named():
    observers = {"bond_instrument": OBSERVATION}
    assert core._unseen_write_observer("no include here", "bond_instrument", observers) == OBSERVATION
    with_include = f'#include "{OBSERVATION}"'
    assert core._unseen_write_observer(with_include, "bond_instrument", observers) is None
    assert core._unseen_write_observer("no include here", "currency", observers) is None


def test_every_observed_trading_model_includes_its_observer():
    for entity, header in core._write_observers(str(TRADING)).items():
        models = [m for m in sorted((TRADING / "modeling").glob("*.org"))
                  if re.search(rf"^#\+(entity|name)_singular:\s*{entity}\s*$",
                               m.read_text(encoding="utf-8"), re.M)]
        assert models, entity
        for model in models:
            core._require_visible_write_observer(str(model), entity)


def test_codegen_refuses_a_model_that_drops_the_include(tmp_path):
    source = TRADING / "modeling/ores.trading.bond_instrument.org"
    text = source.read_text(encoding="utf-8").replace(f'#include "{OBSERVATION}"', "")
    copy = TRADING / "modeling" / ".probe.ores.trading.bond_instrument.org"
    copy.write_text(text, encoding="utf-8")
    try:
        with pytest.raises(ValueError, match="bond_instrument"):
            core._require_visible_write_observer(str(copy), "bond_instrument")
    finally:
        copy.unlink()
