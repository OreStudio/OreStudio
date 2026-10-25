"""Tests for the hypertable sql knobs.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_hypertable_shape.py

A hypertable is chunked on a column, and every unique index it carries must name
that column, so the create emits both from the same flag. A model that asks for
a hypertable and names no partition column would render a ``create_hypertable``
call with an empty name and a primary key built around it; both fail when the
schema is loaded, so the model is refused when it is read instead.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_hypertable  # noqa: E402


def model(**sql):
    return {"entity_singular": "tick", "sql": {"hypertable": True, **sql}}


def test_a_hypertable_that_names_its_partition_column_is_accepted():
    validate_hypertable(model(hypertable_partition_column="ts"))


def test_a_hypertable_with_no_partition_column_is_refused():
    with pytest.raises(ValueError, match="needs :hypertable_partition_column:"):
        validate_hypertable(model())


def test_a_table_that_is_not_a_hypertable_is_not_asked_for_one():
    validate_hypertable({"entity_singular": "tick", "sql": {}})


def test_the_tick_model_states_the_four_hypertable_knobs():
    text = (REPO_ROOT / "projects/ores.marketdata/modeling"
            / "ores.marketdata.tick.org").read_text()
    assert ":hypertable:                  true" in text
    assert ":hypertable_partition_column: ts" in text
    assert ":hypertable_chunk_interval:   1 day" in text
    assert ":hypertable_retention_interval: 7 days" in text
