"""Tests for the ``sql_only`` facet gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_sql_only_facets_gate.py

A model that states ``:sql_only: true`` is a store and nothing else: a table
that an ingest writes and a hand-written reader reads. It emits no C++ domain
type, no TypeScript twin, no shell command and no screen, because a unit
generated for it would project a table no service mentions.

Two controls keep the gate honest. A market-data entity with no such flag keeps
the whole stack, so the gate cannot pass by dropping every facet everywhere. And
the gate must not narrow the SQL side: the tick's own create file, its drop
script and the index its model declares all still resolve.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"

# The model that states the flag.
SQL_ONLY = REPO_ROOT / "projects/ores.marketdata/modeling/ores.marketdata.tick.org"

# An entity of the same component with no such flag, used as the positive
# control.
NOT_SQL_ONLY = (
    REPO_ROOT / "projects/ores.marketdata/modeling/ores.marketdata.market_fixing.org"
)

# The C++, TypeScript, shell and documentation units the gate must drop.
NON_SQL_TEMPLATES = frozenset({
    "cpp_domain_type_class.hpp.mustache",
    "cpp_domain_type_repository.hpp.mustache",
    "cpp_domain_type_mapper.hpp.mustache",
    "domain_types.ts.mustache",
    "cpp_shell_command_header.hpp.mustache",
    "shell_recipe.org.mustache",
})


def test_a_sql_only_model_resolves_only_sql_units():
    units, model_type, _ = resolve_targets(SQL_ONLY, CODEGEN_BASE)
    assert model_type == "domain_entity"
    assert units
    outside = [u["output"] for u in units
               if not u["output"].startswith("projects/ores.sql/")]
    assert not outside, f"sql_only model rendered a non-SQL unit: {outside}"


def test_a_sql_only_model_resolves_no_cpp_typescript_or_shell_unit():
    units, _, _ = resolve_targets(SQL_ONLY, CODEGEN_BASE)
    templates = {u["template"] for u in units}
    assert not (NON_SQL_TEMPLATES & templates)


def test_a_sql_only_model_keeps_its_table_and_drop_scripts():
    units, _, _ = resolve_targets(SQL_ONLY, CODEGEN_BASE)
    outputs = {u["output"] for u in units}
    assert ("projects/ores.sql/create/marketdata/marketdata_ticks_create.sql"
            in outputs)
    assert ("projects/ores.sql/drop/marketdata/marketdata_ticks_drop.sql"
            in outputs)


def test_an_entity_with_no_such_flag_keeps_the_whole_stack():
    units, _, _ = resolve_targets(NOT_SQL_ONLY, CODEGEN_BASE)
    templates = {u["template"] for u in units}
    assert NON_SQL_TEMPLATES <= templates
