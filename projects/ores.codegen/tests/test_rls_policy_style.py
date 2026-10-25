"""Tests for the rls_policy_style sql feature.

The style selects the shape of the two policies a create file ends with.
``default`` is the shape every tenant-scoped table emits and must not change;
``hot_path`` is the shape a table that filters millions of rows emits, where the
session setting is read through a scalar subquery so the planner evaluates it
once for each query rather than once for each row.

The style is a pair of policies, so the flags that name a different shape are
refused rather than emitted beside it: a model that asked for two shapes would
get one, and the other would stop appearing without a word.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_rls_policy_style.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_rls_isolation  # noqa: E402

TEMPLATE = (REPO_ROOT / "projects/ores.codegen/library/templates"
            / "sql_schema_domain_entity_create.mustache").read_text()


def model(**sql):
    return {
        "entity_singular": "tick",
        "has_tenant_id": True,
        "columns": [{"name": "party_id", "type": "uuid"}],
        "sql": {"rls_tenant_isolation": True,
                "rls_party_isolation": True,
                **sql},
    }


def hot_path_block():
    start = TEMPLATE.index("{{#domain_entity.sql.rls_hot_path}}")
    end = TEMPLATE.index("{{/domain_entity.sql.rls_hot_path}}", start)
    return TEMPLATE[start:end]


def default_block():
    start = TEMPLATE.index("{{^domain_entity.sql.rls_hot_path}}")
    end = TEMPLATE.index("{{/domain_entity.sql.rls_hot_path}}", start)
    return TEMPLATE[start:end]


def test_the_hot_path_style_is_accepted():
    validate_rls_isolation(model(rls_policy_style="hot_path"))


def test_the_default_style_is_accepted():
    validate_rls_isolation(model(rls_policy_style="default"))


def test_a_style_that_names_nothing_is_refused():
    with pytest.raises(ValueError, match="rls_policy_style must be one of"):
        validate_rls_isolation(model(rls_policy_style="fast"))


def test_the_hot_path_style_without_tenant_isolation_is_refused():
    bad = model(rls_policy_style="hot_path")
    bad["sql"]["rls_tenant_isolation"] = False
    bad["sql"]["rls_party_isolation"] = False
    with pytest.raises(ValueError, match="hot_path requires rls_tenant_isolation"):
        validate_rls_isolation(bad)


def test_the_hot_path_style_without_party_isolation_is_refused():
    bad = model(rls_policy_style="hot_path")
    bad["sql"]["rls_party_isolation"] = False
    with pytest.raises(ValueError, match="hot_path requires rls_party_isolation"):
        validate_rls_isolation(bad)


@pytest.mark.parametrize("widening", ["rls_system_tenant_visible",
                                     "rls_own_or_system_tenant_rows",
                                     "rls_service_read_role"])
def test_the_hot_path_style_refuses_a_second_policy_shape(widening):
    bad = model(rls_policy_style="hot_path")
    bad["sql"][widening] = True if widening != "rls_service_read_role" else "auditor"
    with pytest.raises(ValueError, match=f"hot_path cannot combine with {widening}"):
        validate_rls_isolation(bad)


def test_the_hot_path_style_forces_the_table_and_reads_the_setting_once():
    block = hot_path_block()
    assert "enable row level security" in block
    assert "force row level security" in block
    assert ("tenant_id = (select nullif(current_setting("
            "'app.current_tenant_id', true), '')::uuid)") in block
    assert "ores_iam_current_tenant_id_fn()" not in block


def test_the_hot_path_party_policy_is_restrictive_and_select_only():
    block = hot_path_block()
    party = block[block.index(
        "create policy {{domain_entity.rls_table_base}}_tbl_party_isolation_policy"):]
    assert "as restrictive" in party
    assert "for select using (" in party
    assert "for all" not in party
    assert "with check" not in party
    assert "ores_iam_visible_party_ids_fn() is null" not in party
    assert ("party_id in (select unnest(nullif(current_setting("
            "'app.visible_party_ids', true), '')::uuid[]))") in party


def test_the_default_style_still_emits_the_session_functions():
    block = default_block()
    assert "tenant_id = ores_iam_current_tenant_id_fn()" in block
    assert "ores_iam_visible_party_ids_fn() is null" in block
    assert "force row level security" not in block
    assert "current_setting('app.current_tenant_id'" not in block
